### TransitionModel ----

expLambdaT <- function(Lambda,nmoments=10) {
  result <- diag(nrow(Lambda)) + Lambda/(2^nmoments)
  for (mm in 1L:nmoments)
    result <- result %*% result
  result
}



TransitionModel <- R6Class(
  classname="TransitionModel",
  inherit=GrowthModel,
  public = list(
    name="TransitionModel",
    continuous=FALSE,
    nmoments = 10,
    splitter = ~deltaT+dose,
    xtime=character(),
    nStates=2,
    rmat = function(pv=pvec(self),deltaT,dose=deltaT,covars=list()) {
      stop("No matrix definition provided for ", class(self))
    },
    tmat = function(pv=pvec(self),deltaT,dose=deltaT,covars=list()) {
      if (length(self$xtime) > 0) {
        key <- c(deltaT,dose,as.numeric(covars[1L,self$xtime]))
      } else {
        key <- c(deltaT,dose)
      }
      cached <- self$cacheGet(key)
      if (is.null(cached)) {
        cached <- expLambdaT(self$rmat(pv,deltaT,dose,covars),self$nmoments)
        self$cacheSet(key,cached)
      }
      cached
    },
    advance = function(lweights,deltaT,dose=deltaT,covars=list()) {
      if (abs(deltaT) < .0001) return(lweights)
      self$tmat(pvec(self),deltaT,dose,covars) %*% lweights
    },
    retreat = function(rweights,deltaT,dose=deltaT,covars=list()) {
      if (abs(deltaT) < .0001) return(rweights)
      rweights %*% self$tmat(pvec(self),deltaT,dose,covars)
    },
    drawNext = function(theta,deltaT,dose=deltaT,covars=list()) {
      tnames <- names(theta)
      if (is.null(tnames)) tnames <- 1L
      if (length(covars)==0L) {
        df <- data.frame(theta,deltaT=deltaT,dose=dose)
      } else {
        df <- data.frame(theta,deltaT=deltaT,dose=dose,covars)
      }
      df$order <- 1L:nrow(df)
      split(df, self$splitter) |>
        purrr::map(\(sdata) {
          G <- self$tmat(self$pvec,sdata[1,"deltaT"],sdata[1,"dose"],
                         sdata[1,])
          probs <- t(apply(G[sdata[,tnames],],1,\(r) {
            rev(cumsum(rev(r)))
          }))
          result <- sweep(probs,1,runif(nrow(sdata)),">")
          data.frame(result=rowSums(result),order=sdata$order)
        }) |> purrr::list_rbind() |>
        dplyr::arrange(order) |>
        dplyr::pull(result)
    },
    lprob=function(data,par=pvec(self)) {
      self$cacheClear()
      split(data,self$splitter) |>
        purrr::map_dbl(\(sdata) self$lpinner(sdata,par)) |>
        sum()
    },
    lpinner = function(data,par=pvec(self)) {
      G = self$tmat(par,data[1,"deltaT"],data[1,"dose"],data[1,])
      split(data,~subj+occ) |>
        purrr::map_dbl(\(sdata) {
          lweight=sdata[[self$wname[2]]]
          rweight=sdata[[self$wname[3]]]
          sum(lweight*log(rweight%*%G))
        }) |> sum()
    },
    fillCache = function(data,par=pvec(self)) {
      self$cacheClear()
      if (is.na(match("dose",names(data))))
        data$dose <- data$deltaT
      data <- model.frame(self$splitter,data)
      dd <- match(c("deltaT","dose"),names(data))
      split(data,self$splitter) |>
        purrr::walk(\(sdata) {
          self$tmat(par,sdata[1,"deltaT"],sdata[1,"dose"],sdata[1,-dd])
        })
    },
    cacheGet=function(key) {
      if (is.null(private$acache)) return(NULL)
      private$acache$get(key)
    },
    cacheSet=function(key,value) {
      if (is.null(private$acache))
        private$acache <- memoTree$new(length(key))
      private$acache$assign(key,value)
    },
    cacheClear=function() {
      if (!is.null(private$acache))
        private$acache$clear()
    }
  ),
  active=list(
    cache=function() {
      private$acache
    }
  ),
  private=list(
    acache=NULL
  )
)

setOldClass(c("TransitionModel","FModel"))

### UpDownGrowth ----

UpDownGrowth <- R6Class(
  classname="UpDownGrowth",
  inherit=TransitionModel,
  public=list(
    initialize = function(name,nStates,uprate,downrate,
                          qname="theta",
                          wname=c("w.full", "w.left", "w.right")) {
      self$name <- name
      self$nStates <- nStates
      self$uprate <- uprate
      self$downrate <- downrate
      self$qnames <- qname
      self$wname <- wname
    },
    uprate=0,
    downrate=0,
    rmat = function(pv=pvec(self),deltaT,dose=deltaT, covar=list()) {
      up <- exp(pv[1:(self$nStates-1)])
      down <- exp(pv[self$nStates:length(pv)])
      matR <- matrix(0,self$nStates,self$nStates)
      mgcv::sdiag(matR,1) <- up*dose
      mgcv::sdiag(matR,-1) <- down*deltaT
      diag(matR) <- -rowSums(matR)
      matR
    },
    toString=function(digits=2,...) {
      paste0("<UpDownGrowth: ", self$name, " ( ",
             paste(round(self$uprate,digits=digits),
                   collapse = ", "),
             "; ",
             paste(round(self$downrate,digits=digits),
                   collapse= ", "),
             " )>")
    }
  ),
  active=list(
    pvec = function(value) {
      if (missing(value)) return(log(c(self$uprate,self$downrate)))
      if (length(value) == 2L) {
        self$uprate <- exp(value[1])
        self$downrate <- exp(value[2])
      } else {
        self$uprate <- exp(value[1:(self$nStates-1)])
        self$downrate <- exp(value[self$nStates:length(value)])
      }
    }
  )
)

setOldClass(c("UpDownGrowth","TransitionModel","FModel"))


### ActivitiesD ----

ActivitiesD <- R6Class(
  "ActivitiesD",
  inherit=Activities,
  public=list(
    initialize=function(name,growthModels=list(),actions=1L,dt=1.0,
                        dosage=NULL,qname="theta",
                        wname=c("w.full", "w.left", "w.right"),
                        dtname="deltaT",dosname="dose") {
      super$initialize(name=name,growthModels=growthModels,
                       actions=actions,dt=dt,dosage=dosage,
                       qname=qname,wname=wname,dtname=dtname,
                       dosname=dosname)
    },
    advance = function(isubj,iocc,lweights,covar=NULL) {
      self$models[[self$action(isubj,iocc)]]$
        advance(lweights,self$deltaT(isubj,iocc),self$dose(isubj,iocc),covar)
    },
    retreat = function(isubj,iocc,rweights,covar=NULL) {
      self$models[[self$action(isubj,iocc)]]$
        retreat(rweights,self$deltaT(isubj,iocc),self$dose(isubj,iocc),covar)
    },
    tmat = function(isubj,iocc,covar=list()) {
      mod <- self$models[[self$action(isubj,iocc)]]
      mod$tmat(pvec(mod),self$deltaT(isubj,iocc),self$dose(isubj,iocc),covar)
    },
    fillCache = function(data) {
      data <- dplyr::left_join(as_longform(self),data,
                               dplyr::join_by("subj","occ"))
      lapply(unique(data$action), \(act) {
        mod <- self$models[[act]]
        mod$fillCache(dplyr::filter(data,action==act),pvec(mod))
      })
    },
    prepData = identity
  ),
  active=list(
    nmoments = function(value) {
      if (length(self$models)==0L) {
        warn("No models defined.")
        return(integer())
      }
      if (missing(value)) return(self$models[[1]]$nmoments)
      lapply(self$models, \(m) m$nmoments <- value)
    },
    nstates = function(value) {
      if (length(self$models)==0L) {
        warn("No models defined.")
        return(integer())
      }
      if (missing(value)) return(self$models[[1]]$nstates)
      lapply(self$models, \(m) m$nstates <- value)
    }

  )
)

setOldClass(c("ActivitiesD","Activities","ModelSet"))

"advanceWeights.ActivitiesD" <-
          function(model, isubj, iocc, lweights, covar=NULL) {
  model$advance(isubj,iocc,lweights,covar)
}

"retreatWeights.ActivitiesD" <-
          function(model, isubj, iocc, rweights, covar=NULL) {
  model$retreat(isubj,iocc,rweights,covar)
}


