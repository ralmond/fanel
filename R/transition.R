expLambdaT <- function(Lambda,nmoments=10) {
  result <- diag(nrow(Lambda)) + Lambda/(2^nmoments)
  for (mm in 1L:moments)
    result <- result %*% result
  result
}



TransitionModel <- R6Class(
  classname="TransitionModel",
  inherit=FModel,  
  public = list(
    name="TransitionModel",
    continuous=FALSE,
    nmoments = 10,
    splitter = ~deltaT,
    xtime=character(),
    rmat = function(pvec=pvec(self),deltaT,covars=list()) {
      stop("No matrix definition provided for ", class(self))
    },
    tmat = function(pvec=pvec(self),deltaT,covars=list()) {
      key <- c(deltaT,as.numeric(covars[1L,self$xtime]))
      cached <- self$cache(key)$get(key)
      if (is.null(cached)) {
        cached <- expLambdaT(rmat(pvec,deltaT,covars),self$nmoments)
        self$cache(key)$assign(key,cached)
      }
      cached
    },
    advance = function(lweights,deltaT,covars=list()) {
      if (abs(deltaT) < .0001) return(lweights)
      tmat(self(pvec),deltaT,covars) %*% lweights
    },
    retreat = function(rweights,deltaT,covars=list()) {
      if (abs(deltaT) < .0001) return(rweights)
      tmat(self(pvec),deltaT,covars) %*% rweights
    },
    drawNext = function(theta,deltaT,covars=list()) {
      split(data.frame(theta=theta,deltaT=deltaT,covars,order=1L:nrow(covars)),
            self$splitter) |>
        purr::map(\(sdata) {
          G <- self$tmat(self$pvec,sdata[1,"deltaT"],sdata[1,])
          probs <- t(apply(G[dsdata$theta,],1,cumsum))
          data.frame(result=rowSums(sweep,1,runif(length(theta)),">"),
                     order=sdata$order)
        }) |> purr::list_rbind() |>
        arrange(order) |>
        select(result)
    },
    lprob=function(par=pvec(self),data) {
      self$cache$clear()
      split(data,self$splitter) |>
        purr::map_dbl(\(sdata) self$lpinner(par,sdata)) |>
        purr::reduce("+")
    },
    lpinner = function(par=pvec(self),data) {
      G = self$tmat(par,data[1,"deltaT"],data[1,])
      split(data,~subj) |>
        purr::map_dbl(\(sdata) {
          lweight=sdata[[self$wname[2]]]
          rweight=sdata[[self$wname[3]]]
          sum(lweight*log(rweight%*%G))
        }) |> purr::reduce("+")
    },
    fillCache = function(par=pvec(self),data) {
      self$cache$clear()
      split(data,self$splitter) |>
        purr::walk(\(sdata) {
          self$tmat(par,data[1,"deltaT"],data[1,])
        })
    }
  ),
  private=list(
    acache=NULL
  ),
  active=list(
    cache=function(key) {
      if (is.null(private$acache)) {
        private$acache <- memoTree$new(length(key))
      }
      private$acache
    }
  )
)

UpDownGrowth <- R6Class(
  classname="UpDownGrowth",
  inherit=GrowthModelC,
  public=list(
    initialize = function(name,nStates,uprate,downrate,
                          tname="theta", wname="w",
                          xTime="xTime") {
      self$name <- name
      self$nStates <- nStates
      self$uprate <- uprate
      self$downrate <- downrate
      self$tnames <- tname
      self$wname <- wname
      self$xtime <- xTime
    },
    uprate=0,
    downrate=0,
    xtime=character()
    nStates=2,
    rmat = function(pvec=pvec(self),deltaT,covar) {
      if (length(self$xtime)==0L) {
        xTime <- deltaT
      } else {
        xTime <- covar[1L,self$xtime]
      }
      up <- exp(pvec[1:(self$nStates-1)])
      down <- exp(pvec[self$nStates:length(pvec)])
      matR <- matrix(0,self$nStates,self$nStates)
      mgcv::sdiag(matR,1) <- up*xTime
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

ActivitiesD <- R6Class(
  "ActivitiesD",
  inherit=Activities,
  public=list(
    advance = function(subj,it,lweights,deltaT,covar=NULL) {
      self$models[[self$action(subj,it)]]$
        advance(lweights,deltaT,covar)
    },
    retreat = function(subj,it,rweights,deltaT,covar=NULL) {
      self$models[[self$action(subj,it)]]$
        retreat(rweights,deltaT,covar)
    },
    tmat = function(subj,it,deltaT,covar) {
      mod <- self$models[[self$action(subj,it)]]
      mod$tmat(pvec(mod),deltaT,covar)
    },
    fillCache = function(data,workers=Workers$new()) {
      workers$start()
      workers$lapply(unique(data$action), \(act) {
        mod <- self$growthModels[[act]]
        mod$fillCache(pvec(mod),select(data,action==act))
      })
    },
    mstep = function(data,its=3,control=list(),workers=Workers$new()) {
      data <- dplyr::arrange(data,subj,occ) |> dplyr::group_by(subj)
      sapply(self$tnames, \(th) {
        th1 <- paste0(th,"_1")
        data <- dplyr::mutate(data,"{th1}":=lag(.data[[th]]))
        th1
      })
      super$mstep(data,its,control,workers)
    }
  ),
  active=list(
    nmoments = function(value) {
      if (length(self$models)==0L) {
        warn("No models defined.")
        return(integer())
      }
      if (missing(value)) return(self$models[[1]]$nmoments)
      lapply(self$models, \(m) m$nmoments <- value)
    }
  )
)

