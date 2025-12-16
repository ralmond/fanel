expLambdaT <- function(Lambda,nmoments=10) {
  result <- diag(nrow(Lambda)) + Lambda/(2^nmoments)
  for (mm in 1L:nmoments)
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
    splitter = ~deltaT+dose,
    xtime=character(),
    rmat = function(pvec=pvec(self),deltaT,dose=deltaT,covars=list()) {
      stop("No matrix definition provided for ", class(self))
    },
    tmat = function(pvec=pvec(self),deltaT,dose=deltaT,covars=list()) {
      key <- c(deltaT,as.numeric(covars[1L,self$xtime]))
      cached <- self$cache(key)$get(key)
      if (is.null(cached)) {
        cached <- expLambdaT(rmat(pvec,deltaT,dose,covars),self$nmoments)
        self$cache(key)$assign(key,cached)
      }
      cached
    },
    advance = function(lweights,deltaT,dose=deltaT,covars=list()) {
      if (abs(deltaT) < .0001) return(lweights)
      tmat(self(pvec),deltaT,dose,covars) %*% lweights
    },
    retreat = function(rweights,deltaT,dose=deltaT,covars=list()) {
      if (abs(deltaT) < .0001) return(rweights)
      tmat(self(pvec),deltaT,dose,covars) %*% rweights
    },
    drawNext = function(theta,deltaT,dose=deltaT,covars=list()) {
      split(data.frame(theta=theta,deltaT=deltaT,
                       dose=dose,covars,order=1L:nrow(covars)),
            self$splitter) |>
        purrr::map(\(sdata) {
          G <- self$tmat(self$pvec,sdata[1,"deltaT"],sdata[1,"dose"],
                         sdata[1,])
          probs <- t(apply(G[sdata$theta,],1,cumsum))
          data.frame(result=rowSums(outer(runif(length(theta)),">",probs)),
                     order=sdata$order)
        }) |> purrr::list_rbind() |>
        dplyr::arange(order) |>
        dplyr::select(result)
    },
    lprob=function(data,par=pvec(self)) {
      self$cache$clear()
      split(data,self$splitter) |>
        purrr::map_dbl(\(sdata) self$lpinner(sdata,par)) |>
        purrr::reduce("+")
    },
    lpinner = function(data,par=pvec(self)) {
      G = self$tmat(par,data[1,"deltaT"],data[1,])
      split(data,~subj) |>
        purrr::map_dbl(\(sdata) {
          lweight=sdata[[self$wname[2]]]
          rweight=sdata[[self$wname[3]]]
          sum(lweight*log(rweight%*%G))
        }) |> purrr::reduce("+")
    },
    fillCache = function(data,par=pvec(self)) {
      self$cache$clear()
      split(data,self$splitter) |>
        purrr::walk(\(sdata) {
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

setOldClass(c("TransitionModel","FModel"))


UpDownGrowth <- R6Class(
  classname="UpDownGrowth",
  inherit=TransitionModel,
  public=list(
    initialize = function(name,nStates,uprate,downrate,
                          tname="theta", wname="w") {
      self$name <- name
      self$nStates <- nStates
      self$uprate <- uprate
      self$downrate <- downrate
      self$tnames <- tname
      self$wname <- wname
    },
    uprate=0,
    downrate=0,
    nStates=2,
    rmat = function(pvec=pvec(self),deltaT,dose=deltaT,covar) {
      up <- exp(pvec[1:(self$nStates-1)])
      down <- exp(pvec[self$nStates:length(pvec)])
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


ActivitiesD <- R6Class(
  "ActivitiesD",
  inherit=Activities,
  public=list(
    advance = function(isubj,iocc,lweights,covar=NULL) {
      self$models[[self$action(isubj,iocc)]]$
        advance(lweights,self$deltaT(isubj,iocc),self$dose(isubj,iocc),covar)
    },
    retreat = function(isubj,iocc,rweights,covar=NULL) {
      self$models[[self$action(isubj,iocc)]]$
        retreat(rweights,self$deltaT(isubj,iocc),self$dose(isubj,iocc),covar)
    },
    tmat = function(isubj,iocc,covar) {
      mod <- self$models[[self$action(isubj,iocc)]]
      mod$tmat(pvec(mod),self$deltaT(isubj,iocc),self$dose(isubj,iocc),covar)
    },
    fillCache = function(data,workers=Workers$new()) {
      data <- dplyr::left_join(as_longform(self),data,
                               dplyr::join_by("self","occ"))
      workers$start()
      workers$lapply(unique(data$action), \(act) {
        mod <- self$growthModels[[act]]
        mod$fillCache(pvec(mod),dplyr::filter(data,action==act))
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


