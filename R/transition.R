expLambdaT <- function(Lambda,nmoments=10) {
  result <- diag(nrow(Lambda)) + Lambda/(2^nmoments)
  for (mm in 1L:moments)
    result <- result %*% result
  result
}



GrowthModelD <- R6Class(
  classname="GrowthModelD",
  inherit=FModel,  
  public = list(
    name="GrowthModel",
    continuous=FALSE,
    nmoments = 10,
    splitter = ~deltaT,
    rmat = function(pvec=pvec(self),deltaT,covars=list()) {
      stop("No matrix definition provided for ", class(self))
    },
    tmat = function(pvec=pvec(self),deltaT,covars=list()) {
      expLambdaT(rmat(pvec,deltaT,covars),self$nmoments)
    },
    advance = function(weights,deltaT,covars=list()) {
      if (abs(deltaT) < .0001) return(weights)
      weights %*% tmat(self(pvec),deltaT,covars)
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
    }
  )
)

UpDownGrowth <- R6Class(
  classname="UpDownGrowth",
  inherit=GrowthModelC,
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
    rmat = function(pvec=pvec(self),deltaT,covar) {
      up <- exp(value[1:(self$nStates-1)])
      down <- exp(value[self$nStates:length(value)])
      matR <- matrix(0,self$nStates,self$nStates)
      mgcv::sdiag(matR,1) <- up*deltaT/2^self$v
      mgcv::sdiag(matR,-1) <- down*deltaT/2^self$v
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
    advance = function(subj,it,weights,deltaT,covar=NULL) {
      self$models[[self$action(subj,it)]]$
        advance(weights,deltaT,covar)
    },
    rmat = function(subj,it,deltaT,covar) {
      mod <- self$models[[self$action(subj,it)]]
      mod$rmat(pvec(rmat),deltaT,covar)
    },
    tmat = function(subj,it,deltaT,covar) {
      mod <- self$models[[self$action(subj,it)]]
      mod$rmat(pvec(rmat),deltaT,covar)
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
    },
  )
)

