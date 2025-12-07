GrowthModel <- R6Class(
  classname="GrowthModel",
  inherit=FModel,
  public = list(
    name="GrowthModel",
    dosname="dose",
    dtname="deltaT",
    continuous=TRUE,
    drawNext = function(theta,deltaT,dose=deltaT,covars=list()) {
      stop("DrawNext not implemented for ", class(self))
    }
  )
)


BrownianGrowth <- R6Class(
  classname="BrownianGrowth",
  inherit=GrowthModel,
  public=list(
    initialize = function(name,gain,inovSD,tname="theta",wname="w") {
      self$name <- name
      self$gain <- gain
      self$inovSD <- inovSD
      self$tnames <- tname
      self$wname <- wname
    },
    gain=0,
    inovSD=.1,
    drawNext = function(theta,deltaT,dose=deltaT,covars=list()) {
      rnorm(length(theta),theta+self$gain*dose,
            self$inovSD*sqrt(deltaT))
    },
    lprob = function(data,par=self$pvec) {
      deltaT <- data[[self$dtname]]
      dose <- data[[self$dosname]]
      if (is.null(dose)) dose <- deltaT
      theta0 <- data[[self$tnames]]
      theta1 <- data[[paste0(self$tnames,"_1")]]
      weights <- data[[self$wname[1]]]
      mu <- theta0 + par[1]*dose
      sig <- exp(par[2])*sqrt(deltaT)
      sum(dnorm(theta1,mu,sig,log=TRUE)*weights)
    },
    mstep = function(data) {
      deltaT <- data[[self$dtname]]
      dose <- data[[self$dosname]]
      if (is.null(dose)) dose <- deltaT
      theta0 <- data[[self$tnames]]
      theta1 <- data[[paste0(self$tnames),"_1"]]
      weights <- data[[self$wname[1]]]
      self$gain <- wtd.mean((theta1-theta0)/dose,weights)
      self$inovSD <- wtd.sd((theta1-theta0-self$gain)/sqrt(deltaT),
                             weights)
      lprob(data=data)
    },
    toString=function(digits=2,...) {
      paste0("<BrownianGrowth: ", self$name, " ( ",
             round(self$gain,digits=digits),
             ", ",round(self$inovSD,digits=digits)," )>")
    }
  ),
  active=list(
    pvec = function(value) {
      if (missing(value)) return(c(self$gain,log(self$inovSD)))
      self$gain <- value[1]
      self$inovSD <- exp(value[2])
    }
  )
)


Activities <- R6Class(
  "Activities",
  inherit=ModelSet,
  public=list(
    name="ActitivitySet",
    dt=as.Panmat(1),
    dosage=NULL,
    initialize=function(name,actions=1L,dt=1.0,
                        growthModels=list(), dosage=NULL,
                        tname="theta",wname="w",
                        dtname="deltaT",dosname="dose") {
      self$name <- name
      self$index <- as.Panmat(actions)
      self$models <- growthModels
      self$tnames <- tname
      self$wname <- wname
      self$dt <- as.Panmat(dt)
      if (!is.na(dosage)) self$dosage <- as.Panmat(dosage)
      self$dosname <- dosname
      nsubj(self) <- max(self$index$nsubj,self$dt$nsubj,self$dosage$nsubj)
      minocc(self) <- max(self$index$minocc,self$dt$minocc,self$dosage$minocc)
      maxocc(self) <- max(self$index$maxocc,self$dt$maxocc,self$dosage$maxocc)
    },
    drawNext = function(isubj, iocc, theta, covar=NULL) {
      deltaT <- self$deltaT(isubj,iocc)
      if (abs(deltaT)<.0001) {
        return(theta)
      }
      dose <-self$dose(isubj,iocc)
      self$models[[self$action(isubj,iocc)]]$
        drawNext(theta,deltaT,dose,covar)
    },
    mstep = function(data,its=3,control=list(),workers=Workers$new()) {
      workers$start()
      workers$lapply(unique(data$action), \(act) {
        mstep(self$growthModels[[act]], dplyr::filter(data,action==act),
              its=its,control=control,workers=NULL)
      })
    },
    toString=function(...) {
      paste0("<Activities: ",self$name,": ",
             self$nsubjects, " x ",
             self$macocc, " >")
    },
    action = function(subj,iocc) {
      self$index[subj,iocc]
    }
  ),
  active=list(
    dose = function(subj,iocc,value) {
      if (is.null(self$dosage)) return(self$deltaT(subj,iocc))
      if (missing(value)) return(self$dosage[subj,iocc])
      self$dosage[self,iocc] <- value
    },
    deltaT = function(subj,iocc,value) {
      if (missing(value)) return(self$dt[subj,iocc])
      self$dt[self,iocc] <- value
    },
    dtname = function(value) {
      if (missing(value)) self$models[[1L]]$dtname
      purrr:::walk(self$models, \(mod) mod$dtname <- value)
    },
    dosname = function(value) {
      if (missing(value)) self$models[[1L]]$dosname
      purrr:::walk(self$models, \(mod) mod$dosname <- value)
    }
  )
)

setOldClass("Activities")

setMethod("nsubj<-","Activities", function(obj,value) {
  nsubj(obj$index) <-as.integer(value)
  nsubj(obj$dt) <- as.integer(value)
  if (!is.null(obj$dosage)) nsubj(obj$dosage) <- as.integer(value)
  obj
})


setMethod("nocc<-","Activities", function(obj,value) {
  nocc(obj$index) <- as.integer(value)
  nocc(obj$dt) <- as.integer(value)
  if (!is.null(obj$dosage)) nocc(obj$dosage) <- as.integer(value)
  obj
})

setMethod("maxocc<-","Activities", function(obj,value) {
  maxocc(obj$index) <- value
  maxocc(obj$dt) <- as.integer(value)
  if (!is.null(obj$dosage)) maxocc(obj$dosage) <- as.integer(value)
  obj
})


setMethod("minocc<-","Activities", function(obj,value) {
  minocc(obj$index) <- as.integer(value)
  minocc(obj$dt) <- as.integer(value)
  if (!is.null(obj$dosage)) minocc(obj$dosage) <- as.integer(value)
  obj
})

setMethod("drawGrowth", "Activities",
           function(model, isubj, iocc, theta, covar=NULL) {
  model$drawNext(isubj, iocc, theta, covar=NULL)
})



setMethod("as_longform","Activities",
          function(x,n=nsubj(x),maxocc=nocc(x),
                   minocc=1L,weightType="all",
                   name=deparse(substitute(x))) {
  as_longform(x$index,n=n,maxocc=maxocc,minocc=minocc,
              weightType=weightType,name="action") |>
    dplyr::left_join(
               as_longform(x$dt,n=n,maxocc=maxocc,minocc=minocc,
                           weightType=weightType,name="deltaT"),
               dplyr::join_by("subj","occ")) ->
      result
  if (!is.null(x$dosage))
    dplyr::left_join(result,
                     as_longform(x$dosage,n=n,maxocc=maxocc,minocc=minocc,
                                 weightType=weightType,name="dose"),
                     dplyr::join_by("subj","occ")) ->
      result
  result
          })


