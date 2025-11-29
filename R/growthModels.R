GrowthModel <- R6Class(
  classname="GrowthModel",
  inherit=FModel,
  public = list(
    name="GrowthModel",
    continuous=TRUE,
    drawNext = function(theta,deltaT,covars=list()) {
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
      self$wnames <- wname
    },
    gain=0,
    inovSD=.1,
    drawNext = function(theta,deltaT,covars=list()) {
      rnorm(length(theta),theta+self$gain*deltaT,
            self$inovSD*sqrt(deltaT))
    },
    lprob = function(par=self$pvec,data) {
      deltaT <- data$deltaT
      theta0 <- data[[self$tnames]]
      theta1 <- data[[paste0(self$tnames),"_1"]]
      weights <- data[[self$wnames]]
      mu <- theta0+par[1]*deltaT
      sig <- exp(par[2])*sqrt(deltaT)
      sum(dnorm(theta1,mu,sig,log=TRUE)*weights)
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


SpurtGrowth <- R6Class(
  classname="SpurtGrowth",
  inherit=GrowthModel,
  public=list(
    initialize = function(name,gain0,gain1,p,inovSD,tname="theta",wname="w") {
      self$name <- name
      self$gain0 <- gain0
      self$gain1 <- gain1
      self$p <- p
      self$inovSD <- inovSD
      self$tnames <- tname
      self$wname <- wname
    },
    gain0=0,
    gain1=1,
    p=.5,
    inovSD=.1,
    drawNext = function(theta,deltaT,covars=list()) {
      mu <- ifelse(rbinom(length(theta),size=1, p=self$p),
                   self$gain1,self$gain0)*deltaT+
            theta
      sig <- self$inovSD*sqrt(deltaT)
      rnorm(length(theta),mu,sig)
    },
    lprob = function(par=self$pvec,data) {
      deltaT <- data$deltaT
      theta0 <- data[[self$tnames]]
      theta1 <- data[[paste0(self$tnames),"_1"]]
      weights <- data[[self$wnames]]
      mu0 <- theta0+par[1]*deltaT
      mu1 <- theta0+par[2]*deltaT
      p <- invlogit(par[3])
      sig <- exp(par[4])*sqrt(deltaT)
      sum(log((1-p)*dnorm(theta1,mu0,sig) +
              p*dnorm(theta1,mu1,sig)
              )*weights)
    },
    toString=function(digits=2,...) {
      paste0("<SpurtGrowth: ", self$name, " ( ",
             round(self$gain0,digits=digits), ",",
             round(self$gain1,digits=digits), ",",
             round(self$p,digits=digits), ",",
             round(self$inovSD,digits=digits)," )>")
    }
  ),
  active=list(
    pvec = function(value) {
      if (missing(value)) return(c(self$gain0,self$gain1,
                                   logit(self$p),log(self$inovSD)))
      self$gain0 <- value[1]
      self$gain1 <- value[2]
      self$p <- invlogit(value[3])
      self$inovSD <- exp(value[4])
    }
  )
)


Activities <- R6Class(
  "Activities",
  inherit=ModelSet,
  public=list(
    name="ActitivitySet",
    initialize=function(name,actions=1L,
                        growthModels=list(),
                        tname="theta",wname="w") {
      self$name <- name
      self$index <- as.Panmat(actions)
      self$models <- growthModels
      self$tnames <- tname
      self$wname <- wname
    },
    drawNext = function(subj,it,theta,deltaT,covar=NULL) {
      if (deltaT==0) {
        return(theta)
      } else {
        self$models[[self$action(subj,it)]]$
          drawNext(theta,deltaT,covar)
      }
    },
    mstep = function(data,its=3,control=list(),workers=Workers$new()) {
      workers$start()
      workers$lapply(unique(data$action), \(act) {
        mstep(self$growthModels[[act]], dplyr::filter(data,action==act),
              its=its,control=control,workers=NULL)
      })
    },
    toString=function(...) {
      paste0("<Actions: ",self$name,": ",
             self$nsubjects, " x ",
             self$macocc, " >")
    },
    action = function(subj,it) {
      self$index[subj,it]
    }
  )
)

setOldClass("Activities")

setMethod("as_longform","Activities",
          function(x,n=nsubj(x),maxocc=nocc(x),
                   minocc=1L,weightType="all",
                   name=deparse(substitute(x))) {
  as_longform(x$index,n=n,maxocc=maxocc,minocc=minocc,
              weightType=weightType,name="action")
          })
