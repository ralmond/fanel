### GrowthModel ----

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

setOldClass(c("GrowthModel","FModel"))

### BrownianGrowth ----

BrownianGrowth <- R6Class(
  classname="BrownianGrowth",
  inherit=GrowthModel,
  public=list(
    initialize = function(name,gain,inovSD,qname="theta",wname="w",
                          dtname="deltaT", dosname="dose") {
      self$name <- name
      self$gain <- gain
      self$inovSD <- inovSD
      self$qnames <- qname
      self$wname <- wname
      self$dtname <- dtname
      self$dosname <- dosname
    },
    gain=0,
    inovSD=.1,
    drawNext = function(theta,deltaT,dose=deltaT,covars=NULL) {
      rnorm(length(theta),theta,self$inovSD*sqrt(deltaT)) +
        dose*self$gain
    },
    lprob = function(data,par=self$pvec) {
      deltaT <- data[[self$dtname]]
      dose <- data[[self$dosname]]
      if (is.null(dose)) dose <- deltaT
      theta0 <- data[[self$qnames]]
      theta1 <- data[[paste0(self$qnames,"_1")]]
      weights <- data[[self$wname[1]]]
      mu <- theta0 + par[1]*dose
      sig <- exp(par[2])*sqrt(deltaT)
      sum(dnorm(theta1,mu,sig,log=TRUE)*weights)
    },
    mstep = function(data,...) {
      deltaT <- data[[self$dtname]]
      dose <- data[[self$dosname]]
      if (is.null(dose)) dose <- deltaT
      theta0 <- data[[self$qnames]]
      theta1 <- data[[paste0(self$qnames,"_1")]]
      weights <- data[[self$wname[1]]]
      inov <- wtd.mean((theta1-theta0)/dose,weights)
      self$gain <- inov
      self$inovSD <- wtd.sd((theta1-theta0-inov*dose)/sqrt(deltaT),weights)
      self$convergence <- TRUE
      self$lp <- self$lprob(data)
      self
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

setOldClass(c("BrownianGrowth","GrowthModel","FModel"))

### BrownianGrowth2 ----

BrownianGrowth2 <- R6Class(
  classname="BrownianGrowth2",
  inherit=BrownianGrowth,
  public=list(
    initialize = function(name,gain,loss,inovSD,qname="theta",wname="w",
                          dtname="deltaT", dosname="dose") {
      self$name <- name
      self$gain <- gain
      self$loss <- loss
      self$inovSD <- inovSD
      self$qnames <- qname
      self$wname <- wname
      self$dtname <- dtname
      self$dosname <- dosname
    },
    loss=0,
    drawNext = function(theta,deltaT,dose=deltaT,covars=NULL) {
      rnorm(length(theta),theta,self$inovSD*sqrt(deltaT)) +
        dose*self$gain - deltaT*self$loss
    },
    lprob = function(data,par=self$pvec) {
      deltaT <- data[[self$dtname]]
      dose <- data[[self$dosname]]
      if (is.null(dose)) dose <- deltaT
      theta0 <- data[[self$qnames]]
      theta1 <- data[[paste0(self$qnames,"_1")]]
      weights <- data[[self$wname[1]]]
      mu <- theta0 + par[1]*dose -par[2]*deltaT
      sig <- exp(par[3])*sqrt(deltaT)
      sum(dnorm(theta1,mu,sig,log=TRUE)*weights)
    },
    mstep = function(data,...) {
      deltaT <- data[[self$dtname]]
      dose <- data[[self$dosname]]
      if (is.null(dose)) {
        dose <- deltaT
        regress <- FALSE
      } else {
        regress <-
          svd(cbind(deltaT,dose))$d[2] > .001
      }
      theta0 <- data[[self$qnames]]
      theta1 <- data[[paste0(self$qnames,"_1")]]
      weights <- data[[self$wname[1]]]
      if (regress) {
        coef <- lm(y~dt+ds-1,
                   data.frame(y=theta1-theta0,
                              dt=deltaT,
                              ds=dose),
                   weights=weights) |> coef()
        self$gain <- coef[2]
        self$loss <- -coef[1]
      } else {
        inov <- theta1-theta0-self$loss*deltaT
        self$gain <- wtd.mean(inov/dose,weights)
      }
      einov <- theta1-theta0-self$gain*dose+self$loss*deltaT
      self$inovSD <- wtd.sd(einov/sqrt(deltaT),
                            weights)
      self$convergence <- TRUE
      self$lp <- self$lprob(data)
      self
    },
    toString=function(digits=2,...) {
      paste0("<BrownianGrowth2: ", self$name, " ( ",
             paste(
                 round(c(self$gain,self$loss,self$inovSD),
                       digits=digits),
                 collapse=", "), " )>")
    }
  ),
  active=list(
    pvec = function(value) {
      if (missing(value)) return(c(self$gain,self$loss,log(self$inovSD)))
      self$gain <- value[1]
      self$loss <- value[2]
      self$inovSD <- exp(value[3])
    }
  )
)


### Activities ----

Activities <- R6Class(
  "Activities",
  inherit=ModelSet,
  public=list(
    name="ActitivitySet",
    iname="action",
    dt=as.Panmat(1),
    dosage=NULL,
    initialize=function(name,growthModels=list(),actions=1L,dt=1.0,
                        dosage=NULL,qname="theta",wname="w",
                        dtname="deltaT",dosname="dose") {
      self$name <- name
      self$index <- as.Panmat(actions)
      self$models <- growthModels
      self$qnames <- qname
      self$wname <- wname
      self$dt <- as.Panmat(dt)
      if (!is.null(dosage)) self$dosage <- as.Panmat(dosage)
      self$dosname <- dosname
      nsubj(self) <- max(nsubj(self$index),nsubj(self$dt),nsubj(self$dosage))
      minocc(self) <- min(minocc(self$index),minocc(self$dt),minocc(self$dosage))
      maxocc(self) <- max(maxocc(self$index),maxocc(self$dt),maxocc(self$dosage))
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
    prepData = function(data) {
      data <- dplyr::arrange(data,subj,occ,quad) |>
        dplyr::group_by(subj)
      nq <- length(unique(data$quad))
      for(th in self$qnames) {
        th1 <- paste0(th,"_1")
        data <- dplyr::mutate(data,"{th1}":=dplyr::lead(.data[[th]],nq))
      }
      dplyr::filter(data,!is.na(.data[[th1]]))
    },
    toString=function(...) {
      paste0("<",class(self)[1],": ",self$name,": ",
             nsubj(self), " x ",
             maxocc(self), " >")
    },
    action = function(isubj,iocc,value) {
      if (missing(value)) return(self$index[isubj,iocc])
      self$index[isubj,iocc] <- value
    },
    dose = function(isubj,iocc,value) {
      if (is.null(self$dosage)) {
        if (!missing(value)) {
          stop("Trying to set a NULL dosage")
        } else {
          return(self$deltaT(isubj,iocc))
        }
      }
      if (missing(value)) return(self$dosage[isubj,iocc,drop=TRUE])
      self$dosage[isubj,iocc] <- value
    },
    deltaT = function(isubj,iocc,value) {
      if (missing(value)) return(self$dt[isubj,iocc,drop=TRUE])
      self$dt[isubj,iocc] <- value
    }
  ),
  active=list(
    dtname = function(value) {
      if (missing(value)) return(self$models[[1L]]$dtname)
      lapply(self$models, \(mod) {mod$dtname <- value})
    },
    dosname = function(value) {
      if (missing(value)) return(self$models[[1L]]$dosname)
      lapply(self$models, \(mod) {mod$dosname <- value})
    }
  )
)

setOldClass(c("Activities","ModelSet"))

getDT.Activities <- function (obj) {obj$dt}
"getDT<-.Activities" <- function (obj,value) {obj$dt <-value}


"nsubj<-.Activities" <- function(obj,value) {
  nsubj(obj$index) <-as.integer(value)
  nsubj(obj$dt) <- as.integer(value)
  if (!is.null(obj$dosage)) nsubj(obj$dosage) <- as.integer(value)
  obj
}


"nocc<-.Activities" <- function(obj,value) {
  nocc(obj$index) <- as.integer(value)
  nocc(obj$dt) <- as.integer(value)
  if (!is.null(obj$dosage)) nocc(obj$dosage) <- as.integer(value)
  obj
}

"maxocc<-.Activities" <- function(obj,value) {
  maxocc(obj$index) <- value
  maxocc(obj$dt) <- as.integer(value)
  if (!is.null(obj$dosage)) maxocc(obj$dosage) <- as.integer(value)
  obj
}


"minocc<-.Activities" <- function(obj,value) {
  minocc(obj$index) <- as.integer(value)
  minocc(obj$dt) <- as.integer(value)
  if (!is.null(obj$dosage)) minocc(obj$dosage) <- as.integer(value)
  obj
}

drawGrowth.Activities <- function(model, isubj, iocc, theta, covar=NULL) {
  model$drawNext(isubj, iocc, theta, covar=NULL)
}

getTime.Activities <- function(obj) {obj$time}
"getTime<-.Activities" <- function(obj,value) {obj$time <- value}



as_longform.Activities <- function(x,...,n=nsubj(x),mxocc=maxocc(x),
                                   mnocc=minocc(x)) {
  as_longform(x$index,n=n,mxocc=mxocc,mnocc=mnocc,
              name=x$iname) |>
    dplyr::left_join(
               as_longform(x$dt,n=n,mxocc=mxocc,mnocc=mnocc,
                           name=x$dtname),
               dplyr::join_by("subj","occ")) ->
      result
  if (!is.null(x$dosage))
    dplyr::left_join(result,
                     as_longform(x$dosage,n=n,mxocc=mxocc,mnocc=mnocc,
                                 name=x$dosname),
                     dplyr::join_by("subj","occ")) ->
      result
  result
}


