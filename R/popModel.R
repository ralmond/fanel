PopulationModel <- R6Class(
  classname="PopulationModel",
  inherit="FModel",
  public = list(
    name="<PopulationModel>",
    drawInit = function(npart,covars=list()) {
      stop("DrawInit not implemented for ", class(self))
    },
    initProbs = function(theta,covars=list()) {
      stop("probs not implemented for ", class(self))
    }
  )
)


NormalPop <- R6Class(
  classname = "NormalPop",
  inherit=PopulationModel,
  public=list(
    initialize = function(name,mu,sigma,tname="theta",wname="w") {
      self$name <- name
       self$mu <- mu
      self$sigma <- sigma
      self$tnames <- tname
      self$wname <- wname
    },
    mu=0,
    sigma=1,
    drawInit = function(npart,covars=list()) {
      mu <- self$mu
      sigma <- self$sigma
      rnorm(npart,mu,sigma)
    },
    lprob = function(par=self$pvec,data) {
      mu <- par[1]
      sigma <- exp(par[2])
      weights <- data[[self$wname]]
      theta <- data[[self$tnames]]
      sum(dnorm(theta,mu,sigma,log=TRUE)*weights)
    },
    cdf = function(theta,covars=list()) {
      pnorm(theta,self$mu,self$sigma)
    },
    toString=function(digits=2,...) {
      paste0("<NormalPopulation: ",
             self$name, " ( ",round(self$mu,digits=digits),
             ", ",round(self$sigma,digits=digits)," )>")
    }
  ),
  active=list(
    pvec = function(value) {
      if (missing(value)) return(c(self$mu,log(self$sigma)))
      self$mu <- value[1]
      self$sigma <- exp(value[2])
    }
  )
)

softmax <- function (vec) {
  evec <- exp(vec)
  evec/sum(evec)
}


CategoricalPop <- R6Class(
  classname = "CategoricalPop",
  inherit=PopulationModel,
  public=list(
      initialize = function(name,states=seq(0L,2L,1L),
                            probs=rep(1/length(states),length(states)),
                            tname="theta",wname="w") {
      self$name <- name
      self$states <- states
      self$probs <- probs
      self$tnames <- tname
      self$wname <- wname
    },
    states=0L:2L,
    drawInit = function(npart,covars=list()) {
      self$states[rowSums(outer(runif(npart),cumsum(self$probs),"<"))]
    },
    lprob = function(par=self$pvec,data) {
      lprobs <- log(softmax(par))
      weights <- data[[self$wname]]
      theta <- data[[self$tnames]]
      sum(lprobs[theta]*weights)
    },
    initProbs = function(theta,covars=list()) {
       self$probs[theta]
    },
    toString=function(digits=2,...) {
      paste0("<CategoricalPopulation: ",
             self$name, " ( ",paste(self$states,collapse=",")," )>")
    }
  ),
  private=list(ppp=rep(1/3,3)),
  active=list(
    probs = function(value) {
      if (missing(value)) return(private$ppp)
      if (length(value) != length(self$states) ||
          any(value) < 0 || abs(sum(value)-1) < .0001) {
        stop("Value must be non-negative vector of length",
             length(self$states),"which sums to one.")
      }
      private$ppp <- value
    },
    pvec = function(value) {
      if (missing(value)) return(log(private$ppp))
      private$ppp <- softmax(value)
    }
  )
)


Population <- R6Class(
  "Population",
  inherit=ModelSet,
  public=list(
    name="A Population",
    initialize=function(name,popModels,groups=1L,
                        tname="theta",wname="w") {
      self$name <- name
      self$popModels <- popModels
      self$groups <- groups
      self$tnames <- tname
      self$wname <- wname
    },
    group = function(subj) {
      self$index[subj,]
    },
    drawInit = function(subj,npart,covars=NULL) {
      self$models[[self$group(subj)]]$drawInit(npart,covars)
    },
    initProbs = function(subj,theta,covars=list()) {
       self$models[[self$group(subj)]]$initProbs(theta,covars)
    },
    mstep = function(data,its=3,control=list(),workers=Workers$new()) {
      data <- filter(data,occ==0)
      workers$start()
      workers$lapply(unique(data$group),\(g) {
        mstep(self$models[[g]],dplyr::filter(data,group==g),
              its=its,control=control,workers=workers)
      })
    }
  )
)

setOldClass("Population")

setMethod("as_longform","Population",
          function(x,n=nsubj(x),maxocc=nocc(x),
                   minocc=1L,weightType="all",
                   name=deparse(substitute(x))) {
  as_longform(x$index,n=n,maxocc=maxocc,minocc=minocc,
              weightType=weightType,name="group")
          })

