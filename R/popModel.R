PopulationModel <- R6Class(
  classname="PopulationModel",
  inherit=FModel,
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
    initProbs = function(theta,covars=list()) {
      ord <- order(theta,decreasing=FALSE)
      ps <- pnorm(theta[ord],self$mu,self$sigma)
      pcuts <- c(0,ps[-1]-diff(ps)/2,1) #Midpoints of p's.
      diff(pcuts)[order(ord)] #order(ord) undoes the ordering.
    },
    toString=function(digits=2,...) {
      paste0("<NormalPopulation: ",
             self$name, " ( ",round(self$mu,digits=digits),
             ", ",round(self$sigma,digits=digits)," )>")
    },
    mstep=function(data,its=3,control=list(),workers=NULL) {
      weights <- data[[self$wname]]
      theta <- data[[self$tnames]]
      self$mu <- wtd.mean(theta,weights)
      self$sigma <- wtd.sd(theta,weights)
      self$lp <- sum(dnorm(theta,self$mu,self$sigma,log=TRUE)*weights)
      self$convergence <- TRUE
      list(name=self$name,list(mu=self$mu,sigma=self$sigma))
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
      sum(lprobs[match(theta,self$states)]*weights)
    },
    mstep=function(data,its=3,control=list(),workers=NULL) {
      ## Force dummy entries into list so no cells are dropped
      weights <- c(rep(0,length(self$states)),data[[self$wname]])
      theta <- c(self$states,data[[self$tnames]])
      post <- wtd.table(theta,weights,normwt=FALSE)
      self$probs <- post/sum(post)
      self$lp <- sum(log(self$probs)[match(theta,self$states)]*weights)
      self$convergence <- TRUE
      list(name=self$name,probs=self$probs)
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
          any(value < 0) || abs(sum(value)-1) > .0001) {
        stop("Value must be non-negative vector of length ",
             length(self$states),", which sums to one.")
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
      iname="group",
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

setMethod("drawInitial", "Population",
          function(model, isubj, npart, covar=NULL) {
            model$drawInit(isubj,npart,covar)
})

setMethod("ProbInit", "Population",
          function(model, isubj, thetas, covar=NULL) {
            model$initProbs(isubj,thetas,covar)
})



