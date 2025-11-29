EvidenceModel <- R6Class(
  classname="EvidenceModel",
  inherit="FModel",
  public = list(
    name="<EvidenceModel>",
    drawObs = function(theta,covars=list()) {
      stop("DrawData not implemented for ", class(self))
    },
    llike = function(Y,theta,covars=list()) {
      stop("Llike not implemented for ", class(self))
    }
  ),
  private=list(
    dataNames=character()
  ),
  active=list(
    dnames = function(value) {
      if (missing(value)) return(private$dataNames)
      if (length(private$dataNames) > 0L &&
          length(private$dataNames) != length(value)) {
        stop("Data names must have length",length(private$dataNames))
      }
      private$dataNames <- value
    }
  )
)


logit <- function(p) log(p/(1-p))
invlogit <- function(x) 1/(1+exp(-x))

cuts2probs <- function(cuts) {
  if (!is.matrix(cuts)) cuts <- matrix(cuts,1L,length(cuts))
  -t(apply(cbind(1,cuts,0),1,diff))
}


GradedResponse <- R6Class(
  classname="GradedResponse",
  inherit=EvidenceModel,
  public=list(
    initialize = function(name, a, b, tname="theta", wname="w", dname="Y") {
      self$name <- name
      self$a <- a
      self$b <- b
      self$tnames <- tname
      self$wname <- wname
      self$dnames <- dname
    },
    a=1,
    b=0,
    cuts = function(theta,a=self$a,b=self$b) {
      invlogit(outer(a*theta,b,"-"))
    },
    drawObs = function(theta,covars=list()) {
      rowSums(sweep(self$cuts(theta),1,runif(length(theta)),">"))
    },
    llike = function(Y,theta,covars=list()) {
      probs <- cuts2probs(self$cuts(theta))
      log(probs[,Y+1L])
    },
    lprob = function(par=self$pvec,data) {
      weights <- data[[self$wname]]
      theta <- data[[self$tnames]]
      Y <- data[[self$dnames]]
      probs <- cuts2probs(self$cuts(theta,a=exp(par[1]),b=par[-1]))
      sum(sapply(1L:length(theta),\(i) {
        weights[i]*log(probs[i,Y[i]+1L])
      }))
    },
    toString=function(digits=2,...) {
      paste0("<GR: ", self$name, " ( ",
             round(self$a,digits=digits),
             ", ",paste(round(self$b,digits=digits),
                        collapse = ", "), " )>")
    }
  ),
  active=list(
    pvec = function(value) {
      if (missing(value)) return(c(log(self$a),self$b))
      self$a <- exp(value[1])
      self$b <- value[-1]
    }
  )
)

NormalScore <- R6Class(
  classname="NormalScore",
  inherit=EvidenceModel,
  public=list(
    initialize = function(name, bias, se, tname="theta",wname="w",
                          dname="Y") {
      self$name <- name
      self$bias <- bias
      self$se <- se
      self$tnames <- tname
      self$wname <- wname
      self$dnames <- dname
    },
    bias=0,
    se=01,
    drawObs = function(theta,covars=list()) {
      rnorm(length(theta),theta+self$bias,self$se)
    },
    llike = function(Y,theta,covars=list()) {
      dnorm(Y,theta+self$bias,self$se,log=TRUE)
    },
    lprob = function(par=self$pvec,data) {
      weights <- data[[self$wname]]
      theta <- data[[self$tnames]]
      Y <- data[[self$dnames]]
      sum(dnorm(Y,theta+par[1],exp(par[2]),log=TRUE)*weights)
    },
    toString=function(digits=2,...) {
      paste0("<NS: ", self$name, " ( ",
             round(self$bias,digits=digits),
             ", ",paste(round(self$se,digits=digits),
                        collapse = ", "), " )>")
    }
  ),
  active=list(
    pvec = function(value) {
      if (missing(value)) return(c(self$bias,log(self$se)))
      self$bias <- value[1]
      self$se <- exp(value[2])
    }
  )
)


Evidence <- R6Class(
  "Evidence",
  inherit=ModelSet,
  public=list(
    name="Evidence Set",
    initialize=function(name,evidenceModels,
                        tasks=matrix(1L,1L,1L),
                        tname="theta",wname="w",
                        dname="Y") {
      self$name <- name
      self$models <- evidenceModels
      self$index <- as.Pandat(tasks)
      self$tnames <- tname
      self$wname <- wname
      self$dnames <- dname
    },
    task = function(subj,it) {
      self$index[subj,it]
    },
    toString=function(...) {
      paste0("<Evidence: ",self$name,": ",
             self$nsubjects, " x ",
             self$macocc, " >")
    },
    evalEvidence = function(subj,it,theta,Y,cov=NULL) {
      task <- self$task(subj,it)
      if (all(is.na(Y)) || is.na(task)) {
        return(rep(0,dim(self$theta)[1]))
      } else {
        self$models[[task]]$llike(Y,theta,cov)
      }
    },
    drawObs = function(subj,it,theta,cov=NULL) {
      task <- self$task(subj,it)
      if (is.na(task)) {
        return(rep(NA,dim(self$theta)[1]))
      } else {
        self$models[[task]]$drawObs(theta,cov)
      }
    },
    mstep = function(data,its=3,control=list(),workers=Workers$new()) {
      workers$start()
      workers$lapply(unique(data$task),\(tt) {
        mstep(self$models[[tt]],dplyr::filter(data,task==tt),
              its=its,control=control,workers=workers)
      })
    }
  )
)


setOldClass("Evidence")

setMethod("as_longform","Evidence",
          function(x,n=nsubj(x),maxocc=nocc(x),
                   minocc=1L,weightType="all",
                   name=deparse(substitute(x))) {
  as_longform(x$index,n=n,maxocc=maxocc,minocc=minocc,
              weightType=weightType,name="task")
          })

