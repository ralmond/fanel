### EvidenceModel ----

EvidenceModel <- R6Class(
  classname="EvidenceModel",
  inherit=FModel,
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

setOldClass(c("EvidenceModel","FModel"))

### GradedResponse ----

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
    initialize = function(name, a, b, qname="theta", wname="w", dname="Y") {
      self$name <- name
      self$a <- a
      self$b <- b
      self$qnames <- qname
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
    lprob = function(data,par=self$pvec) {
      weights <- data[[self$wname]]
      theta <- data[[self$qnames]]
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

setOldClass(c("GradedResponse","EvidenceModel","FModel"))

### Normal Score ----

NormalScore <- R6Class(
  classname="NormalScore",
  inherit=EvidenceModel,
  public=list(
    initialize = function(name, bias, se, qname="theta",wname="w",
                          dname="Y") {
      self$name <- name
      self$bias <- bias
      self$se <- se
      self$qnames <- qname
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
    lprob = function(data,par=self$pvec) {
      weights <- data[[self$wname]]
      theta <- data[[self$qnames]]
      Y <- data[[self$dnames]]
      sum(dnorm(Y,theta+par[1],exp(par[2]),log=TRUE)*weights)
    },
    mstep = function(data,...) {
      weights <- data[[self$wname]]
      theta <- data[[self$qnames]]
      Y <- data[[self$dnames]]
      self$bias <- wtd.mean(Y,weights) - wtd.mean(theta,weights)
      self$se <- wtd.sd(Y,weights)
      self$converged <- true
      self$lp <- self$lprob(data)
      self
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

setOldClass(c("NormalScore","EvidenceModel","FModel"))

### Evidence ----

Evidence <- R6Class(
  "Evidence",
  inherit=ModelSet,
  public=list(
    name="Evidence Set",
    initialize=function(name,evidenceModels,
                        tasks=matrix(1L,1L,1L),
                        qname="theta",wname="w",
                        dname="Y") {
      self$name <- name
      self$models <- evidenceModels
      self$index <- as.Panmat(tasks)
      self$qnames <- qname
      self$wname <- wname
      self$dnames <- dname
    },
    task = function(isubj,iocc) {
      self$index[isubj,iocc]
    },
    toString=function(...) {
      paste0("<Evidence: ",self$name,": ",
             self$nsubjects, " x ",
             self$macocc, " >")
    },
    evalEvidence = function(isubj,iocc,theta,Y,covar=NULL) {
      task <- self$task(isubj,iocc)
      if (all(is.na(Y)) || is.na(task)) {
        return(rep(0,dim(self$theta)[1]))
      } else {
        self$models[[task]]$llike(Y,theta,cov)
      }
    },
    drawObs = function(isubj,iocc,theta,covar=NULL) {
      task <- self$task(isubj,iocc)
      if (is.na(task)) {
        return(rep(NA,dim(self$theta)[1]))
      } else {
        self$models[[task]]$drawObs(theta,cov)
      }
    }
  )
)


setOldClass(c("Evidence","ModelSet"))


drawData.Evidence <- function(model,isubj,iocc,theta,covar=NULL) {
  model$drawObs(isubj,iocc,theta,covar)
}

evalEvidence.Evidence<- function(model, isubj, iocc, theta, data,
                                 covar=NULL) {
  model$evalEvidence(isubj,iocc,theta,data,covar)
}

as_longform.Evidence <- function(x,...,n=nsubj(x),
                                 mxocc=maxocc(x),
                                 mnocc=minocc(x)) {
  as_longform(x$index,n=n,mxocc=mxocc,mnocc=mnocc,
              name=x$iname)
}

