## This is a little utility to grab the variable names
vnames <- function(x,prefix="theta") {
  if (is.data.frame(x)) return(names(x))
  if (!is.array(x)) stop("Argument must be an array or data frame.")
  dms <- dim(x)
  vnam <- paste0(prefix,1L:dms[length(dms)])
  if (!is.null(dimnames(x)) && !is.null(dimnames(x)[length(dms)])) {
    vnam <- dimnames(x)[length(dms)]
  }
  vnam
}

### Quadrature ----

Quadrature <- R6Class(
  "Quadrature",
  public=list(
    byocc=FALSE,
    bysubj=FALSE,
    wname="w",
    lweights=array(NA_real_,c(1,1,1)),
    isubj=NA_integer_,
    initialize = function(times, qpoints, tnames=vnames(qpoints),
                          byocc=FALSE, bysubj=FALSE,
                          nsubjects=nsubj(times),
                          isubj=NA_integer_) {
      self$byocc <- byocc
      self$bysubj <- bysubj
      private$Times <- as.Panmat(times,minocc=1L)
      self$thetas <- qpoints
      self$nsubj <- nsubjects
      self$isubj <- isubj
    },
    weights = function(type="default") {
      weights <- exp(self$lweights)
      data.frame(w=
        sweep(weights,1:2,apply(weights,1:2,sum),"/")
      )
    },
    resetWeights = function() {
      self$lweights <- array(NA_real_,
                             c(self$nsubj,self$nocc,self$nquad))
    }
  ),
  private=list(
    Theta = array(NA_real_,c(1,1,1,1)),
    Tnames=character(),
    Times=panmat(c(0.0,1.0),minocc=0L)
  ),
  active=list(
    thetas = function(value) {
      if (missing(value)) return(private$Theta)
      value <- as_quad_frame(value)
      private$Theta <- value
      private$Tnames <- names(value)
    },
    theta = function(subj,occ,quad,value) {
      if (static) occ <- 1L
      else occ <- occ +1L
      if (!bysubj) subj <- 1L
      if (missing(quad)) quad <- 1L:nquad(self)
      if (missing(value)) return(private$Theta[subj,occ,quad])
      private$Theta[subj,occ,quad] <- value
    },
    lweight = function(subj,occ,quad,value) {
      occ <- occ +1L
      if (missing(quad)) quad <- 1L:nquad(self)
      if (missing(value)) return(self$lweights[subj,occ,quad])
      private$lweights[subj,occ,quad] <- value
    },
    tnames=function(value) {
      if (missing(value)) return(private$Tnames)
      if (!is.character(value) ||
          length(value) != dim(private$Theta)[4]) {
        stop("Expected character vector of length",dim(private$Theta)[4])
      }
      private$Tnames <- value
    },
    dtheta = function() {
      length(private$Tnames)
    },
    times=function(value) {
      if (missing(value)) return(private$Times)
      private$Times <- as.Panmat(value,minocc=0L)
    },
    nocc = function(value) {
      if (missing(value)) return(nocc(private$Times))
      nocc(private$Times) <- value
    },
    minocc = function(value) {
      if (missing(value)) return(minocc(private$Times))
      minocc(private$Times) <- value
    },
    maxocc = function(value) {
      if (missing(value)) return(maxocc(private$Times))
      maxocc(private$Times) <- value
    },
    nsubj = function(value) {
      if (missing(value)) return(nsubj(private$Times))
      nsubj(private$Times) <- as.integer(value)
    },
    nquad = function(value) {
      if (missing(value)) return(nquad(private$Theta))
      nquad(private$Theta) <- as.integer(value)
    }
  )
)


setOldClass(c("Quadrature","R6"))


nsubj.Quadrature <- function(obj) {obj$nsubj}
"nsubj<-.Quadrature" <- function(obj,value) {
  obj$nsubj <-as.integer(value)
  obj
}

isubj.Quadrature <- function(obj) {obj$isubj}
"isubj<-.Quadrature" <- function(obj,value) {
  obj$isubj <-as.integer(value)
  obj
}

bysubj.Quadrature <- function(obj) {obj$bysubj}
"bysubj<-.Quadrature" <- function(obj,value) {
  obj$bysubj <-as.logical(value)
  obj
}

byocc.Quadrature <- function(obj) {obj$byocc}
"byocc<-.Quadrature" <- function(obj,value) {
  obj$byocc <-as.logical(value)
  obj
}

nocc.Quadrature <- function(obj) {obj$nocc}
"nocc<-.Quadrature" <- function(obj,value) {
  obj$nocc <- as.integer(value)
  obj
}

minocc.Quadrature <- function(obj) {obj$minocc}
"minocc<-.Quadrature" <- function(obj,value) {
  obj$minocc <- as.integer(value)
  obj
}

maxocc.Quadrature <- function (obj) {obj$maxocc}
"maxocc<-.Quadrature" <- function(obj,value) {
  obj$maxocc <- as.integer(value)
  obj
}

nquad.Quadrature <- function(obj) {obj$nquad}
"nquad<-.Quadrature" <- function(obj,value) {
  obj$nquad <- as.integer(value)
  obj
}




as_longform.Quadrature <- function(x,..., n=nsubj(x),mxocc=maxocc(x),
                                   mnocc=minocc(x),weightType="all") {
  cbind(as_longform(x$theta,n=n,mxocc=mxocc,mnocc=mnocc),
        x$weights(weightType))
}


get_subj.Quadrature <- function(x,isubj) {
  new("Quadrature",times=x$times[isubj,,],nsubj=1L,
      minocc=minocc(x),nocc1=nocc(x)-1L,isubj=isubj)
}

"get_subj<-.Quadrature" <-function(x,isubj,value) {
  if (x$bysubj) {
    x$times[isubj,] <-value$times[1L,]
    x$thetas[isubj,,,] <- value$thetas[1L,,,]
  }
  x$lweights[isubj,,] <- value$lweights
  x
}

### Fixed Quad ----

FixedQuad <- R6Class(
  "FixedQuad",
  inherit =  Quadrature,
  public = list(
  )
)

setOldClass(c("FixedQuad","Quadrature","R6"))


fixedQuad <- function(times,qpoints,tnames=vnames(qpoints),
                      byocc=FALSE, bysubj=FALSE,
                      nsubjects=nsubj(times),isubj=NA_integer_) {
  FixedQuad$new(times=times, qpoints=qpoints, tnames=tnames,
                byocc=byocc, bysubj=bysubj, nsubjects=nsubjects,
                isubj=isubj)
}


### ParticleQuad ----

ParticleQuad <- R6Class(
  "ParticleQuad",
  inherit =  Quadrature,
  public = list(
    initialize = function(times, nquadrature, tnames="theta",
                          byocc=TRUE, bysubj=TRUE,
                          nsubjects=nsubj(times),
                          isubj=NA_integer_) {
      qpoints <- data.frame(subj= rep(1L:nsubjects,
                                      each=nocc(times)*nquadrature),
                            occ=rep(rep(minocc(times):maxocc(times),nsubjects),
                                    each=nquadrature),
                            quad=rep(1L:nquadrature,nocc(times)*nsubjects))
      for (tn in tnames) {
        qpoints[[tn]]<-NA_real_
      }
      super$initialize(times,qpoints,tnames,byocc,bysubj,nsubjects)
      self$nquad <- nquadrature
      self$isubj <- isubj
    },
    weights = function(type="default") {
      if (type=="history") {
        weights <- exp(self$lweights)
        result <- sweep(weights,1:2,apply(weights,1:2,sum),"/")
      } else {
        weights <- exp(self$lweights[,self$nocc,])
        weights <- sweep(weights,1,apply(weights,1,sum),"/")
        result <- array(NA_real_,c(self$nsubj,self$nocc,self$nquad))
        for (occ in 1:self$nocc) {
          result[,occ,] <- weights
        }
      }
      data.frame(w=result)
    },
    resetWeights = function() {
      self$lweights <- array(NA_real_,
                             c(self$nsubj,self$nocc,self$nquad))
      self$lweights[,1L,] <- 0
      private$Theta <- array(NA_real_,
                             c(self$nsubj,self$nocc,
                               self$nquad,self$dtheta))
    }
  ),
  private = list(
    nq=1L
  ),
  active = list(
    nquad = function(value) {
      if (missing(value)) return(private$nq)
      private$nq <- value
    }
  )
)

setOldClass(c("ParticleQuad","Quadrature","R6"))


particleQuad <- function(times,nquadrature,tnames,
                      byocc=TRUE, bysubj=TRUE,
                      nsubjects=nsubj(times),
                      isubj=NA_integer_) {
  ParticleQuad$new(times=times, nquadrature=nquadrature, tnames=tnames,
                   byocc=byocc, bysubj=bysubj, nsubjects=nsubjects,
                   isubj=isubj)
}

### BWQuad ----

BWQuad <- R6Class(
  "BWQuad",
  inherit =  Quadrature,
  public = list(
    wname=paste0("w.",c("full","left","right")),
    rweights=array(NA_real_,c(1,1,1)),
    initialize = function(times, qpoints, tnames=vnames(qpoints),
                          byocc=FALSE, bysubj=FALSE,
                          nsubjects=nsubj(times),
                          isubj=NA_integer_) {
      super$initialize(times,qpoints,tnames,byocc,bysubj,nsubjects,isubj)
    },
    weights = function(type="default") {
      switch(type,
        default = {
          weights <- self$lweights*self$rweights
          sweep(weights,1:2,apply(weights,1:2,sum),"/")
        },
        left = {
          weights <- self$lweights
          sweep(weights,1:2,apply(weights,1:2,sum),"/")
        },
        right = self$rweights,
        all=data.frame(full=self$weights("default"),
                       left=self$weights("left"),
                       right=self$weights("right"))
      )
    },
    resetWeights = function() {
      self$lweights <- array(NA_real_,
                             c(self$nsubj,self$nocc,self$nquad))
      self$rweights <- array(NA_real_,
                             c(self$nsubj,self$nocc,self$nquad))
    }
  )
)


setOldClass(c("BWQuad","Quadrature","R6"))



BWquad <- function(times,qpoints,tnames=vnames(qpoints),
                   byocc=FALSE, bysubj=FALSE,
                   nsubjects=nsubj(times),isubj=NA_integer_) {
  BWQuad$new(times=times, qpoints=qpoints, tnames=tnames,
             byocc=byocc, bysubj=bysubj, nsubjects=nsubjects,
             isubj=isubj)
}

