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

Quadrature <- R6Class(
  "Quadrature",
  public=list(
    static=TRUE,
    bysubj=FALSE,
    wname="w",
    lweights=array(NA_real_,c(1,1,1)),
    isubj=NA_integer_,
    initialize = function(times, qpoints, tnames=vnames(qpoints),
                          static=TRUE, bysubj=FALSE,
                          nsubjects=nsubj(times),
                          isubj=NA_integer_) {
      self$static <- static
      self$bysubj <- bysubj
      private$Times <- as.Panmat(times,minocc=1L)
      self$thetas <- qpoints
      self$nsubj <- nsubjects
      self$isubj <- isubj
    },
    weights = function(type="default") {
      weights <- exp(self$lweights)
      sweep(weights,1:2,apply(weights,1:2,sum),"/")
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
      if (is.null(dim(value))) {
        value <- data.frame(theta=value)
      }
      vnames <- vnames(value)
      if (is.data.frame(value)) {
        value <- as.matrix(value)
      }
      vdims <- dim(value)
      if (length(vdims) != 4L) {
        if (length(vdims)+self$fixed+!self$bysubj != 4L) {
          stop("Expected a 4 dimensional array.")
        }
        if (self$fixed) {
          if (self$uniform) {
            dim(value) <- c(1L,1L,vdims)
          } else {
            dim(value) <- c(1L,vdims)
          }
        } else {
          dim(value) <- c(vdims[1],1L,vdims[2:3])
        }
      }
      private$Theta <- value
      private$Tnames <- vnames
    },
    theta = function(subj,occ,quad,value) {
      if (static) occ <- 1L
      else occ <- occ +1L
      if (!bysubj) subj <- 1L
      if (missing(quad)) quad <- 1L:nquad(self)
      if (mising(value)) return(private$Theta[subj,occ,quad,])
      private$Theta[subj,occ,quad,] <- value
    },
    lweight = function(subj,occ,quad,value) {
      occ <- occ +1L
      if (missing(quad)) quad <- 1L:nquad(self)
      if (mising(value)) return(self$lweights[subj,occ,quad])
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
      nsubj(private$Times) <- value
    },
    nquad = function(value) {
      if (!missing(value)) stop("Quadrature points are fixed.")
      dim(private$Theta)[3L]
    }
  )
)


setOldClass("Quadrature")


nsubj.Quadrature <- function(obj) {nsubj(obj)}
"nsubj<-.Quadrature" <- function(obj,value) {
  nsubj(obj) <-as.integer(value)
  obj
}

isubj.Quadrature <- function(obj) {obj$isubj}
"isubj<-.Quadrature" <- function(obj,value) {
  obj$isubj <-as.integer(value)
  obj
}

nocc.Quadrature <- function(obj) {nocc(obj)}
"nocc<-.Quadrature" <- function(obj,value) {
  obj$nocc <- as.integer(value)
  obj
}

minocc.Quadrature <- function(obj) {minocc(obj)}
"minocc<-.Quadrature" <- function(obj,value) {
  minocc(obj) <- as.integer(value)
  obj
}

maxocc.Quadrature <- function (obj) {maxocc(obj)}
"maxocc<-.Quadrature" <- function(obj,value) {
  maxocc(obj) <- as.integer(value)
  obj
}

nquad.Quadrature <- function(obj) {obj$nquad}
"nquad<-.Quadrature" <- function(obj,value) {
  obj$nquad <- as.integer(value)
  obj
}




as_longform.Quadrature <- function(x,..., n=nsubj(x),maxocc=nocc(x),
                                   minocc=1L,weightType="all",
                                   name=deparse(substitute(x))) {
  result <- data.frame(subj=1L:nsubj(x),each=nocc(x)*nquad(x),
                       occ=rep(rep(minocc(x):maxocc(x),each=nquad(x)),
                               nsubj(x)),
                       quad=rep(1L:nquad(x),nsubj(x)*nocc(x)),
                       w=x$weights(weightType))
  thetas <- as.data.frame(matrix(x$thetas,ncol=x$dtheta,
                                 byrow=TRUE))
  names(thetas) <- x$tnames
  if (x$static) {
    if (x$bysubj) {
      thetas <- data.frame(subj=rep(1L:nsubj(x),each=nquad(x)),
                           quad=rep(1L:nquad(x),nsubj(x)),
                           thetas)
      by <- dplyr::join_by("subj","quad")
    } else {
      thetas <- data.frame(quad=1L:nquad(x),
                           thetas)
      by <- dplyr::join_by("quad")
    }
  } else {
    if (x$bysubj) {
      thetas <- data.frame(subj=rep(1L:nsubj(x),each=nquad(x)*nocc(x)),
                           occ=rep(rep(minocc(x):maxocc(x),
                                       each=nquad(x)),
                                   nsubj(x)),
                           quad=rep(1L:nquad(x),nsubj(x)*nocc(x)),
                           thetas)
      by <- dplyr::join_by("subj", "occ", "quad")
    } else {
      thetas <- data.frame(occ=rep(minocc(x):maxocc(x)),
                           quad=rep(1L:nquad(x),nocc(x)),
                           thetas)
      by <- dplyr::join_by("occ", "quad")
    }
  }
  result |>
    dplyr::left_join(as_longform(x$times,n=n,maxocc=maxocc,
                                 minocc=minocc,name=name),
                     dplyr::join_by("subj","occ")) |>
    dplyr::left_join(as_longform(thetas),by)

}


get_subj.Quadrature <- function(x,isubj) {
  new("Quadrature",times=x$times[isubj,,],nsubj=1L,
      minocc=minocc(x),nocc1=nocc(x)-1L,isubj=isubj)
}

"get_subj<-.Quadrature" <-function(x,isubj,value) {
  if (x$bySubj) {
    x$times[isubj,] <-value$times[1L,]
    x$thetas[isubj,,,] <- value$thetas[1L,,,]
  }
  x$lweights[isubj,,] <- value$lweights
  x
}



FixedQuad <- R6Class(
  "FixedQuad",
  inherit =  Quadrature,
  public = list(
  )
)

fixedQuad <- function(times,qpoints,tnames=vnames(qpoints),
                      static=TRUE, bysubj=(nsubj(times)>1L),
                      nsubjects=nsubj(times),isubj=NA_integer_) {
  FixedQuad$new(times=times, qpoints=qpoints, tnames=tnames,
                static=static, bysubj=bysubj, nsubjects=nsubjects,
                isubj=isubj)
}


ParticleQuad <- R6Class(
  "ParticleQuad",
  inherit =  Quadrature,
  public = list(
    initialize = function(times, nquadrature, tnames="theta",
                          static=FALSE, bysubj=TRUE,
                          nsubjects=nsubj(times),
                          isubj=NA_integer_) {
      qpoints <- array(NA_real_,c(nsubjects,nocc(times),nquadrature,
                                  length(tnames)))
      super$initialize(times,qpoints,tnames,static,bysubj,nsubjects)
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
      result
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

particleQuad <- function(times,nquadrature,tnames,
                      static=FALSE, bysubj=(nsubj(times)>1L),
                      nsubjects=nsubj(times),
                      isubj=NA_integer_) {
  ParticleQuad$new(times=times, nquadrature=nquadrature, tnames=tnames,
                   static=static, bysubj=bysubj, nsubjects=nsubjects,
                   isubj=isubj)
}



BWQuad <- R6Class(
  "BWQuad",
  inherit =  Quadrature,
  public = list(
    wname=paste0("w.",c("full","left","right")),
    rweights=array(NA_real_,c(1,1,1)),
    initialize = function(times, qpoints, tnames=vnames(qpoints),
                          static=TRUE, bysubj=FALSE,
                          nsubjects=nsubj(times),
                          isubj=NA_integer_) {
      super$initialize(times,qpoints,tnames,static,bysubj,nsubjects,isubj)
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
        default=data.frame(full=self$weights("default"),
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

BWquad <- function(times,qpoints,tnames=vnames(qpoints),
                   static=TRUE, bysubj=(nsubj(times)>1L),
                   nsubjects=nsubj(times),isubj=NA_integer_) {
  BWQuad$new(times=times, qpoints=qpoints, tnames=tnames,
             static=static, bysubj=bysubj, nsubjects=nsubjects,
             isubj=isubj)
}

