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
    initialize = function(times, quadrature,
                          byocc=FALSE, bysubj=FALSE,
                          nsubjects=nsubj(times),
                          isubj=NA_integer_) {
      self$byocc <- byocc
      self$bysubj <- bysubj
      self$times <- times
      self$quadpoints <- quadrature
      self$nsubj <- nsubjects
      self$isubj <- isubj
    },
    weights = function(type="default") {
      weights <- exp(self$lweights)
      as.vector(
        sweep(weights,2:3,apply(weights,2:3,sum),"/")
      )
    },
    resetWeights = function() {
      self$lweights <- array(NA_real_,
                             c(self$nquad,self$nocc,self$nsubj))
    },
    qpoints = function(subj,occ,value) {
      if (!self$byocc) occ <- 1L
      else occ <- occ +1L
      if (!self$bysubj) subj <- 1L
      quad <- 1L:nquad(self)
      if (missing(value)) return(private$Theta[subj,occ,quad])
      private$Theta[subj,occ,quad] <- value
    },
    lweight = function(subj,occ,quad,value) {
      occ <- occ +1L
      if (missing(quad)) quad <- 1L:nquad(self)
      if (missing(value)) return(self$lweights[quad,occ,subj])
      self$lweights[quad,occ,subj] <- value
    }
  ),
  private=list(
    Theta = NULL,
    Times=panmat(c(0.0,1.0),minocc=0L)
  ),
  active=list(
    quadpoints = function(value) {
      if (missing(value)) return(private$Theta)
      value <- as_quad_frame(value)
      private$Theta <- value
    },
    qnames=function(value) {
      if (missing(value)) return(qname(private$Theta))
      qname(private$Theta) <- value
    },
    dquad = function() {
      length(qname(private$Theta))
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
                                   mnocc=minocc(x),weightType="all",
                                   includeTime=TRUE) {
  w <- as.data.frame(x$weights(weightType))
  names(w) <- x$wname
  result <- cbind(
    as_longform(x$quadpoints,n=n,mxocc=mxocc,mnocc=mnocc),
    w)
  if (includeTime) {
    result <- dplyr::left_join(result,
                               as_longform(x$times,name="times"),
                               dplyr::join_by("subj","occ"))
  }
  result
}


get_subj.Quadrature <- function(x,isub) {
  result <- x$clone()
  if (x$bysubj) {
    result$times <- get_subj(x$times,isub)
    result$quadpoints <- get_subj(x$quadpoints,isub)
  }
  result$lweights <- x$lweights[,,isub,drop=FALSE]
  nsubj(result) <- 1L
  isubj(result) <- isub
  result
}

padSubj.QuadWeights <- function(x,isub) {
  dx <- dim(x)
  nr <- dx[3]
  if (isub <= nr) return(x)
  nc <- dx[1]*dx[2]
  dx[3] <- isub
  array(c(x,rep(NA,(isub-nr)*nc)),dx)
}


"get_subj<-.Quadrature" <-function(x,isub,value) {
  if (x$bysubj) {
    get_subj(x$times,isub) <- value$times
    get_subj(x$quadpoints,isub) <- value$quadpoints
  }
  if (!is.na(isubj(x)) && isubj(x)!=isub) {
    isubj(x) <- NA_integer_
  }
  nsubj(x) <- max(nsubj(x),isub)
  x$lweights <- padSubj.QuadWeights(x$lweights,isub)
  x$lweights[,,isub] <- value$lweights
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


fixedQuad <- function(times,quadrature,
                      byocc=FALSE, bysubj=FALSE,
                      nsubjects=nsubj(times),isubj=NA_integer_) {
  FixedQuad$new(times=times, quadrature=quadrature,
                byocc=byocc, bysubj=bysubj, nsubjects=nsubjects,
                isubj=isubj)
}


### ParticleQuad ----

ParticleQuad <- R6Class(
  "ParticleQuad",
  inherit =  Quadrature,
  public = list(
    initialize = function(times, nquadrature, qnames="theta",
                          byocc=TRUE, bysubj=TRUE,
                          nsubjects=nsubj(times),
                          isubj=NA_integer_) {
      quadrature <- na_quad_frame(nsubjects,minocc(times),
                                  maxocc(times),nquadrature,
                                  qnames,isubj)
      super$initialize(times,quadrature,byocc,bysubj,nsubjects)
      self$nquad <- nquadrature
      self$isubj <- isubj
    },
    weights = function(type="default") {
      if (type=="history") {
        weights <- exp(self$lweights)
        result <- sweep(weights,2:3,apply(weights,2:3,sum),"/")
      } else {
        weights <- exp(self$lweights[,self$nocc,])
        weights <- sweep(weights,2,apply(weights,2,sum),"/")
        result <- array(NA_real_,c(self$nsubj,self$nocc,self$nquad))
        for (occ in 1:self$nocc) {
          result[,occ,] <- weights
        }
      }
      as.vector(result)
    },
    resetWeights = function() {
      self$lweights <- array(NA_real_,
                             c(self$nquad,self$nocc,self$nsubj))
      self$lweights[,1L,] <- 0
      private$Theta <-
       na_quad_frame(self$nsubj,self$minocc, self$maxocc,
                     self$nquad,self$qnames,self$isubj)
    }
  ),
  private = list(
    nq=1L
  ),
  active = list(
    nquad = function(value) {
      if (missing(value)) return(private$nq)
      private$nq <- value
    },
    qnames = function(value) {
      if (missing(value)) return(qname(private$Theta))
      if (length(value) != length(qname(private$Theta))) {
        private$Theta <-  na_quad_frame(self$nsubj,self$minocc, self$maxocc,
                                        self$nquad,value,self$isubj)
      } else {
        qname(private$Theta) <- value
      }
    }
  )
)

setOldClass(c("ParticleQuad","Quadrature","R6"))


particleQuad <- function(times,nquadrature,qnames,
                      byocc=TRUE, bysubj=TRUE,
                      nsubjects=nsubj(times),
                      isubj=NA_integer_) {
  ParticleQuad$new(times=times, nquadrature=nquadrature, qnames=qnames,
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
    initialize = function(times, quadrature,
                          byocc=FALSE, bysubj=FALSE,
                          nsubjects=nsubj(times),
                          isubj=NA_integer_) {
      super$initialize(times,quadrature,byocc,bysubj,nsubjects,isubj)
    },
    weights = function(type="default") {
      switch(type,
        default = {
          weights <- self$lweights*self$rweights
          as.vector(sweep(weights,2:3,apply(weights,2:3,sum),"/"))
        },
        left = {
          weights <- self$lweights
          as.vector(sweep(weights,2:3,apply(weights,2:3,sum),"/"))
        },
        right = {as.vector(self$rweights)},
        all = {data.frame(full=self$weights("default"),
                   left=self$weights("left"),
                   right=self$weights("right"))}
      )
    },
    resetWeights = function() {
      self$lweights <- array(NA_real_,
                             c(self$nquad,self$nocc,self$nsubj))
      self$rweights <- array(NA_real_,
                             c(self$nquad,self$nocc,self$nsubj))
    },
    rweight = function(subj,occ,quad,value) {
      occ <- occ +1L
      if (missing(quad)) quad <- 1L:nquad(self)
      if (missing(value)) return(self$rweights[quad,occ,subj])
      self$rweights[quad,occ,subj] <- value
    }
  )
)


setOldClass(c("BWQuad","Quadrature","R6"))



BWquad <- function(times,quadrature,
                   byocc=FALSE, bysubj=FALSE,
                   nsubjects=nsubj(times),isubj=NA_integer_) {
  BWQuad$new(times=times, quadrature=quadrature,
             byocc=byocc, bysubj=bysubj, nsubjects=nsubjects,
             isubj=isubj)
}

get_subj.BWQuad <- function(x,isub) {
  result <- get_subj.Quadrature(x,isub)
  result$rweights <- x$rweights[,,isub,drop=FALSE]
  result
}

"get_subj<-.BWQuad" <-function(x,isub,value) {
  x <- NextMethod()
  x$rweights <- padSubj.QuadWeights(x$rweights,isub)
  x$rweights[,,isub] <- value$rweights
  x
}
