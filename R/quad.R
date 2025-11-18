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
    initialize = function(times, qpoints, tnames=vnames(qpoints),
                          static=TRUE, bysubj=TRUE,
                          nsubjects=nsubj(times)) {
      self$static <- static
      self$bysubj <- bysubj
      private$Times <- as.Panmat(times,mimocc=1L)
      self$thetas <- qpoints
      self$nsubj <- nsubjects
    },
    weights = function(type="default") {
      weights <- exp(self$lweights)
      sweep(weights,1:2,apply(weights,1:2,sum),"/")
    },
    resetWeights = function() {
      self$lweights <- array(NA_real_,
                             c(self$nsubj,self$nocc,self$nquad))
    },
  ),
  private=list(
    Theta = array(NA_real_,c(1,1,1,1)),
    Tnames=character(),
    Times=panmat(c(0.0,1.0),minocc=0L)
  ),
  active=list(
    thetas = function(value) {
      if (missing(value)) return(private$Theta)
      vnames <- vnames(value)
      if (is.data.frame(value)) {
        value <- as.matrix(value)
      }
      vdims <- dim(value)
      if (length(vdims) != 4L) {
        if (length(vdims)+self$fixed+self$uniform != 4L) {
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
      if (!bysubj) subj <- 1L
      if (missing(quad)) quad <- 1L:nquad(self)
      if (mising(value)) return(private$Theta[subj,occ,quad,])
      private$Theta[subj,occ,quad,] <- value
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


setMethod("nsubj","Quadrature", function(obj) nsubj(obj))
setMethod("nsubj<-","Quadrature", function(obj,value) {
  nsubj(obj) <-as.integer(value)
  obj
})

setMethod("nocc","Quadrature", function(obj) nocc(obj))
setMethod("nocc<-","Quadrature", function(obj,value) {
  obj$nocc <- as.integer(value)
  obj
})

setMethod("minocc","Quadrature", function(obj) minocc(obj))
setMethod("minocc<-","Quadrature", function(obj,value) {
  minocc(obj) <- as.integer(value)
  obj
})
setMethod("maxocc","Quadrature", function(obj) maxocc(obj))
setMethod("maxocc<-","Quadrature", function(obj,value) {
  maxocc(obj) <- as.integer(value) 
  obj
})

setMethod("nquad","Quadrature", function(obj) obj$nquad)
setMethod("nquad<-","Quadrature", function(obj,value) {
  obj$nquad <- as.integer(value) 
  obj
})




setMethod("as_longform","Quadrature",
          function(x,n=nsubj(x),maxocc=nocc(x),
                   minocc=1L,weightType="all",
                   name=deparse(substitute(x))) {
  result <- data.frame(subj=1L:nsubj(obj),each=nocc(obj)*nquad(obj),
                       occ=rep(rep(minocc(obj):maxocc(obj),each=nquad(obj)),
                               nsubj(obj)),
                       quad=rep(1L:nquad(obj),nsubj(obj)*nocc(obj)),
                       w=obj$weights(weightType))
  thetas <- as.data.frame(matrix(obj$thetas,ncol=obj$dtheta,
                                 byrow=TRUE))
  names(thetas) <- obj$tnames
  if (obj$static) {
    if (obj$bysubj) {
      thetas <- data.frame(subj=rep(1L:nsubj(obj),each=nquad(obj)),
                           quad=rep(1L:nquad(obj),nsubj(obj)),
                           thetas)
      by <- dplyr::join_by(subj,quad)
    } else {
      thetas <- data.frame(quad=1L:nquad(obj),
                           thetas)
      by <- dplyr::join_by(quad)
    }
  } else {
    if (obj$bysubj) {
      thetas <- data.frame(subj=rep(1L:nsubj(obj),each=nquad(obj)*nocc(obj)),
                           occ=rep(rep(minocc(obj):maxocc(obj),
                                       each=nquad(obj)),
                                   nsubj(obj)),
                           quad=rep(1L:nquad(obj),nsubj(obj)*nocc(obj)),
                           thetas)
      by <- dplyr::join_by(subj, occ, quad)
    } else {
      thetas <- data.frame(occ=rep(minocc(obj):maxocc(obj)),
                           quad=rep(1L:nquad(obj),nocc(obj)),
                           thetas)
      by <- dplyr::join_by(occ, quad)
    }
  }
  result |>
    dplyr::left_join(as_longform(obj$times,n,maxocc,minocc,name),
                     dplyr::join_by(subj,occ)) |>
    dplyr::left_join(as_longform(thetas),by)
  
})


setMethod("get_subj","Quadrature", function(x,subj) {
    initialize = function(times, qpoints, tnames=vnames(qpoints),
                          static=TRUE, bysubj=TRUE,
                          nsubjects=nsubj(times)) {
      self$static <- static
      self$bysubj <- bysubj
      private$Times <- as.Panmat(times,mimocc=1L)
      self$thetas <- qpoints
      self$nsubj <- nsubjects
    },
  new("Quadrature",times=x$times[subj,,],nsubj=1L,
      minocc=minocc(x),nocc1=nocc(x)-1L)

})

setMethod("get_subj<-","Panel_Frame", function(x,subj,value) {
  x[subj,,] <- value@dat
  x
})

setMethod("add_subj","Panel_Frame", function(x,xnew) {
  get_subj(x,nsubj(x)+1L) <- xnew
})



FixedQuad <- R6Class(
  "FixedQuad",
  inherit =  Quadrature,
  public = list(


  )
)

fixedQuad <- function(times,qpoints,tnames=vnames(qpoints),
                      static=TRUE, bysubj=(nsubj(times)>1L),
                      nsubjects=nsubj(times)) {
  FixedQuad$new(times=times, qpoints=qpoints, tnames=tnames,
                static=static, bysubj=bysubj, nsubjects=nsubjects)
}


ParticleQuad <- R6Class(
  "ParticleQuad",
  inherit =  Quadrature,
  public = list(
    initialize = function(times, nquadrature, tnames="theta",
                          static=TRUE, bysubj=FALSE,
                          nsubjects=nsubj(times)) {
      qpoints <- array(NA_real_,c(nsubjects,nocc(times),nquadrature,
                                  length(tnames)))
      super$initialize(times,qpoints,tnames,static,bysubj,nsubjects)
      self$nquad <- nquadrature
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
                      static=TRUE, bysubj=(nsubj(times)>1L),
                      nsubjects=nsubj(times)) {
  ParticleQuad$new(times=times, nquadrature=nqudrature, tnames=tnames,
                static=static, bysubj=bysubj, nsubjects=nsubjects)
}



BWQuad <- R6Class(
  "BWQuad",
  inherit =  Quadrature,
  public = list(
    wname=paste0("w.",c("full","left","right")),
    rweights=array(NA_real_,c(1,1,1)),
    initialize = function(times, qpoints, tnames=vnames(qpoints),
                          static=TRUE, bysubj=FALSE,
                          nsubjects=nsubj(times)) {
      super$initialize(times,qpoints,tnames,static,bysubj,nsubjects)
    },
    weights = function(type="default") {
      switch(type,
        default = {
          weights <- exp(self$lweights+self$rweights)
          sweep(weights,1:2,apply(weights,1:2,sum),"/")
        },
        left = {
          weights <- exp(self$lweights)
          sweep(weights,1:2,apply(weights,1:2,sum),"/")
        },
        right = exp(self$rweights),
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
                   nsubjects=nsubj(times)) {
  BWQuad$new(times=times, qpoints=qpoints, tnames=tnames,
             static=static, bysubj=bysubj, nsubjects=nsubjects)
}

