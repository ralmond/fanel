Panmat <- setClass(
  "Panmat",
  slots=c(
    mat="matrix",
    nsubj="integer",
    nocc="integer",
    minocc="integer"
  )
)

panmat <- function(mat,nsubj=nrow(mat),nocc=ncol(mat),
                   minocc=1L) {
  if (!is.matrix(mat)) {
    nsubj <- 1L
    nocc <- length(mat)
    mat <- matrix(mat,1L,length(mat))
  }
  new("Panmat",mat=mat,
      nsubj=as.integer(nsubj),
      nocc=as.integer(nocc),
      minocc=as.integer(minocc))
}

setMethod("print","Panmat",function(x, ...) print(x@mat,...))

mat <- function(pmat) {
  pmat@mat
}

"mat<-" <- function (pmat,value) {
  if (!is.matrix(value))
    value <- matrix(value,1L,length(value))
  pmat@mat <- value
  if (nrow(value) > 1L) pmat@nsubj <- nrow(value)
  pmat@nocc <- ncol(value)
  pmat
}

setMethod("as.Panmat","Panmat",function(obj,minocc=1L) obj)
setMethod("as.Panmat","matrix",function(obj,minocc=1L) {
  panmat(obj,minocc=minocc)
})
setMethod("as.Panmat","numeric",function(obj,minocc=1L) {
  panmat(matrix(obj,1L,length(obj)),minocc=1L)
})
setMethod("as.Panmat","character",function(obj,minocc=1L) {
  panmat(matrix(obj,1L,length(obj)),minocc=1L)
})



setMethod("nsubj","Panmat", function(obj) obj@nsubj)
setMethod("nsubj<-","Panmat", function(obj,value) {
  obj@nsubj<-as.integer(value)
  obj
})


setMethod("nocc","Panmat", function(obj) obj@nocc)
setMethod("nocc<-","Panmat", function(obj,value) {
  obj@nocc <- as.integer(value)
  obj
})

setMethod("maxocc","Panmat", function(obj) obj@nocc+1L-obj@minocc)
setMethod("maxocc<-","Panmat", function(obj,value) {
  obj@nocc <- as.integer(value) + obj@minocc
  obj
})


setMethod("minocc","Panmat", function(obj) obj@minocc)
setMethod("minocc<-","Panmat", function(obj,value) {
  obj@minocc <- as.integer(value)
  obj
})


setMethod("[","Panmat",function(x, i, j, ..., drop=FALSE) {
  if (!missing(i) && nrow(x@mat) == 1L) {
    if (is.logical(i)) {
      i <- rep(1L,sum(i))
    } else {
      i <- rep(1L,length(i))
    }
  }
  if (!missing(j)) {
    if (ncol(x@mat) == 1L) {
      if (is.logical(j)) {
        j <- rep(1L,sum(j))
      } else {
        j <- rep(1L,length(j))
      }
    } else {
      if (all(j <= 0L)) {
        j <- j + x@minocc - 1L
      } else {
        j <- j -x@minocc + 1L
      }
    }
  }
  x@mat[i,j,...,drop=drop]
})

setMethod("[<-","Panmat",function(x, i, j, ..., value) {
  if (!missing(i) && nrow(x@mat) == 1L) {
    if (is.logical(i)) {
      i <- rep(1L,sum(i))
    } else {
      i <- rep(1L,length(i))
    }
  }
  if (!missing(j)) {
    if (ncol(x@mat) == 1L) {
      if (is.logical(j)) {
        j <- rep(1L,sum(j))
      } else {
        j <- rep(1L,length(j))
      }
    } else {
      if (all(j <= 0L)) {
        j <- j + x@minocc - 1L
      } else {
        j <- j - x@minocc + 1L
      }
    }
  }
  x@mat[i,j,...] <- value
  x
})


setMethod("as.matrix","Panmat",function(x, ...) x@mat)


setMethod("as_longform","Panmat",
          function(x,n=nsubj(x),maxocc=nocc(x),
                   minocc=1L,weightType="all",
                   name=deparse(substitute(x))) {
  col <- as.vector(t(x@mat))
  ncc <- maxocc-minocc+1L
  if (ncol(x@mat)==1L && ncc > 1L) {
    col <- rep(col,each=ncc)
  }
  if (nrow(x@mat)==1L && n > 1L) {
    col <- rep(col,n)
  }
  result <- data.frame(rep(1L:n,each=ncc),
                       rep(minocc:maxocc,n),
                       col)
  names(result) <- c("subj","occ",name)
  result
})

setMethod("diff","Panmat", function(x, ...) {
  dff <- panmat(t(apply(x@mat,1,diff)))
  nsubj(dff) <- nsubj(x)
  dff
})

setMethod("cumsum","Panmat", function(x) {
  cpm <- panmat(t(apply(cbind(0,x@mat),1,cumsum)))
  nsubj(cpm) <- nsubj(x)
  cpm
})

setMethod("get_subj","Panmat", function(x,subj) {
  as.panmat(x[subj,])
})

setMethod("get_subj<-","Panmat", function(x,subj,value) {
  x[subj,] <- as.matrix(value)
  x
})

setMethod("add_subj","Panmat", function(x,xnew) {
  get_subj(x,nsubj(x)+1L) <- xnew
})
