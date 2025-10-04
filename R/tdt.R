Panmat <- setClass(
  "Panmat",
  slots=c(
    mat="matrix",
    nsubj="integer",
    nocc="integer"
  )
)

panmat <- function(mat,nsubj=nrow(mat),nocc=ncol(mat)) {
  if (!is.matrix(mat)) {
    nsubj <- 1L
    nocc <- length(mat)
    mat <- matrix(mat,1L,length(mat))
  }
  Panmat$new(mat,as.integer(nsubj),as.integer(nocc))
}


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

setGeneric("as.Panmat",function(obj) standardGeneric("as.Panmat"))
setMethod("as.Panmat","Panmat",function(obj) obj)
setMethod("as.Panmat","matrix",function(obj) panmat(obj))

setMethod("print","Panmat",function(x, ...) print(x@mat,...))

setGeneric("nsubj",function(obj) standardGeneric("nsubj"))
setMethod("nsubj","Panmat", function(obj) obj@nsubj)
setGeneric("nsubj<-",function(obj,value) standardGeneric("nsubj<-"))
setMethod("nsubj<-","Panmat", function(obj,value) {
  obj@nsubj<-as.integer(value)
  obj
})

setGeneric("nocc",function(obj) standardGeneric("nocc"))
setMethod("nocc","Panmat", function(obj) obj@nocc)
setGeneric("nocc<-",function(obj,value) standardGeneric("nocc<-"))
setMethod("nocc<-","Panmat", function(obj,value) {
  obj@nocc <- as.integer(value)
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
  if (!missing(j) && ncol(x@mat) == 1L) {
    if (is.logical(j)) {
      i <- rep(1L,sum(j))
    } else {
      i <- rep(1L,length(j))
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
  if (!missing(j) && ncol(x@mat) == 1L) {
    if (is.logical(j)) {
      i <- rep(1L,sum(j))
    } else {
      i <- rep(1L,length(j))
    }
  }
  x@mat[i,j,...] <- value
  x
})


TDT <- R6Class(
  "TDT",
  public=list(
    initialize=function(time,dt) {

    }
  ),
  private=list(
    DeltaT=panmat(1.0),
    Times=panmat(c(0.0,1.0))
  ),
  active=list(
    time=function(value) {
      if (missing(value)) return(private$Times)
      if (!is.matrix(value))
        value <- matrix(value,1L,length(value))
      mat(private$Times) <- value
      mat(private$DeltaT) <- t(apply(value,1,diff))
    },
    dt=function(value) {
          if (missing(value)) return(private$DeltaT)
          if (!is.matrix(value)) {
            value <- matrix(value,1L,length(value))
          }
          mat(private$DeltaT) <- value
          mat(private$Times) <- t(apply(cbind(0,value),1,cumsum))
    },
    maxocc=function() {
      nocc(private$DeltaT)
    }
  )
)

normalForm <- function(data,idcol="subj",timecol="occ") {
  data <- as.data.frame(data)
  if (idcol %in% names(data)) {
    nsubj <- max(data[[idcol]])
    by1 <- expr(!!paste("subj==",idcol))
  } else {
    nsubj <- 1L
    by1 <- NULL
  }
  if (timecol %in% names(data)) {
    by2 <- expr(!!paste("occ==",timecol))
    maxocc <- max(data[[timecol]])
    minocc <- min(data[[timecol]])
  } else {
    by2 <- NULL
    minocc <- 1L
    maxocc <- 1L
  }
  ord <- expand.grid(occ=minocc:maxocc,subj=1L:nsubj)
  by <- dplyr::join_by(!!!c(by1,by2))
  dropcol <- unique(c("subj","occ",idcol,timecol))
  normdat <- dplyr::left_join(ord,data,by) |>
    dplyr::select(!any_of(dropcol))
  return(normdat=normdat,nsubj=nsubj,minocc=minocc,maxocc=maxocc)
}


Panel_Data <- setClass(
  "Panel_Data",
  slots=list(
      dat="data.frame",
      nsubj="integer",
      minocc="integer",
      nocc1="integer"
  )
)

panel_data <- function(data,idcol="subj",timecol="occ") {
  normed <- normalForm(data,idcol,timecol)
  new(Panel_Data,normed$normdat,normed$nsubj,normed$minocc,
      normed$maxocc-normed$minocc)
}

ij2n <- function(pan,i,j) {
  if (is.character(i) || is.character(j))
    stop("Character indexes not supported with panel data.")
  if (any(i<0) || any(j<0))
    stop("Negative indexes not supported with panel data.")
  if (is.logical(i)) {
    if (pan@nsubj==1L) {
      i <- rep(1L,sum(i))
    } else {
      i <- (1L:pan@nsubj)[i]
    }
  } else {
    if (pan@subj==1L) {
      i <- rep(1L,length(i))
    }
  }
  if (is.logical(j)) {
    if (pan@nocc1==0L) {
      j <- rep(0L,sum(j))
    } else {
      j <- (0L:pan@nocc1)[j]*pan@nsubj
    }
  } else {
    if (pan@nocc1==0L) {
      j <- rep(0L,length(j))
    } else {
      j <- (j-pan@minocc)*pan@nsubj
    }
  }
  rep(i,length(j)) + rep(j,each=length(i))
}


setMethod("nsubj","Panel_Data", function(obj) obj@nsubj)
setMethod("nocc","Panel_Data", function(obj) obj@nocc+1L)
setMethod("[","Panel_Data",function(x, i, j, ..., drop=FALSE) {
  if (missing(i)) i <- 1L:x@nsubj
  if (missing(j)) j <- x@minocc + 0L:x@nocc1
  x@dat[ij2n(x,i,j),...,drop=drop]
})

setMethod("[<-","Panel_Data",function(x, i, j, ..., value) {
  if (missing(i)) i <- 1L:x@nsubj
  if (missing(j)) j <- x@minocc + 0L:x@nocc1
  x@dat[ij2n(x,i,j),...] <- value
})



