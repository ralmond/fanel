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

setGeneric("as.Panmat",function(obj) standardGeneric("as.Panmat"))
setMethod("as.Panmat","Panmat",function(obj) obj)
setMethod("as.Panmat","matrix",function(obj) panmat(obj))
setMethod("as.Panmat","numeric",function(obj)
  panmat(matrix(obj,1L,length(obj))))
setMethod("as.Panmat","character",function(obj)
  panmat(matrix(obj,1L,length(obj))))



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

setGeneric("maxocc",function(obj) standardGeneric("maxocc"))
setMethod("maxocc","Panmat", function(obj) obj@nocc+1L-obj@minocc)
setGeneric("maxocc<-",function(obj,value) standardGeneric("maxocc<-"))
setMethod("maxocc<-","Panmat", function(obj,value) {
  obj@nocc <- as.integer(value) + obj@minocc
  obj
})


setGeneric("minocc",function(obj) standardGeneric("minocc"))
setMethod("minocc","Panmat", function(obj) obj@minocc)
setGeneric("minocc<-",function(obj,value) standardGeneric("minocc<-"))
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

setGeneric("as_longform",function(x,n=nsubj(x),maxocc=maxocc(x),
                                  minocc=minocc(x),
                                  name=deparse(substitute(x))) {
  standardGeneric("as_longform")
})

setMethod("as_longform","Panmat",
          function(x,n=nsubj(x),maxocc=nocc(x),
                   minocc=1L,
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




### Panel_Frame

time2occ <- function(data, idcol="subj",timecol="time",
                     occcol="occ",
                     zerobase=FALSE) {
  data <- as.data.frame(data)
  data[[occcol]] <- NA_integer_
  subj <- data[[idcol]]
  if (is.null(subj)) {
    occ<- seq_len(data)-as.integer(zerobase)
    data[[occcol]] <- occ[order(data[[timecol]])]
  } else {
    for (isubj in unique(subj)) {
      xsubj <- isubj==subj
      occ <- 1L:sum(xsubj) -as.integer(zerobase)
      data[xsubj,occcol] <- occ[order(data[xsubj,timecol])]
    }
  }
  data
}

normalForm <- function(data,idcol="subj",occcol="occ",
                       zerostart=TRUE) {
  data <- as.data.frame(data)
  if (idcol %in% names(data)) {
    nsubj <- as.integer(max(data[[idcol]]))
    by1 <- TRUE
    if (idcol != "subj") {
      names(data)[names(data)==idcol] <- "subj"
    }
  } else {
    nsubj <- 1L
    by1 <- FALSE
  }
  if (occcol %in% names(data)) {
    maxocc <- as.integer(max(data[[occcol]]))
    if (zerostart) {
      minocc <- 0L
    } else {
      minocc <- as.integer(min(data[[occcol]]))
    }
    if (occcol != "occ") {
      names(data)[names(data)==occcol] <- "occ"
    }
    if (by1) {
      by <- dplyr::join_by(subj,occ)
    } else {
      by <- dplyr::join_by(occ)
    }
  } else {
    if (by1) {
      by <- dplyr::join_by(subj)
    } else {
      by <- NULL
    }
    minocc <- 1L
    maxocc <- 1L
  }
  ord <- expand.grid(occ=minocc:maxocc,subj=1L:nsubj)
  if (is.null(data)) {
    normdat <- data
  } else {
    normdat <- dplyr::left_join(ord,data,by) |>
      dplyr::select(!dplyr::any_of(c("subj","occ")))
  }
  return(list(normdat=normdat,nsubj=nsubj,minocc=minocc,maxocc=maxocc))
}


Panel_Frame <- setClass(
  "Panel_Frame",
  slots=list(
      dat="data.frame",
      nsubj="integer",
      nocc1="integer",
      minocc="integer"
  )
)

setMethod("print","Panel_Frame",function(x, ...) print(x@dat,...))
setMethod("names","Panel_Frame", function(x) names(x@dat))

panel_frame <- function(data,idcol="subj",occcol="occ",
                        zerostart=TRUE) {
  normed <- normalForm(data,idcol,occcol,zerostart)
  new("Panel_Frame",dat=normed$normdat,
      nsubj=normed$nsubj,
      minocc=normed$minocc,
      nocc1=normed$maxocc-normed$minocc)
}

mt_frame <- function(nsubj,maxocc) {
  new("Panel_Frame",dat=list2DF(nrow=nsubj*(maxocc+1L)),
      nsubj=as.integer(nsubj),
      minocc=0L,nocc1=as.integer(maxocc))
}

ij2n <- function(pan,i,j) {
  if (is.character(i) || is.character(j))
    stop("Character indexes not supported with panel data.")
  if (pan@nsubj==1L) {
    if (is.logical(i)) {
      i <- rep(0L,sum(i))
    } else {
      i <- rep(0L,length(i))
    }
  } else {
    i <- (0L:(pan@nsubj-1L))[i]*(pan@nocc1+1L)
  }
  if (pan@nocc1==0L) {
    if (is.logical(j)) {
      j <- rep(0L,sum(j))
    } else {
      j <- rep(0L,length(j))
    }
  } else {
    if (pan@minocc==0L && is.numeric(j) && all(j>=0L)) {
      j <- j+1L
    }
    j <- (0L:pan@nocc1)[j]
  }
  rep(i,each=length(j)) + rep(j,length(i))+1L
}


setMethod("nsubj","Panel_Frame", function(obj) obj@nsubj)
setMethod("nocc","Panel_Frame", function(obj) obj@nocc1+1L)
setMethod("minocc","Panel_Frame", function(obj) obj@minocc)
setMethod("maxocc","Panel_Frame", function(obj) obj@minocc+obj@nocc1)
setMethod("[","Panel_Frame",function(x, i, j, v, ..., drop=FALSE) {
  if (missing(i)) i <- 1L:x@nsubj
  if (missing(j)) j <- x@minocc + 0L:x@nocc1
  if (missing(v)) {
      x@dat[ij2n(x,i,j),,...,drop=drop]
  } else {
      x@dat[ij2n(x,i,j),v,...,drop=drop]
  }
})

setMethod("[<-","Panel_Frame",function(x, i, j, v, ..., value) {
  if (missing(i)) i <- 1L:x@nsubj
  if (missing(j)) j <- x@minocc + 0L:x@nocc1
  if (missing(v)) {
    x@dat[ij2n(x,i,j),,...] <- value
  } else {
    x@dat[ij2n(x,i,j),v,...] <- value
  }
  x
})


setMethod("as_longform","Panel_Frame",
          function(x,n=nsubj(x),maxocc=nocc(x),
                   minocc=1L,
                   name=deparse(substitute(x))) {
  result <- x@dat
  if (!missing(name)) names(result) <- name
  result <- data.frame(subj=rep(1L:nsubj(x),each=nocc(x)),
                       occ=rep(x@minocc+0L:x@nocc1,
                               rep=nsubj(x)),
                       result)
  if (nocc(x)==1L && maxocc-minocc > 0L) {
    r1 <- result
    for (iocc in minocc:maxocc) {
      r1$occ <- iocc
      result <- rbind(result,r1)
    }
    ## Sort into canonical order.
    result <- result[order(result$subj,result$occ),]
  }
  if (nsubj(x)==1L && n > 1L) {
    r1 <- result
    for (isubj in 2L:n) {
      r1$subj <- isubj
      result <- rbind(result,r1)
    }
  }
  result
})


### Panel_Data

Panel_Data <- R6Class(
  "Panel_Data",
  public=list(
    initialize=function(time,dt,vari=NULL,invar=NULL,datacols=character()) {
      if (missing(time)) {
        self$dt <- dt
      } else {
        if (!missing(dt)) {
          warning("Both time and dt supplied, using time.")
        }
        self$time <- time
      }
      if (is.null(vari)) {
        self$vari <- mt_frame(nsubj(private$Times),nocc(private$Times))
      } else {
        self$vari <- vari
      }
      if (is.null(invar)) {
        self$invar <- list2DF(nrow=nsubj(private$Times))
      } else {
        self$invar <- invar
      }
      self$datacols <- datacols
    },
    print = function(x, ...) {
      print(paste("<Panel Data: ",nsubj(x),
                  "x",nocc(x),">"))
    },
    getVar=function(subj,occ) {
      data.frame(private$invariant[subj,],
                 private$variant[subj,occ])
    },
    getData = function(subj, occ) {
      if (is.null(private$variant)) return (NA)
      private$variant[subj,occ,self$datacols]
    },
    setData = function(subj, occ, value) {
      if (is.null(private$variant)) {
        stop("No variant data available.")
      }
      private$variant[subj,occ,self$datacols] <- value
    }
  ),
  private=list(
    DeltaT=panmat(1.0,minocc=1L),
    Times=panmat(c(0.0,1.0),minocc=0L),
    variant = NULL,
    invariant = NULL,
    dcols = character()
  ),
  active=list(
    time=function(value) {
      if (missing(value)) return(private$Times)
      value <- as.Panmat(value)
      mat(private$Times) <- mat(value)
      mat(private$DeltaT) <- mat(diff(value))
    },
    dt=function(value) {
      if (missing(value)) return(private$DeltaT)
      value <- as.Panmat(value)
      mat(private$DeltaT) <- mat(value)
      mat(private$Times) <- mat(cumsum(value))
    },
    maxocc=function() {
      nocc(private$DeltaT)
    },
    vari=function(value) {
      if (missing(value)) return(private$variant)
      if (!is(value,"Panel_Frame")) {
        value <- panel_frame(value)
      }
      private$variant <- value
    },
    invar=function(value) {
      if (missing(value)) return(private$invariant)
      private$invariant <- as.data.frame(value)
    },
    datacols=function(value) {
      if (missing(value)) return(private$dcols)
      private$dcols <- value
      for (v in value) {
        if (!(v %in% names(private$variant))) {
          private$variant[[v]] <- NA
        }
      }
    }
  )
)

setOldClass("Panel_Data")

panel_data <- function(time, dt, vari = NULL, invar= NULL,
                       datacols=character()) {
  if (missing(time) && missing(dt)) {
    stop("Need at least one of time and dt.")
  }
  if (!missing(time) && !missing(dt)) {
    stop("I'm confused, should I use time or dt?")
  }
  if (!missing(datacols) && !missing(vari)) {
    miscol <- setdiff(datacols,names(vari))
    if (length(miscol) > 0L) {
      warning("Columns ",paste(miscol,sep=", "), " are not in data.  Will be set to NA.")
    }
  }
  if (missing(time)) {
    result <- Panel_Data$new(dt=dt,vari=vari,invar=invar,
        datacols=datacols)
  }
  if (missing(dt)) {
    result <- Panel_Data$new(time=time,vari=vari,invar=invar,
        datacols=datacols)
  }
  result
}

setMethod("nsubj","Panel_Data", function(obj) nsubj(obj$dt))
setMethod("nsubj<-","Panel_Data", function(obj,value) {
  nsubj(obj$dt) <-as.integer(value)
  nsubj(obj$t) <-as.integer(value)
  obj
})


setMethod("nocc","Panel_Data", function(obj) nocc(obj$dt))
setMethod("minocc","Panel_Data", function(obj) minocc(obj$dt))
setMethod("maxocc","Panel_Data", function(obj) maxocc(obj$dt))



setMethod("as_longform","Panel_Data",
          function(x,n=nsubj(x),maxocc=nocc(x),
                   minocc=1L,
                   name=deparse(substitute(x))) {
  if (missing(name)) name <- "time"
  if (missing(minocc)) minocc <- 0L
  as_longform(x$time,n,maxocc,minocc,name) |>
    dplyr::left_join(cbind(subj=1:n,x$invar),
                     dplyr::join_by(subj)) |>
    dplyr::left_join(as_longform(x$vari,n,maxocc,
                                        minocc),
                     dplyr::join_by(subj,occ))
})

long2panel <- function(df, idcol="subj", timecol="time", occcol="occ",
                       datacols=character(),
                       invcols=character(), invar=NULL) {
  ## Create occ from time if needed.
  if (!(occcol %in% names(df))) {
    df <- time2occ(df,idcol=idcol, timecol=timecol, occcol=occcol,
                   zerobase=TRUE)
  }

  nf <- normalForm(dplyr::select(df,!dplyr::any_of(invcols)),
                   idcol,occcol)
  nocc <- nf$maxocc - nf$minocc + 1L
  time <- matrix(nf$normdat[[timecol]],nf$nsubj,nocc, byrow=TRUE)
  if (nf$minocc > 0L) {
    time <- cbind(0,mat(time))
  }
  dat <- dplyr::select(nf$normdat,!dplyr::any_of(timecol))

  if (length(invcols) > 0L) {
    if (!missing(invar)) {
      stop("I'm confused, I got both invcols and separate invar data.")
    }
    invar <- df[df[[occcol]]==nf$minocc,c(idcol,invcols)]
    invar <- invar[1L:nf$nsubj,invcols]
  }

  panel_data(time=as.Panmat(time),
             vari = new("Panel_Frame", dat=dat, nsubj=nf$nsubj,
                        minocc=nf$minocc, nocc1=nocc-1L),
             invar = invar, datacols=datacols)

}
