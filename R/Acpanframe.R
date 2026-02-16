


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
                       qcol="quad", zerostart=TRUE) {
  data <- as.data.frame(data)
  by <- list()
  if (idcol %in% names(data)) {
    nsubj <- as.integer(max(data[[idcol]]))
    bsubj <- TRUE
    by <- list("subj")
    if (idcol != "subj") {
      names(data)[names(data)==idcol] <- "subj"
    }
  } else {
    nsubj <- 1L
    bsubj <- FALSE
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
    bocc <- maxocc > minocc
    if (bocc) by <- c(by,list("occ"))
  } else {
    bocc <- FALSE
    if (zerostart) {
      minocc <- 0L
      maxocc <- 0L
    } else {
      minocc <- 1L
      maxocc <- 1L
    }
  }
  if (qcol %in% names(data)) {
    if (qcol != "quad") {
      names(data)[names(data)==qcol] <- "qcol"
    }
    nquad <- length(unique(data[["quad"]]))
    ord <- expand.grid(quad=1L:nquad,occ=minocc:maxocc,subj=1L:nsubj)
    by <- c(by,list("quad"))
  } else {
    nquad <- 0L
    ord <- expand.grid(occ=minocc:maxocc,subj=1L:nsubj)
  }
  if (is.null(by)) {
    normdat <- data
  } else {
    by <- do.call(dplyr::join_by,by)
    normdat <- dplyr::left_join(ord,data,by) |>
      dplyr::select(!dplyr::any_of(c("subj","occ","quad")))
  }
  return(list(normdat=normdat,nsubj=nsubj,minocc=minocc,maxocc=maxocc,
              nquad=nquad,bysubj=bsubj,byocc=bocc))
}

### Panel_Frame ----
Panel_Frame <- setClass(
  "Panel_Frame",
  slots=list(
      dat="data.frame",
      nsubj="integer",
      bysubj="logical",
      nocc1="integer",
      minocc="integer",
      byocc="logical",
      isubj="integer"
  )
)

setMethod("print","Panel_Frame",function(x, ...) print(x@dat,...))
setMethod("names","Panel_Frame", function(x) names(x@dat))

panel_frame <- function(data,idcol="subj",occcol="occ",
                        zerostart=TRUE,isubj=NA_integer_) {
  normed <- normalForm(data,idcol,occcol,zerostart=zerostart)
  new("Panel_Frame",dat=normed$normdat,
      nsubj=normed$nsubj,
      minocc=normed$minocc,
      nocc1=normed$maxocc-normed$minocc,
      isubj=isubj, bysubj=normed$bysubj, byocc=normed$byocc)
}

mt_frame <- function(nsubj,maxocc) {
  new("Panel_Frame",dat=list2DF(nrow=nsubj*(maxocc+1L)),
      nsubj=as.integer(nsubj),bysubj=(nsubj>1L),
      minocc=0L,nocc1=as.integer(maxocc),byocc=(maxocc>0L))
}

ij2n <- function(pan,i=1L:nsubj(pan),j=minocc(pan):maxocc(pan),
                 q=1L:nquad(pan)) {
  if (is.character(i) || is.character(j) || is.character(q))
    stop("Character indexes not supported with panel data.")
  if (!pan@bysubj) {
    if (is.logical(i)) {
      i <- rep(0L,sum(i))
    } else {
      i <- rep(0L,length(i))
    }
  } else {
    i <- (0L:(pan@nsubj-1L))[i]*nocc(pan)
  }
  if (!pan@byocc) {
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
  ij <- rep(i,each=length(j)) + rep(j,length(i))+1L
  if (nquad(pan)==1L) return(ij)
  if (is.logical(q)) {
    q <- 1L:nquad(pan)[q]
  }
  rep((ij-1L)*nquad(pan),each=length(q)) +
    rep(q,length(ij))

}

frameLength <- function (pf) {
  ifelse(pf@bysubj,pf@nsubj,1L) *
    ifelse(pf@byocc,pf@nocc1+1L,1L) *
    nquad(pf)
}

### Panel_Frame methods ----
setMethod("nsubj","Panel_Frame", function(obj) obj@nsubj)
setMethod("nsubj<-","Panel_Frame", function(obj,value) {
  osubj <- obj@nsubj
  newsubj <- as.integer(value)
  if (obj@bysubj && osubj != newsubj) {
    if (osubj > newsubj) {
      ## Delete unneeded rows
      droprows <- ij2n(obj,(newsubj+1L):osubj)
      obj@dat <- obj@dat[-droprows,,drop=FALSE]
      obj@nsubj <- newsubj #Need old indexes
    } else {
      ## Add needed rows
      odat <- obj@dat
      obj@nsubj <- newsubj # Need new indexes
      obj@dat[1L:frameLength(obj),] <- NA
      keeprows <- ij2n(obj,1L:osubj)
      obj@dat[keeprows,] <- odat
    }
    rownames(obj@dat) <- 1L:frameLength(obj)
  }
  obj
})

setMethod("bysubj","Panel_Frame",function(obj) obj@bysubj)
setMethod("bysubj<-","Panel_Frame",function(obj,value) {
  obj@bysubj <- as.logical(value)
  obj
})


setMethod("nquad","Panel_Frame", function(obj) 1L)
setMethod("nocc","Panel_Frame", function(obj) obj@nocc1+1L)
setMethod("nocc<-","Panel_Frame", function(obj,value) {
  oncc <- obj@nocc1
  nncc <- as.integer(value)-1L
  if (obj@byocc && oncc != nncc) {
    if (oncc > nncc) {
      ## Delete unneeded rows
      obj@dat <- obj@dat[-ij2n(obj,j=(nncc+1L):oncc),,drop=FALSE]
      obj@nocc1 <- nncc # Need old indexes
    } else {
      ## Add needed rows
      odat <- obj@dat
      obj@nocc1 <- nncc # Need new indexes
      obj@dat[1L:frameLength(obj),] <- NA
      obj@dat[ij2n(obj,j=0L:oncc),] <- odat
    }
  }
  rownames(obj@dat) <- 1L:frameLength(obj)
  obj
})

setMethod("byocc","Panel_Frame",function(obj) obj@byocc)
setMethod("byocc<-","Panel_Frame",function(obj,value) {
  obj@byocc <- as.logical(value)
  obj
})


setMethod("minocc","Panel_Frame", function(obj) obj@minocc)
setMethod("minocc<-","Panel_Frame", function(obj,value) {
  obj@minocc <- as.integer(value)
  obj
})
setMethod("maxocc","Panel_Frame", function(obj) obj@minocc+obj@nocc1)
setMethod("maxocc<-","Panel_Frame", function(obj,value) {
  nocc(obj) <- as.integer(value)-obj@minocc+1L
  obj
})

### [,Panel_Frame ----
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

### as_longform,Panel_Frame ----
setMethod("as_longform","Panel_Frame",
          function(x,...,n=nsubj(x),mxocc=maxocc(x),
                   mnocc=minocc(x)) {
  result <- x@dat
  if (nquad(x)==1L) {
    result <- data.frame(subj=rep(1L:nsubj(x),each=nocc(x)),
                         occ=rep(x@minocc+0L:x@nocc1,
                                 rep=nsubj(x)),
                         result)
  } else {
    result <- data.frame(subj=rep(1L:nsubj(x),each=nocc(x)*nquad(x)),
                         occ=rep(rep(x@minocc+0L:x@nocc1,nsubj(x)),
                                 each=nquad(x)),
                         quad=rep(1L:nquad(x),nocc(x)*nsubj(x)),
                         result)
  }

  if (!byocc(x) && mxocc-mnocc > 0L) {
    r1 <- result
    for (iocc in mnocc:mxocc) {
      r1$occ <- iocc
      result <- rbind(result,r1)
    }
    ## Sort into canonical order.
    result <- result[order(result$subj,result$occ),]
  }
  if (!bysubj(x) && n > 1L) {
    r1 <- result
    for (isubj in 2L:n) {
      r1$subj <- isubj
      result <- rbind(result,r1)
    }
  }
  result
})


### get_subj,Panel_Frame ----
setMethod("get_subj","Panel_Frame", function(x,isub) {
  new("Panel_Frame",dat=x[isub,,],nsubj=1L,
      minocc=minocc(x),nocc1=nocc(x)-1L,isubj=as.integer(isub),
      bysubj=bysubj(x),byocc=byocc(x))
})

setMethod("get_subj<-","Panel_Frame", function(x,isub,value) {
  x@bysubj <- TRUE
  x@nsubj <- ifelse(isub>nsubj(x),as.integer(isub),nsubj(x))
  x[isub,,] <- value@dat
  x@isubj <- NA_integer_
  x
})


setMethod("isubj","Panel_Frame", function(obj) obj@isubj)
setMethod("isubj<-","Panel_Frame", function(obj,value) {
  obj@isubj<-as.integer(value)
  obj
})

### Quad Frames ----
### Also have quadrature slot
Quad_Frame <- setClass(
  "Quad_Frame",
  contains="Panel_Frame",
  slots=list(
      nquad="integer"
  )
)
quad_frame <- function(data,idcol="subj",occcol="occ",
                       qcol="quad",
                        zerostart=TRUE,isubj=NA_integer_) {
  normed <- normalForm(data,idcol,occcol,qcol,zerostart)
  new("Quad_Frame",dat=normed$normdat,
      nsubj=normed$nsubj,
      minocc=normed$minocc,
      nocc1=normed$maxocc-normed$minocc, nquad=normed$nquad,
      isubj=isubj, bysubj=normed$bysubj, byocc=normed$byocc)
}

setMethod("nquad","Quad_Frame", function(obj) obj@nquad)
setMethod("nquad<-","Quad_Frame", function(obj,value) {
  oquad <- obj@nquad
  newquad<-as.integer(value)
  if (oquad != newquad) {
    if (oquad > newquad) {
      ## Delete unneeded rows
      droprows <- ij2n(obj,q=(newquad+1L):oquad)
      obj@dat <- obj@dat[-droprows,,drop=FALSE]
      obj@nquad <- newquad  ## Need old numbers
    } else {
      ## Add needed rows
      odat <- obj@dat
      obj@nquad <- newquad ## Need new numbers
      obj@dat[1L:frameLength(obj),] <- NA
      keeprows <- ij2n(obj,q=1L:oquad)
      obj@dat[keeprows,] <- odat
    }
  }
  obj
})

### [,Quad_Frame ----
setMethod("[","Quad_Frame",function(x, i, j, q, v, ..., drop=FALSE) {
  if (missing(i)) i <- 1L:x@nsubj
  if (missing(j)) j <- x@minocc + 0L:x@nocc1
  if (missing(q)) q <- 1L:x@nquad
  if (missing(v)) {
      x@dat[ij2n(x,i,j,q),,...,drop=drop]
  } else {
      x@dat[ij2n(x,i,j,q),v,...,drop=drop]
  }
})

setMethod("[<-","Quad_Frame",function(x, i, j, q, v, ..., value) {
  if (missing(i)) i <- 1L:x@nsubj
  if (missing(j)) j <- x@minocc + 0L:x@nocc1
  if (missing(q)) q <- 1L:x@nquad
  if (missing(v)) {
    x@dat[ij2n(x,i,j,q),,...] <- value
  } else {
    x@dat[ij2n(x,i,j,q),v,...] <- value
  }
  x
})


### get_subj,Quad_Frame ----
setMethod("get_subj","Quad_Frame", function(x,isub) {
  new("Quad_Frame",dat=x[isub,,,],nsubj=1L,
      minocc=minocc(x),nocc1=nocc(x)-1L,isubj=as.integer(isub),
      bysubj=bysubj(x),byocc=byocc(x),nquad=nquad(x))
})

setMethod("get_subj<-","Quad_Frame", function(x,isub,value) {
  x@bysubj <- TRUE
  x@nsubj <- ifelse(isub>nsubj(x),as.integer(isub),nsubj(x))
  x[isub,,,] <- value@dat
  x@isubj <- NA_integer_
  x
})

