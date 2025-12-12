


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
      by <- dplyr::join_by("subj","occ")
    } else {
      by <- dplyr::join_by("occ")
    }
  } else {
    if (by1) {
      by <- dplyr::join_by("subj")
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
      minocc="integer",
      isubj="integer"
  )
)

setMethod("print","Panel_Frame",function(x, ...) print(x@dat,...))
setMethod("names","Panel_Frame", function(x) names(x@dat))

panel_frame <- function(data,idcol="subj",occcol="occ",
                        zerostart=TRUE,isubj=NA_integer_) {
  normed <- normalForm(data,idcol,occcol,zerostart)
  new("Panel_Frame",dat=normed$normdat,
      nsubj=normed$nsubj,
      minocc=normed$minocc,
      nocc1=normed$maxocc-normed$minocc,
      isubj=isubj)
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
                   minocc=1L) {
  result <- x@dat
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

setMethod("get_subj","Panel_Frame", function(x,isubj) {
  new("Panel_Frame",dat=x[isubj,,],nsubj=1L,
      minocc=minocc(x),nocc1=nocc(x)-1L,isubj=isubj)

})

setMethod("get_subj<-","Panel_Frame", function(x,isubj,value) {
  x[isubj,,] <- value@dat
  isubj(x) <- NA_integer_
  x
})


setMethod("isubj","Panmat", function(obj) obj@isubj)
setMethod("isubj<-","Panmat", function(obj,value) {
  obj@isubj<-as.integer(value)
  obj
})

