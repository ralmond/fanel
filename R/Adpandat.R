### Panel_Data

### This file needs to be loaded after panAmat and before panframe

Panel_Data <- R6Class(
  "Panel_Data",
  public=list(
    isubj=NA_integer_,
    initialize=function(time,dt,vari=NULL,invar=NULL,
                        dnames=character(),
                        isubj=NA_integer_) {
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
      self$dnames <- dnames
      self$isubj <- isubj
    },
    toString = function(...) {
      print(paste("<Panel Data: ",nsubj(self),
                  "x",nocc(self),">"))
    },
    print=function(...) {
      print(self$toString(...),...)
    },
    getVar=function(subj,occ) {
      if (missing(subj)) {
        subj <- 1L:nsubj(self)
      }
      if (missing(occ)) {
        occ <- minocc(self):maxocc(self)
      }
      as.data.frame(c(private$invariant[subj,],
                      private$variant[subj,occ]))
    },
    getData = function(subj, occ) {
      if (missing(subj)) {
        subj <- 1L:nsubj(self)
      }
      if (missing(occ)) {
        occ <- minocc(self):maxocc(self)
      }
      if (is.null(private$variant)) return (NA)
      private$variant[subj,occ,self$dnames]
    },
    setData = function(subj, occ, value) {
      if (is.null(private$variant)) {
        stop("No variant data available.")
      }
      private$variant[subj,occ,self$dnames] <- value
    },
    pullPanmat = function(vname) {
      panmat(private$variant[,,vname],nsubj=nsubj(self),
             nocc=nocc(self),minocc=minocc(self),
             isubj=isubj(self))
    },
    add_subj = function(isub,value) {
      get_subj(private$Times,isub) <- value$time
      get_subj(private$DeltaT,isub) <- value$dt
      get_subj(private$variant,isub) <- value$vari
      if (!is.null(value$invar)) {
        private$invariant[isub,] <- value$invar[1L,]
      }
      if (!is.na(self$isubj) && self$isubj != isub) {
        self$isubj <- NA_integer_
      }
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
      maxocc(private$Times)
    },
    minocc=function() {
      maxocc(private$Times)
    },
    nsubj=function(value) {
      if (missing(value)) return(nsubj(private$Times))
      nsubj(private$Times) <- as.integer(value)
      nsubj(private$DeltaT) <- as.integer(value)
    },
    vari=function(value) {
      if (missing(value)) return(private$variant)
      if (!is(value,"Panel_Frame")) {
        value <- panel_frame(value,zerostart=TRUE)
      }
      private$variant <- value
    },
    invar=function(value) {
      if (missing(value)) return(private$invariant)
      private$invariant <- as.data.frame(value)
    },
    invarnames = function() {
      if (is.null(private$invariant)) return(character())
      names(private$invariant)
    },
    dnames=function(value) {
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

setOldClass(c("Panel_Data","R6"))

panel_data <- function(time, dt, vari = NULL, invar= NULL,
                       dnames=character(),isubj=NA_integer_) {
  if (missing(time) && missing(dt)) {
    stop("Need at least one of time and dt.")
  }
  if (!missing(time) && !missing(dt)) {
    stop("I'm confused, should I use time or dt?")
  }
  if (!missing(dnames) && !missing(vari)) {
    miscol <- setdiff(dnames,names(vari))
    if (length(miscol) > 0L) {
      warning("Columns ",paste(miscol,sep=", "), " are not in data.  Will be set to NA.")
    }
  }
  if (missing(time)) {
    result <- Panel_Data$new(dt=dt,vari=vari,invar=invar,
        dnames=dnames,isubj=isubj)
  }
  if (missing(dt)) {
    result <- Panel_Data$new(time=time,vari=vari,invar=invar,
        dnames=dnames,isubj=isubj)
  }
  result
}

nsubj.Panel_Data <- function(obj) {obj$nsubj}
"nsubj<-.Panel_Data" <- function(obj,value) {
  obj$nsubj <-as.integer(value)
  obj
}


nocc.Panel_Data <- function(obj) {nocc(obj$time)}
minocc.Panel_Data <- function(obj) {minocc(obj$time)}
maxocc.Panel_Data <- function(obj) {maxocc(obj$time)}

getTime.Panel_Data <- function(obj) {obj$time}
"getTime<-.Panel_Data" <- function(obj,value) {obj$time <- value}
getDT.Panel_Data <- function(obj) {obj$dt}
"getDT<-.Panel_Data" <- function(obj,value) {obj$dt <- value}



dname.Panel_Data <- function(obj) obj$dnames
"dname<-.Panel_Data" <- function(obj,value) {
  obj$dnames <- value
  obj
}

setMethod(as_longform,"Panel_Data",
          function(x,...,n=nsubj(x),mxocc=maxocc(x),
                                    mnocc=minocc(x)) {
  time <- as_longform(x$time,n=n,mxocc=mxocc,mnocc=mnocc,
                      name="time")
  invar <- cbind(subj=1:n,x$invar)
  var <- as_longform(x$vari,n=n,mxocc=mxocc,mnocc=mnocc)
  dplyr::left_join(time,invar, dplyr::join_by("subj")) |>
    dplyr::left_join(var, dplyr::join_by("subj","occ"))
})

long2panel <- function(df, idcol="subj", timecol="time", occcol="occ",
                       dnames=character(),
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
                        minocc=nf$minocc, nocc1=nocc-1L,
                        byocc=nf$byocc,bysubj=nf$bysubj,
                        isubj=NA_integer_),
             invar = invar, dnames=dnames)

}

setMethod("get_subj","Panel_Data", function(x,isub) {
  invar <- x$invar
  if (!is.null(invar)) invar <- invar[isub,]
  panel_data(time=get_subj(x$time,isub),vari=get_subj(x$vari,isub),
             invar=invar,dnames=x$dnames,isubj=isub)
})

setMethod("get_subj<-","Panel_Data", function(x,isub,value) {
  x$add_subj(isub,value)
  x
})

isubj.Panel_Data <- function(obj) {obj$isubj}
"isubj<-.Panel_Data" <- function(obj,value) {
  obj$isubj<-as.integer(value)
  obj
}

