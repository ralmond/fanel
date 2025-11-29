### Panel_Data

### This file needs to be loaded after panAmat and before panframe

Panel_Data <- R6Class(
  "Panel_Data",
  public=list(
    isubj=NA_integer_,
    initialize=function(time,dt,vari=NULL,invar=NULL,
                        datacols=character(),
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
      self$datacols <- datacols
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
    invarnames = function() {
      if (is.null(private$invariant)) return(character())
      names(private$invariant)
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
                       datacols=character(),isubj=NA_integer_) {
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
        datacols=datacols,isubj=isubj)
  }
  if (missing(dt)) {
    result <- Panel_Data$new(time=time,vari=vari,invar=invar,
        datacols=datacols,isubj=isubj)
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
                   minocc=1L,weightType="all",
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

setMethod("get_subj","Panel_Data", function(x,subj) {
  invar <- x$invar
  if (!is.null(invar)) invar <- invar[subj,]
  panel_data(time=get_subj(x$time,subj),vari=get_subj(x$vari,subj),
             invar=invar,datacols=x$datacols,isubj=subj)
})

setMethod("get_subj<-","Panel_Data", function(x,subj,value) {
  x$time[subj,] <- value$time[1L,]
  x$vari[subj,,] <- value$vari[subj,,]
  if (!is.null(value$invar)) x$invar[subj,] <- value$invar[1L,]
  x
})

setMethod("isubj","Panmat", function(obj) obj$isubj)
setMethod("isubj<-","Panmat", function(obj,value) {
  obj$isubj<-as.integer(value)
  obj
})

