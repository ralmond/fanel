
HMM <- function(name,population,activities,evidence,
                tname=population$tnames,
                dname=evidence$dnames) {
  population$tnames <- tname
  activities$tnames <- tname
  evidence$tnames <- tname
  evidence$dnames <- dname
  result <- list(name=name,population=population,
                 activities=activities,evidence=evidence)
  class(result) <- "HMM"
  result
}

setOldClass("HMM")

setMethod("nsubj","HMM", function(obj) {
  nsubj(obj$population)
})

setMethod("nocc","HMM", function(obj) {
  nocc(obj$evidence)
})

setMethod("maxocc","HMM", function(obj) {
  maxocc(obj$evidence)
})

setMethod("minocc","HMM", function(obj) {
  maxocc(obj$evidence)
})

drawInit <- function(hmm, data, quad, subj, npart=nquad(quad)) {
  hmm$population$drawInit(npart,data$getVar(subj,0))
}

drawObs <- function(hmm,data,quad,subj,occ) {
  hmm$population$drawInit(quad$theta(subj,occ),data$getVar(subj,occ))
}

drawNext <- function(hmm, data, quad, subj, occ) {
  hmm$activities$drawNext(quad$theta(subj,occ),data$dt[subj,occ],
                          data$getVar(subj,occ))
}

llike <- function(hmm, data, quad, subj, occ) {
  hmm$evidence$llike(data$getData(subj,occ),quad$theta(subj,occ),
                     data$getVar(subj,occ))
}


setMethod("mstep","HMM",
           function(obj, data, its=3,control=list(),
                    workers=Workers$new()) {
             mstep(obj$population,data,its=its,control=control,
                   workers=workers)
             mstep(obj$activities,data,its=its,control=control,
                   workers=workers)
             mstep(obj$evidence,data,its=its,control=control,
                   workers=workers)
           })



setMethod("as_longform","HMM",
          function(x,n=nsubj(x),maxocc=maxocc(x),
                   minocc=minocc(x),weightType="all",
                   name=deparse(substitute(x))) {
            as_longform(x$evidence) |>
              dplyr::left_join(as_longform(x$activities),
                               dplyr::join_by(subj,occ)) |>
              dplyr::left_join(as_longform(x$populuation),
                               dplyr::join_by(subj))
          })

longform <- function(hmm,quad,data) {
  as_longform(quad) |>
    dplyr::left_join(as_longform(hmm), dplyr::join_by(subj,occ)) |>
    dplyr::left_join(as_longform(data), dplyr::join_by(subj,occ))
}
