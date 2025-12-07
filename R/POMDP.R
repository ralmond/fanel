
POMDP <- function(name,population,activities,evidence,
                tname=population$tnames,
                dname=evidence$dnames) {
  population$tnames <- tname
  activities$tnames <- tname
  evidence$tnames <- tname
  evidence$dnames <- dname
  result <- list(name=name,population=population,
                 activities=activities,evidence=evidence)
  class(result) <- "POMDP"
  result
}

setOldClass("POMDP")

setMethod("nsubj","POMDP", function(obj) {
  nsubj(obj$population)
})

setMethod("nocc","POMDP", function(obj) {
  nocc(obj$evidence)
})

setMethod("maxocc","POMDP", function(obj) {
  maxocc(obj$evidence)
})

setMethod("minocc","POMDP", function(obj) {
  maxocc(obj$evidence)
})


setMethod("drawInitial", "POMDP",
          function(model, isubj, npart, covar=NULL) {
            drawInitial(model$population,isubj,npart,covar)
})

setMethod("ProbInit", "POMDP",
          function(model, isubj, thetas, covar=NULL) {
            ProbInit(model$population,isubj,thetas,covar)
          })

setMethod("drawGrowth", "POMDP",
           function(model, isubj, iocc, theta, covar=NULL) {
             drawGrowth(model$actvities, isubj, iocc, theta, covar=NULL)
           })


setMethod("advanceWeights", "POMDP",
          function(model, isubj, iocc, lweights, covar=NULL) {
  advanceWeights(model$activities,isubj,iocc,lweights,covar)
})

setMethod("retreatWeights", "POMDP",
          function(model, isubj, iocc, rweights, covar=NULL) {
  retreatWeights(model$actvities,isubj,iocc,rweights,covar)
})

setMethod("drawData", "POMDP",
          function(model,isubj,iocc,theta,covar=NULL) {
            drawData(model$evidence,isubj,iocc,theta,covar)
})

setMethod("evalEvidence", "POMDP",
           function(model, isubj, iocc, theta, data, covar=NULL) {
             evalEvidence(model$evidence, isubj,iocc,theta,data,covar)
})




setMethod("mstep","POMDP",
           function(obj, data, its=3,control=list(),
                    workers=Workers$new()) {
             mstep(obj$population,data,its=its,control=control,
                   workers=workers)
             mstep(obj$activities,data,its=its,control=control,
                   workers=workers)
             mstep(obj$evidence,data,its=its,control=control,
                   workers=workers)
           })



setMethod("as_longform","POMDP",
          function(x,n=nsubj(x),maxocc=maxocc(x),
                   minocc=minocc(x),weightType="all",
                   name=deparse(substitute(x))) {
            as_longform(x$evidence) |>
              dplyr::left_join(as_longform(x$activities),
                               dplyr::join_by("subj","occ")) |>
              dplyr::left_join(as_longform(x$populuation),
                               dplyr::join_by("subj"))
          })

longform <- function(hmm,quad,data) {
  by <- dplyr::join_by("subj","occ")
  as_longform(quad) |>
    dplyr::left_join(as_longform(hmm), by) |>
    dplyr::left_join(as_longform(data), by)
}
