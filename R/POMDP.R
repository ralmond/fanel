POMDP <- function(name,population,activities,evidence,
                tname=population$tnames,
                dname=evidence$dnames) {
  population$tnames <- tname
  activities$tnames <- tname
  evidence$tnames <- tname
  evidence$dnames <- dname
  components=list(population=population,activities=activities,
                  evidence=evidence)
  result <- list(name=name,components)
  class(result) <- "POMDP"
  result
}

population <- function(model) {model$components$population}
activities <- function(model) {model$components$activities}
evidence <- function(model) {model$components$evidence}
"population<-" <- function(model,value) {
  model$components$population <- value
  model
}
"activities<-" <- function(model,value) {
  model$components$activities <- value
  model
}
"evidence<-" <- function(model,value) {
  model$components$evidence <- value
  model
}

setOldClass("POMDP")

nsubj.POMDP<- function(obj) {
  nsubj(population(obj))
}

nocc.POMDP <- function(obj) {
  nocc(evidence(obj))
}

maxocc.POMDP <- function(obj) {
  maxocc(evidence(obj))
}

minocc.POMDP <- function(obj) {
  maxocc(evidence(obj))
}

nmodels.POMDP <-function(obj) {
  nmodels(population(obj)) +
  nmodels(evidence(obj)) +
  nmodels(activities(obj))
}

drawInitial.POMDP <- function(model, isubj, npart, covar=NULL) {
  drawInitial(population(model),isubj,npart,covar)
}

probInit.POMDP <- function(model, isubj, thetas, covar=NULL) {
  probInit(population(model),isubj,thetas,covar)
}

drawGrowth.POMDP <- function(model, isubj, iocc, theta, covar=NULL) {
  drawGrowth(activities(model), isubj, iocc, theta, covar=NULL)
}

advanceWeights.POMDP <- function(model, isubj, iocc, lweights, covar=NULL) {
  advanceWeights(activities(model),isubj,iocc,lweights,covar)
}

retreatWeights.POMDP <- function(model, isubj, iocc, rweights, covar=NULL) {
  retreatWeights(activities(model),isubj,iocc,rweights,covar)
}

drawData.POMDP <- function(model,isubj,iocc,theta,covar=NULL) {
  drawData(evidence(evidence),isubj,iocc,theta,covar)
}

evalEvidence.POMDP <- function(model, isubj, iocc, theta, data, covar=NULL) {
  evalEvidence(evidence(model), isubj,iocc,theta,data,covar)
}





setMethod("mstep","POMDP",
           function(obj, data, ..., its=3,control=list(),
                    workers=Workers$new(nmodels(obj))) {
             workers$start()
             obj$components |>
               lapply(\(com) com$split_m(data)) |>
               purrr::list_flatten() |>
               workers$lapply(\(pair) {
                 pair[[1]]$mstep(pair[[2]])
               }) -> result
             Workers$flagStop()
             result
           })



setMethod("as_longform","POMDP",
          function(x,n=nsubj(x),maxocc=maxocc(x),
                   minocc=minocc(x),weightType="all",
                   name=deparse(substitute(x))) {
            as_longform(evidence(x)) |>
              dplyr::left_join(as_longform(activities(x)),
                               dplyr::join_by("subj","occ")) |>
              dplyr::left_join(as_longform(population(x)),
                               dplyr::join_by("subj"))
          })

longform <- function(quad,model,data) {
  by <- dplyr::join_by("subj","occ")
  as_longform(quad) |>
    dplyr::left_join(as_longform(model), by) |>
    dplyr::left_join(as_longform(data), by)
}
