particleFilter.HMM <- function (object, covars, quad=NULL, nquad=nquad(quad),
                                ..., workers=Workers$new()) {

  if (missing(quad)) {
    if (missing(nquad)) {
      stop("Either 'quad' or 'nquad' must be supplied.")
    }
    quad <- particleQuad(covars$time,nquad,object$population$tnames)
  }

  workers$start()

  workers$lapply(split_subj(covars), \(cov,hmm,qua) {
    nq <- nquad(qua)
    isubj <- cov$isubj
    qua <- qua$clone()
    qua$isubj <- isubj
    qua$resetWeights()
    lweights <- rep(0.0,nq)
    thetas <- hmm$PopulationModels$drawInit(isubj,nq,cov$getInvar(isubj))
    if (!all(is.na(cov$getData(isubj,0L))) {
      lweights <- hmm$evidence$evalEvidence(isubj,0L,thetas,cov$getVar(isubj,0L))
    }
    qua$theta(1L,0L,) <- thetas
    qua$lweight(1L,0L,) <- lweights
    for (iocc in 1L:hmm$maxocc) {
      thetas <- hmm$activities(isubj,iocc,thetas,cov$dt[isubj,iocc],cov$getVar(isubj,iocc))
      lweights <- lweights + hmm$evidence$llike(isubj,iocc,cov$getData(isubj,iocc),thetas,
                                                cov$getVar(isubj,iocc))
      qua$theta(1L,iocc,) <- thetas
      qua$lweights(1L,iocc,) <- lweights
    }
    qua
  }, object,quad) -> qlist

  workers$stopFlag()
  bind_subj(qlist)
}

