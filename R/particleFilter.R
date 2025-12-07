particleFilter <- function (object, covars, quad=NULL, nquad=nquad(quad),
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
    thetas <- drawInitial(hmm,isubj,nq,cov$getInvar(isubj))
    lweights <- evalEvidence(hmm,isubj,0L,thetas,cov$getVar(isubj,0L))

    qua$theta(1L,0L,,thetas)
    qua$lweight(1L,0L,,lweights)
    for (iocc in 1L:hmm$maxocc) {
      thetas <- drawGrowth(hmm,isubj,iocc,thetas,cov$getVar(isubj,iocc))
      lweights <- lweights + evalEvidence(hmm,isubj,iocc,
                                         cov$getData(isubj,iocc),thetas,
                                         cov$getVar(isubj,iocc))
      qua$theta(1L,iocc,,thetas)
      qua$lweights(1L,iocc,,lweights)
    }
    qua
  }, object,quad) -> qlist

  workers$stopFlag()
  bind_subj(qlist)
}

