bwFilter.HMM <- function(object,covars,quad,...,workers=Workers$new()) {

  workers$start()

  workers$lapply(split_subj(covars), \(cov,hmm,qua) {
    isubj <- cov$isubj
    qua <- get_subj(qua,isubj)
    maxocc(qua) <- maxocc(cov)
    qua$isubj <- isubj
    qua$resetWeights()
    thetas <- qua$theta(1L,1L)

    ## Forwards
    lweights <- hmm$population$initProbs(isubj,thetas,cov$getInvar(isubj))
    lweights <- lweights*exp(hmm$evidence$
                             llike(isubj,0L,cov$getData(isubj,0L),
                                   thetas,cov$getVar(isubj,0L)))
    qua$lweights[1L,1L,] <- lweights

    for (iocc in 1L:maxocc(cov)) {
      lweights <- hmm$activities$advance(isubj,iocc,
                                         lweights,cov$dt[isubj,iocc],
                                         cov$getVar(isubj,iocc))
      lweights <- lweights*exp(hmm$evidence$
                               llike(isubj,iocc,cov$getData(isubj,iocc),
                                     thetas,
                                     cov$getVar(isubj,iocc)))
      qua$lweights[1L,iocc+1L,] <- lweights
    }

    ## Reverse
    iocc <- maxocc(cov)
    rweights <- rep(1,nquad(qua))
    rweights <- rweights*exp(hmm$evidence$
                             llike(isubj,iocc,cov$getData(isubj,iocc),
                                   thetas,
                                   cov$getVar(isubj,iocc)))
    qua$rweights[1L,iocc+1L,] <- rweights

    for (iocc in maxocc(cov):1L) {
      rweights <- hmm$activities$retreat(isubj,iocc,
                                         rweights,cov$dt[isubj,iocc],
                                         cov$getVar(isubj,iocc))
      rweights <- rweights*exp(hmm$evidence$
                               llike(isubj,iocc-1L,cov$getData(isubj,iocc-1L),
                                     thetas,
                                     cov$getVar(isubj,iocc-1L)))
      qua$rweights[1L,iocc,] <- rweights
    }
    qua
  },object,get_subj(quad,1)) -> qlist

  workers$stopFlag()
  bind_subj(qlist)
}
