bwFilter <- function(object,covars,quad,...,workers=Workers$new()) {

  workers$start()

  workers$lapply(split_subj(covars), \(cov,hmm,qua) {
    isubj <- cov$isubj
    qua <- get_subj(qua,isubj)
    maxocc(qua) <- maxocc(cov)
    qua$isubj <- isubj
    qua$resetWeights()
    thetas <- qua$theta(1L,1L)

    ## Forwards
    lweights <- ProbInit(hmm,isubj,thetas,cov$getInvar(isubj))
    lweights <- lweights*exp(evalEvidence(hmm,isubj,0L,
                                          cov$getData(isubj,0L),
                                          thetas,cov$getVar(isubj,0L)))
    qua$lweights[1L,1L,] <- lweights

    for (iocc in 1L:maxocc(cov)) {
      lweights <- advanceWeights(hmm,isubj,iocc,lweights,
                                 cov$getVar(isubj,iocc))
      lweights <- lweights*exp(evalEvidence(hmm,isubj,iocc,
                                            cov$getData(isubj,iocc),
                                            thetas,cov$getVar(isubj,iocc)))
      qua$lweights[1L,iocc+1L,] <- lweights
    }

    ## Reverse
    iocc <- maxocc(cov)
    rweights <- rep(1,nquad(qua))
    rweights <- rweights*exp(evalEvidence(hmm,isubj,iocc,
                                          cov$getData(isubj,iocc),
                                          thetas, cov$getVar(isubj,iocc)))
    qua$rweights[1L,iocc+1L,] <- rweights

    for (iocc in maxocc(cov):1L) {
      rweights <- retreatWeights(hmm,isubj,iocc, rweights,
                                 cov$getVar(isubj,iocc))
      rweights <- rweights*exp(evalEvidence(hmm,isubj,iocc-1L,
                                            cov$getData(isubj,iocc-1L),
                                            thetas,
                                            cov$getVar(isubj,iocc-1L)))
      qua$rweights[1L,iocc,] <- rweights
    }
    qua
  },object,get_subj(quad,1)) -> qlist

  workers$stopFlag()
  bind_subj(qlist)
}
