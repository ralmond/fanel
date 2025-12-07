qFilter <- function(object,covars,quad,wfun,...,workers=Workers$new()) {

  workers$start()

  workers$lapply(split_subj(covars), \(cov,hmm,qua,wf) {
    isubj <- cov$isubj
    qua <- get_subj(qua,isubj)
    maxocc(qua) <- maxocc(cov)
    qua$isubj <- isubj
    qua$resetWeights()
    thetas <- qua$theta(1L,1L)
    tstar <- qua$times[1L,]
    nt <- length(tstar)
    nq <- nquad(qua)

    iprobs <- ProbInit(hmm,isubj,thetas, cov$getInvar(isubj))
    qua$lweights[isubj,,] <- outer(log(iprobs),
                                     wf(cov$dt[isubj,0L],tstar),"*")

    for (iocc in 0L:maxocc(cov)) {
    qua$lweights[isubj,,] <- qua$lweights[isubj,,] +
      outer(evalEvidence(hmm,isubj,iocc,cov$getData(isubj,iocc),thetas,
                             cov$getVar(isubj,iocc)),
            wf(cov$dt[isubj,iocc],tstar),"*")
    }
    qua
  },object,get_subj(quad,1),wfun) -> qlist

  workers$stopFlag()
  bind_subj(qlist)

  workers$stopFlag()
  bind_subj(qlist)
}
