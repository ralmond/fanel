simulate.POMDP <- function(object, nsim, seed, ...,
                         covars=NULL,
                         workers=Workers$new()) {
  if (missing(covars)) {
    covars <- panel_data(dt=getDT(object))
  } else if (!isTRUE(all.equal(getTime(object),getTime(covars)))) {
    stop("Time codes are different for object and covariates.")
  }

  if (missing(nsim)) nsim=nsubj(object)
  if (!missing(seed)) {
    workers$seed <- seed
  }
  workers$start()

  ldata <- as_longform(covars)

  workers$lapply(split(ldata,~subj),\(cov,hmm) {
    isubj <- cov$subj[1]

    tnames <- tnames(hmm)
    theta <- as.data.frame(lapply(tnames,\(n) rep(NA,nocc(cov))))
    names(theta) <- tnames

    dnames <- dnames(hmm)
    data <- as.data.frame(lapply(dnames,\(n) rep(NA,nocc(cov))))
    names(data) <- dnames

    theta[1L,] <- drawInitial(hmm,isubj,1L,cov[1L,])

    for (iocc in 1L:maxocc(cov)) {
      theta[iocc+1L,] <- drawGrowth(hmm,isubj,iocc, theta[iocc,],
                                  cov[iocc+1L,])
      data[iocc,] <- drawData(hmm,isubj,iocc, theta[iocc,],
                              cov[iocc+1L,])
    }
    names(theta) <- paste0(tnames,"_sim")
    cbind(cov,theta,data)
  }, object) |> purrr::list_rbind() -> result

  workers$stopFlag()
  long2panel(result,dnames=evidence(object)$dnames,
             invcols=covars$invarnames)
}

