particleFilter.HMM <- function (object, covars, quad=NULL, nquad=nquad(quad),
                                ..., workers=Workers$new())

  if (missing(quad)) {
    if (missing(nquad)) {
      stop("Either 'quad' or 'nquad' must be supplied.")
    }
    quad <- particleQuad(covars$time,nquad,object$population$tnames)
  }

  ## Setup Clusters
  psapply <- sapply
  if (!debug) {
    clust <- inject(makeCluster(hmm$clspec,hmm$cltype,!!!hmm$clargs))
    stopOnExit <- hmm$stopClusterOnError
    withr::defer({if (stopOnExit) stopCluster(clust)})
    psapply <- function(...) parSapply(clust,...)
    if (!missing(seed)) {
      clusterSetRNGStream(clust,seed)
      mc.reset.stream()
    }
  }
  if(isTRUE(weightLog)) hmm$weightLog <- list()

  hmm$npart <- npart
  hmm$lweights <- matrix(0,npart,hmm$nsubjects)
  hmm$theta <- array(NA_real_,c(npart,hmm$nsubjects,hmm$maxocc+1L))
  hmm$theta[,,1L] <- psapply(1L:hmm$nsubjects, \(subj) {
    hmm$drawPop(subj,npart)
  })

  for (it in 1L:hmm$maxocc) {
    if (debug) cat("Time: ",it,".\n")
    hmm$theta[,,it+1L] <- psapply(1L:hmm$nsubjects, \(subj) {
      hmm$drawGrowth(subj,it)
    })

    hmm$lweights <- hmm$lweights +
      psapply(1L:hmm$nsubjects, \(subj) {
        hmm$evalEvidence(subj,it)
      })
    if(isTRUE(weightLog))
      hmm$weightLog <- c(hmm$weightLog,hmm$lweights)
  }
  stopOnExit <- TRUE
  invisible(hmm)
}

