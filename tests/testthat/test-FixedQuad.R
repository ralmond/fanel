
test_that("vnames works for data frame", {
  df <- data.frame(a = 1:3, b = 1:3)
  expect_equal(vnames(df, prefix = "x"), c("a", "b"))
})

test_that("vnames works for array", {
  arr <- array(1:24, dim = c(4,3,2))
  expect_equal(vnames(arr, prefix = "v"), c("v1", "v2"))
})

# Setup helper to create test data
create_test_times <- function(n = 5) {
  panmat(1:n)
}


test_that("Quadrature-class {!byocc, !bysubj}", {
  times <- panmat(1L:5L)
  thetav <- qnorm((0:10 +.5)/11)
  q1 <- fixedQuad(times,thetav)
  expect_true(is(q1,"FixedQuad"))
  expect_equal(maxocc(q1),5L)
  expect_equal(minocc(q1),1L)
  expect_equal(nocc(q1),5L)
  expect_equal(nsubj(q1),1L)
  expect_true(is.na(isubj(q1)))
  expect_equal(nquad(q1),11L)
  expect_false(byocc(q1))
  expect_false(bysubj(q1))

  expect_equal(q1$quadpoints,as_quad_frame(thetav))
  expect_equal(q1$times,times)
  expect_equal(q1$wname,"w")
  expect_equal(q1$qnames,"theta")
  expect_equal(q1$dquad,1L)

})

test_that("Quadrature-class {byocc, !bysubj}", {
  times <- panmat(1L:5L)
  thetas <-
    data.frame(occ=rep(0L:4L,each=11),
               quad=rep(1L:11L,5),
               theta=do.call("c",
                             lapply(1L:5L, \(t) {
                      qnorm((0:10 +.5)/11,(t-3)/2)
              })))

  q1 <- fixedQuad(times,thetas,byocc=TRUE)
  expect_true(is(q1,"FixedQuad"))
  expect_equal(maxocc(q1),5L)
  expect_equal(minocc(q1),1L)
  expect_equal(nocc(q1),5L)
  expect_equal(nsubj(q1),1L)
  expect_true(is.na(isubj(q1)))
  expect_equal(nquad(q1),11L)
  expect_true(byocc(q1))
  expect_false(bysubj(q1))

  expect_equal(q1$quadpoints,as_quad_frame(thetas))
  expect_equal(q1$times,times)
  expect_equal(q1$wname,"w")
  expect_equal(q1$qnames,"theta")
  expect_equal(q1$dquad,1L)

})

test_that("Quadrature-class {!byocc, bysubj}", {
  times <- panmat(1L:5L)
  thetas <- data.frame(subj=rep(1L:2L,each=11L),
                       quad=rep(1L:11L,2L),
                       theta = c(qnorm((0:10 +.5)/11,-.5),
                                  qnorm((0:10 +.5)/11,.5)))
  q1 <- fixedQuad(times,thetas,
                  nsubjects=2L,bysubj=TRUE)
  expect_true(is(q1,"FixedQuad"))
  expect_equal(maxocc(q1),5L)
  expect_equal(minocc(q1),1L)
  expect_equal(nocc(q1),5L)
  expect_equal(nsubj(q1),2L)
  expect_true(is.na(isubj(q1)))
  expect_equal(nquad(q1),11L)
  expect_false(byocc(q1))
  expect_true(bysubj(q1))

  expect_equal(q1$quadpoints,as_quad_frame(thetas))
  nsubj(times) <- 2L
  expect_equal(q1$times,times)
  expect_equal(q1$wname,"w")
  expect_equal(q1$qnames,"theta")
  expect_equal(q1$dquad,1L)

})


test_that("Quadrature-class {as_longform.Quadrature}", {
  times <- panmat(0L:4L,minocc=0L)
  btimes <- ((1:5)-3)/2
  bsubj <- c(-1,1)
  thetav <- qnorm((0:10 +.5)/11)
  q1 <- fixedQuad(times,data.frame(theta=thetav))
  nsubj(q1) <- 2L
  q1$resetWeights()
  for (iocc in 1L:4L) {
    for (isubj in 1L:2L) {
      q1$lweight(isubj,iocc,1L:nquad(q1),
                 dnorm(q1$qpoints(isubj,iocc)$theta,
                       btimes[iocc]+bsubj[isubj],
                       log=TRUE))
    }
  }
  lf1 <- as_longform(q1)
  expect_equal(nrow(lf1),length(thetav)*nocc(times)*2)
  expect_equal(lf1[lf1$subj==1L & lf1$occ==0L,"theta"],
               thetav)
  expect_equal(names(lf1),c("subj","occ","quad","theta","w","times"))
  expect_equal(sum(lf1[lf1$subj==1L & lf1$occ==1L,"w"]),
               1.0,tolerance=.0001)


  thetas2 <-
    data.frame(occ=rep(0L:4L,each=11),
               quad=rep(1L:11L,5),
               theta=do.call("c",
                             lapply(1L:5L, \(t) {
                      qnorm((0:10 +.5)/11,(t-3)/2)
              })))

  q2 <- fixedQuad(times,thetas2,byocc=TRUE)
  nsubj(q2) <- 2L
  q2$resetWeights()
  for (iocc in 1L:4L) {
    for (isubj in 1L:2L) {
      q2$lweight(isubj,iocc,1L:nquad(q2),
                 dnorm(q2$qpoints(isubj,iocc)$theta,
                       btimes[iocc]+bsubj[isubj],
                       log=TRUE))
    }
  }
  lf2 <- as_longform(q2)
  expect_equal(nrow(lf2),nrow(thetas2)*2)
  expect_equal(lf2[lf2$subj==1L & lf2$occ==2L,"theta"],
               thetas2[thetas2$occ==2L,"theta"])
  expect_equal(names(lf2),c("subj","occ","quad","theta","w","times"))
  expect_equal(sum(lf2[lf2$subj==1L & lf2$occ==1L,"w"]),
               1.0,tolerance=.0001)

  thetas3 <- data.frame(subj=rep(1L:2L,each=11L),
                       quad=rep(1L:11L,2L),
                       theta = c(qnorm((0:10 +.5)/11,-.5),
                                  qnorm((0:10 +.5)/11,.5)))
  q3 <- fixedQuad(times,thetas3,
                  nsubjects=2L,bysubj=TRUE)

  q3$resetWeights()
  for (iocc in 1L:4L) {
    for (isubj in 1L:2L) {
      q3$lweight(isubj,iocc,1L:nquad(q3),
                 dnorm(q2$qpoints(isubj,iocc)$theta,
                       btimes[iocc]+bsubj[isubj],
                       log=TRUE))
    }
  }
  lf3 <- as_longform(q3)
  expect_equal(nrow(lf3),nrow(thetas3)*5)
  expect_equal(lf3[lf3$subj==1L & lf3$occ==2L,"theta"],
               thetas3[thetas3$subj==1L,"theta"])
  expect_equal(names(lf3),c("subj","occ","quad","theta","w","times"))
  expect_equal(sum(lf3[lf3$subj==1L & lf3$occ==1L,"w"]),
               1.0,tolerance=.0001)


})

test_that("Quadrature setters work", {
  times <- panmat(1L:5L)
  thetas <- data.frame(subj=rep(1L:2L,each=11L),
                       quad=rep(1L:11L,2L),
                       theta = c(qnorm((0:10 +.5)/11,-.5),
                                 qnorm((0:10 +.5)/11,.5)))
  x <- fixedQuad(times,thetas,
                  nsubjects=2L,bysubj=TRUE)
  x$times <- create_test_times(4)
  expect_true(nocc(x$times) == 4)
})


test_that("Quadrature-class {get_subj.Quadrature}", {

  times <- panmat(0L:4L,minocc=0L)
  btimes <- ((1:5)-3)/2
  bsubj <- c(-1,1)
  thetav <- qnorm((0:10 +.5)/11)
  qqq <- fixedQuad(times,data.frame(theta=thetav))
  nsubj(qqq) <- 2L
  qqq$resetWeights()

  thetas3 <- data.frame(subj=rep(1L:2L,each=11L),
                       quad=rep(1L:11L,2L),
                       theta = c(qnorm((0:10 +.5)/11,-.5),
                                  qnorm((0:10 +.5)/11,.5)))
  q3 <- fixedQuad(times,thetas3,
                  nsubjects=2L,bysubj=TRUE)

  q3$resetWeights()


  q1 <- get_subj(qqq,1L)
  q2 <- get_subj(qqq,2L)
  expect_equal(isubj(q1),1L)
  expect_equal(isubj(q2),2L)
  expect_equal(q1$qpoints(1,1)$theta,thetav)
  expect_equal(q2$qpoints(1,1)$theta,thetav)

  q31 <- get_subj(q3,1L)
  q32 <- get_subj(q3,2L)
  expect_equal(isubj(q31),1L)
  expect_equal(isubj(q32),2L)
  expect_equal(q31$qpoints(1,1)$theta,
               thetas3[thetas3$subj==1L,"theta"])
  expect_equal(q32$qpoints(1,1)$theta,
               thetas3[thetas3$subj==2L,"theta"])


  for (iocc in 1L:4L) {
    q1$lweight(1L,iocc,1L:nquad(q1),
                 dnorm(q1$qpoints(1L,iocc)$theta,
                       btimes[iocc]+bsubj[1L],
                       log=TRUE))
    q2$lweight(1L,iocc,1L:nquad(q2),
                 dnorm(q2$qpoints(1L,iocc)$theta,
                       btimes[iocc]+bsubj[2L],
                       log=TRUE))
    q31$lweight(1L,iocc,1L:nquad(q31),
                 dnorm(q31$qpoints(1L,iocc)$theta,
                       btimes[iocc]+bsubj[1L],
                       log=TRUE))
    q32$lweight(1L,iocc,1L:nquad(q32),
                 dnorm(q32$qpoints(1L,iocc)$theta,
                       btimes[iocc]+bsubj[2L],
                       log=TRUE))
  }

  get_subj(q1,2L) <- q2
  expect_equal(nsubj(q1),2L)
  expect_equal(q1$qpoints(1,1)$theta,thetav)
  expect_equal(q1$qpoints(2,2)$theta,thetav)
  expect_equal(q1$lweight(2L,2L,1:nquad(q2)),
               q2$lweight(1L,2L,1:nquad(q2)))

  get_subj(q31,2L) <- q32
  expect_equal(nsubj(q31),2L)
  expect_equal(q31$qpoints(1,1)$theta,
               thetas3[thetas3$subj==1L,"theta"])
  expect_equal(q31$qpoints(2,2)$theta,
               thetas3[thetas3$subj==2L,"theta"])
  expect_equal(q31$lweight(2L,2L,1:nquad(q31)),
               q32$lweight(1L,2L,1:nquad(q32)))

})

test_that("Quadrature-class {min/maxocc.Quadrature}", {
  times <- panmat(1L:5L)
  thetav <- qnorm((0:10 +.5)/11)
  q1 <- fixedQuad(times,thetav)

  minocc(q1) <- 6L
  expect_equal(minocc(q1),6L)
  expect_equal(maxocc(q1),10L)
  expect_equal(nocc(q1),5L)

  nocc(q1) <- 4L
  expect_equal(minocc(q1),6L)
  expect_equal(maxocc(q1),9L)
  expect_equal(nocc(q1),4L)

  maxocc(q1) <- 10L
  expect_equal(minocc(q1),6L)
  expect_equal(maxocc(q1),10L)
  expect_equal(nocc(q1),5L)

})

test_that("Quadrature-class {nquad,Quadrature}", {
  times <- panmat(1L:5L)
  thetav <- qnorm((0:10 +.5)/11)
  q1 <- fixedQuad(times,thetav)

  nquad(q1) <- 9L
  expect_equal(nquad(q1),9L)
  expect_equal(length(q1$qpoints(1,1)$theta),9L)

  nquad(q1) <- 11L
  expect_equal(nquad(q1),11L)
  expect_equal(length(q1$qpoints(1,1)$theta),11L)

  q1$quadpoints <- qnorm((0:5 +.5)/6)
  expect_equal(nquad(q1),6L)


})


test_that("Quadrature-class {n/isubj.Quadrature}", {
  times <- panmat(0L:4L,minocc=0L)
  thetav <- qnorm((0:10 +.5)/11)
  qqq <- fixedQuad(times,data.frame(theta=thetav))

  expect_equal(nsubj(qqq),1L)
  expect_true(is.na(isubj(qqq)))
  nsubj(qqq) <- 2L
  expect_equal(nsubj(qqq),2L)
  qqq$resetWeights()

  q1 <- get_subj(qqq,1L)
  q2 <- get_subj(qqq,2L)
  expect_equal(isubj(q1),1L)
  expect_equal(isubj(q2),2L)

  q3 <- q1$clone()
  isubj(q3) <- 3L
  expect_equal(isubj(q3),3L)

  get_subj(q1,2L) <- q2
  expect_equal(nsubj(q1),2L)
  expect_true(is.na(isubj(q1)))

})


test_that("Quadrature$weights('history') works", {
  times <- panmat(0L:4L,minocc=0L)
  thetav <- qnorm((0:10 +.5)/11)
  x <- fixedQuad(times,data.frame(theta=thetav))
  x$resetWeights()
  w <- x$weights("history")
  expect_equal(length(w), 5*11)
})

test_that("Quadrature clone works", {
  times <- panmat(0L:4L,minocc=0L)
  thetav <- qnorm((0:10 +.5)/11)
  x <- fixedQuad(times,data.frame(theta=thetav))
  x_clone <- x$clone()
  expect_true(inherits(x_clone, "Quadrature"))
  expect_equal(x_clone$times, x$times)
})




test_that("{FixedQuad$wname:}", {
  ## {The name to use for the weight object.}
  times <- panmat(0L:4L,minocc=0L)
  thetav <- qnorm((0:10 +.5)/11)
  qqq <- fixedQuad(times,data.frame(theta=thetav))
  qqq$resetWeights()

  expect_equal(qqq$wname,"w")
  qqq$wname<-"weight"
  lf <- as_longform(qqq)
  expect_equal(names(lf),c("subj","occ","quad","theta","weight","times"))

})

test_that("{FixedQuad$qnames, $dquad:}", {
  ## {A "character" vector containing the names
  ##       for the quadrature variables.  Should have length $dquad}
  times <- panmat(0L:4L,minocc=0L)
  thetav <- qnorm((0:10 +.5)/11)
  qqq <- fixedQuad(times,data.frame(theta=thetav))
  qqq$resetWeights()

  expect_equal(qqq$qnames,"theta")
  expect_equal(qqq$dquad,1L)
  qqq$qnames <- "Q"
  expect_equal(qqq$qnames,"Q")
  lf <- as_longform(qqq)
  expect_equal(names(lf),c("subj","occ","quad","Q","w","times"))

  q1 <- fixedQuad(times,expand.grid(x=c(-1,0,1),y=c(0,1)))
  expect_equal(q1$qnames,c("x","y"))
  expect_equal(q1$dquad,2L)

})

test_that("{FixedQuad$times:}", {
  ## {Object of class "Panmat" mapping
  ##       measurement occasions to times.}

  times <- panmat(0L:2L,minocc=0L)
  thetav <- qnorm((0:4 +.5)/5)
  qqq <- fixedQuad(times,data.frame(theta=thetav))
  nsubj(qqq)<- 2L
  qqq$resetWeights()
  expect_length(qqq$weights(),nsubj(qqq)*nocc(qqq)*nquad(qqq))

  qqq$times <- panmat(1L:5L)
  qqq$resetWeights()
  expect_length(qqq$weights(),nsubj(qqq)*nocc(qqq)*nquad(qqq))
  expect_equal(nsubj(qqq),1L)

})

test_that("{FixedQuad$weights(type='default')}", {
  ## {Returns normalized weights (on probability,
  ##       not log scale).}
  times <- panmat(0L:2L,minocc=0L)
  thetav <- qnorm((0:4 +.5)/5)
  qqq <- fixedQuad(times,data.frame(theta=thetav))
  nsubj(qqq)<- 2L
  qqq$resetWeights()

  for (iocc in 0L:2L) {
    for (isubj in 1L:2L) {
      qqq$lweight(isubj,iocc,1L:nquad(qqq),0)
    }
  }
  wt <- qqq$weights()
  expect_length(wt,nsubj(qqq)*nocc(qqq)*nquad(qqq))
  expect_true(all(wt==.2))

})


test_that("FixedQuad inherits correctly", {
  f <- FixedQuad$new(
    times = create_test_times(3),
    quadrature = qnorm((0:4 +.5)/5),
    byocc = TRUE,
    bysubj = FALSE,
    nsubjects = 2,
    isubj = 1
  )
  expect_true(inherits(f, "Quadrature"))
})
