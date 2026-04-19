# Setup helper to create test data
create_test_times <- function(n = 5) {
  panmat(1:n)
}

test_that("BWQuad initializes correctly", {
  b <- BWQuad$new(
    times = create_test_times(3),
    quadrature = data.frame("theta"=c(-2:2)),
    byocc = FALSE,
    bysubj = FALSE,
    nsubjects = 2
  )
  expect_equal(b$wname[1], "w.full")
  expect_true(is.na(b$rweights))
})

test_that("BWQuad$weights('default') works", {
  b <- BWQuad$new(
    times = create_test_times(3),
    quadrature = data.frame("theta"=c(-2:2)),
    byocc = FALSE,
    bysubj = FALSE,
    nsubjects = 2
  )
  b$resetWeights()
  w <- b$weights("default")
  expect_equal(length(w), 2*3*5)
})

test_that("BWQuad$weights('left') works", {
  b <- BWQuad$new(
    times = create_test_times(3),
    quadrature = data.frame("theta"=c(-2:2)),
    byocc = FALSE,
    bysubj = FALSE,
    nsubjects = 2
  )
  w <- b$weights("left")
  expect_true(length(w) > 0)
})

test_that("BWQuad$weights('right') works", {
  b <- BWQuad$new(
    times = create_test_times(3),
    quadrature = data.frame("theta"=c(-2:2)),
    byocc = FALSE,
    bysubj = FALSE,
    nsubjects = 2
  )
  w <- b$weights("right")
  expect_true(length(w) > 0)
})

test_that("BWQuad$weights('all') returns data frame", {
  b <- BWQuad$new(
    times = create_test_times(3),
    quadrature = data.frame("theta"=c(-2:2)),
    byocc = FALSE,
    bysubj = FALSE,
    nsubjects = 2
  )
  w <- b$weights("all")
  expect_true(inherits(w, "data.frame"))
  expect_equal(ncol(w), 3)
})

test_that("BWQuad$resetWeights works", {
  b <- BWQuad$new(
    times = create_test_times(3),
    quadrature = data.frame("theta"=c(-2:2)),
    byocc = FALSE,
    bysubj = FALSE,
    nsubjects = 2
  )
  b$resetWeights()
  expect_true(all(is.na(b$lweights)))
  expect_true(all(is.na(b$rweights)))
})

test_that("BWQuad inherits correctly", {
  b <- BWQuad$new(
    times = create_test_times(3),
    quadrature = data.frame("theta"=c(-2:2)),
    byocc = FALSE,
    bysubj = FALSE,
    nsubjects = 2
  )
  expect_true(inherits(b, "Quadrature"))
})


test_that("BWQuad {BWQuad-class} {!byocc, !bysubj}",{
  times <- panmat(1L:5L)
  thetav <- qnorm((0:10 +.5)/11)
  q1 <- BWquad(times,thetav)
  expect_true(is(q1,"BWQuad"))
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
  expect_equal(q1$wname,c("w.full","w.left","w.right"))
  expect_equal(q1$qnames,"theta")
  expect_equal(q1$dquad,1L)

})

test_that("BWQuad  {byocc, !bysubj}", {
  times <- panmat(1L:5L)
  thetas <-
    data.frame(occ=rep(0L:4L,each=11),
               quad=rep(1L:11L,5),
               theta=do.call("c",
                             lapply(1L:5L, \(t) {
                               qnorm((0:10 +.5)/11,(t-3)/2)
                             })))

  q1 <- BWquad(times,thetas,byocc=TRUE)
  expect_true(is(q1,"BWQuad"))
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
  expect_equal(q1$wname,c("w.full","w.left","w.right"))
  expect_equal(q1$qnames,"theta")
  expect_equal(q1$dquad,1L)

})

test_that("BWQuad {!byocc, bysubj}", {
  times <- panmat(1L:5L)
  thetas <- data.frame(subj=rep(1L:2L,each=11L),
                       quad=rep(1L:11L,2L),
                       theta = c(qnorm((0:10 +.5)/11,-.5),
                                 qnorm((0:10 +.5)/11,.5)))
  q1 <- BWquad(times,thetas,
                  nsubjects=2L,bysubj=TRUE)
  expect_true(is(q1,"BWQuad"))
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
  expect_equal(q1$wname,c("w.full","w.left","w.right"))
  expect_equal(q1$qnames,"theta")
  expect_equal(q1$dquad,1L)

})

test_that("BWQuad {as_longform.Quadrature}",{
  times <- panmat(1L:5L)
  thetav <- qnorm((0:10 +.5)/11)
  q1 <- BWquad(times,thetav)
  nsubj(q1) <- 2
  ql1 <- as_longform(q1)
  expect_equal(nrow(ql1),2*5*11)
  expect_equal(unique(ql1$subj),1:2)
  expect_equal(unique(ql1$occ),1:5)
  expect_equal(unique(ql1$quad),1:11)
})


test_that("BWQuad {get_subj.Quadrature}",{
  times <- panmat(1L:5L)
  thetav <- qnorm((0:10 +.5)/11)
  q1 <- BWquad(times,thetav)
  nsubj(q1) <- 2L
  q1$resetWeights()
  q1$lweight(1L,0L:4L,,1:11)
  q1$lweight(2L,0L:4L,,12:22)
  q1$rweight(1L,0L:4L,,11:1)
  q1$rweight(2L,0L:4L,,22:12)

  q1.1 <- get_subj(q1,1L)
  q1.2 <- get_subj(q1,2L)

  expect_equal(q1.1$lweights[,1,1],1:11)
  expect_equal(q1.2$lweights[,1,1],12:22)
  expect_equal(q1.1$rweights[,1,1],11:1)
  expect_equal(q1.2$rweights[,1,1],22:12)

  get_subj(q1.1,2L) <- q1.2
  expect_equal(q1.2$lweights[,2,1],12:22)
  expect_equal(q1.2$rweights[,2,1],22:12)
})


test_that("BWQuad {nquad,Quadrature}",{
  times <- panmat(1L:5L)
  thetav <- qnorm((0:10 +.5)/11)
  q1 <- BWquad(times,thetav)
  expect_equal(nquad(q1),11)
})

