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
})
test_that("BWQuad {get_subj.Quadrature}",{
})
test_that("BWQuad {get_subj<-.Quadrature}",{
})
test_that("BWQuad {nquad,Quadrature}",{
})
test_that("BWQuad {nquad<-.Quadrature}",{
})


test_that("BWQuad {$weights(type='default')}",{
})
test_that("BWQuad {$resetWeights()}",{
})
test_that("BWQuad {$lweight(subj,occ,quad,value)}",{
})
