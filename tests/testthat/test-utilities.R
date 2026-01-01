test_that(" {cuts2probs}", {
  expect_equal(cuts2probs(c(.75,.25)),matrix(c(.25,.5,.25),1,3))
  mat <- matrix(c(.4,.3,.1,.7,.5,.4),2,3,byrow=TRUE)
  emat <- matrix(c(.6,.3, .1,.2, .2,.1, .1,.4),2,4)
  expect_equal(cuts2probs(mat),emat)
})
test_that(" {logit}", {
  lg3 <- logit(c(.25,.5,.75))
  expect_equal(lg3,-rev(lg3),tolerance=.00001)
})
test_that(" {invlogit}", {
  seed <- c(.25,.5,.75)
  expect_equal(invlogit(logit(seed)),seed,tolerance=.00001)
})



