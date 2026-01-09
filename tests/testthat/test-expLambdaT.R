test_that("expLambdaT {expLambdaT}", {

  I <- diag(3)
  ee <- diag(rep(exp(1),3))
  expect_equal(expLambdaT(I,25),ee,
               tolerance=.0001)

  lam1 <- matrix(c(-.25,.25,0, 0.01,-.26,.25, 0,0.01,-.01),3,3,byrow=TRUE)
  expect_equal(expLambdaT(lam1,1),(I+lam1/2)%*%(I+lam1/2))
})


