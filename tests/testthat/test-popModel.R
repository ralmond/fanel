test_that("NormalPop initialize",{
   apop <- NormalPop$new("Pop",3,1/2)
   expect_s3_class(apop,"PopulationModel")
   expect_equal(apop$name,"Pop")
   expect_equal(apop$mu,3)
   expect_equal(apop$sigma,1/2)

})

test_that("NormalPop pvec", {
   apop <- NormalPop$new("Pop",0,1)
   expect_equal(apop$pvec,c(0,0))
   apop$pvec <- c(1,log(2))
   expect_equal(apop$mu,1)
   expect_equal(apop$sigma,2)
})

test_that("NormalPop drawInit", {
   apop <- NormalPop$new("Pop",0,1)
   z <- apop$drawInit(100)
   expect_lt(sum(z^2),qchisq(.99,100))
})

test_that("NormalPop lprob", {
   apop <- NormalPop$new("Pop",0,1)
   theta <- 1:3
   weights <- 1:3
   data <- data.frame(theta=theta,w=weights)
   expect_equal(apop$lprob(data,c(0,0)),
                sum(dnorm(theta,log=TRUE)*weights))
   expect_equal(apop$lprob(data,c(1,log(2))),
                sum(dnorm(theta,1,2,log=TRUE)*weights))
})


test_that("NormalPop cdf",{
   apop <- NormalPop$new("standard",0,1)
   expect_equal(apop$initProbs(qnorm(c(1,3,5)/6)),c(1,1,1)/3,tolerance=.00001)
})

test_that("NormalPop mstep",{
  apop <- NormalPop$new("standard",0,1)
  qq <- qnorm(((0:10)+.5)/11)
  tdat <- rnorm(10,1,.5)
  lapply(1:length(tdat), \(isubj) {
    weight <- dnorm(qq,tdat[isubj])
    weight <- weight/sum(weight)
    data.frame(subj=isubj,theta=qq,w=weight)
  }) |> dplyr::bind_rows() -> testdata
  apop <- apop$mstep(testdata)
  expect_true(apop$convergence)
  covwt <- cov.wt(testdata["theta"],testdata$w,method="ML")
  expect_equal(apop$mu,covwt$center,tolerance=.001,
               ignore_attr=TRUE)
  expect_equal(apop$sigma,sqrt(covwt$cov),tolerance=.001,
               ignore_attr=TRUE)

})


