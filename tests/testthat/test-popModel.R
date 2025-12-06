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
   expect_equal(apop$lprob(c(0,0),data),
                sum(dnorm(theta,log=TRUE)*weights))
   expect_equal(apop$lprob(c(1,log(2)),data),
                sum(dnorm(theta,1,2,log=TRUE)*weights))
})


test_that("NormalPop cdf",{
   apop <- NormalPop$new("standard",0,1)
   expect_equal(apop$initProbs(qnorm(c(1,3,5)/6)),c(1,1,1)/3,tolerance=.00001)
})

test_that("NormalPop mstep",{

})


test_that("CategoricalPop",{

})
