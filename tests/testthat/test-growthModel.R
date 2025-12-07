test_that("BrownianGrowth initialize",{
  agr <- BrownianGrowth$new("B",.1,.04)
  expect_equal(agr$name,"B")
  expect_equal(agr$gain,.1)
  expect_equal(agr$inovSD,.04)
})

test_that("BrownianGrowth pvec", {
  agr <- BrownianGrowth$new("B",.1,.04)
  expect_equal(agr$pvec,c(.1,log(.04)))
  agr$pvec <- c(0,0)
  expect_equal(agr$gain,0)
  expect_equal(agr$inovSD,1)
})

test_that("BrownianGrowth draw", {
  tol <- .0001
  agr <- BrownianGrowth$new("B",0,tol/25)

  ## Theta
  samp <- agr$drawNext(c(-1,0,1),1)
  expect_equal(samp,c(-1,0,1),tolerance=tol)

  ## deltaT --mean
  agr$gain <- 1
  samp <- agr$drawNext(rep(0,3),1:3)
  expect_equal(samp,1:3,tolerance=tol)

  ## deltaT -- SD
  agr$gain <- 0
  agr$inovSD <- 1
  samp1 <- agr$drawNext(rep(0,100),1)
  expect_lt(sum(samp1^2),qchisq(.999,100))
  samp2 <- agr$drawNext(rep(0,100),4)
  expect_lt(sum(samp2^2),4*qchisq(.999,100))
})

test_that("BrownianGrowth lprob", {
  agr <- BrownianGrowth$new("B",1,1)
  theta0 <- rep(0,3)
  theta1 <- theta0+1:3
  weights <- 1:3
  test_dat <- data.frame(deltaT=1,dose=1,theta=theta0,
                         theta_1=theta1,w=weights)
  expect_equal(agr$lprob(test_dat,c(0,0)),
               sum(dnorm(theta1,log=TRUE)*weights))
  expect_equal(agr$lprob(test_dat,c(1,log(2))),
               sum(dnorm(theta1,1,2,log=TRUE)*weights))
})

