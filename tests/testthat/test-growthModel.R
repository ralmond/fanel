### BrownianGrowth ----
test_that("BrownianGrowth initialize",{
  agr <- BrownianGrowth$new("B",.1,.04)
  expect_equal(agr$name,"B")
  expect_equal(agr$gain,.1)
  expect_equal(agr$inovSD,.04)
  expect_true(agr$continuous)
  expect_equal(agr$toString(),
               "<BrownianGrowth: B ( 0.1, 0.04 )>")
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

test_that("BrownianGrowth$mstep",{
  agr <- BrownianGrowth$new("B",0,1)
  deltaT <- 4
  dose <- 4
  gain <- 1/4
  inovSD <- 1/4
  theta0 <- rnorm(100)
  weights <- runif(100)
  inov <- rnorm(100,gain*dose,
                inovSD*sqrt(deltaT))
  test_dat <- data.frame(deltaT=deltaT,dose=dose,
                         theta=theta0,theta_1=theta0+inov,
                         w=weights)
  agr <- mstep(agr,test_dat)
  expect_equal(agr$gain,wtd.mean(inov,weights)/dose,
               tolerance=.0001)
  expect_equal(agr$inovSD,wtd.sd(inov,weights)/sqrt(deltaT),
               tolerance=.0001)
  expect_true(agr$convergence)
  expect_equal(agr$lp,
               agr$lprob(test_dat))

})


### BrownianGrowth2 ----

test_that("BrownianGrowth2",{
  agr <- BrownianGrowth2$new("B2",.1,.01,.04)
  ## BrownianGrowth2$new(\var{name,\var{gain},\var{loss},\var{inovSD})}
  expect_true(is(agr,"BrownianGrowth2"))
  expect_equal(agr$gain,.1)
  expect_equal(agr$loss,.01)
  expect_equal(agr$inovSD,.04)
  expect_equal(agr$name,"B2")
  expect_equal(agr$wname,"w")
  expect_equal(agr$qnames,"theta")
  expect_equal(agr$dtname,"deltaT")
  expect_equal(agr$dosname,"dose")
  expect_true(agr$continuous)
  expect_equal(agr$toString(),
               "<BrownianGrowth2: B2 ( 0.1, 0.01, 0.04 )>")
})

test_that("{BrownianGrowth2$pvec}", {
  agr <- BrownianGrowth2$new("B2",.1,.01,.04)
  ##       c(self$gain,self$loss,exp(self$inovSD)).}
  expect_equal(agr$pvec,c(.1,.01,log(.04)))
  agr$pvec <- c(.2,.025,log(.05))
  expect_equal(agr$gain,.2)
  expect_equal(agr$loss,.025)
  expect_equal(agr$inovSD,.05,tolerance=.0001)
})


test_that("{BrownianGrowth2$lprob}", {
  ## {(data,pvec=pvec(self)):  The R6 method
  ##       which implement this.}
  agr <- BrownianGrowth2$new("B2",1,.1,1)
  theta0 <- rep(0,3)
  theta1 <- theta0+1:3
  weights <- 1:3
  test_dat <- data.frame(deltaT=2,dose=1,theta=theta0,
                         theta_1=theta1,w=weights)
  expect_equal(agr$lprob(test_dat,c(0,0,0)),
               sum(dnorm(theta1,0,sqrt(2),log=TRUE)*weights))
  expect_equal(agr$lprob(test_dat,c(1,.1,log(2))),
               sum(dnorm(theta1,1-.2,2*sqrt(2),log=TRUE)*weights))
})
test_that("{BrownianGrowth2$drawNext}", {
  ## {signature(theta,deltaT,dose=deltaT,
  ## 	covars=list()): Draws a random next value for the latent
  ##       variables.}
  tol <- .0001
  agr <- BrownianGrowth2$new("B2",0,0,tol/25)

  ## Theta
  samp <- agr$drawNext(c(-1,0,1),1,2)
  expect_equal(samp,c(-1,0,1),tolerance=tol)

  ## deltaT --gain
  agr$gain <- 1
  samp <- agr$drawNext(rep(0,3),1,1:3)
  expect_equal(samp,1:3,tolerance=tol)

  ## deltaT --loss
  agr$loss <- .1
  samp <- agr$drawNext(rep(0,3),1,1:3)
  expect_equal(samp,(1:3)-.1,tolerance=tol)

  ## deltaT -- SD
  agr$gain <- 0
  agr$loss <- 0
  agr$inovSD <- 1
  samp1 <- agr$drawNext(rep(0,100),1)
  expect_lt(sum(samp1^2),qchisq(.999,100))
  samp2 <- agr$drawNext(rep(0,100),4)
  expect_lt(sum(samp2^2),4*qchisq(.999,100))
})

test_that("{mstep(GrowthModel) non-regression}", {
  ## {(obj = "FModel", data= "data.frame", ...):
  ##        In this case a closed form maximum is available.}
  ##$convergence
  ##$lp
  agr <- BrownianGrowth2$new("B",0,0,1)

  deltaT <- 4
  dose <- 4
  gain <- 1/4
  inovSD <- 1/4
  theta0 <- rnorm(100)
  weights <- runif(100)
  ## Non-regression method
  inov <- rnorm(100,gain*dose,
                inovSD*sqrt(deltaT))
  test_dat <- data.frame(deltaT=deltaT,dose=dose,
                         theta=theta0,theta_1=theta0+inov,
                         w=weights)
  agr <- mstep(agr,test_dat)
  expect_equal(agr$gain,wtd.mean(inov,weights)/dose,
               tolerance=.0001)
  expect_equal(agr$loss,0)
  expect_equal(agr$inovSD,wtd.sd(inov,weights)/sqrt(deltaT),
               tolerance=.0001)
  expect_true(agr$convergence)
  expect_equal(agr$lp,
               agr$lprob(test_dat))

})

test_that("{mstep(GrowthModel) regression}", {
  ## {(obj = "FModel", data= "data.frame", ...):
  ##        In this case a closed form maximum is available.}
  ##$convergence
  ##$lp
  agr <- BrownianGrowth2$new("B",0,0,1)

  deltaT <- 4
  dose <- rep(c(2,4),50)
  gain <- 1/4
  loss <- .0025
  inovSD <- 1/4
  theta0 <- rnorm(100)
  weights <- runif(100)
  ## Regression method
  inov <- rnorm(100,gain*dose-loss*deltaT,
                inovSD*sqrt(deltaT))
  test_dat <- data.frame(deltaT=deltaT,dose=dose,
                         theta=theta0,theta_1=theta0+inov,
                         w=weights)
  agr <- mstep(agr,test_dat)
  expect_equal(agr$gain,wtd.mean((inov+agr$loss*deltaT)/dose,weights),
               tolerance=.0001,ignore_attr=TRUE)
  expect_equal(agr$loss,
               -wtd.mean((inov-agr$gain*dose)/deltaT,weights),
               tolerance=.0001, ignore_attr=TRUE)
  expect_equal(agr$inovSD,wtd.sd((inov-agr$gain*dose+agr$loss*deltaT)/sqrt(deltaT),weights),
               tolerance=.0001,ignore_attr=TRUE)
  expect_true(agr$convergence)
  expect_equal(agr$lp,
               agr$lprob(test_dat))

})
