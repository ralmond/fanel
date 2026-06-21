test_that("expLambdaT",{
  lam <- matrix(c(-.2,.1,.2,-.1),2,2)
  m1 <- matrix(c(.9,.05,.1,.95),2,2)
  expect_equal(expLambdaT(lam,1),m1%*%m1)
})

test_that("UpDownGrowth$new", {
  ## UpDownGrowth$new(\var{name, \var{nStates}, \var{uprate},
  ##   \var{downrate}, \var{tname}="theta", \var{wname}=c("w.full",
  ##   "w.left", "w.right"))}.
  udg <- UpDownGrowth$new("Level1",3,c(.2,.1),.01)
  ##$name
  expect_equal(udg$name,"Level1")
  ##$nStates
  expect_equal(udg$nStates,3)
  ##$uprate
  expect_equal(udg$uprate,c(.2,.1))
  ##$downrate
  expect_equal(udg$downrate,.01)
  ##$qnames
  expect_equal(udg$qnames,"theta")
  ##$wname
  expect_equal(udg$wname,c("w.full","w.left","w.right"))
  ##$dosname
  expect_equal(udg$dosname,"dose")
  ##$dtname
  expect_equal(udg$dtname,"deltaT")
  ##$continuous
  expect_false(udg$continuous)
  ##$nmoments
  expect_equal(udg$nmoments,10)
  ##$xtime
  expect_equal(udg$xtime,character())
  ##$toString()
  expect_equal(udg$toString(),
               "<UpDownGrowth: Level1 ( 0.2, 0.1; 0.01 )>")
})

test_that("{pvec TransitionModel}", {
  ##{(obj = "FModel"): Returns the
  ##       parameters as a vector.  The actual value depends on the
  ##       particular model class. }
  udg <- UpDownGrowth$new("Level1",3,c(.2,.1),.01)
  expect_equal(pvec(udg),log(c(.2,.1,.01)))
  ## pvec<-
  pvec(udg) <- log(c(.1,.2,.02))
  expect_equal(udg$uprate,c(.1,.2))
  expect_equal(udg$downrate,.02)
})



test_that("{TransitionModel$rmat}", {
  ##{(pvec=pvec(self), deltaT, dose=deltaT,
  ## 	covars=list()): This calculateds the rate matrix (input to
  ##       \link{expLambdaT}).}
  udg <- UpDownGrowth$new("Level1",3,c(.2,.1),.01)
  expect_equal(udg$rmat(pvec(udg),1,1),
               matrix(c(-.2,.2,0,
                        .01,-.11,.1,
                        0,.01,-.01),3,3,byrow=TRUE))
  expect_equal(udg$rmat(pvec(udg),2,.5),
               matrix(c(-.1,.1,0,
                        .02,-.07,.05,
                        0,.02,-.02),3,3,byrow=TRUE))
})
test_that("{TransitionModel$tmat}", {
  ##{(pvec=pvec(self), deltaT, dose=deltaT,
  ## 	covars=list()): This calculateds the transition matrix (output
  ##       from \link{expLambdaT}).}
  udg <- UpDownGrowth$new("Level1",3,c(.2,.1),.01)
  udg$nmoments <- 1
  rm <- diag(3)+matrix(c(-.1,.1,0,
                        .005,-.055,.05,
                        0,.005,-.005),3,3,byrow=TRUE)
  expect_equal(udg$tmat(pvec(udg),1,1),
               rm%*%rm)
  expect_equal(udg$cacheGet(c(1,1)),
               rm%*%rm)

  expect_null(udg$cacheGet(c(2,.5)))
  udg$tmat(pvec(udg),2,.5)
  expect_false(is.null(udg$cacheGet(c(2,.5))))

  expect_null(udg$cacheGet(c(2,1)))
  udg$tmat(pvec(udg),2,1)
  expect_false(is.null(udg$cacheGet(c(2,1))))


})


test_that("{TransitionModel$advance}", {
  ##{(lweights, deltaT, dose=deltaT,
  ## 	covars=list()): Multiplies tmat by lweights.  (See
  ##       \link{bwFilter}.)}
  udg <- UpDownGrowth$new("Level1",3,c(.2,.1),.01)
  udg$nmoments <- 1
  rm <- diag(3)+matrix(c(-.1,.1,0,
                         .005,-.055,.05,
                         0,.005,-.005),3,3,byrow=TRUE)
  lw <- rep(1,3)
  expect_equal(udg$advance(lw,1,1),rm%*%rm%*%lw)
})
test_that("{TransitionModel$retreat}", {
  ##{(rweights, deltaT, dose=deltaT,
  ## 	covars=list()): Multiplies rweights by tmat. (See
  ##       \link{bwFilter}.)}
  udg <- UpDownGrowth$new("Level1",3,c(.2,.1),.01)
  udg$nmoments <- 1
  rm <- diag(3)+matrix(c(-.1,.1,0,
                         .005,-.055,.05,
                         0,.005,-.005),3,3,byrow=TRUE)
  lw <- rep(1,3)
  expect_equal(udg$retreat(lw,1,1),lw%*%rm%*%rm)
})


test_that("{TransitionModel$drawNext}", {
  ##{signature(theta,deltaT,dose=deltaT,
## 	covars=list()): Draws a random next value for the latent
##       variables.}
  udg0 <- UpDownGrowth$new("Null",3,c(0,0),0)
  theta <- rep(1:3,10)
  deltaT <- rep(1:2,15)
  dose <- rep(1:5,6)

  theta1 <- udg0$drawNext(theta,deltaT,dose)
  expect_equal(theta1,theta)
})


test_that("{TransitionModel$splitter}", {
  ##{An expression giving the variables which
  ##       influence matrix creation.  Defaults to ~deltaT+dose.}
  udg0 <- UpDownGrowth$new("Null",3,c(0,0),0)
  expect_equal(toString(udg0$splitter),"~, deltaT + dose")
})

test_that("{TransitionModel$fillCache}", {
  ##{(data,par=self(pvec)):  Clears the
##       cache and precalculates the matrixes needed for data.}
  udg0 <- UpDownGrowth$new("Null",3,c(0,0),0)
  theta <- rep(1:3,10)
  deltaT <- rep(1:2,15)
  dose <- rep(1:5,6)

  dat <- data.frame(theta=theta,deltaT=deltaT,dose=dose)
  udg0$tmat(deltaT=3,dose=3) ## Put something in the cache

  udg0$fillCache(dat)
  expect_false(is.null(udg0$cacheGet(c(2,1))))
  expect_false(is.null(udg0$cacheGet(c(1,5))))
  expect_true(is.null(udg0$cacheGet(c(3,3))))

})


test_that("{TransitionModel$lpinner}", {
  ##{(data,par=self(pvec)):  An inner
  ##       calculation for $lprob.}
  udg <- UpDownGrowth$new("Level1",3,c(.2,.1),.01)
  udg$nmoments <- 1
  rm <- diag(3)+matrix(c(-.1,.1,0,
                         .005,-.055,.05,
                         0,.005,-.005),3,3,byrow=TRUE)

  testdat <- read.csv(test_path("transtest.csv")) |>
    dplyr::filter(dose==1)
  exp1 <- sum(c(.3,.4,.3)*log(c(.2,.2,.2)%*%rm%*%rm))
  expect_equal(udg$lpinner(testdat),4*exp1,tolerance=.0001)

})

test_that("{lprob TransitionModel}", {
  ##{(obj = "FModel", data= "data.frame",
  ## 	pvec = "numeric",):
  ##       Calculates the log probability of the data (which includes imputed
  ##       values for the latent variables and weights).}
  udg <- UpDownGrowth$new("Level1",3,c(.2,.1),.01)
  udg$nmoments <- 1
  g1 <- udg$tmat(pvec(udg),1,1)
  g2 <- udg$tmat(pvec(udg),1,2)

  testdat <- read.csv(test_path("transtest.csv"))
  exp1 <- sum(c(.3,.4,.3)*log(c(.2,.2,.2)%*%g1))
  exp2 <- sum(c(.3,.4,.3)*log(c(.2,.2,.2)%*%g2))
  expect_equal(lprob(udg,testdat),4*exp1+2*exp2,tolerance=.0001)
})

#These tests should be covered in FModel
# test_that("{mstep TransitionModel}", {
#   ##{(obj = "FModel", data= "data.frame", its="integer",
#   ## 	control="list", workers = "Workers"):  Runs the m-step to
#   ##       optimize parameters (inherited from \linkS4class{FModel}).}
# })
#
#
# test_that("{TransitionModel$convergence:}", {
#   ##{Logical variable indicating whether or
#   ##       not the last mstep converged.}
# })
# test_that("{TransitionModel$lp:}", {
#   ##{The log-posterior after the last mstep.}
# })
#


