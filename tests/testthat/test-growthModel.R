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

test_that("{GrowthModel$gain}", {
  ## {The rate at which the latent variable advances.}
})
test_that("{GrowthModel$loss}", {
  ## {The rate at which the latent variable retreats.}
})
test_that("{GrowthModel$inovSD}", {
  ## {The rate at which the variance of the
  ##       innovation varies with time.}
})
test_that("{GrowthModel$pvec}", {
  ## {The parameter vector, c(self$gain,
  ## 	log(self$inovSD)).}
})

test_that("{GrowthModel$convergence:}", {
  ## {Logical variable indicating whether or
  ##       not the last mstep converged.}
})
test_that("{GrowthModel$lp:}", {
  ## {The log-posterior after the last mstep.}
})
test_that("{GrowthModel$name:}", {
  ## {A name for the model, primarily used for printing.}
})
test_that("{GrowthModel$wname:}", {
  ## {The name of the weight column(s). }
})
test_that("{GrowthModel$tnames:}", {
  ## {The name(s) of the latent variable(s).}
})
test_that("{GrowthModel$pvec:}", {
  ## {Either c(self$gain,exp(self$inovSD)) or
  ##       c(self$gain,self$loss,exp(self$inovSD)).}
})
test_that("{GrowthModel$dosname}", {
  ## {The name of the dosage column in the
  ##       data for mstep, defaults to \dQuote{dose}.}
})
test_that("{GrowthModel$dtname}", {
  ## {The name of the delta time column in the
  ##       data for mstep, defaults to \dQuote{deltaT}.}
})
test_that("{GrowthModel$continuous}", {
  ## {A logical value, true for models designed
  ##       to be used with the \link{particleFilter}, and false for
  ##       \linkS4class{TransitionModel}.}
})

test_that("{GrowthModelinitialize}", {
  ## {signature(name,gain,inovSD) or
  ##       signature(name,loss,gain,inovSD):  Called by
  ##       $new}
})

test_that("{mstep(GrowthModel)}", {
  ## {(obj = "FModel", data= "data.frame", ...):
  ##        In this case a closed form maximum is available.}
})
test_that("{GrowthModel$mstep}", {
  ## {(data, ...):  This method does the
  ##       optimization.}
})
test_that("{lprob(GrowthModel)}", {
  ## {(obj = "FModel", data= "data.frame",
  ## 	pvec = "numeric",):
  ##       Calculates the log probability of the data (which includes imputed
  ##       values for the latent variables and weights).}
})
test_that("{GrowthModel$lprob}", {
  ## {(data,pvec=pvec(self)):  The R6 method
  ##       which implement this.}
})
test_that("{GrowthModel$drawNext}", {
  ## {signature(theta,deltaT,dose=deltaT,
  ## 	covars=list()): Draws a random next value for the latent
  ##       variables.}
})
test_that("{GrowthModel$lprob}", {
  ## {signature(data,par=pvec(self)):
  ##       Calculated the log likelihood of the estimated growth given
  ##       argument parameters.}
})
test_that("{GrowthModel$print}", {
  ## {signature(...): Prints the object.}
})
test_that("{GrowthModel$toString}", {
  ## {signature(...): Creates a string
  ##       represenation of the object.}
})

test_that("GrowthModel Constructors", {
  ## To create a new object of this type call:
  ## BrownianGrowth$new(\var{name,\var{gain},\var{inovSD})},
  ## BrownianGrowth2$new(\var{name,\var{gain},\var{loss},\var{inovSD})}
})
