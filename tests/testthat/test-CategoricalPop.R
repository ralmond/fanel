test_that("CategoricalPop-class {CategoricalPop-class}", {
  apop <- CategoricalPop$new("standard",0:3,rep(1/4,4))
  expect_equal(apop$probs,rep(1/4,4))
  expect_equal(apop$states,0:3)
  expect_equal(apop$name,"standard")
  expect_equal(apop$qnames,"theta")
  expect_equal(apop$wname,"w")
  expect_equal(apop$toString(),
  "<CategoricalPopulation: standard ( 0, 1, 2, 3 )>")
})


test_that("{CategoricalPop$states}", {
  apop <- CategoricalPop$new("standard",0:3,(4:1)/10)
  expect_equal(apop$states,0:3)
  apop$states <- 1:4
  expect_equal(apop$states,1:4)
  expect_error(apop$states <- 1:3)
  expect_error(apop$probs <- rep(1/5,5))
  expect_error(apop$probs <- rep(1,4))

})

test_that("{CategoricalPop$wname:}", {
  apop <- CategoricalPop$new("standard",0:3,(4:1)/10,
                             qname="level",wname="wei")
  expect_equal(apop$qnames,"level")
  expect_equal(apop$wname,"wei")
})

test_that("{CategoricalPop$pvec}", {
  apop <- CategoricalPop$new("standard",0:3,(4:1)/10)
  expect_equal(apop$pvec,log((4:1)/10))
  apop$pvec <- rep(0,4)
  expect_equal(apop$probs,rep(0.25,4))
})
test_that("{CategoricalPop$drawInit}", {
  ## {(npart,covars=list()):
  ##       Draws a random starting position. (Used with random quadratures,
  ##       i.e., particle filter, and simulations.)}
  apop <- CategoricalPop$new("standard",0:3,(4:1)/10)
  withr::local_seed(123334)
  N <- 500
  obs <- as.numeric(table(apop$drawInit(N)))
  #print(obs)
  exp <- apop$probs*N
  #print(exp)
  expect_lt(sum((obs-exp)^2/exp),qchisq(.99,3))
})
test_that("{CategoricalPop$initProbs}", {
  ## {signature(theta,covars=list()):
  ##       Calculates the probability of the initial quadrature.}
  probs <- (4:1)/10
  apop <- CategoricalPop$new("standard",0:3,probs)
  expect_equal(apop$initProbs(3:0),rev(probs))
})

test_that("{CategoricalPop$lprob}", {
  ## {signature(data,par=self$pvec):
  ##       Calculates the log probability of the the starting position.}
  probs <- (4:1)/10
  weights <- c(1,2,2,1)/6
  apop <- CategoricalPop$new("standard",0:3,probs)
  lp <- apop$lprob(data.frame(theta=0:3,w=weights))
  expect_equal(lp,sum(log(probs)*weights),tolerance=.00001)
})
test_that("{CategoricalPop$mstep}", {
  ## {(data, ...):
  ## 	Does the optimization directly using expected counts of states.}
  weights <- (4:1)/10
  apop <- CategoricalPop$new("standard",0:3,rep(1/4,4))
  N <- 5
  testdat <- data.frame(theta=rep(0:3,N), w=rep(weights,N))
  apop <- apop$mstep(testdat)
  expect_true(apop$convergence)
  expect_equal(apop$probs,weights,ignore_attr=TRUE)
  expect_equal(apop$lp,sum(N*weights*log(weights)),
               tolerance=.00001)


})

