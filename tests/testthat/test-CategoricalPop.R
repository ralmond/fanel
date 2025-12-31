test_that("CategoricalPop-class {CategoricalPop-class}", {
})
test_that("CategoricalPop-class {CategoricalPop}", {
})


test_that("{CategoricalPop$states}", {
  ## {The set of possible values.  This should have
  ##       the same length as $probs.  It should also be the values of
  ##       the \dQuote{theta} element in the \link{Quadrature}.}
})

test_that("{CategoricalPop$name:}", {
  ## {A name for the model, primarily used for printing.}
})
test_that("{CategoricalPop$convergence:}", {
  ## {Logical variable indicating whether or
  ##       not the last mstep converged.}
})
test_that("{CategoricalPop$lp:}", {
  ## {The log-posterior after the last mstep.}
})
test_that("{CategoricalPop$wname:}", {
  ## {The name of the weight column(s). }
})
test_that("{CategoricalPop$tnames:}", {
  ## {The name(s) of the latent variable(s).}
})

test_that("{CategoricalPop$pvec}", {
  ## {The the log of the probabilities.  Setting this
  ##       field converts back using softmax to covert back to a simplex. }
})
test_that("{CategoricalPop$probs}", {
  ## {The probabilities for each state.  Note that
  ##       the setter requires a normalized vector.}
})

test_that("{CategoricalPop$initialize}", {
  ## {(name,states,probs,tnames,wname):
  ##       Constructor, called by $new.}
})
test_that("{CategoricalPop$drawInit}", {
  ## {(npart,covars=list()):
  ##       Draws a random starting position. (Used with random quadratures,
  ##       i.e., particle filter, and simulations.)}
})
test_that("{CategoricalPop$initProbs}", {
  ## {signature(theta,covars=list()):
  ##       Calculates the probability of the initial quadrature.}
})
test_that("{CategoricalPop$lprob}", {
  ## {signature(data,par=self$pvec):
  ##       Calculates the log probability of the the starting position.}
})
test_that("{CategoricalPop$mstep}", {
  ## {(data, ...):
  ## 	Does the optimization directly using expected counts of states.}
})


test_that("CategoricalPop Constructor)",{
  ## CategoricalPop$new(\var{name,\var{states}=seq(0L,2L,1L),
  ##   \var{probs}=rep(1/length(states),length(states)), tname="theta",
  ##   wname="w")}.
})
