test_that("TransitionModel-class {TransitionModel-class}", {
})
test_that("TransitionModel-class {TransitionModel}", {
})
test_that("TransitionModel-class {UpDownGrowth-class}", {
})
test_that("TransitionModel-class {UpDownGrowth}", {
})

test_that("{TransitionModel$convergence:}", {
  ##{Logical variable indicating whether or
  ##       not the last mstep converged.}
})
test_that("{TransitionModel$lp:}", {
  ##{The log-posterior after the last mstep.}
})
test_that("{TransitionModel$name:}", {
  ##{A name for the model, primarily used for printing.}
})
test_that("{TransitionModel$wname}", {
  ##{The names of the weight columns.  This is
  ##       expected to be of length 3 with the first one being the overall
  ##       weight, the second the left weights and the third the right
  ##       weights (see \linkS4class{BWQuad}).}
})
test_that("{TransitionModel$tnames:}", {
  ##{The name(s) of the latent variable(s).}
})
test_that("{TransitionModel$pvec:}", {
  ##{A vector of possibly transformed parameters
  ##       used in mstep.}
})
test_that("{TransitionModel$dosname}", {
  ##{The name of the dosage column in the
  ##       data for mstep, defaults to \dQuote{dose}.}
})
test_that("{TransitionModel$dtname}", {
  ##{The name of the delta time column in the
  ##       data for mstep, defaults to \dQuote{deltaT}.}
})
test_that("{TransitionModel$continuous}", {
  ##{A logical value, true for models designed
##       to be used with the \link{particleFilter}, and false for
##       \linkS4class{TransitionModel}.}
})

test_that("{TransitionModel$nStates}", {
  ##{The number of rows and columns in the matrix.}
})
test_that("{TransitionModel$nmoments}", {
  ##{Number of times to exponentiate matrix for
  ##       \link{expLambdaT}.}
})
test_that("{TransitionModel$splitter}", {
  ##{An expression giving the variables which
  ##       influence matrix creation.  Defaults to ~deltaT+dose.}
})
test_that("{TransitionModel$cache}", {
  ##{A \linkS4class{memoTree} used to cache
  ##       matrixes.}
})
test_that("{mstep TransitionModel}", {
  ##{(obj = "FModel", data= "data.frame", its="integer",
  ## 	control="list", workers = "Workers"):  Runs the m-step to
  ##       optimize parameters (inherited from \linkS4class{FModel}).}
})
test_that("{TransitionModel$mstep}", {
  ##{(data, ..., its=3,control=list(),
  ## 	workers = NULL):  This method does the optimization (see
  ##       \linkS4class{FModel}).}
})
test_that("{lprob TransitionModel}", {
  ##{(obj = "FModel", data= "data.frame",
  ## 	pvec = "numeric",):
  ##       Calculates the log probability of the data (which includes imputed
  ##       values for the latent variables and weights).}
})
test_that("{TransitionModel$lprob}", {
  ##{(data,pvec=pvec(self)):  The R6 method
  ##       which implement this.}
})
test_that("{pvec TransitionModel}", {
  ##{(obj = "FModel"): Returns the
  ##       parameters as a vector.  The actual value depends on the
  ##       particular model class. }
})
test_that("{pvec<- TransitionModel}", {
  ##{(obj = "FModel", value): Sets the parameter
  ##       vector. }
})
test_that("{TransitionModel$drawNext}", {
  ##{signature(theta,deltaT,dose=deltaT,
## 	covars=list()): Draws a random next value for the latent
##       variables.}
})
test_that("{TransitionModel$lprob}", {
  ##{signature(data,par=pvec(self)):
##       Calculated the log likelihood of the estimated growth given
##       argument parameters.} 
})
test_that("{TransitionModel$print}", {
  ##{signature(...): Prints the object.}
})
test_that("{TransitionModel$toString}", {
  ##{signature(...): Creates a string
  ##       represenation of the object.}
})
test_that("{TransitionModel$xtime}", {
  ##{A character vector giving the name of
  ##       additional covariates used in building the matrix.}
})

test_that("{TransitionModel$rmat}", {
  ##{(pvec=pvec(self), deltaT, dose=deltaT,
  ## 	covars=list()): This calculateds the rate matrix (input to
  ##       \link{expLambdaT}).}
})
test_that("{TransitionModel$tmat}", {
  ##{(pvec=pvec(self), deltaT, dose=deltaT,
  ## 	covars=list()): This calculateds the transition matrix (output
  ##       from \link{expLambdaT}).}
})
test_that("{TransitionModel$advance}", {
  ##{(lweights, deltaT, dose=deltaT,
  ## 	covars=list()): Multiplies tmat by lweights.  (See
  ##       \link{bwFilter}.)}
})
test_that("{TransitionModel$retreat}", {
  ##{(rweights, deltaT, dose=deltaT,
  ## 	covars=list()): Multiplies rweights by tmat. (See
  ##       \link{bwFilter}.)}
})
test_that("{TransitionModel$lpinner}", {
  ##{(data,par=self(pvec)):  An inner
  ##       calculation for $lprob.}
})
test_that("{TransitionModel$fillCache}", {
  ##{(data,par=self(pvec)):  Clears the
##       cache and precalculates the matrixes needed for data.}
})


test_that("UpDownGrowth$new", {
  ## UpDownGrowth$new(\var{name, \var{nStates}, \var{uprate},
  ##   \var{downrate}, \var{tname}="theta", \var{wname}=c("w.full",
  ##   "w.left", "w.right"))}.
})
