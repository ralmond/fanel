test_that("FModel-class {FModel-class}", {
})
test_that("FModel-class {FModel}", {
})
test_that("FModel-class {lprob.FModel}", {
})
test_that("FModel-class {pvec.FModel}", {
})
test_that("FModel-class {pvec<-.FModel}", {
})
test_that("FModel-class {mstep.FModel}", {
})
test_that("FModel-class {maxocc.FModel}", {
})
test_that("FModel-class {maxocc<-.FModel}", {
})
test_that("FModel-class {minocc.FModel}", {
})
test_that("FModel-class {minocc<-.FModel}", {
})
test_that("FModel-class {nocc.FModel}", {
})
test_that("FModel-class {nocc<-.FModel}", {
})
test_that("FModel-class {nsubj.FModel}", {
})
test_that("FModel-class {nsubj<-.FModel}", {
})


test_that("{FModel$convergence:}", {
  ## {Logical variable indicating whether or
  ##       not the last mstep converged.}
})
test_that("{FModel$lp:}", {
  ## {The log-posterior after the last mstep.}
})
test_that("{FModel$name:}", {
  ## {A name for the model, primarily used for printing.}
})
test_that("{FModel$wname:}", {
  ## {The name of the weight column(s). }
})
test_that("{FModel$tnames:}", {
  ## {The name(s) of the latent variable(s).}
})
test_that("{FModel$pvec:}", {
  ## {A vector of possibly transformed parameters
  ##       used in mstep.}
})


test_that("{FModelmstep}", {
  ## {(obj = "FModel", data= "data.frame", ...,
  ## 	its="integer", control="list", workers = "Workers"):  Runs the
  ##       m-step to optimize parameters (see section below).}
})
test_that("{FModel$mstep}", {
  ## {(data, ..., its=3,control=list(),
  ## 	workers = NULL):  This method does the optimization (see
  ##       section below).}
})
test_that("{FModellprob}", {
  ## {(obj = "FModel", data= "data.frame",
  ## 	pvec = "numeric",):
  ##       Calculates the log probability of the data (which includes imputed
  ##       values for the latent variables and weights).}
})
test_that("{FModel$lprob}", {
  ## {(data,pvec=pvec(self)):  The R6 method
  ##       which implement this.}
})
test_that("{FModelpvec}", {
  ## {(obj = "FModel"): Returns the
  ##       parameters as a vector.  The actual value depends on the
  ##       particular model class. }
})
test_that("{FModelpvec<-}", {
  ## {(obj = "FModel", value): Sets the parameter
  ##       vector. }
})
test_that("{FModel$pvec}", {
  ## {The active property which corresponds to the
  ##       pvec accessor.}
})


test_that("{FModel$toString}", {
  ## {Provides a string to identify the model.}
})
test_that("{FModel$print}", {
  ## {Prints the object using the $toString method.}
})
