test_that("FModel-class {mstep.FModel}", {
  F0Model <- R6Class(
    classname="TestModel",
    inherit=FModel,
    public = list(
      eta=12,
      lprob=function(data,par=self$pvec) {
        par*par
      }),
    active = list(
      pvec = function (value) {
        if (missing(value)) return(self$eta)
        self$eta <- value
      })
  )
  f0m <- F0Model$new()
  f0m <- mstep(f0m,data.frame(),method="Brent",lower=-25,upper=25)
#test_that("{FModel$convergence:}", {
  ## {Logical variable indicating whether or
  ##       not the last mstep converged.}
#})
  expect_true(f0m$convergence)
  expect_equal(pvec(f0m),0)
#test_that("{FModel$lp:}", {
  ## {The log-posterior after the last mstep.}
#})
  expect_equal(f0m$lp,0)

})



#test_that("FModel-class {FModel-class}", {})
#test_that("FModel-class {FModel}", {})
test_that("lprob.FModel, pvec.FModel", {
  F0Model <- R6Class(
    classname="TestModel",
    inherit=FModel,
    public = list(
      eta=12,
      lprob=function(data,par=self$pvec) {
        par*par
      }),
    active = list(
      pvec = function (value) {
        if (missing(value)) return(self$eta)
        self$eta <- value
      })
  )
  f0m <- F0Model$new()
  expect_equal(pvec(f0m),12)
  pvec(f0m) <- 1
  expect_equal(f0m$eta,1)
  expect_equal(lprob(f0m,data.frame()),1)

})

test_that("{FModel$name:}", {
  ## {A name for the model, primarily used for printing.}
  fm<-FModel$new()
  expect_equal(fm$name,"FModel")
  fm$name <- "Fred"
  expect_equal(fm$name,"Fred")
  expect_equal(fm$toString(),
               "<Fred>")
  expect_output(print(fm),"<Fred>")
})
test_that("{FModel$wname:}", {
  ## {The name of the weight column(s). }
  fm <- FModel$new()
  expect_equal(fm$wname,"w")
  fm$wname <- c("w.all","w.left","w.right")
  expect_equal(fm$wname,c("w.all","w.left","w.right"))
})
test_that("{FModel$qnames:}", {
  ## {The name(s) of the latent variable(s).}
  fm <- FModel$new()
  expect_equal(fm$qnames,character())
  fm$qnames <- c("theta1","theta2")
  expect_equal(fm$qnames,c("theta1","theta2"))
  expect_error(fm$qnames <- "theta")
})
##test_that("{FModel$pvec:}", {
  ## {A vector of possibly transformed parameters
  ##       used in mstep.}
##})


# test_that("{FModel$mstep}", {
#   ## {(data, ..., its=3,control=list(),
#   ## 	workers = NULL):  This method does the optimization (see
#   ##       section below).}
# })
# test_that("{FModel$lprob}", {
#   ## {(obj = "FModel", data= "data.frame",
#   ## 	pvec = "numeric",):
#   ##       Calculates the log probability of the data (which includes imputed
#   ##       values for the latent variables and weights).}
# })
# test_that("{FModel$pvec}", {
#   ## {(obj = "FModel"): Returns the
#   ##       parameters as a vector.  The actual value depends on the
#   ##       particular model class. }
# })
# test_that("{FModel$pvec<-}", {
#   ## {(obj = "FModel", value): Sets the parameter
#   ##       vector. }
# })

