test_that("ModelSet-class {ModelSet-class}", {
  expect_warning(ms <- ModelSet$new("test",list()))
  expect_equal(ms$name,"test")
  expect_equal(ms$toString(),
               "<ModelSet: test: 0 >")
  expect_output(print(ms),"<ModelSet: test: 0 >")
})


TestModel <- R6Class(
  classname="TestModel",
  inherit=FModel,
  public = list(
    initialize = function(name="tester") {
      self$name <- name
    },
    eta=12,
    lprob=function(data,par=self$pvec) {
      n <- sum(data[[self$wname]])
      n*(par*par+1)
    }),
  active = list(
    pvec = function (value) {
      if (missing(value)) return(self$eta)
      self$eta <- value
    })
)

test_that("ModelSet-class {maxocc.ModelSet}", {
  ms <- ModelSet$new("test",list(TestModel$new()),
                     rep(1,5))
  expect_equal(minocc(ms),1)
  expect_equal(maxocc(ms),5)
  expect_equal(nocc(ms),5)

  minocc(ms) <- 0L
  expect_equal(minocc(ms),0L)
  expect_equal(maxocc(ms),4L)
  expect_equal(nocc(ms),5L)

  maxocc(ms) <- 3L
  expect_equal(minocc(ms),0L)
  expect_equal(maxocc(ms),3L)
  expect_equal(nocc(ms), 4L)

  nocc(ms) <- 3L
  expect_equal(minocc(ms),0L)
  expect_equal(maxocc(ms),2L)
  expect_equal(nocc(ms),3L)

  expect_equal(nsubj(ms),1L)
  nsubj(ms) <- 2L
  expect_equal(nsubj(ms),2L)

})

test_that("{ModelSet$models:}", {
  mod1 <- TestModel$new("mod1")
  mod2 <- TestModel$new("mod2")
  ms <- ModelSet$new("test",list(mod1,mod2),
                     rep(1:2,5))
  expect_equal(nmodels(ms),2L)
  expect_equal(ms$models,list(mod1,mod2))

  ## {A "list" of
  ##       \linkS4class{FModel} objects containing the actual models.}

})



test_that("{ModelSet$index:}", {
  ## {Object of class "Panmat" giving the
  ##       indexes of which model is used by which (subject, occasion)
  ##       combination.}

  mod1 <- TestModel$new("mod1")
  mod2 <- TestModel$new("mod2")
  ms <- ModelSet$new("test",list(mod1,mod2),
                     rep(1:2,5))
  expect_true(is(ms$index,"Panmat"))
  expect_equal(ms$iname,character())
  ms$iname <- "index"
  expect_equal(ms$iname,"index")
})
test_that("{ModelSet$qnames:}", {
  ## {A "character" vector giving the names
  ##       of the latent variables. }
  ## qnames and wnames are delegated to the contained models
  mod1 <- TestModel$new("mod1")
  mod2 <- TestModel$new("mod2")
  ms <- ModelSet$new("test",list(mod1,mod2),
                     rep(1:2,5))

  expect_equal(ms$qnames,"theta")
  expect_equal(ms$wname,"w")

  ms$qnames <- "ability"
  expect_equal(ms$qnames,"ability")
  expect_equal(ms$models[[1]]$qnames,"ability")
  expect_equal(ms$models[[2]]$qnames,"ability")

  ms$wname <- "ab"
  expect_equal(ms$wname,"ab")
  expect_equal(ms$models[[1]]$wname,"ab")
  expect_equal(ms$models[[2]]$wname,"ab")

  ## {A "character" vector giving the name(s)
  ##       of the data column.}

  ms$dnames <- "Y"
  expect_equal(dname(ms),"Y")
  expect_equal(dname(ms$models[[1]]),"Y")
  expect_equal(dname(ms$models[[2]]),"Y")
})




test_that("{ModelSetas_longform}", {
  ## {signature(x = "ModelSet"): Returns
  ##       longform (link{as_longform}) data consisting of the index
  ##       variable. }
  pm <- panmat(matrix(1:10,2,5,byrow=TRUE))
  mod1 <- TestModel$new("mod1")
  mod2 <- TestModel$new("mod2")
  ms <- ModelSet$new("test",list(mod1,mod2),pm)
  ms$iname <- "task"

  expect_equal(as_longform(ms),
               data.frame(subj=rep(1:2,each=5),
                          occ=rep(1:5,2),
                          task=1:10))
})


test_that("{ModelSet$prepData}", {
  ## {Prepares data for the mstep calculations.
  ##       Defaults to removing missing index values,
  ##       but is often overridden by
  ##       subclasses.}
  testdat <- read.csv(test_path("testdata.csv"))
  testdat[testdat$occ==0L,"occ"] <- NA
  mod1 <- TestModel$new("mod1")
  mod2 <- TestModel$new("mod2")
  ms <- ModelSet$new("test",list(mod1,mod2),
                     rep(1:2,5))
  ms$iname <- "occ"
  filtdat <- ms$prepData(testdat)
  expect_equal(nrow(filtdat),45)
  expect_equal(unique(filtdat$occ),1:3)

})

test_that("{ModelSet$split_m}", {
  ## {Assigns data to models and builds a list of
  ##       (model, data) pairs.}
  mod1 <- TestModel$new("mod1")
  mod2 <- TestModel$new("mod2")
  mod3 <- TestModel$new("mod3")
  mod4 <- TestModel$new("mod4")
  ms <- ModelSet$new("test",list(mod1,mod2,mod3,mod4),
                     1:4)

  ms$iname <- "task"
  testdat <- read.csv(test_path("testdata.csv"))

  worklist <- ms$split_m(testdat)
  expect_equal(length(worklist),4L)
  expect_equal(worklist[[1]][[1]]$name,"mod1")
  expect_equal(nrow(worklist[[1]][[2]]),10L)
  expect_equal(worklist[[2]][[1]]$name,"mod2")
  expect_equal(nrow(worklist[[2]][[2]]),10L)
  expect_equal(worklist[[3]][[1]]$name,"mod3")
  expect_equal(nrow(worklist[[3]][[2]]),5L)
  expect_equal(worklist[[4]][[1]]$name,"mod4")
  expect_equal(nrow(worklist[[4]][[2]]),20L)

})
test_that("{ModelSet$mstep}", {
  ## {signature(data = "Panel_Data",...,
  ## 	its = "integer", control = "list", workers = "Workers"):
  ##       This is the implementation of the m-step.  Generic method calls
  ##       $prepData and $split_m and then runs over the pairs.}
  mod1 <- TestModel$new("mod1")
  mod2 <- TestModel$new("mod2")
  mod3 <- TestModel$new("mod3")
  mod4 <- TestModel$new("mod4")
  ms <- ModelSet$new("test",list(mod1,mod2,mod3,mod4),
                     1:4)

  ms$iname <- "task"
  testdat <- read.csv(test_path("testdata.csv"))

  ms1 <- mstep(ms,testdat)

  expect_equal(ms1$convergence,rep(TRUE,4))
  expect_equal(ms1$lp,9)
})

