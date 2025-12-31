test_that("ModelSet-class {ModelSet-class}", {
})
test_that("ModelSet-class {ModelSet}", {
})
test_that("ModelSet-class {maxocc.ModelSet}", {
})
test_that("ModelSet-class {maxocc<-.ModelSet}", {
})
test_that("ModelSet-class {minocc.ModelSet}", {
})
test_that("ModelSet-class {minocc<-.ModelSet}", {
})
test_that("ModelSet-class {mstep.ModelSet}", {
})
test_that("ModelSet-class {nocc.ModelSet}", {
})
test_that("ModelSet-class {nocc<-.ModelSet}", {
})
test_that("ModelSet-class {nsubj.ModelSet}", {
})
test_that("ModelSet-class {nsubj<-.ModelSet}", {
})
test_that("ModelSet-class {nmodels.ModelSet}", {
})

test_that("{ModelSet$name:}", {
  ## {A "character" scalar giving the
  ##       a name for the collection. }
})
test_that("{ModelSet$models:}", {
  ## {A "list" of 
  ##       \linkS4class{FModel} objects containing the actual models.}
})
test_that("{ModelSet$index:}", {
  ## {Object of class "Panmat" giving the
  ##       indexes of which model is used by which (subject, occasion)
  ##       combination.}
})
test_that("{ModelSet$iname:}", {
  ## {String giving the name of the index (e.g.,
  ##       \dQuote{group}, \dQuote{task}, or \dQuote{action}).}
})
test_that("{ModelSet$tnames:}", {
  ## {A "character" vector giving the names
  ##       of the latent variables. }
})
test_that("{ModelSet$wname:}", {
  ## {A "character" vector giving the name(s) of
  ##       the weight columnn. }
})
test_that("{ModelSet$dnames:}", {
  ## {A "character" vector giving the name(s)
  ##       of the data column.}
})


test_that("{ModelSetmaxocc,maxocc<-}", {
  ## {:  Gets/sets
  ##       the maximum  occasion associated with the index data. }
})
test_that("{ModelSetminocc, minocc<-}", {
  ## {: Gets/sets the minimum
  ##       occasion associated with the index data.  Usually 0 or 1. }
})
test_that("{ModelSetmstep}", {
  ## {Implements the mstep (see below).}
})
test_that("{ModelSetnocc,nocc<-}", {
  ## {Gets/sets the number of occasions.}
})
test_that("{ModelSetnsubj,nsubj<-}", {
  ## {Gets/sets the number of
  ##       subjects.}
})
test_that("{ModelSetnmodels}", {
  ## {Returns the number of models.}
})
test_that("{ModelSetas_longform}", {
  ## {signature(x = "ModelSet"): Returns
  ##       longform (link{as_longform}) data consisting of the index
  ##       variable. }
})


test_that("{ModelSet$toString}", {
  ## {Generates a charcter representation.}
})
test_that("{ModelSet$print}", {
  ## {Prints the object; default method uses
  ##       $toString so only that needs to be modified.}
})
test_that("{ModelSet$prepData}", {
  ## {Prepares data for the mstep calculations.
  ##       Defaults to identity function, but is often overridden by
  ##       subclasses.} 
})
test_that("{ModelSet$split_m}", {
  ## {Assigns data to models and builds a list of
  ##       (model, data) pairs.}
})
test_that("{ModelSet$mstep}", {
  ## {signature(data = "Panel_Data",...,
  ## 	its = "integer", control = "list", workers = "Workers"):
  ##       This is the implementation of the m-step.  Generic method calls
  ##       $prepData and $split_m and then runs over the pairs.}
})


