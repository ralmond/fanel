test_that("Evidence-class {Evidence-class}", {
})
test_that("Evidence-class {Evidence}", {
})
test_that("Evidence-class {as_longform.Evidence}", {
})
test_that("Evidence-class {drawData.Evidence}", {
})
test_that("Evidence-class {evalEvidence.Evidence}", {
})

test_that("Evidence Constructor", {
##   The constructor is Evidence$new(name, evidenceModels,
##     tasks=matrix(1L,1L,1L), tname="theta", wname="w", dname="Y").
})

test_that("{FixedQuad$$name:}", {
  ## {A "character" scalar giving the
  ##       a name for the collection. }
})
test_that("{FixedQuad$$models:}", {
  ## {A "list" of
  ##       \linkS4class{PopulationModel} (\linkS4class{FModel})
  ##       objects containing the actual models.}
})
test_that("{FixedQuad$$index:}", {
  ## {Object of class "Panmat" giving the
  ##       indexes of which model is used by which (subject, occasion)
  ##       combination.}
})
test_that("{FixedQuad$$iname:}", {
  ## {String giving the name of the index, defaults
  ##       to \dQuote{group}.}
})
test_that("{FixedQuad$$tnames:}", {
  ## {A "character" vector giving the names
  ##       of the latent variables. }
})
test_that("{FixedQuad$$wname:}", {
  ## {A "character" vector giving the name(s) of
  ##       the weight columnn. }
})
test_that("{FixedQuad$$dnames:}", {
  ## {A "character" vector giving the name(s)
  ##       of the data column.}
})

test_that("{FixedQuad$maxocc,maxocc<-}", {
  ## {:  Gets/sets
  ##       the maximum  occasion associated with the index data. }
})
test_that("{FixedQuad$minocc, minocc<-}", {
  ## {: Gets/sets the minimum
  ##       occasion associated with the index data.  Usually 0 or 1. }
})
test_that("{FixedQuad$mstep}", {
  ## {Implements the mstep (see below).}
})
test_that("{FixedQuad$nocc,nocc<-}", {
  ## {Gets/sets the number of occasions.}
})
test_that("{FixedQuad$nsubj,nsubj<-}", {
  ## {Gets/sets the number of subjects.}
})
test_that("{FixedQuad$nmodels}", {
  ## {Returns the number of models.}
})
test_that("{FixedQuad$as_longform}", {
  ## {signature(x = "ModelSet"): Returns
  ##       longform (link{as_longform}) data consisting of the group
  ##       variable. }
})
test_that("{FixedQuad$drawData, $drawObs}", {
  ## {(model,isubj,iocc,theta,
  ##         cov=NULL): Draws random values for data values.  Used in
  ##       \link{simulate.POMDP}. }
})
test_that("{FixedQuad$evalEvidence}", {
  ## {(model,isubj,iocc,theta,Y,covar=NULL):
  ##       Calculates the log-likehood of the data given the latent variable.}
})


test_that("{FixedQuad$$toString}", {
  ## {Generates a charcter representation.}
})
test_that("{FixedQuad$$print}", {
  ## {Prints the object; default method uses
  ##       $toString so only that needs to be modified.}
})
test_that("{FixedQuad$$prepData}", {
  ## {Returns data, but could be overridden if
  ##       needed by a subclass.}
})
test_that("{FixedQuad$$split_m}", {
  ## {Assigns data to models and builds a list of
  ##       (model, data) pairs.}
})
test_that("{FixedQuad$$mstep}", {
  ## {signature(data = "Panel_Data",...,
  ## 	its = "integer", control = "list", workers = "Workers"):
  ##       This is the implementation of the m-step.  Generally only the
  ##       $prepData method needs to be overridden.}
})

