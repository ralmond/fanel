test_that(" {Population-class}", {
})
test_that(" {Population}", {
})
test_that(" {as_longform,Population-method}", {
})

test_that("{Population} constructor", {
  ##   The constructor is \code{Population$new(name, popModels, groups=1L,
  ##     tname="theta", wname="w")}.
})
test_that("{Population$name:}", {
  ## {A "character" scalar giving the
  ##       a name for the collection. }
})
test_that("{Population$models:}", {
  ## {A "list" of
  ##       \linkS4class{PopulationModel} (\linkS4class{FModel})
  ##       objects containing the actual models.}
})
test_that("{Population$index:}", {
  ## {Object of class "Panmat" giving the
  ##       indexes of which model is used by which (subject, occasion)
  ##       combination.}
})
test_that("{Population$iname:}", {
  ## {String giving the name of the index, defaults
  ##       to \dQuote{group}.}
})
test_that("{Population$tnames:}", {
  ## {A "character" vector giving the names
  ##       of the latent variables. }
})
test_that("{Population$wname:}", {
  ## {A "character" vector giving the name(s) of
  ##       the weight columnn. }
})
test_that("{Population$dnames:}", {
  ## {A "character" vector giving the name(s)
  ##       of the data column.  Not used.}
})

test_that("{Populationmaxocc,maxocc<-}", {
  ## {:  Gets/sets
  ##       the maximum  occasion associated with the index data. }
})
test_that("{Populationminocc, minocc<-}", {
  ## {: Gets/sets the minimum
  ##       occasion associated with the index data.  Usually 0 or 1. }
})
test_that("{Populationmstep}", {
  ## {Implements the mstep (see below).}
})
test_that("{Populationnocc,nocc<-}", {
  ## {Gets/sets the number of occasions.}
})
test_that("{Populationnsubj,nsubj<-}", {
  ## {Gets/sets the number of subjects.}
})
test_that("{Populationnmodels}", {
  ## {Returns the number of models.}
})
test_that("{Populationas_longform}", {
  ## {signature(x = "ModelSet"): Returns
  ##       longform (link{as_longform}) data consisting of the group
  ##       variable. }
})
test_that("{PopulationdrawInitial,$drawInit}", {
  ## {(model, isubj,
  ## 	npart, covar=NULL), calls the
  ##       \linkS4class{PopulationModel$drawInit} method with the
  ##       model with referenced by $group(isubj).}
})
test_that("{PopulationprobInit,$initProbs}", {
  ## {(model, isubj,
  ## 	theta, covar=NULL), calls the
  ##       \linkS4class{PopulationModel$initProbs} method with the
  ##       model with referenced by $group(isubj).}
})

test_that("{Population$toString}", {
  ## {Generates a charcter representation.}
})
test_that("{Population$print}", {
  ## {Prints the object; default method uses
  ##       $toString so only that needs to be modified.}
})
test_that("{Population$prepData}", {
  ## {Prepares data for the mstep calculations,
  ##       filters to only include occasion 0.}
})
test_that("{Population$split_m}", {
  ## {Assigns data to models and builds a list of
  ##       (model, data) pairs.}
})
test_that("{Population$mstep}", {
  ## {signature(data = "Panel_Data",...,
  ## 	its = "integer", control = "list", workers = "Workers"):
  ##       This is the implementation of the m-step.  Generally only the
  ##       $prepData method needs to be overridden.}
})
