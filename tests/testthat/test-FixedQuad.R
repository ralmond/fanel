test_that("Quadrature-class {Quadrature-class}", {
})
test_that("Quadrature-class {Quadrature}", {
})
test_that("Quadrature-class {FixedQuad-class}", {
})
test_that("Quadrature-class {FixedQuad}", {
})
test_that("Quadrature-class {fixedQuad}", {
})
test_that("Quadrature-class {as_longform.Quadrature}", {
})
test_that("Quadrature-class {get_subj.Quadrature}", {
})
test_that("Quadrature-class {get_subj<-.Quadrature}", {
})
test_that("Quadrature-class {maxocc.Quadrature}", {
})
test_that("Quadrature-class {maxocc<-.Quadrature}", {
})
test_that("Quadrature-class {minocc.Quadrature}", {
})
test_that("Quadrature-class {minocc<-.Quadrature}", {
})
test_that("Quadrature-class {nocc.Quadrature}", {
})
test_that("Quadrature-class {nocc<-.Quadrature}", {
})
test_that("Quadrature-class {nquad,Quadrature}", {
})
test_that("Quadrature-class {nquad<-.Quadrature}", {
})
test_that("Quadrature-class {nsubj.Quadrature}", {
})
test_that("Quadrature-class {nsubj<-.Quadrature}", {
})
test_that("Quadrature-class {isubj.Quadrature}", {
})
test_that("Quadrature-class {isubj<-.Quadrature}", {
})

test_that("{FixedQuad$static:}", {
  ## {A logical value.  If true then the quadrature
  ##       points are the same for each occasion.}
})
test_that("{FixedQuad$bySubj:}", {
  ## {A logical value.  If false then the quadrature
  ##       points are different for each subject.}
})
test_that("{FixedQuad$wname:}", {
  ## {The name to use for the weight object.}
})
test_that("{FixedQuad$lweights:}", {
  ## {A subj x occ x quad
  ##       array giving the log weights.  See the $lweight() function
  ##       for accessing parts of the array.}
})
test_that("{FixedQuad$thetas:}", {
  ## {The full nsubj x nocc x
  ##       nquad x dtheta array of quadrature points.  See the
  ##       $theta() function below for accessing part of the array.}
})
test_that("{FixedQuad$tnames:}", {
  ## {A "character" vector containing the names
  ##       for the quadrature variables.  Should have length $dtheta}
})
test_that("{FixedQuad$dtheta:}", {
  ## {An "integer" containing the number of
  ##       dimenions for each quadrature point.}
})
test_that("{FixedQuad$times:}", {
  ## {Object of class "Panmat" mapping
  ##       measurement occasions to times.}
})
test_that("{FixedQuad$minocc:}", {
  ## {An "integer" giving the index for the
  ##       smallest index in this quadrature.}
})
test_that("{FixedQuad$maxocc:}", {
  ## {An "integer" giving the index for the
  ##       largest index in this quadrature.}
})
test_that("{FixedQuad$nsubj:}", {
  ## {An "integer" giving the number of
  ##       subjects in this quadrature.}
})
test_that("{FixedQuad$isubj:}", {
  ## {An integer giving the index the subject which
  ##       is the focus of this quadrature (see \link{split_subj}).}
})
test_that("{FixedQuad$nquad:}", {
  ## {An "integer" giving the number of
  ##       quadrature points.}
})

test_that("{FixedQuad$$weights(type='default')}", {
  ## {Returns normalized weights (on probability,
  ##       not log scale).}
})
test_that("{FixedQuad$$resetWeights()}", {
  ## {Clears the weights before re-running filter.}
})
test_that("{FixedQuad$$theta(subj,occ,quad,value)}", {
  ## {Gets (if value is missing)
  ##       or sets the quadrature points for a given subject, occasion.  The
  ##       quad field can be missing to set all quadrature points.}
})
test_that("{FixedQuad$$lweight(subj,occ,quad,value)}", {
  ## {Gets (if value is missing)
  ##       or sets the log weights for a given occasion. Again, quad
  ##       can be omitted.}
})

test_that("{FixedQuad$as_longform}", {
  ## {signature(x = "Quadrature"): Converts the
  ##       quadrature into very long form (with indexes subj, occ and quad). }
})
test_that("{FixedQuad$maxocc}", {
  ## {signature(obj = "Quadrature"): Gets the maximum
  ##       occasion index.}
})
test_that("{FixedQuad$maxocc<-}", {
  ## {signature(obj = "Quadrature"): Sets the
  ##       maximum occasion index. }
})
test_that("{FixedQuad$minocc}", {
  ## {signature(obj = "Quadrature"): Gets the minimum
  ##       occasion index.}
})
test_that("{FixedQuad$minocc<-}", {
  ## {signature(obj = "Quadrature"): Sets the
  ##       minimum occasion index. }
})
test_that("{FixedQuad$nocc}", {
  ## {signature(obj = "Quadrature"): Gets the number of
  ##       occasions. }
})
test_that("{FixedQuad$nocc<-}", {
  ## {signature(obj = "Quadrature"): Sets the number
  ##       of occasions.}
})
test_that("{FixedQuad$nquad}", {
  ## {signature(obj = "Quadrature"): Gets the number
  ##       of quadrature points. }
})
test_that("{FixedQuad$nquad<-}", {
  ## {signature(obj = "Quadrature"): Sets the number
  ##       of quadrature points (only works for \link{particleFilter}).}
})
test_that("{FixedQuad$nsubj}", {
  ## {signature(obj = "Quadrature"): Gets the number
  ##       of subjects. }
})
test_that("{FixedQuad$nsubj<-}", {
  ## {signature(obj = "Quadrature"): Sets the number
  ##       of subjects.}
})
test_that("{FixedQuad$isubj}", {
  ## {signature(obj = "Quadrature"): Gets the index of
  ##       the focus subject (see \link{split_subj}).}
})
test_that("{FixedQuad$isubj<-}", {
  ## {signature(obj = "Quadrature"): Sets the index
  ##       of the focus subject.}
})
test_that("{FixedQuad$get_subj}", {
  ## {signature(x = "Quadrature", subj="integer"):
  ##       Returns a
  ##       subquadrature focused on a single individual.  See
  ##       \link{split_subj}. }
})
test_that("{FixedQuad$get_subj<-}", {
  ## {signature(x = "Quadrature", subj =
  ## 	"integer", value = "Quadrature"): Replaces
  ##       sub-quadrature focused on a single individual.  See
  ##       \link{split_subj}. }
})
