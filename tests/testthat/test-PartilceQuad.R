# Setup helper to create test data
create_test_times <- function(n = 5) {
  panmat(1:n)
}

test_that("ParticleQuad initializes correctly", {
  p <- particleQuad(times = panmat(1:3),
                    qnames="theta",
                    nquadrature = 5,
                    nsubjects = 2)
  expect_equal(p$nquad, 5)
  expect_equal(p$isubj, NA_integer_)
  expect_true(p$byocc)
  expect_true(p$bysubj)
  expect_equal(p$nsubj,2)
  pm <- panmat(1:3)
  nsubj(pm) <-2
  expect_equal(p$times,pm)
})

test_that("ParticleQuad$weights('history') works", {
  p <- ParticleQuad$new(
    times = panmat(1:3),
    nquadrature = 4,
    byocc = TRUE,
    bysubj = TRUE,
    nsubjects = 2
  )
  p$resetWeights()
  w <- p$weights("history")
  expect_equal(length(w), 2*4*3)
})

test_that("ParticleQuad$weights('default') works", {
  p <- ParticleQuad$new(
    times = create_test_times(3),
    nquadrature = 4,
    byocc = TRUE,
    bysubj = TRUE,
    nsubjects = 2
  )
  p$resetWeights()
  w <- p$weights("default")
  expect_equal(length(w), 2*4*3)
})

test_that("ParticleQuad$resetWeights works", {
  p <- ParticleQuad$new(
    times = create_test_times(3),
    nquadrature = 3,
    byocc = TRUE,
    bysubj = TRUE,
    nsubjects = 2
  )
  p$qpoints(1,1,runif(3))
  p$resetWeights()
  expect_true(all(is.na(p$quadpoints[1,1,])))
  expect_equal(p$lweight(1,0,1),0)
  expect_true(is.na(p$lweight(1,1,1)))
})

test_that("ParticleQuad$initialize with invalid nquadrature raises", {
  expect_error(
    ParticleQuad$new(
      times = create_test_times(3),
      nquadrature = 0,
      byocc = TRUE,
      bysubj = TRUE,
      nsubjects = 2
    ),
    "There must be at least 1 quadrature point."
  )
})

test_that("ParticleQuad inherits correctly", {
  p <- ParticleQuad$new(
    times = create_test_times(3),
    nquadrature = 5,
    byocc = TRUE,
    bysubj = TRUE,
    nsubjects = 2
  )
  expect_true(inherits(p, "Quadrature"))
  expect_true(inherits(p, "R6"))
})


test_that("ParticleQuad-class {nquad,Quadrature}", {
  p <- particleQuad(times = panmat(1:3),
                    qnames="theta",
                    nquadrature = 5,
                    nsubjects = 2)
  expect_equal(nquad(p),5)
  p$resetWeights()
  expect_equal(dim(p$lweights),c(5,3,2))
  expect_equal(nrow(p$quadpoints@dat),5*3*2)

  nquad(p) <- 7
  p$resetWeights()
  expect_equal(dim(p$lweights),c(7,3,2))
  expect_equal(nrow(p$quadpoints@dat),7*3*2)

})


test_that("ParticleQuad$qnames", {
  p <- particleQuad(times = panmat(1:3),
                    qnames="theta",
                    nquadrature = 5,
                    nsubjects = 2)
  expect_equal(nquad(p),5)
  p$resetWeights()
  expect_equal(dim(p$lweights),c(5,3,2))
  expect_equal(nrow(p$quadpoints@dat),5*3*2)
  expect_equal(ncol(p$quadpoints@dat),1)
  expect_equal(p$dquad,1)

  p$qnames <- c("theta1","theta2")
  p$resetWeights()
  expect_equal(dim(p$lweights),c(5,3,2))
  expect_equal(nrow(p$quadpoints@dat),5*3*2)
  expect_equal(ncol(p$quadpoints@dat),2)
  expect_equal(p$dquad,2)
})

