test_that("multiplication works", {
  expect_equal(2 * 2, 4)
})

## start, stop

test_that("Workers start and stop", {
  w <- Workers$new(2)
  w$start()
  expect_true(all(w$isAlive()))
  w$stop()
  expect_false(any(w$isAlive()))
  w$debug <- TRUE
  w$start()
  expect_false(any(w$isAlive()))

})

## lapply, sapply
test_that("Workers lapply and sapply", {
  w <- Workers$new(2)
  w$start()
  expect_equal(w$lapply(1:2,identity),list(1,2))
  expect_equal(w$sapply(list(1:2),identity),matrix(1:2,2,1))
  w$stop()
  expect_equal(w$lapply(1:2,identity),list(1,2))
  expect_equal(w$sapply(list(1:2),identity),matrix(1:2,2,1))

})

## condStop

test_that("Workers condStop",{
  w <- Workers$new(2)
  wtest <- function(w,data=1:2,fun=identity) {
    w$start()
    w$lapply(data,fun)
  }
  try(wtest(w))
  expect_false(any(w$isAlive()))

  w$stopClusterOnError=FALSE
  wtest1 <- function(w,data=1:2,fun=identity) {
    w$start()
    w$lapply(data,fun)
    w$flagStop()
  }
  try(wtest1(w))
  expect_false(any(w$isAlive()))
})

## stop on error
test_that("Workers stop on error",{
  w <- Workers$new(2)
  w$stopClusterOnError=FALSE
  wtest1 <- function(w,data=1:2,fun=identity) {
    w$start()
    w$lapply(data,fun)
    w$flagStop()
  }
  try(wtest1(w,fun=\(x) stop("Signal an error.")))
  expect_true(all(w$isAlive()))

})
