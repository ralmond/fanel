test_that("{memoTree$depth:}", {

  mt1 <- memoTree$new(1)
  expect_equal(mt1$depth,1)
  expect_no_error(mt1$exists(1))
  expect_error(mt1$exists(c(1,2)))

  mt2 <- memoTree$new(2)
  expect_equal(mt2$depth,2)
  expect_no_error(mt2$exists(1))
  expect_no_error(mt2$exists(c(1,2)))
  expect_error(mt2$exists(c(1,2,3)))

})


test_that("{memoTree$exists}", {
  cacheA <- memoTree$new(2)

  cacheA$assign(c(1,2), "One-Two")
  cacheA$assign(c(2,1), "Two-One")
  expect_false(cacheA$exists(c(1,1)))
  expect_true(cacheA$exists(c(1,2)))
  expect_false(cacheA$exists(c(2,2)))
  expect_true(cacheA$exists(c(2,1)))
  expect_true(cacheA$exists(1))
  expect_true(cacheA$exists(2))
  expect_false(cacheA$exists(3))

  cacheA$clear()
  expect_false(cacheA$exists(c(1,2)))
  expect_false(cacheA$exists(c(2,1)))

})
test_that("{memoTree$get}", {
  cacheA <- memoTree$new(2)

  cacheA$assign(c(1,2), "One-Two")
  cacheA$assign(c(2,1), "Two-One")
  expect_equal(cacheA$get(c(1,2)),"One-Two")
  expect_equal(cacheA$get(c(2,1)),"Two-One")
  expect_null(cacheA$get(c(1,1)))

})
