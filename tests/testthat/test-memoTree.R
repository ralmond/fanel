test_that("memoTree-class {memoTree-class}", {
})
test_that("memoTree-class {memoTree}", {
})

test_that("{memoTree$depth:}", {
  ## {Integer giving number of layers below.}
})
test_that("{memoTree$cache:}", {
  ## {An environment used to cache the values.}
})

test_that("{memoTree$initialize}", {
  ## {(depth=1L): Called by $new .}
})
test_that("{memoTree$exists}", {
  ## {(key): Returns true if key exists and
  ##       false otherwise.}
})
test_that("{memoTree$get}", {
  ## {(key): Returns the value if it is in the
  ##       cache and NULL otherwise.}
})
test_that("{memoTree$assign}", {
  ## {(key,value): Assigns a value to the key
  ##       in the cache.}
})
test_that("{memoTree$clear}", {
  ## {(): Clears the cached values.}
})
