
test_that(" {wtd.Ecdf}", {
  ## wtd.Ecdf(x, weights = NULL, type = c("i/n", "(i-1)/(n-1)", "i/(n+1)"),
  ##     normwt = FALSE, na.rm = TRUE)
  x <- c(1:5,1:5,1:5,1:5,1:5)
  w <- c(1:5,5:1,1:5,5:1,rep(3,5))/15
  expect_equal(wtd.Ecdf(x,w)$x,c(1,1:5))
  expect_equal(wtd.Ecdf(x,w)$ecdf,(0:5)/5)
})

test_that(" {wtd.loess.noiter}", {
  ## wtd.loess.noiter(x, y, weights = rep(1, length(x)), span = 2/3, degree = 1,
  ##     cell = 0.13333, type = c("all", "ordered all", "evaluate"),
  ##     evaluation = 100, na.rm = TRUE)
  x <- rep(1:5,each=5)
  y <- c(1:5,1:5,1:5,1:5,1:5)
  w <- c(1:5,5:1,1:5,5:1,rep(3,5))/15
  expect_equal(wtd.loess.noiter(x,y,w)$x,x)
  yy <- wtd.loess.noiter(x,y,w)$y
  expect_gt(yy[1],yy[6])
  expect_gt(yy[6],yy[11])
  expect_gt(yy[16],yy[21])


})

test_that(" {wtd.mean}", {
  ## wtd.mean(x, weights = NULL, normwt = "ignored", na.rm = TRUE)
  x <- c(1:5,5:1,1:5,1:5)
  w <- c(1:5,1:5,rep(2,10))/10
  expect_equal(wtd.mean(x,w),3)
})

test_that(" {wtd.sd}", {
  ## wtd.sd(x, weights = NULL, normwt = FALSE, na.rm = TRUE, method = c("unbiased",
  ##     "ML"))
  x <- c(1:5,1:5,1:5,1:5,1:5)
  w <- c(1:5,5:1,1:5,5:1,rep(3,5))/15
  expect_equal(wtd.sd(x,w),sqrt(2))
})

test_that(" {wtd.quantile}", {
  ## wtd.quantile(x, weights = NULL, probs = c(0, 0.25, 0.5, 0.75, 1),
  ##        type = c("quantile", "(i-1)/(n-1)", "i/(n+1)", "i/n"),
  ##        normwt = FALSE, na.rm = TRUE)
  x <- c(1:5,1:5,1:5,1:5,1:5)
  w <- c(1:5,5:1,1:5,5:1,rep(3,5))/15
  expect_equal(wtd.quantile(x,w)$val,1:5)
})

test_that(" {wtd.rank}", {
  ## wtd.rank(x, weights = NULL, normwt = FALSE, na.rm = TRUE)
  x <- c(1:5,1:5,1:5,1:5,1:5)
  w <- c(1:5,5:1,1:5,5:1,rep(3,5))/15
  expect_equal(wtd.rank(x,w),x)
})

test_that(" {wtd.table}", {
## wtd.table(x, weights = NULL, type = c("list", "table"), normwt = FALSE,
##     na.rm = TRUE)
  x <- 1:5
  w <- (1:5)/15
  expect_equal(wtd.table(x,w)$x,x)
  expect_equal(wtd.table(x,w)$sum.of.weights,w)
})

test_that(" {wtd.var}", {
## wtd.var(x, weights = NULL, normwt = FALSE, na.rm = TRUE, method = c("unbiased",
##     "ML"))
  x <- c(1:5,1:5,1:5,1:5,1:5)
  w <- c(1:5,5:1,1:5,5:1,rep(3,5))/15
  expect_equal(wtd.var(x,w),2)
})


