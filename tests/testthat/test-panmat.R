### Panmat
test_that("Panmat 2x5", {
  pm <- panmat(matrix(1:10,2,5,byrow=TRUE))
  expect_equal(nsubj(pm),2L)
  expect_equal(nocc(pm),5L)
  expect_equal(as.numeric(pm[2,3]),8)
  pm[2,] <- 1:5
  expect_equal(as.numeric(pm[2,3]),3)

  expect_equal(nsubj(pm),2L)
  expect_equal(nocc(pm),5L)

  expect_equal(as.matrix(pm),matrix(rep(1:5,2),2,5,byrow=TRUE))

  expect_equal(as_longform(pm),
               data.frame(subj=rep(1:2,each=5),occ=rep(1:5,2),
                          pm=rep(1:5,2)))

})

test_that("Panmat 1x5", {
  pm <- panmat(matrix(1:5,1,5))
  expect_equal(nsubj(pm),1L)
  expect_equal(nocc(pm),5L)
  expect_equal(as.numeric(pm[2,3]),3)
  pm[1,] <- 6:10
  expect_equal(as.numeric(pm[2,3]),8)

  expect_equal(nsubj(pm),1L)
  nsubj(pm) <- 3L
  expect_equal(nsubj(pm),3L)
  expect_equal(nocc(pm),5L)

  expect_equal(as_longform(pm),
               data.frame(subj=rep(1:3,each=5),occ=rep(1:5,3),
                          pm=rep(6:10,3)))


})


test_that("Panmat 2x1", {
  pm <- panmat(matrix(1:2,2,1))
  expect_equal(nsubj(pm),2L)
  expect_equal(nocc(pm),1L)
  expect_equal(as.numeric(pm[2,3]),2)
  pm[1,] <- 5
  expect_equal(as.numeric(pm[1,3]),5)

  expect_equal(nsubj(pm),2L)
  expect_equal(nocc(pm),1L)
  nocc(pm) <- 5L
  expect_equal(nocc(pm),5L)

  expect_equal(as_longform(pm),
               data.frame(subj=rep(1:2,each=5),occ=rep(1:5,2),
                          pm=rep(c(5,2),each=5)))

})

test_that("as_longform Panmat", {
  pm <- panmat(matrix(1:10,2,5,byrow=TRUE),minocc=0L)
  long <- as_longform(pm)
  expect_equal(dim(long),c(10,3))
  expect_equal(names(long),c("subj","occ","pm"))
  expect_equal(long$occ,rep(1:5,2))

  pm1 <- panmat(matrix(1:5,1,5),nsubj=3L)
  long1 <- as_longform(pm1)
  expect_equal(dim(long1),c(15,3))

  pm2 <- panmat(matrix(1:5,1,5))
  long2 <- as_longform(pm2,n=5L)
  expect_equal(dim(long2),c(25,3))


})

test_that(" {Panmat-class}", {
})
test_that(" {Panmat}", {
})
test_that("[,Panmat-method",{
})
test_that("[<-,Panmat-method",{
})
test_that(" {as.Panmat}", {
})
test_that(" {as.Panmat,Panmat-method}", {
})
test_that(" {as.Panmat,matrix-method}", {
})
test_that(" {as.Panmat,character-method}", {
})
test_that(" {as.Panmat,numeric-method}", {
})
test_that(" {as_longform,Panmat-method}", {
})
test_that(" {nocc,Panmat-method}", {
})
test_that(" {nocc<-,Panmat-method}", {
})
test_that(" {maxocc,Panmat-method}", {
})
test_that(" {maxocc<-,Panmat-method}", {
})
test_that(" {minocc,Panmat-method}", {
})
test_that(" {minocc<-,Panmat-method}", {
})
test_that(" {nsubj,Panmat-method}", {
})
test_that(" {nsubj<-,Panmat-method}", {
})
test_that(" {isubj,Panmat-method}", {
})
test_that(" {isubj<-,Panmat-method}", {
})
test_that(" {panmat}", {
})
test_that(" {mat}", {
})
test_that(" {mat<-}", {
})
test_that(" {diff,Panmat-method}", {
})
test_that(" {cumsum,Panmat-method}", {
})
test_that(" {get_subj,Panmat-method}", {
})
test_that(" {get_subj<-,Panmat-method}", {
})
