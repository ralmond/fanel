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
  expect_equal(long$occ,rep(0:4,2))

  pm1 <- panmat(matrix(1:5,1,5),nsubj=3L)
  long1 <- as_longform(pm1)
  expect_equal(dim(long1),c(15,3))

  pm2 <- panmat(matrix(1:5,1,5))
  long2 <- as_longform(pm2,n=5L)
  expect_equal(dim(long2),c(25,3))


})

test_that("all.equal.Panmat", {
  matpm <- matrix(1:10,2,5,byrow=TRUE)
  pm <- panmat(matpm)
  pm2 <- panmat(matrix(1:10,2,5,byrow=TRUE))
  expect_true(all.equal(pm,pm2))
  ## target other
  expect_match(all.equal(pm,matpm),"Target is a Panmat, but.*")
  ## mat different
  pm2m <- panmat(matrix(1:10,2,5))
  expect_false(isTRUE(all.equal(pm,pm2m)))
  ## nsubj
  pm1a <- panmat(matrix(1:5,1,5))
  pm1b <- panmat(matrix(1:5,1,5),nsubj=5)
  expect_match(all.equal(pm1a,pm1b),"nsubj.*")
  ## isubj different
  pm1c <- pm1a
  isubj(pm1c) <- 1L
  expect_match(all.equal(pm1a,pm1c),"isubj.*")
  expect_match(all.equal(pm1c,pm1a),"isubj.*")
  ## minocc different
  pm1d <- panmat(matrix(1:5,1,5),minocc=0L)
  expect_true(any(grepl("minocc.*",all.equal(pm1a,pm1d))))
  ## maxocc different
  pm1e <- panmat(matrix(1:4,1,4))
  expect_true(any(grepl("maxocc.*",all.equal(pm1a,pm1e))))

})



test_that(" {as.Panmat}", {
  matpm <- matrix(1:10,2,5,byrow=TRUE)
  pm <- panmat(matpm)
  expect_true(all.equal(pm,as.Panmat(pm)))
  expect_true(all.equal(pm,as.Panmat(matpm)))
  pm1 <- panmat(1:5)
  expect_true(all.equal(pm1,as.Panmat(1:5)))
  pmc <- panmat(c("red","blue","green"))
  expect_true(all.equal(pmc,as.Panmat(c("red","blue","green"))))

})

test_that(" {maxocc,Panmat-method}", {
  matpm <- matrix(1:10,2,5,byrow=TRUE)
  pm1 <- panmat(matpm,minocc=1L)
  pm0 <- panmat(matpm,minocc=0L)
  expect_equal(minocc(pm1),1L)
  expect_equal(minocc(pm0),0L)
  expect_equal(maxocc(pm1),5L)
  expect_equal(maxocc(pm0),4L)

  minocc(pm0) <- 2L
  expect_equal(maxocc(pm0),6L)

  pmm <- panmat(matrix(1:2,2,1),minocc=0,nocc=5)
  expect_equal(maxocc(pmm),4L)
  maxocc(pmm) <- 3L
  expect_equal(nocc(pmm),4L)
  expect_equal(maxocc(pmm),3L)
  minocc(pmm) <- 1L
  expect_equal(nocc(pmm),4L)
  expect_equal(maxocc(pmm),4L)
  expect_equal(minocc(pmm),1L)


})

test_that(" {mat}", {
  matpm <- matrix(1:10,2,5,byrow=TRUE)
  pm <- panmat(matpm)
  expect_equal(nsubj(pm),2L)
  expect_equal(nocc(pm),5L)
  expect_equal(as.numeric(pm[2,3]),8)
  expect_equal(mat(pm),matpm)

  mat2 <- matrix(6:9,1,4)
  mat(pm) <- mat2
  expect_equal(nsubj(pm),2L)
  expect_equal(nocc(pm),4L)
  expect_equal(as.numeric(pm[2,3]),8)
  expect_equal(mat(pm),mat2)


})
test_that(" {diff,Panmat-method}", {
  pm <- panmat(matrix(c(1:5,(1:5)*2),2,5,byrow=TRUE),minocc=1L)
  pmd <- diff(pm)
  pme <- panmat(matrix(c(rep(1,4),rep(2,4)),2,4,byrow=TRUE))
  expect_equal(pmd,pme)
})
test_that(" {cumsum,Panmat-method}", {
  pm <- panmat(matrix(c(0:4,(0:4)*2),2,5,byrow=TRUE),minocc=1L)
  pme <- panmat(matrix(c(rep(1,4),rep(2,4)),2,4,byrow=TRUE))
  pmc <- cumsum(pme)
  expect_equal(pmc,pm)

  pm1 <- panmat(matrix(1:2,2,1),minocc=2L,nocc=4L)
  pm1c <-cumsum(pm1)
  expect_equal(pm1c,pm)
})


test_that(" {isubj,Panmat-method}", {
  pm <- panmat(1:5)
  expect_true(is.na(isubj(pm)))
  isubj(pm) <- 3L
  expect_equal(isubj(pm),3L)
})

test_that(" {get_subj,Panmat-method}", {
  pm <- panmat(matrix(c(0:4,(0:4)*2),2,5,byrow=TRUE),minocc=1L)
  pm1 <- panmat(0:4,isubj=1L)
  pm2 <- panmat((0:4)*2,isubj=2L)
  expect_equal(get_subj(pm,1L),pm1)
  expect_equal(get_subj(pm,2L),pm2)

})
test_that(" {get_subj<-,Panmat-method}", {
  pm <- panmat(matrix(c(0:4,(0:4)*2),2,5,byrow=TRUE),minocc=1L)
  pme1 <- panmat(matrix(c((0:4),(0:4)),2,5,byrow=TRUE),minocc=1L)
  pme2 <- panmat(matrix(c((0:4)*2,(0:4)),2,5,byrow=TRUE),minocc=1L)
  pm1 <- panmat(0:4,isubj=2L)
  pm2 <- panmat((0:4)*2,isubj=1L)
  get_subj(pm,2) <- pm1
  expect_equal(pm,pme1)
  get_subj(pm,1) <- pm2
  expect_equal(pm,pme2)

  pmt <- panmat(matrix(c(0:5,2*(0:5),3*(0:5)),3,6,byrow=TRUE),
                minocc=0L)
  pmt1 <- get_subj(pmt,1)
  pmt2 <- get_subj(pmt,2)
  pmt3 <- get_subj(pmt,3)

  get_subj(pmt1,2) <- pmt3
  expect_true(is.na(isubj(pmt1)))
  expect_equal(nsubj(pmt1),2L)
  expect_equal(pmt1[1,],pmt[1,])
  expect_equal(pmt1[2,],pmt[3,])

})
