### normalForm

test_that("normalForm colnames", {
  dat <- data.frame(subj=rep(1L:2L,each=4L),
                    occ=rep(1L:4L,2L),
                    value=1:8)
  nf <- normalForm(dat)
  expect_equal(nf$nsubj,2L)
  expect_equal(nf$minocc,0L)
  expect_equal(nf$maxocc,4L)
  expect_equal(nf$normdat,data.frame(value=c(NA,1:4,NA,5:8)),ignore_attr=TRUE)

  names(dat) <- c("foo","bar","value")
  nf <- normalForm(dat[,3:1],"foo","bar")
  expect_equal(nf$nsubj,2L)
  expect_equal(nf$minocc,0L)
  expect_equal(nf$maxocc,4L)
  expect_equal(nf$normdat,data.frame(value=c(NA,1:4,NA,5:8)),ignore_attr=TRUE)

})

## fill missing
test_that("normalForm missing values", {
  dat <- data.frame(subj=rep(1L:2L,each=4L),
                    occ=rep(1L:4L,2L),
                    value=1:8)
  nf <- normalForm(dat[-7,])
  expect_equal(nf$nsubj,2L)
  expect_equal(nf$minocc,0L)
  expect_equal(nf$maxocc,4L)
  expect_equal(nf$normdat,data.frame(value=c(NA,1:4,NA,5:6,NA,8)),ignore_attr=TRUE)

  names(dat) <- c("foo","bar","value")
  nf <- normalForm(dat[-5,3:1],"foo","bar")
  expect_equal(nf$nsubj,2L)
  expect_equal(nf$minocc,0L)
  expect_equal(nf$maxocc,4L)
  expect_equal(nf$normdat,data.frame(value=c(NA,1:4,NA,NA,6:8)),ignore_attr=TRUE)

})

## scrambled order
test_that("normalForm scrambled order", {
  dat <- data.frame(subj=rep(1L:2L,each=4L),
                    occ=rep(1L:4L,2L),
                    value=1:8)
  nf <- normalForm(dat[sample.int(8,8),])
  expect_equal(nf$nsubj,2L)
  expect_equal(nf$minocc,0L)
  expect_equal(nf$maxocc,4L)
  expect_equal(nf$normdat,data.frame(value=c(NA,1:4,NA,5:8)),ignore_attr=TRUE)

  names(dat) <- c("foo","bar","value")
  nf <- normalForm(dat[sample.int(8,8),3:1],"foo","bar")
  expect_equal(nf$nsubj,2L)
  expect_equal(nf$minocc,0L)
  expect_equal(nf$maxocc,4L)
  expect_equal(nf$normdat,data.frame(value=c(NA,1:4,NA,5:8)),ignore_attr=TRUE)

})

## start at zero
test_that("normalForm zerostart", {
  dat <- data.frame(subj=rep(1L:2L,each=4L),
                    occ=rep(0L:3L,2L),
                    value1=1:8)
  nf <- normalForm(dat)
  expect_equal(nf$nsubj,2L)
  expect_equal(nf$minocc,0L)
  expect_equal(nf$maxocc,3L)
  expect_equal(nf$normdat,data.frame(value=1:8),ignore_attr=TRUE)

  names(dat) <- c("foo","bar","value")
  nf <- normalForm(dat[,3:1],"foo","bar")
  expect_equal(nf$nsubj,2L)
  expect_equal(nf$minocc,0L)
  expect_equal(nf$maxocc,3L)
  expect_equal(nf$normdat,data.frame(value=1:8),ignore_attr=TRUE)

})



### panel_frame
test_that("Panel Frame, 1-base",{
  dat <- data.frame(subj=rep(1L:2L,each=4L),
                    occ=rep(1L:4L,2L),
                    value1=1:8,value2=101:108)
  pd <- panel_frame(dat,zerostart=FALSE)
  expect_equal(nsubj(pd),2L)
  expect_equal(nocc(pd),4L)
  expect_equal(minocc(pd),1L)
  expect_equal(maxocc(pd),4L)
  lf <- as_longform(pd)
  expect_equal(lf[lf$occ>0L,],dat,ignore_attr=TRUE)

  expect_equal(pd[1,3],data.frame(value1=3,value2=103),
               ignore_attr=TRUE)
  expect_equal(pd[1:2,3],data.frame(value1=c(3,7),value2=c(103,107)),
               ignore_attr=TRUE)
  expect_equal(pd[,3],data.frame(value1=c(3,7),value2=c(103,107)),
               ignore_attr=TRUE)
  expect_equal(pd[1,3:4],data.frame(value1=3:4,value2=103:104),
               ignore_attr=TRUE)
  expect_equal(pd[1,],data.frame(value1=c(1:4),value2=c(101:104)),
               ignore_attr=TRUE)

  pd[1,3]$value2 <- pd[1,3]$value1
  expect_equal(pd[,3],data.frame(value1=c(3,7),value2=c(3,107)),
               ignore_attr=TRUE)
  pd[,3]$value2 <- pd[,3]$value1
  expect_equal(pd[,3],data.frame(value1=c(3,7),value2=c(3,7)),
               ignore_attr=TRUE)
  pd[1,]$value2 <- pd[1,]$value1
  expect_equal(pd[1,],data.frame(value1=c(1:4),value2=c(1:4)),
               ignore_attr=TRUE)

})

test_that("Panel Frame, 0-base",{
  dat <- data.frame(subj=rep(1L:2L,each=4L),
                    occ=rep(0L:3L,2L),
                    value1=1:8,value2=101:108)
  pd <- panel_frame(dat)
  expect_equal(nsubj(pd),2L)
  expect_equal(nocc(pd),4L)
  expect_equal(as_longform(pd),dat)

  expect_equal(pd[1,0],data.frame(value1=1,value2=101),
               ignore_attr=TRUE)
  expect_equal(pd[1:2,0],data.frame(value1=c(1,5),value2=c(101,105)),
               ignore_attr=TRUE)
  expect_equal(pd[,2],data.frame(value1=c(3,7),value2=c(103,107)),
               ignore_attr=TRUE)
  expect_equal(pd[1,2:3],data.frame(value1=3:4,value2=103:104),
               ignore_attr=TRUE)
  expect_equal(pd[1,],data.frame(value1=1:4,value2=101:104),
               ignore_attr=TRUE)

  pd[1,0]$value2 <- pd[1,0]$value1
  expect_equal(pd[,0],data.frame(value1=c(1,5),value2=c(1,105)),
               ignore_attr=TRUE)
  pd[,2]$value2 <- pd[,2]$value1
  expect_equal(pd[,2],data.frame(value1=c(3,7),value2=c(3,7)),
               ignore_attr=TRUE)
  pd[1,]$value2 <- pd[1,]$value1
  expect_equal(pd[1,],data.frame(value1=1:4,value2=1:4),
               ignore_attr=TRUE)

})


test_that("as_longform Panel_Frame", {
  dat <- data.frame(subj=rep(1L:2L,each=4L),
                    occ=rep(1L:4L,2L),
                    value1=1:8,value2=101:108)
  pd <- panel_frame(dat,zerostart=FALSE)
  expect_equal(as_longform(pd),dat)

})

test_that(" {isubj,Panel_Frame-method}", {
  dat <- data.frame(subj=rep(1L:2L,each=4L),
                    occ=rep(1L:4L,2L),
                    value1=1:8,value2=101:108)
  pd <- panel_frame(dat,zerostart=FALSE)
  pd1 <- get_subj(pd,1L)
  pd2 <- get_subj(pd,2L)
  expect_true(is.na(isubj(pd)))
  expect_equal(isubj(pd1),1L)
  expect_equal(isubj(pd2),2L)

  isubj(pd2) <- 7
  expect_equal(isubj(pd2),7L)

})

test_that(" {get_subj,Panel_Frame-method}", {
  dat <- data.frame(subj=rep(1L:2L,each=4L),
                    occ=rep(1L:4L,2L),
                    value1=1:8,value2=101:108)
  pd <- panel_frame(dat,zerostart=FALSE)

  pd1 <- get_subj(pd,1L)
  pd2 <- get_subj(pd,2L)
  expect_equal(pd1[1,],pd[1,])
  expect_equal(pd2[1,],pd[2,])

  isubj(pd1) <- 2L
  get_subj(pd,2) <- pd1
  expect_equal(pd[2,]$value1,1:4)
  expect_equal(pd[2,]$value2,101:104)
})




