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


### Panel_Data

test_that("tdt delta t",{
  tf <- panel_data(dt=rep(1,5))
  expect_equal(mat(tf$dt),matrix(1,1,5))
  expect_equal(mat(tf$time),matrix(0:5,1,6))
  expect_equal(tf$maxocc,5)
  expect_equal(as_longform(tf),
               data.frame(subj=rep(1L,6L),occ=0L:5L,
                          time=0L:5L))

  tf$dt <- matrix(1:2,2,3)
  expect_equal(as.numeric(tf$dt[1,3]),1)
  expect_equal(as.numeric(tf$dt[2,3]),2)
  expect_equal(as.matrix(tf$time),matrix(c(0:3,2*(0:3)),2,4,byrow=TRUE))
  expect_equal(as_longform(tf),
               data.frame(subj=rep(1L:2L,each=4L),
                          occ=rep(0L:3L,2L),
                          time=c(0:3,2*(0:3))))
})

test_that("pandat times",{
  tf <- panel_data(time=0:5)
  expect_equal(mat(tf$dt),matrix(1,1,5))
  expect_equal(mat(tf$time),matrix(0:5,1,6))
  expect_equal(tf$maxocc,5)
  expect_equal(as_longform(tf),
               data.frame(subj=rep(1L,6L),occ=0L:5L,
                          time=0L:5L))



  tf$time <- matrix(c(0:3,2*(0:3)),2,4,byrow=TRUE)
  expect_equal(as.numeric(tf$dt[1,3]),1)
  expect_equal(as.numeric(tf$dt[2,3]),2)
  expect_equal(tf$dt,as.Panmat(matrix(1:2,2,3)))
  expect_equal(as_longform(tf),
               data.frame(subj=rep(1L:2L,each=4L),
                          occ=rep(0L:3L,2L),
                          time=c(0:3,2*(0:3))))

})

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

test_that("Panel Frame complete", {

  long <- data.frame(subj=rep(1:2,each=6),
                     occ=rep(0:5,2),
                     dat1=c(NA,0,0,0,1,1,NA,1,0,1,0,1),
                     dat2=c(NA,rep(10,5),NA,rep(20,5)),
                     cov1=c(NA,101:105,NA,111:115),
                     cov2=c(NA,201:205,NA,NA,212:215))
  short <- data.frame(inv1=c("M","F"),inv2=c(300,500))
  time=matrix(c(0:5,2*(0:5)),2,6,byrow=TRUE)
  pd <- panel_data(time=time,vari=long,invar=short,
                   datacols=c("dat1","dat2"))
  expect_equal(pd$datacols,c("dat1","dat2"))
  expect_equal(dim(mat(pd$time)),c(2,6))
  expect_equal(dim(mat(pd$dt)),c(2,5))
  expect_equal(dim(pd$invar),c(2,2))
  expect_equal(nsubj(pd$vari),2L)
  expect_equal(nocc(pd$vari),6L)
  expect_equal(minocc(pd$vari),0L)
  expect_equal(maxocc(pd$vari),5L)

  expect_equal(pd$getVar(1,0),data.frame(inv1="M",inv2=300,dat1=NA_integer_,
                                         dat2=NA_integer_,
                                         cov1=NA_integer_,
                                         cov2=NA_integer_))
  expect_equal(pd$getVar(2,1),data.frame(inv1="F",inv2=500,dat1=1,dat2=20,
                                         cov1=111,cov2=NA_integer_),
               ignore_attr=TRUE)
  expect_equal(pd$getData(1,3),data.frame(dat1=0,dat2=10),
               ignore_attr=TRUE)
  pd$setData(1,3,data.frame(dat1=2,dat2=11))
  expect_equal(pd$getData(1,3),data.frame(dat1=2,dat2=11),
               ignore_attr=TRUE)

})


test_that("long2panel invar0", {
  long <- data.frame(subj=rep(1:2,each=6),
                     occ=rep(0:5,2),
                     time=c(0:5,2*(0:5)),
                     dat1=c(NA,0,0,0,1,1,NA,1,0,1,0,1),
                     dat2=c(NA,rep(10,5),NA,rep(20,5)),
                     cov1=c(NA,101:105,NA,111:115),
                     cov2=c(NA,201:205,NA,NA,212:215),
                     inv1=c("M",rep(NA,5),"F",rep(NA,5)),
                     inv2=c(300,rep(NA,5),500,rep(NA,5)))
  pd <- long2panel(long,datacols=c("dat1","dat2"),
                   invcols=c("inv1","inv2"))
  expect_equal(pd$datacols,c("dat1","dat2"))
  expect_equal(dim(mat(pd$time)),c(2,6))
  expect_equal(dim(mat(pd$dt)),c(2,5))
  expect_equal(dim(pd$invar),c(2,2))
  expect_equal(nsubj(pd$vari),2L)
  expect_equal(nocc(pd$vari),6L)
  expect_equal(minocc(pd$vari),0L)
  expect_equal(maxocc(pd$vari),5L)

  expect_equal(pd$getVar(1,0),data.frame(inv1="M",inv2=300,dat1=NA_integer_,
                                         dat2=NA_integer_,
                                         cov1=NA_integer_,
                                         cov2=NA_integer_))
  expect_equal(pd$getVar(2,1),data.frame(inv1="F",inv2=500,dat1=1,dat2=20,
                                         cov1=111,cov2=NA_integer_),
               ignore_attr=TRUE)
  expect_equal(pd$getData(1,3),data.frame(dat1=0,dat2=10),
               ignore_attr=TRUE)

})

test_that("long2panel invar df", {
  long <- data.frame(subj=rep(1:2,each=6),
                     occ=rep(0:5,2),
                     time=c(0:5,2*(0:5)),
                     dat1=c(NA,0,0,0,1,1,NA,1,0,1,0,1),
                     dat2=c(NA,rep(10,5),NA,rep(20,5)),
                     cov1=c(NA,101:105,NA,111:115),
                     cov2=c(NA,201:205,NA,NA,212:215))
  short <- data.frame(inv1=c("M","F"),inv2=c(300,500))
  pd <- long2panel(long,datacols=c("dat1","dat2"),invar=short)

  expect_equal(pd$datacols,c("dat1","dat2"))
  expect_equal(dim(mat(pd$time)),c(2,6))
  expect_equal(dim(mat(pd$dt)),c(2,5))
  expect_equal(dim(pd$invar),c(2,2))
  expect_equal(nsubj(pd$vari),2L)
  expect_equal(nocc(pd$vari),6L)
  expect_equal(minocc(pd$vari),0L)
  expect_equal(maxocc(pd$vari),5L)

  expect_equal(pd$getVar(1,0),data.frame(inv1="M",inv2=300,dat1=NA_integer_,
                                         dat2=NA_integer_,
                                         cov1=NA_integer_,
                                         cov2=NA_integer_))
  expect_equal(pd$getVar(2,1),data.frame(inv1="F",inv2=500,dat1=1,dat2=20,
                                         cov1=111,cov2=NA_integer_),
               ignore_attr=TRUE)
  expect_equal(pd$getData(1,3),data.frame(dat1=0,dat2=10),
               ignore_attr=TRUE)


})

test_that("long2panel no occ", {
  long <- data.frame(subj=rep(1:2,each=6),
                     time=c(0:5,2*(0:5)),
                     dat1=c(NA,0,0,0,1,1,NA,1,0,1,0,1),
                     dat2=c(NA,rep(10,5),NA,rep(20,5)),
                     cov1=c(NA,101:105,NA,111:115),
                     cov2=c(NA,201:205,NA,NA,212:215))
  short <- data.frame(inv1=c("M","F"),inv2=c(300,500))
  pd <- long2panel(long,datacols=c("dat1","dat2"))

  expect_equal(dim(mat(pd$time)),c(2,6))
  expect_equal(dim(mat(pd$dt)),c(2,5))

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

test_that("as_longform Panel_Frame", {
  dat <- data.frame(subj=rep(1L:2L,each=4L),
                    occ=rep(1L:4L,2L),
                    value1=1:8,value2=101:108)
  pd <- panel_frame(dat,zerostart=FALSE)
  expect_equal(as_longform(pd),dat)

})

test_that("as_longform Panel_Data", {
  long <- data.frame(subj=rep(1:2,each=6),
                     occ=rep(0:5,2),
                     time=c(0:5,2*(0:5)),
                     dat1=c(NA,0,0,0,1,1,NA,1,0,1,0,1),
                     dat2=c(NA,rep(10,5),NA,rep(20,5)),
                     cov1=c(NA,101:105,NA,111:115),
                     cov2=c(NA,201:205,NA,NA,212:215),
                     inv1=c(rep("M",6),rep("F",6)),
                     inv2=c(rep(300,6),rep(500,6)))
  pd <- long2panel(long,datacols=c("dat1","dat2"),
                   invcols=c("inv1","inv2"))
  lf <- as_longform(pd)
  expect_setequal(names(lf),names(long))
  expect_equal(lf,long[names(lf)])
})


