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
                   dnames=c("dat1","dat2"))
  expect_equal(pd$dnames,c("dat1","dat2"))
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

test_that("as_longform Panel_Frame", {
  dat <- data.frame(subj=rep(1L:2L,each=4L),
                    occ=rep(1L:4L,2L),
                    value1=1:8,value2=101:108)
  pd <- panel_frame(dat,zerostart=FALSE)
  expect_equal(as_longform(pd),dat)

})

test_that("Panel_Frame-class {Panel_Frame-class}", {
})
test_that("Panel_Frame-class {Panel_Frame}", {
})
test_that("{[,Panel_Frame-method}",{
})
test_that("[<-,Panel_Frame-method}",{
})
test_that(" {nocc,Panel_Frame-method}", {
})
test_that(" {minocc,Panel_Frame-method}", {
})
test_that(" {maxocc,Panel_Frame-method}", {
})
test_that(" {nsubj,Panel_Frame-method}", {
})
test_that(" {isubj,Panel_Frame-method}", {
})
test_that(" {isubj<-,Panel_Frame-method}", {
})
test_that(" {names,Panel_Frame-method}", {
})
test_that(" {get_subj,Panel_Frame-method}", {
})
test_that(" {get_subj<-,Panel_Frame-method}", {
})
test_that(" {as_longform,Panel_Frame-method}", {
})
test_that(" {panel_frame}", {
})


