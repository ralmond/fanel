### Panel_Data

test_that("Panel Data complete", {

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
  pd <- long2panel(long,dnames=c("dat1","dat2"),
                   invcols=c("inv1","inv2"))
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
  pd <- long2panel(long,dnames=c("dat1","dat2"),invar=short)

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


})

test_that("long2panel no occ", {
  long <- data.frame(subj=rep(1:2,each=6),
                     time=c(0:5,2*(0:5)),
                     dat1=c(NA,0,0,0,1,1,NA,1,0,1,0,1),
                     dat2=c(NA,rep(10,5),NA,rep(20,5)),
                     cov1=c(NA,101:105,NA,111:115),
                     cov2=c(NA,201:205,NA,NA,212:215))
  short <- data.frame(inv1=c("M","F"),inv2=c(300,500))
  pd <- long2panel(long,dnames=c("dat1","dat2"))

  expect_equal(dim(mat(pd$time)),c(2,6))
  expect_equal(dim(mat(pd$dt)),c(2,5))

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
  pd <- long2panel(long,dnames=c("dat1","dat2"),
                   invcols=c("inv1","inv2"))
  lf <- as_longform(pd)
  expect_setequal(names(lf),names(long))
  expect_equal(lf,long[names(lf)])
})

test_that(" {dname.Panel_Data-method}", {
  long <- data.frame(subj=rep(1:2,each=6),
                     occ=rep(0:5,2),
                     dat1=c(NA,0,0,0,1,1,NA,1,0,1,0,1),
                     dat2=c(NA,rep(10,5),NA,rep(20,5)),
                     cov1=c(NA,101:105,NA,111:115),
                     cov2=c(NA,201:205,NA,NA,212:215))
  short <- data.frame(inv1=c("M","F"),inv2=c(300,500))
  time=matrix(c(0:5,2*(0:5)),2,6,byrow=TRUE)
  pd <- panel_data(time=time,vari=long,invar=short,
                   dnames=character())
  expect_equal(dname(pd),character())
  expect_length(pd$getData(1,1),0)

  dname(pd) <- c("dat1","dat2")
  expect_equal(dname(pd),c("dat1","dat2"))
  expect_length(pd$getData(1,1),2)

})

test_that("Panel_Data-class {isubj.Panel_Data}", {
  long <- data.frame(subj=rep(1:3,each=6),
                     occ=rep(0:5,3),
                     dat1=c(NA,rep(1,5),NA,rep(2,5),NA,rep(3,5)),
                     dat2=c(NA,rep(10,5),NA,rep(20,5),NA,rep(30,5)),
                     cov1=c(NA,101:105,NA,111:115,NA,121:125),
                     cov2=c(NA,201:205,NA,NA,212:215,NA,221:225))
  short <- data.frame(inv1=c("M","F","O"),inv2=c(100,200,300))
  time<-matrix(c(0:5,2*(0:5),3*(0:5)),3,6,byrow=TRUE)
  pd <- panel_data(time=time,vari=long,invar=short,
                   dnames=c("dat1","dat2"))
  expect_true(is.na(isubj(pd)))
  pd1 <- get_subj(pd,1L)
  expect_equal(isubj(pd1),1L)
  pd2 <- get_subj(pd,2L)
  expect_equal(isubj(pd2),2L)
  expect_equal(pd2$time[1,],pd$time[2,])
  expect_equal(pd2$dt[1,],pd$dt[2,])
  expect_equal(pd2$getVar(1,),pd$getVar(2,))
  expect_equal(pd2$getData(1,),pd$getData(2,))
  pd3 <- get_subj(pd,3L)
  expect_equal(isubj(pd3),3L)
  isubj(pd3) <- 7L
  expect_equal(isubj(pd3),7L)

  get_subj(pd1,2) <- pd3
  expect_true(is.na(isubj(pd1)))
  expect_equal(nsubj(pd1),2L)
  expect_equal(pd1$dt[1,],pd$dt[1,])
  expect_equal(pd1$getVar(1,),pd$getVar(1,))
  expect_equal(pd1$getData(1,),pd$getData(1,))
  expect_equal(pd1$dt[2,],pd$dt[3,])
  expect_equal(pd1$getVar(2,),pd$getVar(3,))
  expect_equal(pd1$getData(2,),pd$getData(3,))

})

test_that("split_subj",{
})
test_that("add_subj",{
})
test_that("bind_subj",{
})

