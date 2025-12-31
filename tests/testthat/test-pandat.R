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


test_that("Panel_Data-class {Panel_Data-class}", {
})
test_that("Panel_Data-class {Panel_Data}", {
})
test_that("Panel_Data-class {nocc.Panel_Data}", {
})
test_that("Panel_Data-class {minocc.Panel_Data}", {
})
test_that("Panel_Data-class {maxocc.Panel_Data}", {
})
test_that("Panel_Data-class {nsubj.Panel_Data}", {
})
test_that("Panel_Data-class {nsubj<-.Panel_Data}", {
})
test_that("Panel_Data-class {isubj.Panel_Data}", {
})
test_that("Panel_Data-class {isubj<-.Panel_Data}", {
})
test_that("Panel_Data-class {get_subj.Panel_Data}", {
})
test_that("Panel_Data-class {get_subj<-,Panel_Data-method}", {
})
test_that("Panel_Data-class {as_longform,Panel_Data-method}", {
})
test_that("Panel_Data-class {panel_data}", {
})
