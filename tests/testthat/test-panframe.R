### normalForm ----

test_that("normalForm colnames", {
  dat <- data.frame(subj=rep(1L:2L,each=4L),
                    occ=rep(1L:4L,2L),
                    value=1:8)
  nf <- normalForm(dat)
  expect_equal(nf$nsubj,2L)
  expect_equal(nf$minocc,0L)
  expect_equal(nf$maxocc,4L)
  expect_equal(nf$normdat,data.frame(value=c(NA,1:4,NA,5:8)),ignore_attr=TRUE)
  expect_true(nf$bysubj)
  expect_true(nf$byocc)

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

test_that("normalForm bysubj byocc", {
  dat <- data.frame(occ=rep(1L:8L,1L),
                    value=1:8)
  nf <- normalForm(dat)
  expect_equal(nf$nsubj,1L)
  expect_equal(nf$minocc,0L)
  expect_equal(nf$maxocc,8L)
  expect_false(nf$bysubj)
  expect_true(nf$byocc)

  dat <- data.frame(subj=rep(1L:2L,each=1L),
                    value=1:2)
  nf <- normalForm(dat,zerostart=FALSE)
  expect_equal(nf$nsubj,2L)
  expect_equal(nf$minocc,1L)
  expect_equal(nf$maxocc,1L)
  expect_true(nf$bysubj)
  expect_false(nf$byocc)
})

test_that("normalForm quad", {
  dat <- data.frame(quad=1:3,
                    theta=c(-1,0,1))
  nf <- normalForm(dat)
  expect_equal(nf$nsubj,1L)
  expect_equal(nf$minocc,0L)
  expect_equal(nf$maxocc,0L)
  expect_false(nf$bysubj)
  expect_false(nf$byocc)
  expect_equal(nf$nquad,3L)
  expect_equal(nf$normdat,data.frame(theta=c(-1,0,1)),
               ignore_attr=TRUE)
})



### panel_frame ----
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

test_that("get_subj<-.PanelFrame", {
  long <- data.frame(subj=rep(1:3,each=6),
                     occ=rep(0:5,3),
                     dat1=c(NA,rep(1,5),NA,rep(2,5),NA,rep(3,5)),
                     dat2=c(NA,rep(10,5),NA,rep(20,5),NA,rep(30,5)),
                     cov1=c(NA,101:105,NA,111:115,NA,121:125),
                     cov2=c(NA,201:205,NA,NA,212:215,NA,221:225))

  pf <- panel_frame(long,zerostart = TRUE)
  pf1 <- get_subj(pf,1L)
  pf2 <- get_subj(pf,2L)
  pf3 <- get_subj(pf,3L)

  get_subj(pf1,3L) <- pf2
  expect_true(is.na(isubj(pf1)))
  expect_equal(nsubj(pf1),3L)
  expect_equal(pf1[1,]$cov1,pf[1,]$cov1)
  expect_equal(pf1[3,]$cov1,pf[2,]$cov1)

})

test_that("panel_frame nsubj<-",{
  long <- data.frame(subj=rep(1:3,each=6),
                     occ=rep(0:5,3),
                     dat1=c(NA,rep(1,5),NA,rep(2,5),NA,rep(3,5)),
                     dat2=c(NA,rep(10,5),NA,rep(20,5),NA,rep(30,5)),
                     cov1=c(NA,101:105,NA,111:115,NA,121:125),
                     cov2=c(NA,201:205,NA,NA,212:215,NA,221:225))

  pf0 <- panel_frame(long,zerostart = TRUE)
  pf2 <- pf0
  nsubj(pf2) <- 2L
  expect_equal(nsubj(pf2),2L)
  expect_equal(pf2[,],pf0[1:2,])

  pf4 <- pf0
  nsubj(pf4) <- 4L
  expect_equal(nsubj(pf4),4L)
  expect_equal(pf4[1:3,],pf0[,])
  expect_true(all(is.na(pf4[4,])))

})

test_that("panel_frame nocc<-",{
  long <- data.frame(subj=rep(1:3,each=6),
                     occ=rep(0:5,3),
                     dat1=c(NA,rep(1,5),NA,rep(2,5),NA,rep(3,5)),
                     dat2=c(NA,rep(10,5),NA,rep(20,5),NA,rep(30,5)),
                     cov1=c(NA,101:105,NA,111:115,NA,121:125),
                     cov2=c(NA,201:205,NA,NA,212:215,NA,221:225))

  pf0 <- panel_frame(long,zerostart = TRUE)
  pf4 <- pf0
  maxocc(pf4) <- 4L
  expect_equal(nocc(pf4),5L)
  expect_equal(pf4[,],pf0[,0L:4L],ignore_attr=TRUE)

  pf6 <- pf0
  maxocc(pf6) <- 6L
  expect_equal(nocc(pf6),7L)
  expect_equal(pf6[,0L:5L],pf0[,],ignore_attr=TRUE)
  expect_true(all(is.na(pf6[,6L])))

})

### Quad_Frame ----

test_that("Quad_Frame ",{
  df <- data.frame(quad=1:3,theta=c(-1,0,1))
  qf <- quad_frame(df)
  expect_equal(nquad(qf),3L)
  expect_false(bysubj(qf))
  expect_false(byocc(qf))
  expect_equal(qf[1,1,],df["theta"],ignore_attr=TRUE)
  expect_equal(qf[2,2,2]$theta,0)
  qf[1,1,c(1,3)] <- c(-.96,.96)
  expect_equal(qf[1,1,1]$theta,-.96)
  expect_equal(qf[2,2,2]$theta,0)
  expect_equal(qf[3,3,3]$theta,.96)
})

test_that("Quad_Frame bysubj",{
  df <- data.frame(subj=rep(1:2,each=3),quad=rep(1:3,2),
                   theta=c(-1,0,1, -.5,.5,1.5))
  qf <- quad_frame(df)
  expect_equal(nquad(qf),3L)
  expect_equal(nsubj(qf),2L)
  expect_true(bysubj(qf))
  expect_false(byocc(qf))
  expect_equal(qf[1,2,2]$theta,0)
  expect_equal(qf[2,2,2]$theta,0.5)
  qf[2,2,]$theta <-c(-1.5,-.5,.5)
  expect_equal(qf[1,2,2]$theta,0)
  expect_equal(qf[2,2,2]$theta,-0.5)

})

test_that("Quad_Frame byocc",{
  df <- data.frame(occ=rep(1:3,each=2),quad=rep(1:2,3),
                   theta=rep(c(-1,1),3)+ rep(c(-.5,0,.5),each=2))
  qf <- quad_frame(df)
  expect_equal(nquad(qf),2L)
  expect_equal(nsubj(qf),1L)
  expect_equal(nocc(qf),4L)
  expect_equal(minocc(qf),0L)
  expect_equal(maxocc(qf),3L)
  expect_false(bysubj(qf))
  expect_true(byocc(qf))
  expect_true(is.na(qf[1,1,0]$theta))
  expect_equal(qf[1,2,2]$theta,1)
  expect_equal(qf[2,3,2]$theta,1.5)
  qf[2,2,]$theta <-c(-.75,.75)
  expect_equal(qf[1,2,2]$theta,.75)
  expect_equal(qf[1,1,2]$theta,0.5)
})

test_that("Quad_Frame nquad",{
  df1 <- data.frame(subj=rep(1:2,each=3),quad=rep(1:3,2),
                   theta=c(-1,0,1, -.5,.5,1.5))
  qf1 <- quad_frame(df1)
  expect_equal(nquad(qf1),3L)
  nquad(qf1) <- 2L
  expect_equal(nquad(qf1),2L)
  expect_equal(qf1[2,1,]$theta,c(-0.5,.5))
  expect_equal(qf1[1,1,]$theta,c(-1,0))
  expect_length(qf1[,,]$theta,4)

  nquad(qf1) <- 3L
  expect_equal(nquad(qf1),3L)
  expect_equal(qf1[2,1,1]$theta,-0.5)
  expect_length(qf1[,,]$theta,6)
  expect_true(is.na(qf1[2,1,3]$theta))




  df2 <- data.frame(occ=rep(1:3,each=2),quad=rep(1:2,3),
                   theta=rep(c(-1,1),3)+ rep(c(-.5,0,.5),each=2))
  qf2 <- quad_frame(df2)
  expect_equal(nquad(qf2),2L)

  nquad(qf2) <- 3L
  expect_equal(nquad(qf2),3L)
  expect_equal(qf2[2,1,1]$theta,-1.5)
  expect_length(qf2[,,]$theta,12)
  expect_true(is.na(qf2[2,1,3]$theta))

})

test_that("Quad_Frame get_subj",{
  df1 <- data.frame(subj=rep(1:2,each=15),
                    occ=rep(rep(1:3,2),each=5),
                    quad=rep(1:5,6),
                    theta=1:30)
  qf1 <- quad_frame(df1)
  qf1.1 <- get_subj(qf1,1L)
  expect_equal(isubj(qf1.1),1L)
  expect_equal(qf1.1[1,,],qf1[1,,],ignore_attr=TRUE)

  qf1.2 <- get_subj(qf1,2L)
  expect_equal(isubj(qf1.2),2L)
  expect_equal(qf1.2[1,,],qf1[2,,],ignore_attr=TRUE)

  get_subj(qf1.1,2) <- qf1.2
  expect_equal(nsubj(qf1.1),2L)
  expect_true(is.na(isubj(qf1.1)))
  expect_equal(qf1.1[1:2,,],qf1[1:2,,],ignore_attr=TRUE)

  df2 <- data.frame(subj=rep(1:2,each=5),
                    quad=rep(1:5,2),
                    theta=1:30)
  qf2 <- quad_frame(df2)
  qf2.1 <- get_subj(qf2,1L)
  expect_equal(isubj(qf2.1),1L)
  expect_equal(qf2.1[1,,],qf2[1,,],ignore_attr=TRUE)

  qf2.2 <- get_subj(qf2,2L)
  expect_equal(isubj(qf2.2),2L)
  expect_equal(qf2.2[1,,],qf2[2,,],ignore_attr=TRUE)

  get_subj(qf2.1,2) <- qf2.2
  expect_equal(nsubj(qf2.1),2L)
  expect_true(is.na(isubj(qf2.1)))
  expect_equal(qf2.1[1:2,,],qf2[1:2,,],ignore_attr=TRUE)

  df3 <- data.frame(quad=1:5,theta=1:5)
  qf3 <- quad_frame(df3)
  qf3.1 <- get_subj(qf3,1L)
  expect_equal(isubj(qf3.1),1L)
  expect_equal(qf3.1[1,,],qf3[1,,],ignore_attr=TRUE)

  qf3.2 <- get_subj(qf3,2L)
  expect_equal(isubj(qf3.2),2L)
  expect_equal(qf3.2[1,,],qf3[2,,],ignore_attr=TRUE)

  get_subj(qf3.1,2) <- qf3.2
  expect_equal(nsubj(qf3.1),2L)
  expect_true(is.na(isubj(qf3.1)))
  expect_equal(qf3.1[1:2,,],qf3[1:2,,],ignore_attr=TRUE)


})

