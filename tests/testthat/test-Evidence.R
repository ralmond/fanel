test_that("Evidence Constructor", {
##   The constructor is Evidence$new(name, evidenceModels,
##     tasks=matrix(1L,1L,1L), tname="theta", wname="w", dname="Y").
  m1 <- NormalScore$new("m1",0,1)
  m2 <- NormalScore$new("m2",1,1)
  evid <- Evidence$new("Evid",list(m1,m2),
                      panmat(c(1,2,1,2)))

  expect_equal(evid$name,"Evid")
  expect_equal(evid$iname,"task")
  expect_equal(length(evid$models),2)
  expect_equal(evid$qnames,"theta")
  expect_equal(evid$wname,"w")
  expect_equal(evid$dnames,"Y")
  expect_equal(evid$toString(),
               "<Evidence: Evid: 2 >")

})

test_that("Evidence-class {as_longform.Evidence}", {
  m1 <- NormalScore$new("m1",0,1)
  m2 <- NormalScore$new("m2",1,1)
  tasks <- rbind(c(1,1,1,1),c(2,2,2,2),c(1,2,1,2))
  evid <- Evidence$new("Evid",list(m1,m2),
                       panmat(tasks))

  lf <- as_longform(evid)
  expect_equal(lf,data.frame(
    subj=rep(1:3,each=4),
    occ=rep(1:4,3),
    task=as.numeric(t(tasks))))
})


test_that("Evidence-class {drawData.Evidence}", {
  ## {(model,isubj,iocc,theta,
  ##         cov=NULL): Draws random values for data values.
  m1 <- NormalScore$new("m1",-1,.1)
  m2 <- NormalScore$new("m2",1,.1)
  tasks <- rbind(c(1,1,1,NA),c(2,2,2,2),c(1,2,1,2))
  evid <- Evidence$new("Evid",list(m1,m2),
                       panmat(tasks))
  expect_lt(drawData(evid,1,2,0),0)
  expect_gt(drawData(evid,2,3,0),0)
  expect_true(is.na(drawData(evid,1,4,0)))

})


test_that("Evidence-class {evalEvidence.Evidence}", {
  ## {(model,isubj,iocc,theta,Y,covar=NULL):
  ##       Calculates the log-likehood of the data given the latent variable.}
  m1 <- NormalScore$new("m1",-1,.1)
  m2 <- NormalScore$new("m2",1,.1)
  tasks <- rbind(c(1,1,1,NA),c(2,2,2,2),c(1,2,1,2))
  evid <- Evidence$new("Evid",list(m1,m2),
                       panmat(tasks))

  expect_equal(evalEvidence(evid,1,1,0,-.9),
               dnorm(-.9,-1,.1,log=TRUE))
  expect_equal(evalEvidence(evid,2,2,0,.9),
               dnorm(.9,1,.1,log=TRUE))
  expect_equal(evalEvidence(evid,1,4,0,0),0)

})


