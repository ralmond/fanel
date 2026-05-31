## Activities ----
test_that("Activities-class {Activities}",{
  ## (name, growthModels=list(),
  ##     actions=1L, dt=1.0, dosage=NULL, tname="theta",
  ##     wname="w", dtname="deltaT", dosname="dose")}
  agr1 <- BrownianGrowth$new("B1",.1,.04)
  agr2 <- BrownianGrowth$new("B2",.2,.04)
  acts <- Activities$new("acts",list(agr1,agr2),
                         actions=c(1,1,2,2))
  expect_equal(nsubj(acts),1)
  expect_equal(nocc(acts),4)
  expect_equal(maxocc(acts),4)
  expect_equal(minocc(acts),1)
  expect_equal(acts$name,"acts")
  expect_equal(length(acts$models),2)
  expect_equal(acts$index,as.Panmat(c(1,1,2,2)))
  expect_equal(acts$iname,"action")
  expect_equal(acts$qnames,"theta")
  expect_equal(acts$wname,"w")
  expect_equal(acts$toString(),
              "<Activities: acts: 1 x 4 >")

})

test_that("Activities-class {{$action(isubj,iocc)}:}",{
  agr1 <- BrownianGrowth$new("B1",.1,.04)
  agr2 <- BrownianGrowth$new("B2",.2,.04)
  acts <- Activities$new("acts",list(agr1,agr2),
                         actions=c(1,1,2,2))
  expect_equal(acts$action(1,1),1,ignore_attr=TRUE)
  expect_equal(acts$action(2,3),2,ignore_attr=TRUE)
  acts$action(1,2,2)
  expect_equal(acts$index,as.Panmat(c(1,2,2,2)))
})

test_that("Activities-class {{$dt}:}",{
  agr1 <- BrownianGrowth$new("B1",.1,.04)
  agr2 <- BrownianGrowth$new("B2",.2,.04)
  acts <- Activities$new("acts",list(agr1,agr2),
                         actions=c(1,1,2,2),
                         dt=c(1,2,1,2))

  ##$dt
  expect_equal(acts$dt,as.Panmat(c(1,2,1,2)))

  ##$deltaT
  expect_equal(acts$deltaT(1,2),2,ignore_attr=TRUE)
  expect_equal(acts$deltaT(2,3),1,ignore_attr=TRUE)
  acts$deltaT(1,2,1)
  expect_equal(acts$dt,as.Panmat(c(1,1,1,2)))
  ##$dtname
  expect_equal(acts$dtname,"deltaT")
  acts$dtname <- "time"
  expect_equal(acts$dtname,"time")
  expect_equal(acts$models[[2]]$dtname,"time")

  })
test_that("Activities-class {{$dosage}:}",{
  agr1 <- BrownianGrowth$new("B1",.1,.04)
  agr2 <- BrownianGrowth$new("B2",.2,.04)
  acts <- Activities$new("acts",list(agr1,agr2),
                         actions=c(1,1,2,2))
  agr1 <- BrownianGrowth$new("B1",.1,.04)
  agr2 <- BrownianGrowth$new("B2",.2,.04)
  acts <- Activities$new("acts",list(agr1,agr2),
                         actions=c(1,1,2,2),
                         dosage=c(1,2,1,2))

  ##$dosage
  expect_equal(acts$dosage,as.Panmat(c(1,2,1,2)))

  ##$dose
  expect_equal(acts$dose(1,2),2,ignore_attr=TRUE)
  expect_equal(acts$dose(2,3),1,ignore_attr=TRUE)
  acts$dose(1,2,1)
  expect_equal(acts$dosage,as.Panmat(c(1,1,1,2)))
  ##$dosname
  expect_equal(acts$dosname,"dose")
  acts$dosname <- "effort"
  expect_equal(acts$dosname,"effort")
  expect_equal(acts$models[[2]]$dosname,"effort")

  acts$dosage <- NULL
  expect_error(acts$dose(1,4,1))

})

test_that("Activities-class {{maxocc},{maxocc<-}}",{
  agr1 <- BrownianGrowth$new("B1",.1,.04)
  agr2 <- BrownianGrowth$new("B2",.2,.04)
  acts <- Activities$new("acts",list(agr1,agr2),
                         actions=1,dt=1)
  nsubj(acts) <- 2
  expect_equal(nsubj(acts$index),2)
  expect_equal(nsubj(acts$dt),2)

  nocc(acts) <- 5
  expect_equal(nocc(acts$index),5)
  expect_equal(nocc(acts$dt),5)

  maxocc(acts) <- 4
  expect_equal(maxocc(acts$index),4)
  expect_equal(maxocc(acts$dt),4)

  minocc(acts) <- 2
  expect_equal(minocc(acts$index),2)
  expect_equal(minocc(acts$dt),2)

  ## Again with dosage
  acts <- Activities$new("acts",list(agr1,agr2),
                         actions=1,dt=1,dosage=1)
  nsubj(acts) <- 2
  expect_equal(nsubj(acts$index),2)
  expect_equal(nsubj(acts$dt),2)
  expect_equal(nsubj(acts$dosage),2)

  nocc(acts) <- 5
  expect_equal(nocc(acts$index),5)
  expect_equal(nocc(acts$dt),5)
  expect_equal(nocc(acts$dosage),5)

  maxocc(acts) <- 4
  expect_equal(maxocc(acts$index),4)
  expect_equal(maxocc(acts$dt),4)
  expect_equal(maxocc(acts$dosage),4)

  minocc(acts) <- 2
  expect_equal(minocc(acts$index),2)
  expect_equal(minocc(acts$dt),2)
  expect_equal(minocc(acts$dosage),2)


})

test_that("Activities-class {{drawGrowth},{$drawNext}}",{
  agr1 <- BrownianGrowth$new("B1",.1,.04)
  agr2 <- BrownianGrowth$new("B2",.2,.04)
  acts <- Activities$new("acts",list(agr1,agr2),
                         actions=c(1,1,2,2))
  t0 <- rep(0,100)
  expect_lt(abs(mean(c(drawGrowth(acts,1,1,t0)))-.1),.012)
  expect_lt(abs(mean(c(drawGrowth(acts,1,3,t0)))-.2),.012)

})

test_that("Activities-class {{as_longform}}",{
  agr1 <- BrownianGrowth$new("B1",.1,.04)
  agr2 <- BrownianGrowth$new("B2",.2,.04)
  acts <- Activities$new("acts",list(agr1,agr2),
                         actions=matrix(rep(c(1,2),4),2,4),
                         dt=1:4)

  expect_equal(as_longform(acts),
               data.frame(subj=rep(1:2,each=4),
                          occ=rep(1:4,2),
                          action=rep(1:2,each=4),
                          deltaT=rep(1:4,2)))

  acts <- Activities$new("acts",list(agr1,agr2),
                         actions=matrix(rep(c(1,2),4),2,4),
                         dt=1:4,dosage=c(1,2,1,2))

  expect_equal(as_longform(acts),
               data.frame(subj=rep(1:2,each=4),
                          occ=rep(1:4,2),
                          action=rep(1:2,each=4),
                          deltaT=rep(1:4,2),
                          dose=rep(1:2,4)))
})


test_that("Activities-class {{$prepData}}",{
  agr1 <- BrownianGrowth$new("B1",.1,.04)
  agr2 <- BrownianGrowth$new("B2",.2,.04)
  acts <- Activities$new("acts",list(agr1,agr2),
                         actions=c(1,1,2,2),
                         qname="theta")
  testdat <- read.csv(test_path("testdata.csv"))
  testdat$theta <- testdat$Theta + testdat$occ*.1
  prepdat <- acts$prepData(testdat)
  tdt1 <- testdat[testdat$subj==1,"theta"] |>
    dplyr::lead(5) |> na.omit()
  pdt11 <- dplyr::filter(prepdat,subj==1) |>
    dplyr::pull(theta_1)
  expect_equal(as.numeric(pdt11),tdt1,
               ignore_attr=TRUE)

  tdt2 <- testdat[testdat$subj==2,"theta"] |>
    dplyr::lead(5) |> na.omit()
  pdt12 <- as.data.frame(prepdat)[prepdat$subj==2,"theta_1"]
  expect_equal(as.numeric(pdt12),tdt2,
               ignore_attr=TRUE)


})

test_that("Activities-class {{$split_m}}",{
  agr1 <- BrownianGrowth$new("B1",.1,.04)
  agr2 <- BrownianGrowth$new("B2",.2,.04)
  agr3 <- BrownianGrowth$new("B3",0,.04)
  acts <- Activities$new("acts",list(agr1,agr2,agr3),
                         actions=matrix(c(1,2,3,NA,1:3,NA,2,2,2,NA),3,4,byrow=TRUE),
                         qname="Theta")
  testdat <- read.csv(test_path("testdata.csv"))
  split <- acts$split_m(testdat)
  expect_equal(split[[1]][[1]],agr1)
  expect_equal(nrow(split[[1]][[2]]),10)
  expect_equal(split[[2]][[1]],agr2)
  expect_equal(nrow(split[[2]][[2]]),25)
})

test_that("Activities-class {{mstep}}",{
  agr1 <- BrownianGrowth$new("B1",.1,.04)
  agr2 <- BrownianGrowth$new("B2",.2,.04)
  agr3 <- BrownianGrowth$new("B3",0,.04)
  acts <- Activities$new("acts",list(agr1,agr2,agr3),dt=1,
                         actions=matrix(c(1,2,3,NA,1:3,NA,2,2,2,NA),3,4,byrow=TRUE),
                         qname="Theta")
  testdat <- read.csv(test_path("testdata.csv"))
  testdat$deltaT <- 1
  acts <- mstep(acts,testdat)
  expect_true(acts$models[[1]]$convergence)
  expect_true(acts$models[[2]]$convergence)
  expect_true(acts$models[[3]]$convergence)

})


### ActivitiesD ----
test_that("Activities-class {ActivitiesD}",{
})
test_that("ActivitiesD {{advanceWeights},{$advance}}",{
})
test_that("ActivitiesD {{retreatWeights},{$retreat}}",{
})
test_that("ActivitiesD {{$tmat}}",{
})
test_that("ActivitiesD {{$fillCache}}",{
})
test_that("ActivitiesD {{$nmoments}}",{
})

