
test_that(" {Population}", {
  apop <- Population$new("BigStudy",
                         list(NormalPop$new("Reference",0,1),
                              NormalPop$new("Focal",-.5,1.2)),
                         groups=c(rep(1,7),rep(2,3)),
                         qname="Ability",
                         wname="w")
  expect_equal(nsubj(apop),10)
  expect_equal(nmodels(apop),2)

  expect_equal(apop$name,"BigStudy")
  expect_equal(apop$toString(),"<Population: BigStudy: 2 >")

  expect_length(apop$models,2)
  expect_true(is(apop$models[[2]],"NormalPop"))

  expect_equal(apop$group(3),1,ignore_attr=TRUE)
  expect_equal(apop$group(9),2,ignore_attr=TRUE)

  expect_equal(apop$iname,"group")
  expect_equal(apop$qnames,"Ability")
  expect_equal(apop$wname,"w")



})

test_that(" {as_longform,Population-method}", {
  apop <- Population$new("BigStudy",
                         list(NormalPop$new("Reference",0,1),
                              NormalPop$new("Focal",-.5,1.2)),
                         groups=c(rep(1,7),rep(2,3)),
                         qname="Ability",
                         wname="w")
  expect_equal(as_longform(apop),
               data.frame(subj=1:10,
                          group=c(rep(1,7),rep(2,3))))

})



test_that("{Population nsubj,nsubj<-}", {
  ## {Gets/sets the number of subjects.}
  apop <- Population$new("BigStudy",
                         list(NormalPop$new("Reference",0,1)),
                         groups=1,
                         qname="Ability",
                         wname="w")

  expect_equal(nsubj(apop),1)
  nsubj(apop) <- 100
  expect_equal(nsubj(apop$index),100)
})

test_that("{Population drawInitial,$drawInit}", {
  apop <- Population$new("BigStudy",
                         list(NormalPop$new("Reference",0,1),
                              NormalPop$new("Focal",-.5,1.2)),
                         groups=c(rep(1,7),rep(2,3)),
                         qname="Ability",
                         wname="w")
  expect_true(abs(mean(drawInitial(apop,1,5)))<5*qnorm(.999))
  expect_true(abs(mean(drawInitial(apop,9,5))+.5)<1.2*5*qnorm(.999))
  expect_length(probInit(apop,1,c(-1,0,1)),3)
  expect_equal(sum(probInit(apop,1,c(-1,0,1))),1.0)
  expect_length(probInit(apop,9,c(-1,0,1)),3)
  expect_equal(sum(probInit(apop,9,c(-1,0,1))),1.0)


})


test_that("{Population$prepData}", {
  ## {Prepares data for the mstep calculations,
  ##       filters to only include occasion 0.}
  apop <- Population$new("BigStudy",
                         list(NormalPop$new("Reference",0,1),
                              NormalPop$new("Focal",-.5,1.2)),
                         groups=c(1,2,1),
                         qname="Ability",
                         wname="w")
  testdat <- read.csv(test_path("testdata.csv"))

  filtdat <- apop$prepData(testdat)
  expect_equal(nrow(filtdat),15)
  expect_true(all(filtdat$occ==0))

})

test_that("{Population$split_m}", {
  ## {Assigns data to models and builds a list of
  ##       (model, data) pairs.}
  pops <- list(NormalPop$new("Reference",0,1),
               NormalPop$new("Focal",-.5,1.2))
  apop <- Population$new("BigStudy",pops,
                         groups=c(1,2,1),
                         qname="Theta",
                         wname="w")
  testdat <- read.csv(test_path("testdata.csv"))
  splitdat <- apop$split_m(testdat)
  expect_length(splitdat,2)
  expect_equal(splitdat[[1]][[1]],pops[[1]])
  expect_equal(splitdat[[2]][[1]],pops[[2]])
  expect_equal(nrow(splitdat[[1]][[2]]),10)
  expect_equal(nrow(splitdat[[2]][[2]]),5)

})

test_that("{Population$mstep}", {
  ## {signature(data = "Panel_Data",...,
  ## 	its = "integer", control = "list", workers = "Workers"):
  ##       This is the implementation of the m-step.  Generally only the
  ##       $prepData method needs to be overridden.}
  pops <- list(NormalPop$new("Reference",0,1),
               NormalPop$new("Focal",-.5,1.2))
  apop <- Population$new("BigStudy",pops,
                         groups=c(1,2,1),
                         qname="Theta",
                         wname="w")
  testdat <- read.csv(test_path("testdata.csv"))
  apop <- apop$mstep(testdat)
  expect_true(apop$models[[1]]$convergence)
  expect_true(apop$models[[2]]$convergence)
  expect_false(is.na(apop$lp))

})
