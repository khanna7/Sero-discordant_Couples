##############################################################
### Test Wrapper Function for "transmission_d10_sdp.R"
##############################################################

##############################################################
### Progression of versions
##############################################################
### 7 Oct 2014: Test wrapper function
##############################################################

##############################################################

   ## Load libraries
      library(tergm)
      library(ergm) 
      library(network) 
      library(networkDynamic) 
      
##############################################################

##############################################################
   ## Read in burnin data
      load("../../Burnin_Data/burnin.21Feb_UG_phi004_newatts_run1.RData")

   ## Source function
      source("transmission_d10_sdp.R")
      ##source("transmission_d10.R")
##############################################################

##############################################################
   ## Needed information
      ## parameters
         nw <- network.extract(nw, at=1042)
         verbose <- TRUE
         preg.susc.mult <- 1
         circum.mult <- 1
         scenario <- "baseline" 
         baseline.art.coverage.rate <- 1
         baseline.preg.coverage.rate <- 1
         given.dur.inf.by.age <- 100 
         eligible.cd4 <- 1000 
         baseline.f.ges.visit <- 1 
         decline.ui <- 1

      ## edge attributes
         set.edge.attribute(nw, "known.sdp", 1)
##############################################################

##############################################################

   ## Test
      ## Simulate
         set.seed(1234)
         test.net <- transmission(nw=nw,
                                  verbose=verbose,
                                  preg.susc.mult=preg.susc.mult,
                                  circum.mult=circum.mult, 
                                  scenario=scenario,
                                  baseline.art.coverage.rate=
                                  baseline.art.coverage.rate,
                                  baseline.preg.coverage.rate=
                                  baseline.preg.coverage.rate,
                                  given.dur.inf.by.age=
                                  given.dur.inf.by.age,
                                  eligible.cd4=
                                  eligible.cd4,
                                  baseline.f.ges.visit=
                                  baseline.f.ges.visit,
                                  decline.ui=
                                  decline.ui
                                  )

##############################################################
