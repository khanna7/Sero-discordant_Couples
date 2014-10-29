##############################################################
### Test Wrapper Function for "transmission_d10_sdp.R"
##############################################################

##############################################################
### Progression of versions
##############################################################

### 26 Oct 2014: Add testing of all on a/c of HBHTC

### 17 Oct 2014: Test wrapper function
##############################################################

##############################################################
      rm(list=ls())
   ## Load libraries
      library(statnet)

##############################################################

##############################################################
   ## Read in burnin data
      ##load("../../Burnin_Data/burnin.21Feb_UG_phi004_newatts_run1.RData")
      load("../Uganda_Runs/nw28Oct_UG_sdp_trial_run1.RData")

   ## Source function
      source("../Engine/function_sdp_testandtreat_d1.R")
##############################################################

##############################################################

   ## edge attributes
      
   ## Needed parameters
      ##nw = network.extract(nw, at=1042)
      verbose = TRUE

      hbhtc.testing.coverage=1
      known.sdp.art.coverage=0.5
      notknown.sdp.art.coverage=0.5
      known.sdp.art.at.cd4=1000
      notknown.sdp.art.at.cd4=100

   ## Needed edge attributes
      ##set.edge.attribute(nw, "primary.sdp", 1)

##############################################################
   ## Test function
      ##debug(testandtreat.sdp)
      test.net <- testandtreat.sdp(nw=nw,
                                   verbose=verbose,
                                   hbhtc.testing.coverage=
                                   hbhtc.testing.coverage,
                                   known.sdp.art.coverage=
                                   known.sdp.art.coverage,
                                   known.sdp.art.at.cd4=
                                   known.sdp.art.at.cd4,
                                   time=time,
                                   )

##############################################################
##  For testing
    nw
    test.net
    table(test.net%e%"longest.ptshp", exclude=NULL)
    table(test.net%e%"known.longest.sdp", exclude=NULL)
    table(test.net%e%"known.sdp", exclude=NULL)
