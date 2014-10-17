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
      source("function_sdp_testandtreat_d1.R")
##############################################################

##############################################################
   ## Needed parameters
      ##nw = network.extract(nw, retain.all.vertices=TRUE)
      verbose = TRUE
      sdp.testing.coverage = 1
      sdp.art.at.cd4 = 350
      sdp.art.coverage = 1
      time=1041

      test.net <- testandtreat.sdp(nw=nw,
                                   verbose=verbose,
                                   sdp.testing.coverage=
                                   sdp.testing.coverage,
                                   sdp.art.at.cd4=
                                   sdp.art.at.cd4,
                                   time=time
                                   )

##############################################################
