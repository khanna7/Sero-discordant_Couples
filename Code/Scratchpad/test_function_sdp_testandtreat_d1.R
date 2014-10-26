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
      nw = network.extract(nw, at=1042)
      verbose = TRUE

      sdp.testing.coverage = 1
      sdp.art.at.cd4 = 350
      sdp.art.coverage = 1
      time=1041

      hbhtc.all.testing.coverage=0.80
      hbhtc.all.art.at.cd4=1000
      hbhtc.all.art.coverage=0.764

   ## Needed edge attributes
      set.edge.attribute(nw, "primary.sdp", 1)

##############################################################
   ## Test function
      test.net <- testandtreat.sdp(nw=nw,
                                   verbose=verbose,
                                   sdp.testing.coverage=
                                   sdp.testing.coverage,
                                   sdp.art.at.cd4=
                                   sdp.art.at.cd4,
                                   sdp.art.coverage=
                                   sdp.art.coverage,
                                   time=time,
                                   hbhtc.all.testing.coverage=
                                   hbhtc.all.testing.coverage,
                                   hbhtc.all.art.at.cd4=
                                   hbhtc.all.art.at.cd4,
                                   hbhtc.all.art.coverage=
                                   hbhtc.all.art.coverage
                                   )
      nw%v%"art.status"
##############################################################
