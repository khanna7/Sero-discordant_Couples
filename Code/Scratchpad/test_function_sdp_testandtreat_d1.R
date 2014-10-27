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

      hbhtc.testing.coverage=1
      known.sdp.art.coverage=0.5
      notknown.sdp.art.coverage=0.5
      known.sdp.art.at.cd4=1000
      notknown.sdp.art.at.cd4=1000

   ## Needed edge attributes
      set.edge.attribute(nw, "primary.sdp", 1)

##############################################################
   ## Test function
      test.net <- testandtreat.sdp(nw=nw,
                                   verbose=verbose,
                                   hbhtc.testing.coverage=
                                   hbhtc.testing.coverage,
                                   known.sdp.art.coverage=
                                   known.sdp.art.coverage,
                                   notknown.sdp.art.coverage=
                                   notknown.sdp.art.coverage,
                                   known.sdp.art.at.cd4=
                                   known.sdp.art.at.cd4,
                                   notknown.sdp.art.at.cd4=
                                   notknown.sdp.art.at.cd4,
                                   time=time,
                                   )
      nw%v%"art.status"
##############################################################
