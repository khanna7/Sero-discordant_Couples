##############################################################
### Test Wrapper Function to "Identify Sero-Discordant Couples
### and Update ART Status of Infected Partners"
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
      ## ug.nw <- network.extract(nw, at=1041)

   ## Source function
      source("function_identify_couples_d1.R")
##############################################################

##############################################################

   ## Test
      ## Simulate
         set.seed(7)
         test.net <- identify.sdp.update.art(nw=nw,
                                             verbose=TRUE,
                                             sdp.coverage=1,
                                             time=1042
                                             )

      ## Compare results
         (nw%v%"art.status")[24148] # before sdp intervention
         (test.net%v%"art.status")[24148] # after sdp intervenion
##############################################################



