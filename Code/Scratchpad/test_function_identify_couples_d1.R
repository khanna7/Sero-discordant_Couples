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
      source("../Engine/function_sdp_identify_d1.R")
##############################################################

##############################################################

   ## Test
      ## Simulate
         ##set.seed(7)
         ## undebug(identify.sdp)
         test.net <- identify.sdp(nw=nw,
                                  verbose=TRUE,
                                  ##sdp.coverage=1,
                                  time=1042
                                  )

      ## Results
         length(which(test.net%e%"primary.sdp" == 1))
         network.size(test.net)
##############################################################



