##############################################
### ART metrics
##############################################

  ## At the last time step

##############################################

##############################################
  rm(list=ls())
  library(statnet)

  ## Needed data
     n.sim <- 5 #num of simulations
     last.time.pt <- 1300
       ## baseline.data are labeled from 1040 to 1300
       ## others are from 780 to 1040, extracting at 1300
       ## will give network at last time step for all cases

##############################################
  ### Write function to compute
  ### %vertical transmissions
##############################################

  compute.art.metrics <- function(sdp_scn,
                                  n.sim=n.sim,
                                  date,
                                  last.time.pt,
                                  ...
                                  ){

      out.mat <- matrix(NA,
                        ncol=5,
                        nrow=n.sim)

     ## Record proportion of vertical transmissions
     for (i in 1:n.sim){
       ##browser()

       if (sdp_scn != "baseline_cp"){
         load(paste("../SouthAfrica_Runs_Do_Not_Commit/nw",date,"_ZA_",sdp_scn,"_run",
                    i,".RData",sep=""))
       } else {
          load(paste("../SouthAfrica_Runs_Do_Not_Commit/nw",date,"_UG_",sdp_scn,"_run",
                    i,".RData",sep=""))
        }
       
       net <- network.collapse(nw, at=last.time.pt)
       
       infected <- which(net%v%"inf.status" == 1)
       on.art <- which(net%v%"art.status" == 1)
       time.of.art.initiation <- net%v%"time.of.art.initiation"

       out.mat[i, 1] <- length(on.art)/length(infected)
       out.mat[i, 2] <- mean(time.of.art.initiation, na.rm=TRUE)
       out.mat[i, 3] <- quantile(time.of.art.initiation, na.rm=TRUE, 0.25)
       out.mat[i, 4] <- median(time.of.art.initiation, na.rm=TRUE)
       out.mat[i, 5] <- quantile(time.of.art.initiation, na.rm=TRUE, 0.75)
        
     }
      return(out.mat)
    }

##############################################

##############################################
  ### Apply above function to different
  ### scenarios
##############################################

  sdp.curr.art.metrics <- compute.art.metrics("sdp_curr",
                                              n.sim=n.sim,
                                              date="6Apr", 
                                              last.time.pt=last.time.pt
                                              )

  sdp.high.art.metrics <- compute.art.metrics("sdp_high",
                                              n.sim=n.sim,
                                              date="5Apr", 
                                              last.time.pt=last.time.pt
                                              )

  baseline.art.metrics <- compute.art.metrics("baseline_cp",
                                              n.sim=n.sim,
                                              date="27May", #2014,
                                              last.time.pt=last.time.pt
                                              )

  sdp.scenarioIV.art.metrics <- compute.art.metrics("sdp_scenarioIV",
                                                    n.sim=n.sim,
                                                    date="5Apr", 
                                                    last.time.pt=last.time.pt
                                                    )


##############################################


##############################################
 ### Save object
##############################################
  save.image("za_art_metrics.RData")

##############################################
