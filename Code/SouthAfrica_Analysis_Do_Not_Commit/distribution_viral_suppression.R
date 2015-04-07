##############################################
### How many of those on ART are virally suppressed?
##############################################

  ## At the last time step

##############################################

##############################################
  rm(list=ls())
  library(statnet)

  ## Needed data
     n.sim <- 5 #num of simulations
     last.time.pt <- 1300 #needs to say 1300 because that was how these runs were numbered
       ## baseline.data are labeled from 1040 to 1300
       ## others are from 780 to 1040, extracting at 1300
       ## will give network at last time step for all cases

##############################################
  ### Write function to compute
  ### %vertical transmissions
##############################################

  compute.viral.supp <- function(sdp_scn,
                                  n.sim=n.sim,
                                  date,
                                  last.time.pt,
                                  viral.supp.level=2,
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
       viral.load.today <- net%v%"viral.load.today"
       suppressed <- which(viral.load.today < viral.supp.level)
       
       on.art.virally.suppressed <- intersect(on.art,
                                              suppressed)

       out.mat[i, 1] <- length(infected)
       out.mat[i, 2] <- length(on.art) 
       out.mat[i, 3] <- length(on.art.virally.suppressed)             
       out.mat[i, 4] <- length(on.art.virally.suppressed)/length(infected)
       out.mat[i, 5] <- network.size(net)
     }
      return(out.mat)
    }

##############################################

##############################################
  ### Apply above function to different
  ### scenarios
##############################################

  sdp.curr.viral.supp.data <- compute.viral.supp("sdp_curr",
                                                  n.sim=n.sim,
                                                  date="6Apr", 
                                                  last.time.pt=last.time.pt
                                                  )

  sdp.high.viral.supp.data <- compute.viral.supp("sdp_high",
                                                  n.sim=n.sim,
                                                  date="5Apr", 
                                                  last.time.pt=last.time.pt
                                                  )

  baseline.viral.supp.data <- compute.viral.supp("baseline_cp",
                                                 n.sim=n.sim,
                                                 date="27May", #2014,
                                                 last.time.pt=last.time.pt
                                                 )

  sdp.scenarioIV.viral.supp.data <- compute.viral.supp("sdp_scenarioIV",
                                                       n.sim=n.sim,
                                                       date="5Apr", 
                                                       last.time.pt=last.time.pt
                                                       )


##############################################


##############################################
 ### Save object
##############################################
  save.image("za_viral_supp.RData")

##############################################
