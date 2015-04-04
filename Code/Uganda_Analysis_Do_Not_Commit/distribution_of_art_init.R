##############################################
### Count vertical infectionss
##############################################

  ## In every year

##############################################

##############################################
  rm(list=ls())
  library(statnet)

  ## Needed data
     n.sim <- 5 #num of simulations
     time.pt.seq <- seq(1040, 1300, by=26) #sequence of time-points

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
       load(paste("../Uganda_Runs_Do_Not_Commit/nw",date,"_UG_",sdp_scn,"_run",
                  i,".RData",sep=""))
       
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
                                              date="2Nov", #2014,
                                              last.time.pt=1300
                                              )

  sdp.high.art.metrics <- compute.art.metrics("sdp_high",
                                              n.sim=n.sim,
                                              date="2Nov", #2014,
                                              last.time.pt=1300
                                              )

  baseline.art.metrics <- compute.art.metrics("bl_cp",
                                              n.sim=n.sim,
                                              date="28Feb", #2014,
                                              last.time.pt=1300
                                              )

  sdp.scenarioIV.art.metrics <- compute.art.metrics("sdp_scenarioIV",
                                                    n.sim=n.sim,
                                                    date="28Feb", #2015
                                                    last.time.pt=1300
                                                    )


##############################################


##############################################
 ### Save object
##############################################
  save.image("ug_art_metrics.RData")

##############################################
