###########################################
### SDP intervention runs
###########################################

##########################################
### Progression of versions
##########################################
## 1 Mar 2015: combine scenario IV results here

## 7 Nov 2014: plot runs for uganda
##########################################

rm(list=ls())

   time.interv <- 1:260
   time.interv.yrs <- time.interv/26

   n.sim <- 5

##########################################
### Read data from CSV files
##########################################

 ## Baseline
   for (i in 1:n.sim){
     assign(paste("ug.bl.cp.run.",i, sep=""),
            read.csv(paste("~/Projects/Home-Based/PMTCT/Code/Development2/Uganda_Runs/28Feb_UG_bl_cp_run", i,
                           ".prev.inc.data.csv",
                           sep=""),
                     sep=" ", header=FALSE)
            )
   }                    

  ## SDP current coverage
     for (i in 1:n.sim){
       assign(paste("ug.sdp.curr.run.",i, sep=""),
              read.csv(paste("../Uganda_Runs_Do_Not_Commit/5Apr_UG_sdp_curr_run", i,
                             ".prev.inc.data.csv",
                             sep=""),
                       sep=" ", header=FALSE)
              )
     }                    

  ## SDP high coverage
     for (i in 1:n.sim){
       assign(paste("ug.sdp.high.run.",i, sep=""),
              read.csv(paste("../Uganda_Runs_Do_Not_Commit/5Apr_UG_sdp_high_run", i,
                             ".prev.inc.data.csv",
                             sep=""),
                       sep=" ", header=FALSE)
              )
     }                    

  ## SDP Scenario IV
     for (i in 1:n.sim){
       assign(paste("ug.sdp.scenarioIV.run.",i, sep=""),
              read.csv(paste("../Uganda_Runs_Do_Not_Commit/5Apr_UG_sdp_scenarioIV_run", i,
                             ".prev.inc.data.csv",
                             sep=""),
                       sep=" ", header=FALSE)
              )
     }                    

  ## No decline in UI + Scenario II
     for (i in 1:n.sim){
       assign(paste("ug.sdp.curr.nodecui.run.",i, sep=""),
              read.csv(paste("../Uganda_Runs_Do_Not_Commit/5Apr_UG_sdp_curr_nodecui_run", i,
                             ".prev.inc.data.csv",
                             sep=""),
                       sep=" ", header=FALSE)
              )
     }                    

##########################################
### Compute mean across ten years
##########################################

  ## Baseline
     mean.inci.bl.cp <- matrix(NA, ncol=n.sim, nrow=260)
     for (i in 1:n.sim){ 
       data <- get(paste("ug.bl.cp.run.",i, sep=""))
       mean.inci.bl.cp[,i] <- (data[,6]/(data[,2]-data[,7])) 
     }

  ## SDP curr
     mean.inci.sdp.curr <- matrix(NA, ncol=n.sim, nrow=260)
     for (i in 1:n.sim){ 
       data <- get(paste("ug.sdp.curr.run.",i, sep=""))
       mean.inci.sdp.curr[,i] <- (data[,6]/(data[,2]-data[,7])) 
     }

  ## SDP high
     mean.inci.sdp.high <- matrix(NA, ncol=n.sim, nrow=260)
     for (i in 1:n.sim){ 
       data <- get(paste("ug.sdp.high.run.",i, sep=""))
       mean.inci.sdp.high[,i] <- (data[,6]/(data[,2]-data[,7])) 
     }

  ## SDP scenario IV model testing
     mean.inci.sdp.scenarioIV <- matrix(NA, ncol=n.sim, nrow=260)
     for (i in 1:n.sim){ 
       data <- get(paste("ug.sdp.scenarioIV.run.",i, sep=""))
       mean.inci.sdp.scenarioIV[,i] <- (data[,6]/(data[,2]-data[,7])) 
     }

  ## SDP current coverage + no decline in UI
     mean.inci.sdp.curr.nodecui <- matrix(NA, ncol=n.sim, nrow=260)
     for (i in 1:n.sim){ 
       data <- get(paste("ug.sdp.curr.nodecui.run.",i, sep=""))
       mean.inci.sdp.curr.nodecui[,i] <- (data[,6]/(data[,2]-data[,7])) 
     }

##########################################

##########################################
### Chunk up for annual incidence rates 
##########################################

  ## formula for confidence intervals:
  ## mean for each year over 10 years +/-
  ## t*sd(for each year over 10 years)/sqrt(n)
  ## where n is the number of simulations

     ## Baseline
     bl.cp <- apply(mean.inci.bl.cp, 1, mean)

     bl.cp <- split(bl.cp, ceiling(seq_along(bl.cp)/26))
     bl.cp.mean <- unlist(lapply(bl.cp, mean))*26*100
     bl.cp.lowci <- (bl.cp.mean)-((qt(0.975, df=n.sim-1)*unlist(lapply(bl.cp,
                                                     sd))*(26*100))/sqrt(n.sim)) 
     bl.cp.upci <- (bl.cp.mean)+((qt(0.975, df=n.sim-1)*unlist(lapply(bl.cp,
                      sd))*(26*100))/sqrt(n.sim)) ##30Jun14

     ## SDP current
     sdp.curr <- apply(mean.inci.sdp.curr, 1, mean)

     sdp.curr <- split(sdp.curr, ceiling(seq_along(sdp.curr)/26))
     sdp.curr.mean <- unlist(lapply(sdp.curr, mean))*26*100
     sdp.curr.lowci <- (sdp.curr.mean)-((qt(0.975, df=n.sim-1)*unlist(lapply(sdp.curr,
                                                     sd))*(26*100))/sqrt(n.sim)) 
     sdp.curr.upci <- (sdp.curr.mean)+((qt(0.975, df=n.sim-1)*unlist(lapply(sdp.curr,
                      sd))*(26*100))/sqrt(n.sim)) ##30Jun14

     ## SDP high
     sdp.high <- apply(mean.inci.sdp.high, 1, mean)

     sdp.high <- split(sdp.high, ceiling(seq_along(sdp.high)/26))
     sdp.high.mean <- unlist(lapply(sdp.high, mean))*26*100
     sdp.high.lowci <- (sdp.high.mean)-((qt(0.975, df=n.sim-1)*unlist(lapply(sdp.high,
                                                     sd))*(26*100))/sqrt(n.sim)) 
     sdp.high.upci <- (sdp.high.mean)+((qt(0.975, df=n.sim-1)*unlist(lapply(sdp.high,
                      sd))*(26*100))/sqrt(n.sim)) ##30Jun14


     ug.inc.data <- cbind(
                      bl.cp.mean, bl.cp.lowci, bl.cp.upci,
                      sdp.curr.mean, sdp.curr.lowci, sdp.curr.upci,
                      sdp.high.mean, sdp.high.lowci, sdp.high.upci)

     ## Scenario IV model testing
     sdp.scenarioIV <- apply(mean.inci.sdp.scenarioIV, 1, mean)

     sdp.scenarioIV <- split(sdp.scenarioIV, ceiling(seq_along(sdp.scenarioIV)/26))
     sdp.scenarioIV.mean <- unlist(lapply(sdp.scenarioIV, mean))*26*100
     sdp.scenarioIV.lowci <- (sdp.scenarioIV.mean)-((qt(0.975, df=n.sim-1)*unlist(lapply(sdp.scenarioIV,
                                                     sd))*(26*100))/sqrt(n.sim)) 
     sdp.scenarioIV.upci <- (sdp.scenarioIV.mean)+((qt(0.975, df=n.sim-1)*unlist(lapply(sdp.scenarioIV,
                      sd))*(26*100))/sqrt(n.sim)) ##30Jun14

     ## SDP current coverage + no decline in UI
     sdp.curr.nodecui <- apply(mean.inci.sdp.curr.nodecui, 1, mean)

     sdp.curr.nodecui <- split(sdp.curr.nodecui, ceiling(seq_along(sdp.curr.nodecui)/26))
     sdp.curr.nodecui.mean <- unlist(lapply(sdp.curr.nodecui, mean))*26*100
     sdp.curr.nodecui.lowci <- (sdp.curr.nodecui.mean)-((qt(0.975, df=n.sim-1)*unlist(lapply(sdp.curr.nodecui,
                                                     sd))*(26*100))/sqrt(n.sim)) 
     sdp.curr.nodecui.upci <- (sdp.curr.nodecui.mean)+((qt(0.975, df=n.sim-1)*unlist(lapply(sdp.curr.nodecui,
                      sd))*(26*100))/sqrt(n.sim)) ##30Jun14


     ## Combine
     ug.inc.data <- cbind(
                      bl.cp.mean, bl.cp.lowci, bl.cp.upci,
                      sdp.curr.mean, sdp.curr.lowci, sdp.curr.upci,
                      sdp.high.mean, sdp.high.lowci, sdp.high.upci,
                      sdp.scenarioIV.mean, sdp.scenarioIV.lowci, sdp.scenarioIV.upci,
                      sdp.curr.nodecui.mean, sdp.curr.nodecui.lowci,
                                             sdp.curr.nodecui.upci
                      )

     colnames(ug.inc.data) <-
       c("Baseline.Curr.Mean", "Baseline.Curr.LowCI", "Baseline.Curr.HighCI",
         "SDP.Curr.Mean", "SDP.Curr.LowCI", "SDP.Curr.HighCI",
         "SDP.High.Mean", "SDP.High.LowCI", "SDP.High.HighCI",
         "SDP.ScenarioIV.Mean", "SDP.ScenarioIV.LowCI", "SDP.ScenarioIV.HighCI",
         "SDP.Curr.Nodecui.Mean",
                                 "SDP.Curr.Nodecui.LowCI",
                                 "SDP.Curr.Nodecui.UpCI"
         )

##########################################

##########################################
### Save object
##########################################
  save.image(file="ug_sdp_inc_comp_wci.RData")
##########################################
