###########################################
### SDP intervention runs
###########################################

##########################################
### Progression of versions
##########################################
## 7 Nov 2014: plot runs for uganda
##########################################

rm(list=ls())

   time.interv <- 1:260
   time.interv.yrs <- time.interv/26

   n.sim <- 5

##########################################
### Read data from CSV files
##########################################

  ## SDP current coverage
     for (i in 1:n.sim){
       assign(paste("ug.sdp.curr.run.",i, sep=""),
              read.csv(paste("../Uganda_Runs_Do_Not_Commit/2Nov_UG_sdp_curr_run", i,
                             ".prev.inc.data.csv",
                             sep=""),
                       sep=" ", header=FALSE)
              )
     }                    

  ## SDP high coverage
     for (i in 1:n.sim){
       assign(paste("ug.sdp.high.run.",i, sep=""),
              read.csv(paste("../Uganda_Runs_Do_Not_Commit/2Nov_UG_sdp_high_run", i,
                             ".prev.inc.data.csv",
                             sep=""),
                       sep=" ", header=FALSE)
              )
     }                    

##########################################
### Compute mean across ten years
##########################################

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

##########################################

##########################################
### Chunk up for annual incidence rates 
##########################################

  ## formula for confidence intervals:
  ## mean for each year over 10 years +/-
  ## t*sd(for each year over 10 years)/sqrt(n)
  ## where n is the number of simulations

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


     ug.inc.data <- cbind(sdp.curr.mean, sdp.curr.lowci, sdp.curr.upci,
                          sdp.high.mean, sdp.high.lowci, sdp.high.upci)



     colnames(ug.inc.data) <-
       c("SDP.Curr.Mean", "SDP.Curr.LowCI", "SDP.Curr.HighCI",
         "SDP.High.Mean", "SDP.High.LowCI", "SDP.High.HighCI")

##########################################

##########################################
### Save object
##########################################
  save.image(file="ug_sdp_inc_comp_wci.RData")
##########################################
