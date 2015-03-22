##############################################
### Count vertical infections
##############################################

  ## In the 10th year, or in every year?

##############################################
  library(statnet)

  ## Needed data
     n.sim <- 5 #num of simulations
     time.pt.seq <- seq(780, 1040, by=26) #sequence of time-points

##############################################
### Different scenarios
##############################################

  ## SDP Current
     sdp.curr.data <- matrix(NA,
                             ncol=length(time.pt.seq)-1,
                             nrow=n.sim)

     ## Record proportion of vertical transmissions
     for (i in 1:n.sim){
       ##browser()
       load(paste("../SouthAfrica_Runs_Do_Not_Commit/nw9Nov_ZA_sdp_curr_run",
                  i,".RData",sep=""))
       
       net <- nw
       time.of.infection <- get.vertex.attribute(net, "time.of.infection")
       infector.id <- get.vertex.attribute(net, "infector.ID")
       
       for (j in 1:(length(time.pt.seq)-1)){
         
         infected.last.year <- intersect(which(time.of.infection > time.pt.seq[j]),
                                          which(time.of.infection <= time.pt.seq[j+1])
                                               )
         infected.vertically <- which(is.na(infector.id))
         
         infected.vertically.last.year <- intersect(infected.last.year,
                                                    infected.vertically)

         sdp.curr.data[i,j] <- length(infected.vertically.last.year)/
                                               length(infected.last.year)

       }

     }


  ## SDP High
     sdp.high.data <- matrix(NA,
                             ncol=length(time.pt.seq)-1,
                             nrow=n.sim)

     ## Record proportion of vertical transmissions
     for (i in 1:n.sim){
       ##browser()
       load(paste("../SouthAfrica_Runs_Do_Not_Commit/nw9Nov_ZA_sdp_high_run",
                  i,".RData",sep=""))
       
       net <- nw
       time.of.infection <- get.vertex.attribute(net, "time.of.infection")
       infector.id <- get.vertex.attribute(net, "infector.ID")
       
       for (j in 1:(length(time.pt.seq)-1)){
         
         infected.last.year <- intersect(which(time.of.infection > time.pt.seq[j]),
                                          which(time.of.infection <= time.pt.seq[j+1])
                                               )
         infected.vertically <- which(is.na(infector.id))
         
         infected.vertically.last.year <- intersect(infected.last.year,
                                                    infected.vertically)

         sdp.high.data[i,j] <- length(infected.vertically.last.year)/
                                               length(infected.last.year)

       }

     }

  ## Scenario IV: model testing
     sdp.scenarioIV.data <- matrix(NA,
                             ncol=length(time.pt.seq)-1,
                             nrow=n.sim)

     ## Record proportion of vertical transmissions
     for (i in 1:n.sim){
       ##browser()
       load(paste("../SouthAfrica_Runs_Do_Not_Commit/nw28Feb_ZA_sdp_scenarioIV_run",
                  i,".RData",sep=""))
       
       net <- nw
       time.of.infection <- get.vertex.attribute(net, "time.of.infection")
       infector.id <- get.vertex.attribute(net, "infector.ID")
       
       for (j in 1:(length(time.pt.seq)-1)){
         
         infected.last.year <- intersect(which(time.of.infection > time.pt.seq[j]),
                                          which(time.of.infection <= time.pt.seq[j+1])
                                               )
         infected.vertically <- which(is.na(infector.id))
         
         infected.vertically.last.year <- intersect(infected.last.year,
                                                    infected.vertically)

         sdp.scenarioIV.data[i,j] <- length(infected.vertically.last.year)/
                                               length(infected.last.year)

       }

     }

  ## Scenario IV: model testing + Reduced prevalence at recruitment
     sdp.scenarioIV_reduced_rec_prev.data <- matrix(NA,
                             ncol=length(time.pt.seq)-1,
                             nrow=n.sim)

     ## Record proportion of vertical transmissions
     for (i in 1:n.sim){
       ##browser()
       load(paste("../SouthAfrica_Runs_Do_Not_Commit/nw5Mar_ZA_sdp_scenarioIV_reduced_rec_prev_run",
                  i,".RData",sep=""))
       
       net <- nw
       time.of.infection <- get.vertex.attribute(net, "time.of.infection")
       infector.id <- get.vertex.attribute(net, "infector.ID")
       
       for (j in 1:(length(time.pt.seq)-1)){
         
         infected.last.year <- intersect(which(time.of.infection > time.pt.seq[j]),
                                          which(time.of.infection <= time.pt.seq[j+1])
                                               )
         infected.vertically <- which(is.na(infector.id))
         
         infected.vertically.last.year <- intersect(infected.last.year,
                                                    infected.vertically)

         sdp.scenarioIV_reduced_rec_prev.data[i,j] <- length(infected.vertically.last.year)/
                                               length(infected.last.year)

       }

     }

  ## SDP Current with no decline in unprotected intercourse
     sdp.curr.vert.nodecui.data <- matrix(NA,
                                             ncol=length(time.pt.seq)-1,
                                             nrow=n.sim)

     ## Record proportion of vertical transmissions
     for (i in 1:n.sim){
       ##browser()
       load(paste("../SouthAfrica_Runs_Do_Not_Commit/nw8Mar_ZA_sdp_curr_run",
                  i,".RData",sep=""))
       
       net <- nw
       time.of.infection <- get.vertex.attribute(net, "time.of.infection")
       infector.id <- get.vertex.attribute(net, "infector.ID")
       
       for (j in 1:(length(time.pt.seq)-1)){
         
         infected.last.year <- intersect(which(time.of.infection > time.pt.seq[j]),
                                          which(time.of.infection <= time.pt.seq[j+1])
                                               )
         infected.vertically <- which(is.na(infector.id))
         
         infected.vertically.last.year <- intersect(infected.last.year,
                                                    infected.vertically)

         sdp.curr.vert.nodecui.data[i,j] <- length(infected.vertically.last.year)/
                                               length(infected.last.year)

       }

     }

  ## Baseline
     baseline.data <- matrix(NA,
                             ncol=length(time.pt.seq)-1,
                             nrow=n.sim)

    
    ## Baseline times are enumerated from 1040 to 1300
       time.pt.seq <- time.pt.seq+260#!!!!!!!!!!!!!!

     ## Record proportion of vertical transmissions
     for (i in 1:n.sim){
       ##browser()
       load(paste("~/Projects/Home-Based/PMTCT/Code/Development2/SouthAfrica_Development_Runs/nw27May_UG_baseline_cp_run",
                  i,".RData",sep=""))
       
       net <- nw
       time.of.infection <- get.vertex.attribute(net, "time.of.infection")
       infector.id <- get.vertex.attribute(net, "infector.ID")
       
       for (j in 1:(length(time.pt.seq)-1)){
         
         infected.last.year <- intersect(which(time.of.infection > time.pt.seq[j]),
                                          which(time.of.infection <= time.pt.seq[j+1])
                                               )
         infected.vertically <- which(is.na(infector.id))
         
         infected.vertically.last.year <- intersect(infected.last.year,
                                                    infected.vertically)

         baseline.data[i,j] <- length(infected.vertically.last.year)/
                                               length(infected.last.year)

       }

     }

   ## Reset Baseline times are enumerated from 1040 to 1300
       time.pt.seq <- time.pt.seq-260#!!!!!!!!!!!!!!
     
##############################################

##############################################
### Different scenarios
##############################################

  ## Compute means and confidence intervals
     t.baseline.data <- t(baseline.data)
     t.sdp.curr.data <- t(sdp.curr.data)
     t.sdp.high.data <- t(sdp.high.data)
     t.sdp.scenarioIV.data <- t(sdp.scenarioIV.data)
     t.sdp.scenarioIV_reduced_rec_prev.data <- t(sdp.scenarioIV_reduced_rec_prev.data)
     t.sdp.curr.vert.nodecui.data <- t(sdp.curr.vert.nodecui.data)

  ## Write function to compute means and ci's of vertical infections
     print.mean.ci <- function(t.data.matrix, ...){
       n.sim <- ncol(t.data.matrix)
       out.mat <- matrix(0, nrow=nrow(t.data.matrix), ncol=3)
       
       out.mat[,1] <- apply(t.data.matrix, 1, mean)
       out.ci <- apply(t.data.matrix, 1, sd)*qt(0.975, df=n.sim-1)
       out.mat[,2] <- out.mat[,1] - out.ci
       out.mat[,3] <- out.mat[,1] + out.ci

       return(out.mat)
     }

   ## Apply function to compute means and confidence intervals
      t.baseline.data.ci <- print.mean.ci(t.baseline.data)
      t.sdp.curr.data.ci <- print.mean.ci(t.sdp.curr.data)
      t.sdp.high.data.ci <- print.mean.ci(t.sdp.high.data)
      t.sdp.scenarioIV.data.ci <- print.mean.ci(t.sdp.scenarioIV.data)
      t.sdp.scenarioIV_reduced_rec_prev.data.ci <- print.mean.ci(t.sdp.scenarioIV_reduced_rec_prev.data)
      t.sdp.curr.vert.nodecui.data.ci <- print.mean.ci(t.sdp.curr.vert.nodecui.data)

##############################################


##############################################
 ### Save object
##############################################
  save.image("za_count_vertical_infections.RData")

##############################################
