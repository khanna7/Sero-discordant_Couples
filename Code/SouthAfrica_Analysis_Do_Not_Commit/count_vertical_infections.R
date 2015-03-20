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
 ### Save object
##############################################
  save.image("za_count_vertical_infections.RData")

##############################################
