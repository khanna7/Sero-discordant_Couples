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
  ### Write function to compute
  ### %vertical transmissions
##############################################

  compute.prop.vert.infections <- function(sdp_scn,
                                           n.sim=n.sim,
                                           time.pt.seq=time.pt.seq,
                                           date,
                                           ...
                                           ){

      out.mat <- matrix(NA,
                        ncol=length(time.pt.seq)-1,
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

         out.mat[i,j] <- length(infected.vertically.last.year)/
                                               length(infected.last.year)

       }  
     }
      return(out.mat)
    }

##############################################

##############################################
  ### Apply above function to different
  ### scenarios
##############################################

  sdp.curr.data <- compute.prop.vert.infections("sdp_curr",
                                                n.sim=n.sim,
                                                time.pt.seq=time.pt.seq,
                                                date="6Apr" #2014
                                                )

  sdp.high.data <- compute.prop.vert.infections("sdp_high",
                                                n.sim=n.sim,
                                                time.pt.seq=time.pt.seq,
                                                date="5Apr" #2014
                                                )

  sdp.scenarioIV.data <- compute.prop.vert.infections("sdp_scenarioIV",
                                                      n.sim=n.sim,
                                                      time.pt.seq=time.pt.seq,
                                                      date="5Apr" #2015
                                                      )

  sdp.curr_nodecui.data <- compute.prop.vert.infections("sdp_curr_nodecui",
                                                      n.sim=n.sim,
                                                      time.pt.seq=time.pt.seq,
                                                      date="6Apr" #2015
                                                      )

  baseline.data <- compute.prop.vert.infections("baseline_cp",
                                                n.sim=n.sim,
                                                time.pt.seq=time.pt.seq,
                                                date="27May" #2014 in this case
                                                )

##############################################

##############################################
### Means and confidence intervals
### in different scenarios
##############################################

  ## Compute means and confidence intervals
     t.baseline.data <- t(baseline.data)
     t.sdp.curr.data <- t(sdp.curr.data)
     t.sdp.high.data <- t(sdp.high.data)
     t.sdp.scenarioIV.data <- t(sdp.scenarioIV.data)
     t.sdp.curr.nodecui.data <- t(sdp.curr_nodecui.data)

  ## Write function to compute means and ci's of vertical infections
     print.mean.ci <- function(t.data.matrix, ...){
       n.sim <- ncol(t.data.matrix)
       out.mat <- matrix(0, nrow=nrow(t.data.matrix), ncol=3)
       
       out.mat[,1] <- apply(t.data.matrix, 1, mean)
       out.ci <- apply(t.data.matrix, 1, sd)*qt(0.975, df=n.sim-1)*1/sqrt(n.sim)
       out.mat[,2] <- out.mat[,1] - out.ci
       out.mat[,3] <- out.mat[,1] + out.ci

       return(out.mat)
     }

   ## Apply function to compute means and confidence intervals
      t.baseline.data.ci <- print.mean.ci(t.baseline.data)
      t.sdp.curr.data.ci <- print.mean.ci(t.sdp.curr.data)
      t.sdp.high.data.ci <- print.mean.ci(t.sdp.high.data)
      t.sdp.scenarioIV.data.ci <- print.mean.ci(t.sdp.scenarioIV.data)
      t.sdp.curr.nodecui.data.ci <- print.mean.ci(t.sdp.curr.nodecui.data)

##############################################


##############################################
 ### Save object
##############################################
  save.image("za_count_vertical_infections.RData")

##############################################
