
  rm(list=ls())

  library(ggplot2)
  library(plyr)
  library(reshape2)

  load("za_sdp_inc_comp_wci.RData")

  za.inc.df <- as.data.frame(za.inc.data)
  za.inc.df <- cbind(1:10, za.inc.df)
  colnames(za.inc.df)[1] <- "Year"

  zero.inc <- 2.313
  za.inc.zero <- c(0, rep(c(zero.inc, zero.inc, zero.inc), 6))
  za.inc.df <- rbind(za.inc.zero, za.inc.df)

  melted.data <- melt(za.inc.df, id="Year")

  mean.start.id <- 1:11
  melted.data.meanvalues <- melted.data[c(mean.start.id,
                                          33+(mean.start.id),
                                          (33*2)+(mean.start.id),
                                          (33*3)+(mean.start.id),
                                          (33*4)+(mean.start.id),
                                          (33*5)+(mean.start.id)
                                          ),]

  lb.start.id <- mean.start.id+11
  lb.data <- melted.data[c(lb.start.id,
                                          33+(lb.start.id),
                                          (33*2)+(lb.start.id),
                                          (33*3)+(lb.start.id),
                                          (33*4)+(lb.start.id),
                                          (33*5)+(lb.start.id)
                                          ),]

  ub.start.id <- mean.start.id+22
  ub.data <- melted.data[c(ub.start.id,
                                          33+(ub.start.id),
                                          (33*2)+(ub.start.id),
                                          (33*3)+(ub.start.id),
                                          (33*4)+(ub.start.id),
                                          (33*5)+(ub.start.id)
                                          ),]

  melted.data.mean.lb.ub <- cbind(melted.data.meanvalues,
                                  lb.data$value,
                                  ub.data$value)

  colnames(melted.data.mean.lb.ub) <- c("Year",
                                        "Scenario",
                                        "Mean",
                                        "LB",
                                        "UB"
                                        )



  ## construct line plot
 
   cbPalette <- c("#999999", "#E69F00", "#56B4E9", "#009E73",
                  "#F0E442", "#0072B2", "#D55E00", "#CC79A7")

# The palette with black:
   cbPalette.black <- c("#000000", "#E69F00", "#56B4E9", "#009E73",
                         "#F0E442", "#0072B2", "#D55E00", "#CC79A7")


  line.plot <- 
  ggplot(melted.data.mean.lb.ub, aes(color=Scenario,
                                     x=Year,
                                     y=Mean,
                                     ymin=LB,
                                     ymax=UB)## ,
                                     ## scale_fill_hue(l=80, c=45)
         )+ #plot
      geom_line()+ #mean-value-lines
      geom_errorbar(width=0.25, linetype=2)+
      scale_color_manual(values=cbPalette.black[1:6])+
      ylim(0,3.2)     


   format.axes <- line.plot+
      ylab("Incidence")+
      theme(axis.text.y=element_text(face='bold'))+
      theme(axis.text.x=element_text(face='bold'))+
      scale_x_continuous(breaks=c(0, 1, 4, 7, 10))

   za.w.legend <- line.plot+ggtitle("South Africa")+
                  theme(legend.position=c(0.5,0.2),
                  legend.background = element_rect(fill="gray90")
                  )


  pdf(file="trial-sa.pdf")
  za.w.legend
  dev.off()



      

