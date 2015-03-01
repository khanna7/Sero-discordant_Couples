###########################################
### Incidence graphs: SDP intervention runs
###########################################

##########################################
### Progression of versions
##########################################
## 9 Nov 2014: incidence graphs for South Africa
##########################################

##########################################
### Load libraries and objects
##########################################

  library(ggplot2)
  load("za_sdp_inc_comp_wci.RData")

  za.inc.df <- as.data.frame(za.inc.data)
  za.inc.df <- cbind(1:10, za.inc.df)
  colnames(za.inc.df)[1] <- "Year"

  ## Start at same 0
  zero.inc <- 2.313
    ## from ("/homes/khanna7/Projects/Home-Based/PMTCT/Code/Development2/SouthAfrica_Development_Runs/plots_annualinc_samezero_forjustin.R
  za.inc.zero <- c(0, rep(c(zero.inc, zero.inc, zero.inc), 3))
    ## upper and lower bound of zero will be the same here
  za.inc.df <- rbind(za.inc.zero, za.inc.df)
##########################################

##########################################
### Make plot
##########################################

  ## With legend
  cols <- c("No SDP Interv."="black",
            "Low"="blue",
            "High"="green")
  cols <- rev(cols)

  inc.plot.2 <-
  ggplot(za.inc.df, aes(x=Year))+
  geom_line(aes(y=za.inc.df$Baseline.Curr.Mean, color="No SDP Interv."))+
  geom_point(aes(y=za.inc.df$Baseline.Curr.Mean, color="No SDP Interv."))+
  geom_line(aes(y=za.inc.df$SDP.Curr.Mean, color="Low"))+
  geom_point(aes(y=za.inc.df$SDP.Curr.Mean, color="Low"))+
  geom_line(aes(y=za.inc.df$SDP.High.Mean, color="High"))+
  geom_point(aes(y=za.inc.df$SDP.High.Mean, color="High"))+
  scale_colour_manual(name="SDP Intervention Coverage",values=cols)

  inc.plot.x.2 <- inc.plot.2+scale_x_continuous(breaks=c(0, 1, 4, 7, 10))+
                theme(axis.text.x=element_text(face='bold', size=12))

  inc.plot.xy.2 <- inc.plot.x.2+ylim(0,3)+ylab("Incidence")+
                 theme(axis.text.y=element_text(face='bold', size=12))

  inc.plot.err.2 <- inc.plot.xy.2+
                              geom_errorbar(
                                aes(x=c(0:10)),
                                    ymax=za.inc.df$Baseline.Curr.HighCI,
                                    ymin=za.inc.df$Baseline.Curr.LowCI,
                                    linetype=2,
                                    alpha=0.5,
                                    width=0.25)+
                                geom_errorbar(
                                aes(x=c(0:10)),
                                    ymax=za.inc.df$SDP.Curr.HighCI,
                                    ymin=za.inc.df$SDP.Curr.LowCI,
                                    linetype=2,
                                    alpha=0.5,
                                    width=0.25,
                                    color="green")+
                                  geom_errorbar(
                                aes(x=c(0:10)),
                                    ymax=za.inc.df$SDP.High.HighCI,
                                    ymin=za.inc.df$SDP.High.LowCI,
                                    linetype=2,
                                    alpha=0.5,
                                    width=0.25,
                                    color="green")
  inc.plot.title.2 <- inc.plot.err.2+
                      ggtitle("South Africa")+
                      theme(legend.position=c(0.5, 0.1),
                            legend.background=element_rect(fill="grey90"))

##########################################
### Save plot pdf
##########################################

  pdf(file="za_sdp_inc.pdf")
  inc.plot.title.2
  dev.off()

