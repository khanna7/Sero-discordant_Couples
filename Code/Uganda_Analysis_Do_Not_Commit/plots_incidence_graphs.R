###########################################
### Incidence graphs: SDP intervention runs
###########################################

##########################################
### Progression of versions
##########################################
## 1 Mar 2015: Combine Scenario IV

## 7 Nov 2014: incidence graphs for Uganda
##########################################

##########################################
### Load libraries and objects
##########################################

  library(ggplot2)
  load("ug_sdp_inc_comp_wci.RData")

  ug.inc.df <- as.data.frame(ug.inc.data)
  ug.inc.df <- cbind(1:10, ug.inc.df)
  colnames(ug.inc.df)[1] <- "Year"

  ## Start at same 0
  zero.inc <- 0.828
    ## from ("/homes/khanna7/Projects/Home-Based/PMTCT/Code/Development2/Uganda_Runs/plots_figures_inc_samezero.R")
  ug.inc.zero <- c(0, rep(c(zero.inc, zero.inc, zero.inc), 5))
    ## upper and lower bound of zero will be the same here
  ug.inc.df <- rbind(ug.inc.zero, ug.inc.df)
##########################################

##########################################
### Make plot
##########################################

  ## With legend
  cols <- c("No SDP Interv."="black",
            "Low"="blue",
            "High"="green",
            "ScnIV-Test"="red",
            "ScnIV.Test.Red.Rec.Prev"="brown")
  cols <- rev(cols)

  inc.plot.2 <-
  ggplot(ug.inc.df, aes(x=Year))+
  geom_line(aes(y=ug.inc.df$Baseline.Curr.Mean, color="No SDP Interv."))+
  geom_point(aes(y=ug.inc.df$Baseline.Curr.Mean, color="No SDP Interv."))+
  geom_line(aes(y=ug.inc.df$SDP.Curr.Mean, color="Low"))+
  geom_point(aes(y=ug.inc.df$SDP.Curr.Mean, color="Low"))+
  geom_line(aes(y=ug.inc.df$SDP.High.Mean, color="High"))+
  geom_point(aes(y=ug.inc.df$SDP.High.Mean, color="High"))+
  geom_line(aes(y=ug.inc.df$SDP.ScenarioIV.Mean, color="ScnIV-Test"))+
  geom_point(aes(y=ug.inc.df$SDP.ScenarioIV.Mean, color="ScnIV-Test"))+
  geom_line(aes(y=ug.inc.df$SDP.ScenarioIV.Mean, color="ScnIV-Test.Red.Rec.Prev"))+
  geom_point(aes(y=ug.inc.df$SDP.ScenarioIV.Mean, color="ScnIV-Test.Red.Rec.Prev"))
  scale_colour_manual(name="SDP Intervention Coverage",values=cols)

  inc.plot.x.2 <- inc.plot.2+scale_x_continuous(breaks=c(0, 1, 4, 7, 10))+
                theme(axis.text.x=element_text(face='bold', size=12))

  inc.plot.xy.2 <- inc.plot.x.2+ylim(0,3)+ylab("Incidence")+
                 theme(axis.text.y=element_text(face='bold', size=12))

  inc.plot.err.2 <- inc.plot.xy.2+
                              geom_errorbar(
                                aes(x=c(0:10)),
                                    ymax=ug.inc.df$Baseline.Curr.HighCI,
                                    ymin=ug.inc.df$Baseline.Curr.LowCI,
                                    linetype=2,
                                    alpha=0.5,
                                    width=0.25,
                                    color="black")+
                                geom_errorbar(
                                aes(x=c(0:10)),
                                    ymax=ug.inc.df$SDP.Curr.HighCI,
                                    ymin=ug.inc.df$SDP.Curr.LowCI,
                                    linetype=2,
                                    alpha=0.5,
                                    width=0.25,
                                    color="blue")+
                                  geom_errorbar(
                                aes(x=c(0:10)),
                                    ymax=ug.inc.df$SDP.High.HighCI,
                                    ymin=ug.inc.df$SDP.High.LowCI,
                                    linetype=2,
                                    alpha=0.5,
                                    width=0.25,
                                    color="green")+
                                  geom_errorbar(
                                aes(x=c(0:10)),
                                    ymax=ug.inc.df$SDP.ScenarioIV.HighCI,
                                    ymin=ug.inc.df$SDP.ScenarioIV.LowCI,
                                    linetype=2,
                                    alpha=0.5,
                                    width=0.25,
                                    color="red")+
                                    geom_errorbar(
                                aes(x=c(0:10)),
                                    ymax=ug.inc.df$SDP.ScenarioIV.Red.Rec.Prev.HighCI,
                                    ymin=ug.inc.df$SDP.ScenarioIV.Red.Rec.Prev.LowCI,
                                    linetype=2,
                                    alpha=0.5,
                                    width=0.25,
                                    color="brown")

  inc.plot.title.2 <- inc.plot.err.2+
                      ggtitle("Uganda")+
                      theme(legend.position=c(0.7, 0.7),
                            legend.background=element_rect(fill="grey90"))

##########################################
### Save plot pdf
##########################################

  pdf(file="ug_sdp_inc.pdf")
  inc.plot.title.2
  dev.off()

