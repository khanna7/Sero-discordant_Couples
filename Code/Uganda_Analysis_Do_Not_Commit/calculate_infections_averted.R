###########################################
### SDP intervention runs:
### Compute % of infections averted
### to baseline
###########################################

rm(list=ls())

## data
load(file="ug_sdp_total_inf_comp_wci.RData")

## SDP current
total.inf.bl.cp.persim <- apply(total.inf.bl.cp, 2, sum)
total.inf.sdp.curr.persim <- apply(total.inf.sdp.curr, 2, sum)
total.inf.sdp.high.persim <- apply(total.inf.sdp.high, 2, sum)
total.inf.sdp.curr.nodecui.persim <- apply(total.inf.sdp.curr.nodecui, 2, sum)

prop.averted.sdp.curr.persim <- (total.inf.bl.cp.persim-
                                     total.inf.sdp.curr.persim)/
                                         (total.inf.bl.cp.persim)

prop.averted.sdp.high.persim <- (total.inf.bl.cp.persim-
                                     total.inf.sdp.high.persim)/
                                         (total.inf.bl.cp.persim)

prop.averted.sdp.curr.nodecui.persim <- (total.inf.bl.cp.persim-
                                             total.inf.sdp.curr.nodecui.persim)/
                                                 (total.inf.bl.cp.persim)

mean(prop.averted.sdp.curr.persim)
mean(prop.averted.sdp.high.persim)
mean(prop.averted.sdp.curr.nodecui.persim)

sd(prop.averted.sdp.curr.persim)*dt(0.975, df=n.sim-1)/sqrt(n.sim)
sd(prop.averted.sdp.high.persim)*dt(0.975, df=n.sim-1)/sqrt(n.sim)
sd(prop.averted.sdp.curr.nodecui.persim)*dt(0.975, df=n.sim-1)/sqrt(n.sim)

