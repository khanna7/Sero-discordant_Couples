###########################################
### SDP intervention runs:
### Compute % of infections averted
### to baseline
###########################################

rm(list=ls())

## data
load(file="za_sdp_total_inf_comp_wci.RData")

## SDP current: %Infections averted over 10 years
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

#################################################################
## SDP current: %Infections averted ANNUALLY over 10 years
###########################################################

## Compute total number of infections per year per simulatino
   ## Columns show simulation number
   ## Rows show year: Index 0 is year 1, ..

bl.cp.total.inf.per.year.per.sim <- 
  apply(total.inf.bl.cp, 2, function (x){
    tapply( x , (seq_along(x)-1) %/% 26, sum)
  }
        )

sdp.curr.total.inf.per.year.per.sim <- 
  apply(total.inf.sdp.curr, 2, function (x){
    tapply( x , (seq_along(x)-1) %/% 26, sum)
  }
        )

sdp.high.total.inf.per.year.per.sim <- 
  apply(total.inf.sdp.high, 2, function (x){
    tapply( x , (seq_along(x)-1) %/% 26, sum)
  }
        )

sdp.curr.nodecui.total.inf.per.year.per.sim <- 
  apply(total.inf.sdp.curr.nodecui, 2, function (x){
    tapply( x , (seq_along(x)-1) %/% 26, sum)
  }
        )

## Now compute % averted per year per sim

   ## SDP Curr
   prop.averted.peryear.per.sim.sdp.curr <-
  (bl.cp.total.inf.per.year.per.sim -
    sdp.curr.total.inf.per.year.per.sim)/
     (bl.cp.total.inf.per.year.per.sim)

   ## SDP High
   prop.averted.peryear.per.sim.sdp.high <-
  (bl.cp.total.inf.per.year.per.sim -
    sdp.high.total.inf.per.year.per.sim)/
     (bl.cp.total.inf.per.year.per.sim)

   ## SDP Curr + No decline UI
   prop.averted.peryear.per.sim.sdp.curr.nodecui <-
  (bl.cp.total.inf.per.year.per.sim -
    sdp.curr.nodecui.total.inf.per.year.per.sim)/
     (bl.cp.total.inf.per.year.per.sim)


## Mean and Standard errors
   ## Mean
   apply(prop.averted.peryear.per.sim.sdp.curr, 1, mean)
   apply(prop.averted.peryear.per.sim.sdp.high, 1, mean)
   apply(prop.averted.peryear.per.sim.sdp.curr.nodecui, 1, mean)

   ## Standard Errors
   apply(prop.averted.peryear.per.sim.sdp.curr, 1, function(x) {
     sd(x)*dt(0.975, df=n.sim-1)/sqrt(n.sim)}
         )

   apply(prop.averted.peryear.per.sim.sdp.high, 1, function(x) {
     sd(x)*dt(0.975, df=n.sim-1)/sqrt(n.sim)}
         )

   apply(prop.averted.peryear.per.sim.sdp.curr.nodecui, 1, function(x) {
     sd(x)*dt(0.975, df=n.sim-1)/sqrt(n.sim)}
         )


########################################################
### Compare "curr sdp" to "high sdp" and add infections
### averted up to a time point
########################################################

add.elems <- function(x){
    y <- rep(0, length(x))
    y[1] <- x[1]

    for (i in 2:length(y)){
        y[i] <- sum(x[1:i])
    }
    return(y)
}


## cumulative number of infections upto a year
## in bl.cp
cum.bl.cp.upto.year <- apply(bl.cp.total.inf.per.year.per.sim,
                             2,
                             function(x) {add.elems(x)}
                             )

cum.sdp.curr.upto.year <- apply(sdp.curr.total.inf.per.year.per.sim,
                                2,
                                function(x) {add.elems(x)}
                                )

cum.sdp.high.upto.year <- apply(sdp.high.total.inf.per.year.per.sim,
                                2,
                                function(x) {add.elems(x)}
                                )

curr.relto.bl <- cum.bl.cp.upto.year - cum.sdp.curr.upto.year
high.relto.bl <- cum.bl.cp.upto.year - cum.sdp.high.upto.year

prop.averted.high.relto.curr <-
    (high.relto.bl -
         curr.relto.bl)/(high.relto.bl) #this is correct
## high rel tobl > curr relto bl
## we want to compute what % more are averted in high
## than curr

apply(prop.averted.high.relto.curr, 1, mean)
########################################################
