library(statnet)

load("nw28Oct_UG_sdp_trial_run1.RData")
nw <- network(nw)

infected.dur.interv <- which((nw%v%"time.of.infection") > 1042)
head(infected.dur.interv)

(nw%v%"sex")[infected.dur.interv]
table((nw%v%"sex")[infected.dur.interv])

(nw%v%"inf.status")[infected.dur.interv]
table((nw%v%"inf.status")[infected.dur.interv])

(nw%v%"art.status")[infected.dur.interv]
table((nw%v%"art.status")[infected.dur.interv])

(nw%v%"art.type")[infected.dur.interv]
table((nw%v%"art.type")[infected.dur.interv], exclude=NULL)

(nw%v%"time.of.art.initiation")[infected.dur.interv]
summary((nw%v%"time.of.art.initiation")[infected.dur.interv])

(nw%v%"cd4.count.today")[infected.dur.interv]
summary((nw%v%"cd4.count.today")[infected.dur.interv])

(nw%v%"viral.load.today")[infected.dur.interv]
summary((nw%v%"viral.load.today")[infected.dur.interv])

xtabs(~((nw%v%"sex")[infected.dur.interv])+
      ((nw%v%"art.status")[infected.dur.interv]))

xtabs(~((nw%v%"sex")[infected.dur.interv])+
      ((nw%v%"art.type")[infected.dur.interv]))
