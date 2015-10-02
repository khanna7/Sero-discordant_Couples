## Baseline descriptives

   library(statnet)

   ## South Africa
   load("../Uganda_Runs_Do_Not_Commit/interv.5Apr_UG_sdp_scenarioIV_run1.RData")
   ug.nw <- nw

   ug.net <- network.collapse(ug.nw, at=1300)

   ug.inf <- (which(ug.net%v%"inf.status" == 1))
   ug.on.art <- (which(ug.net%v%"art.status" == 1))
   ug.noton.art <- (which(ug.net%v%"art.status" == 0))
   ug.cd4.less.350 <- (which(ug.net%v%"cd4.count.today" <= 350))
   ug.opta.ceased <- (which(ug.net%v%"art.type" == 3))
   ug.born.last.yr <- (which(ug.net%v%"age" <= 19))

   ug.inf.noton.art <- intersect(ug.inf, ug.noton.art)
   ug.inf.cd4less350 <- (intersect(ug.inf,ug.cd4.less.350))
   ug.inf.art.elig <- intersect(ug.inf.cd4less350,
                                ug.noton.art)

   ug.inf.born.last.yr <- intersect(ug.inf,
                                    ug.born.last.yr)

   len.ug.inf <- length(ug.inf)
   len.ug.on.art <- length(ug.on.art)
   len.ug.cd4.less.350 <- length(ug.cd4.less.350)
   len.ug.noton.art <- length(ug.noton.art)
   len.ug.inf.noton.art <- length(ug.inf.noton.art)
   len.ug.cd4.less.350 <- length(ug.cd4.less.350)
   len.ug.inf.art.elig <- length(ug.inf.art.elig)
   len.ug.opta.ceased <- length(ug.opta.ceased)
   len.ug.born.last.yr <- length(ug.born.last.yr)
   len.ug.inf.born.last.yr <- length(ug.inf.born.last.yr)

   len.ug.on.art/len.ug.inf
   len.ug.inf.noton.art/len.ug.inf
   len.ug.inf.art.elig/len.ug.inf
   length(ug.opta.ceased)/len.ug.inf
   len.ug.inf.born.last.yr/len.ug.born.last.yr

   intersect(ug.inf.art.elig, #elig means who are not currently on ART
             ug.on.art)

