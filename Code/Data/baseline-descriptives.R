## Baseline descriptives

   library(statnet)

   ## Uganda
   load("burnin.21Feb_UG_phi004_newatts_run1.RData")
   ug.nw <- nw

   ug.net <- network.collapse(ug.nw, at=1040)

   ug.inf <- (which(ug.net%v%"inf.status" == 1))
   ug.on.art <- (which(ug.net%v%"art.status" == 1))
   ug.noton.art <- (which(ug.net%v%"art.status" == 0))
   ug.cd4.less.350 <- (which(ug.net%v%"cd4.count.today" <= 350))

   ug.inf.noton.art <- intersect(ug.inf, ug.noton.art)
   ug.inf.cd4less350 <- (intersect(ug.inf,ug.cd4.less.350))
   ug.inf.art.elig <- intersect(ug.inf.cd4less350,
                                ug.noton.art)

   len.ug.inf <- length(ug.inf)
   len.ug.on.art <- length(ug.on.art)
   len.ug.cd4.less.350 <- length(ug.cd4.less.350)
   len.ug.noton.art <- length(ug.noton.art)
   len.ug.inf.noton.art <- length(ug.inf.noton.art)
   len.ug.cd4.less.350 <- length(ug.cd4.350)
   len.ug.inf.art.elig <- length(ug.inf.art.elig)

   len.ug.on.art/len.ug.inf
   len.ug.inf.noton.art/len.ug.inf
   len.ug.inf.art.elig/len.ug.inf

   intersect(ug.inf.art.elig, #elig means who are not currently on ART
             ug.on.art)

   ## South Africa
   load("burnin.12March_ZA_phi004_incdur1000_newatts_30yrburn_run1.RData")
   za.nw <- nw

   za.net <- network.collapse(za.nw, at=1040)

   za.inf <- (which(za.net%v%"inf.status" == 1))
   za.on.art <- (which(za.net%v%"art.status" == 1))
   za.noton.art <- (which(za.net%v%"art.status" == 0))
   za.cd4.less.350 <- (which(za.net%v%"cd4.count.today" <= 350))

   za.inf.noton.art <- intersect(za.inf, za.noton.art)
   za.inf.cd4less350 <- (intersect(za.inf,za.cd4.less.350))
   za.inf.art.elig <- intersect(za.inf.cd4less350,
                                za.noton.art)

   len.za.inf <- length(za.inf)
   len.za.on.art <- length(za.on.art)
   len.za.cd4.less.350 <- length(za.cd4.less.350)
   len.za.noton.art <- length(za.noton.art)
   len.za.inf.noton.art <- length(za.inf.noton.art)
   len.za.cd4.less.350 <- length(za.cd4.less.350)
   len.za.inf.art.elig <- length(za.inf.art.elig)

   len.za.on.art/len.za.inf
   len.za.inf.noton.art/len.za.inf
   len.za.inf.art.elig/len.za.inf

   intersect(za.inf.art.elig, #elig means who are not currently on ART
             za.on.art)
