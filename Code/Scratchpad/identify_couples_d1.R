##############################################################
### Identify Sero-discordant couples
##############################################################


##############################################################
### Progression of versions
##############################################################
### 25 Sep 2014
    ## a. Read in burnin data

##############################################################

##############################################################
## Steps
##############################################################

   ## Load libraries

   ## Read in burnin data

   ## Enlist serodiscordant partnerships,
      ##find positive partner, find positive partners'
      ##longest partnership
##############################################################

##############################################################

   ## Load libraries
      library(tergm)
      library(ergm) 
      library(network) 
      library(networkDynamic) 
      
##############################################################

##############################################################
   ## Read in burnin data
      load("../../Burnin_Data/burnin.21Feb_UG_phi004_newatts_run1.RData")
      ug.nw <- network.extract(nw, at=1041)

      load("../../Burnin_Data/burnin.12March_ZA_phi004_incdur1000_newatts_30yrburn_run1.RData")
      za.nw <- network.extract(nw, at=781)

##############################################################

##############################################################
   ## Uganda: Enlist serodiscodant partnerships
         nw <- ug.nw

      ## Edgelist
         nw.el <- as.edgelist(nw, retain.all.vertices = T)

      ## Relevant attributes of actors
         status.el <- matrix((nw %v% "inf.status")[nw.el], ncol = 2)
         inf.time <- nw %v% "inf.time"
         time.since.infection <- nw %v% "time.since.infection" # ASK

         inf.status <- nw %v% "inf.status"
         circum.status <- nw %v% "circum.status" # ASK
         curr.pregnancy.status <- nw %v% "curr.pregnancy.status" # ASK
         art.status <- nw %v% "art.status" # ASK
         infectivity.today <- nw %v% "infectivity.today" # ASK
         age <- nw%v%"age" #22Aug2013
         sex <- nw%v%"sex" ## 30 Oct 2013 -- more attribute inf recorded
         cd4.count.today <- nw%v%"cd4.count.today" ## for understanding detail about infectors
         viral.load.today <- nw%v%"viral.load.today"
         preg.status <- nw%v%"curr.pregnancy.status"
         circum.status <- nw%v%"circum.status"
         time.since.infection <- nw%v%"time.since.infection"
         art.status <- nw%v%"art.status"
         art.type <- nw%v%"art.type"

      ## Male partner HIV+
         discordant.mpos <- intersect(which(status.el[, 1] == 1),
                                      which(status.el[, 2] == 0))

         transmittable.m <- nw.el[discordant.mpos, 1]
         infectible.f <- nw.el[discordant.mpos, 2]

      ## Female partner HIV+
         discordant.fpos <- intersect(which(status.el[, 2] == 1),
                                    which(status.el[, 1] == 0)
                                    )
         transmittable.f <- nw.el[discordant.fpos, 2]
         infectible.m    <- nw.el[discordant.fpos, 1]

      ## positively serodiscordant men with multiple partners
         sdp.m.conc.p <- which(duplicated(transmittable.m))
         ptns.sdp.m.conc.p <-
  
      ## positively serodiscordant women with multiple partners
         sdp.f.conc.p <- transmittable.f[which(duplicated(transmittable.f))]

##############################################################
