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

         set.edge.attribute(nw, "primary.sdp", value=0)
         (nw%e%"primary.sdp")

      ## Edgelist
         nw.el <- as.edgelist(nw, retain.all.vertices = T)
         ## nw.el <- as.edgelist(nw)

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

      ## hiv-infected serodiscordant men with multiple partners
         sdp.m.conc.p <- transmittable.m[which(duplicated(transmittable.m))]
  
      ## hiv-infected serodiscordant women with multiple partners
         sdp.f.conc.p <- transmittable.f[which(duplicated(transmittable.f))]

      ## we now know which:
         ##1. hiv-infected serodiscordant men
         ##   have multiple partners, and,
         ##2. hiv-infected serodiscordant women
         ##   have multiple partners, and,

      ## find edgeIDs for multiple partnerships of
      ## HIV-infected men
         which((nw.el[,1]) == sdp.m.conc.p[1]) ###...
         which((nw.el[,1]) == sdp.m.conc.p[length(sdp.m.conc.p)])

      ## find edgeIDs for multiple partnerships of
      ## HIV-infected women
         which((nw.el[,2]) == sdp.f.conc.p[1]) ###...
         which((nw.el[,2]) == sdp.f.conc.p[length(sdp.f.conc.p)])

      ## now obtain formation, dissolution and
      ## duration from "egde.activity," 
         nw.el.activity <- unlist(get.edge.activity(nw))
         nw.el.form.diss <- matrix(nw.el.activity,
                                   nrow=length(nw.el.activity)/2,
                                   ncol=2, byrow=TRUE)
         not.inf <- which(nw.el.form.diss[,2] != Inf)
         nw.el.form.diss <- nw.el.form.diss[-not.inf,]
         nw.el.form.diss <- cbind(nw.el, nw.el.form.diss)
         
      ## for longest partnership of HIV-infected partners
      ## set "primary.sdp"=1

         primary.sdp <- rep(-1, nrow(nw.el))

         ## where infected partner is male
         for(i in 1:length(transmittable.m)){
           edgeID <- which(nw.el.form.diss[,1] ==
                           transmittable.m[i])
           sht.pt <- which(rank(nw.el.form.diss[edgeID,3],
                                ties.method='min') <= 1)
                     ## ptshp with lowest formation time
                     ## (all ptshps have diss. time of Inf)
           sht.pt <- min(sht.pt)
           cat(sht.pt, " ", edgeID[sht.pt], " ", "\n"
               )
           primary.sdp[edgeID[sht.pt]] <- 1
           ## update ART status
           art.status[nw.el[edgeID[sht.pt], 1]] <- 1
           cat("ART-statuses to be updated are ",
               (nw.el[edgeID[sht.pt], 1]), "\n")
         } 

         ## where infected partner is female
         for(i in 1:length(transmittable.f)){
           edgeID <- which(nw.el.form.diss[,2] ==
                           transmittable.f[i])
           sht.pt <- which(rank(nw.el.form.diss[edgeID,3],
                                ties.method='min') <= 1)
           sht.pt <- min(sht.pt)
           cat(sht.pt, " ", edgeID[sht.pt], " ", "\n"
               )
           primary.sdp[edgeID[sht.pt]] <- 1
           ## update ART status
           art.status[nw.el[edgeID[sht.pt], 2]] <- 1
           cat("ART-statuses to be updated are ",
               (nw.el[edgeID[sht.pt], 2]), "\n")
         } 

                 
         nw%v%"art.status" <- art.status

##############################################################

