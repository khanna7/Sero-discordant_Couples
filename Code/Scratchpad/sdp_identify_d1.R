##############################################################
### Identify Sero-discordant couples
##############################################################


##############################################################
### Progression of versions
##############################################################
### 25 Sep 2014
    ## a. Read in burnin data

### 16 Oct 2014:
    ## a. The identification of primary SDP and test&treat
          ## for them should  be separate. Comment out change
          ## in ART status here
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
         status.el <- matrix((nw %v% "inf.status")[nw.el],
                             ncol = 2)
         inf.status <- nw %v% "inf.status"
         art.status <- nw%v%"art.status"

      ## Relevant attributes of edges
         primary.sdp <- nw%e%"primary.sdp"

      ## Parameter for SDP coverage
         sdp.coverage <- 0.80

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
         sdp.coverage <- 1
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
         ##   coin.toss <- runif(1, 0, 1) #16oct14:commented out
         ##   cat("Coin-toss for men is ", coin.toss, "\n")
         ##   if (coin.toss <= sdp.coverage){
         ##   art.status[nw.el[edgeID[sht.pt], 1]] <- 1
         ##   cat("ART-statuses to be updated are ",
         ##       (nw.el[edgeID[sht.pt], 1]), "\n")
         ## }
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
           ## coin.toss <- runif(1, 0, 1) #16oct14:commented out
           ## cat("Coin-toss for women is ", coin.toss, "\n")
           ## if (coin.toss <= sdp.coverage){
           ## art.status[nw.el[edgeID[sht.pt], 2]] <- 1
           ## cat("ART-statuses to be updated are ",
           ##     (nw.el[edgeID[sht.pt], 2]), "\n")
         }
         } 

         ## to check, following code can be used
            ## Set sdp.coverage=1
            ## (nw%v%"sex")[11093] #sex of +ve partner
            ## (nw%v%"inf.status")[11093] #infection status
            ## (nw%v%"art.status")[11093] #old ART status
            ## (art.status)[11093] #updated ART status
      
         ## nw%v%"art.status" <- art.status #16oct14
         nw%e%"primary.sdp" <- primary.sdp
##############################################################

