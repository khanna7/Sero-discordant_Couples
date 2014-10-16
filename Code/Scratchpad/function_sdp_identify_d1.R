##############################################################
### Wrapper Function to Identify Sero-Discordant Couples
### and Update ART Status of Infected Partners
##############################################################

##############################################################
### Progression of versions
##############################################################
### 7 Oct 2014: Write wrapper function

### 16 Oct 2014:
    ## a. The identification of primary SDP and test&treat
          ## for them should  be separate. Comment out change
          ## in ART status here
##############################################################

identify.sdp.update.art <- function(nw,
                                    verbose,
                                    sdp.coverage,
                                    time,
                                    ...
                                    ){

    ## Edgelist from cross-sectional network
       nw.el <- as.edgelist(network.extract(nw, at = time,
                                            retain.all.vertices = T))

    ## Relevant attributes of actors
       status.el <- matrix((nw %v% "inf.status")[nw.el], ncol = 2)
       inf.status <- nw %v% "inf.status"
       art.status <- nw%v%"art.status"

    ## Relevant attributes of edges
       primary.sdp <- nw%e%"primary.sdp"

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
         nw.el.activity <- unlist(get.edge.activity(
                                    network.extract(nw,
                                                    at = time,
                                                    retain.all.vertices = T))
                                  )
         nw.el.form.diss <- matrix(nw.el.activity,
                                   nrow=length(nw.el.activity)/2,
                                   ncol=2, byrow=TRUE)
         not.inf <- which(nw.el.form.diss[,2] != Inf)
         nw.el.form.diss <- nw.el.form.diss[-not.inf,]
         nw.el.form.diss <- cbind(nw.el, nw.el.form.diss)

       ## for longest partnership of HIV-infected partners
       ## set "primary.sdp"=1
       
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
         ##   coin.toss <- runif(1, 0, 1) ##16OCt14: comment out
         ##   if (verbose){
         ##     cat("Coin-toss for men is ", coin.toss, "\n")
         ##   }
         ##   if (coin.toss <= sdp.coverage){
         ##   art.status[nw.el[edgeID[sht.pt], 1]] <- 1
         ##   if (verbose){
         ##     cat("ART-statuses to be updated are ",
         ##         (nw.el[edgeID[sht.pt], 1]), "\n")
         ##   }
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
         ##    coin.toss <- runif(1, 0, 1) ##16Oct14: comment out
         ##    if (verbose){
         ##      cat("Coin-toss for women is ", coin.toss, "\n")
         ##    }
         ##    if (coin.toss <= sdp.coverage){
         ##      art.status[nw.el[edgeID[sht.pt], 2]] <- 1
         ##      if (verbose){
         ##        cat("ART-statuses to be updated are ",
         ##            (nw.el[edgeID[sht.pt], 2]), "\n")
         ##      }
         ## }
         }

       ## update relevant attributes 
       ## nw%v%"art.status" <- art.status ##16OCt14:comment out
       nw%e%"primary.sdp" <- primary.sdp
       
       ## return network object
       return(nw)
     }
