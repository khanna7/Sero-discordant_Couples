# Notes for computation

## Identify serodiscordant couples

### Follow the steps below.

1. *Before any intervention runs*,  
   * Add placeholder for edge-attribute "primary.sdp" for primary serodiscordant partnerships.  
    
```R
     set.edge.attribute(nw, "primary.sdp", value=0)
     (nw%e%"primary.sdp")
     ## nw is a networkDynamic object -- created through network.extract()
     ## Note: in set.edge.attribute, if you add a fourth argument e=seq_along(x$mel), then 
     ## even inactive edges will be assigned a value of 0.
```

2. *At time of HBHCT*,   
   * Identify all serodiscordant partnerships
```R
      ## Edgelist
         nw.el <- as.edgelist(nw, retain.all.vertices = T)
      
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
         
      ## Retreive ART status attribute
        art.status <- nw%v%"art.status"
```  
  * Check which HIV-infected actors in serodiscordant
    partnerships have >1 partnership at that time. Of these partnerships, idenfify the longest running partnership
```R
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

                 
         nw%v%"art.status" <- art.status #update art status
         nw%e%"primary.sdp" <- primary.sdp #update edge-attribute "primary.sdp"
         
```

