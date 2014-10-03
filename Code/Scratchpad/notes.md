# Notes for computation

## Identify serodiscordant couples

### Follow the steps below.

1. *Before any intervention runs*,  
   * Add placeholder for edge-attribute "primary_sdp" for primary serodiscordant partnerships.  
    
```R
     set.edge.attribute(nw, "primary_sdp", value=0)
     (nw%e%"primary_sdp")
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

         nw.el.duration <- nw.el.form.diss[,2]-
                           nw.el.form.diss[,1]

      ## for longest partnership of HIV-infected partners
      ## set "primary_sdp"=1

         primary.sdp <- rep(NA, nrow(nw.el))

         sdp.m.1.p <- transmittable.m[which(duplicated(transmittable.m)==FALSE)] #males with 1 partner

         for(i in 1:length(sdp.m.1.p)){
           edgeID <- which(nw.el[,1] == sdp.m.1.p[i])
             cat("", "\n")
             cat(c(edgeID, min(edgeID), "\n"))
             cat(min(nw.el.form.diss[edgeID,1]), "\n")
             cat(pmin(nw.el.form.diss[edgeID,1]), "\n")
           } 

  * Set indicator for primary serodiscordant 
    partneships  (`primary_sdp = 1`) for these partnerships.
```

