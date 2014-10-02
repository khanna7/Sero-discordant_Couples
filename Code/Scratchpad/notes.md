# Notes for computation

## Identify serodiscordant couples

### Follow the steps below.

*Before any intervention runs*,  
  * Add placeholder for edge-attribute "primary_sdp" for primary serodiscordant partnerships.  
    
```R
     set.edge.attribute(nw, "primary_sdp", value=0)
     (nw%e%"primary_sdp")
     ## nw is a networkDynamic object -- created through network.extract()
     ## Note: in set.edge.attribute, if you add a fourth argument e=seq_along(x$mel), then 
     ## even inactive edges will be assigned a value of 0.
```

*At time of HBHCT*,   
  * Identify all serodiscordant partnerships  
  * Check which HIV-infected actors in serodiscordant
    partnerships have >1 partnership at that time  
  * Of these partnerships, idenfify the longest running partnership 
  * Set indicator for primary serodiscordant 
    partneships  (`primary_sdp = 1`) for these partnerships.
    

*Identify serodiscordant partnerships*
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

In which SDPs, does the infected individual have more than 1 partner at a given time?


Identify longest running partnership for HIV-infected individuals in SD


Set indicator `primary_sdp = 1` 

