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

identify.longest.ptshp           <- function(nw,
                                    verbose,
                                    time,
                                    ...
                                    ){

       ## nw <- as.network(nw)  
       ## Complete network
       nw.act.df <- get.edge.activity(nw, e=seq_along(nw$mel),
                                         as.spellList=TRUE)

       extant.edges <- (which(nw.act.df$terminus.censored == TRUE))

       nw.act.extedge.df <- nw.act.df[extant.edges,]
       nw.el <- cbind(nw.act.extedge.df$head,
                      nw.act.extedge.df$tail)
       
       ## Cross-sectional network
       nw.xn <- network.extract(nw, at=time, retain.all.vertices=T)
       nw.xn.el <- as.edgelist(nw.xn)

       el.to.consider <- intersect(
                           which(nw.act.extedge.df$tail %in% nw.xn.el[,1]),
                           which(nw.act.extedge.df$head %in% nw.xn.el[,2])
                           )

       ## remove extraneous edges that appear in "edge.activity" list
       nw.el.to.consider <- nw.el[el.to.consider,]
       nw.act.extedge.to.consider <- nw.act.extedge.df[el.to.consider,]

       ## Relevant attributes of actors
       status.el <- matrix((nw %v% "inf.status")[nw.el], ncol = 2)
       inf.status <- nw %v% "inf.status"
       art.status <- nw%v%"art.status"

       ## Relevant attributes of edges
       longest.ptshp <- nw%e%"longest.ptshp"


       ## Find edgeIDs of edges thare are sole partnership of the actors

       ## unique.male <- which(!duplicated(nw.act.extedge.to.consider$tail))
       ## unique.female <- which(!duplicated(nw.act.extedge.to.consider$head))

       ## unique.pt <- intersect(unique.male, unique.female)
       ## raw.unique.pt.edge.id <- nw.act.extedge.to.consider$edge.id[unique.pt]
       ##               ## edgeID's start at 4000, for whatever reason
                               
       ## nw <- set.edge.attribute(nw, attrname="longest.ptshp",
       ##                          1,
       ##                          raw.unique.pt.edge.id)
       
       ## For edges that belong to actors with multiple partnerships,
       ## find shortest partnership

        ## edge id's of longest partnerships from male perspective
           male.ptshp <- split(nw.act.extedge.to.consider,
                               f=nw.act.extedge.to.consider$tail)

           primary.from.male <- rep(NA, length(male.ptshp))

           for (i in 1:length(primary.from.male)){
             df <- male.ptshp[[i]]
             ranking <- rank(df$onset, ties.method='min')
             primary.from.male[i] <- df[min(pmin(ranking)),]$edge.id
           }

        ## edge id's of longest partnerships from female perspective
           female.ptshp <- split(nw.act.extedge.to.consider,
                               f=nw.act.extedge.to.consider$head)

           primary.from.female <- rep(NA, length(female.ptshp))

           for (i in 1:length(primary.from.female)){
             df <- female.ptshp[[i]]
             ranking <- rank(df$onset, ties.method='min')
             primary.from.female[i] <- df[min(pmin(ranking)),]$edge.id
           }

       ## update relevant attributes xn network
          nw <- set.edge.attribute(nw, attrname="longest.ptshp",
                                   1,
                                   primary.from.male)

          nw <- set.edge.attribute(nw, attrname="longest.ptshp",
                                   1,
                                   primary.from.female)
       
       ## return network object
          return(nw)
     }
