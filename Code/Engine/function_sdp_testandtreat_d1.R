##############################################################
### Test and Treat Sero-discordant Partners: Wrapper function 
##############################################################


##############################################################
### Progression of versions
##############################################################
### 26 Oct 2014:
    ## a. We should not be extracting only cross-sectional network
    ## at the top and calling it "nw." That changes the network object
    ## "nw" that all processes are run on. Only edgelist is needed.

    ## b. Test for all via HBHTC: then treat for known-SDP & not-known-SDP

### 17 Oct 2014: Wrapper function

### 16 Oct 2014: Test and treat SDP:
    ## a. For primary SDP, each partner tests w/ 80% probability
    ## b. If both partners are tested,
          ## if positive partner's CD4 meets criteria, then
             ## positive partner initiates ART w/ coverage rate

    ## c. check below if "longest.ptshp.check" should be intersected
    ## with "known.sdp"
##############################################################


##############################################################
  ## Function
     testandtreat.sdp <- function(nw,
                                  verbose,
                                  hbhtc.testing.coverage,
                                  known.sdp.art.coverage,
                                  notknown.sdp.art.coverage,
                                  known.sdp.art.at.cd4,
                                  notknown.sdp.art.at.cd4,
                                  time,
                                  ...
                                  ){
       

  ## Extract relevant node attributes
     inf.status <- nw%v%"inf.status"
     cd4.count.today <- nw%v%"cd4.count.today"
     art.status <- nw%v%"art.status"
     art.type <- nw%v%"art.type"

  ## Extract relevant edge attributes
     longest.ptshp <- nw%e%"longest.ptshp"
     known.sdp <- nw%e%"known.sdp"

  ## Extract edgelist, convert to matrix form
     nw.el <- as.edgelist(nw, retain.all.vertices = T)
     status.el <- matrix((nw %v% "inf.status")[nw.el],
                          ncol = 2)

 ###########################################################
 ############# TEST FOR ALL VIA HBHTC
 ###########################################################
     
  ## Identify population size
     pop.size <- network.size(nw)
     
  ## testing all
     tested.today <- rbinom(n=pop.size, size=1,
                            prob=hbhtc.testing.coverage)

 ###########################################################
  ############# TREAT  KNOWN-SDP VIA HBHTC
 ###########################################################
     
  ## identify couples eligible for test-and-treat
     partner.cum.status <- rowSums(status.el)
     longest.ptshp.check <- intersect(which(partner.cum.status == 1),
                                    which(longest.ptshp == 1))

  ## test-sdp 
     test.el <- matrix(0,
                       nrow=nrow(nw.el),
                       ncol=ncol(nw.el))

     ## browser()
     for(i in 1:nrow(nw.el)){
       test.el[i, 1] <- tested.today[nw.el[i, 1]]
       test.el[i, 2] <- tested.today[nw.el[i, 2]]
     }

     known.sdp <- intersect(which(rowSums(status.el) == 1),
                            which(rowSums(test.el) == 2))


   ## treat known-sdp
     if(length(known.sdp) > 0){
       for (i in 1:length(known.sdp)){
         pos.partner <- which(status.el[known.sdp[i],] == 1)
         pos.ind <- nw.el[known.sdp[i],pos.partner]
         if (cd4.count.today[pos.ind] <= known.sdp.art.at.cd4){
           if (art.status[pos.ind] != 1){
             coin.flip <- rbinom(1, 1, known.sdp.art.coverage)
             art.status[pos.ind] <- coin.flip
             if (coin.flip == 1){
               art.type[pos.ind] <- 1
             } 
           }
         }
       }
     }

###########################################################
  ############# TREAT  NOT-KNOWN-SDP VIA HBHTC
 ###########################################################
   ## browser()
     
   notknown.sdp <- intersect(which(rowSums(status.el) == 1),
                               which(rowSums(test.el) == 1))
     
    ## treat not-known-sdp
       if (length(notknown.sdp) > 0){
       for (i in 1:length(notknown.sdp)){
         pos.partner <- which(status.el[notknown.sdp[i],] == 1)
         pos.ind <- nw.el[notknown.sdp[i],pos.partner]
         if (cd4.count.today[pos.ind] <= notknown.sdp.art.at.cd4){
           if (art.status[pos.ind] != 1){
             coin.flip <- rbinom(1, 1, notknown.sdp.art.coverage)
             art.status[pos.ind] <- coin.flip
             if (coin.flip == 1){
               art.type[pos.ind] <- 1
             } 
           }
         }
       }
     }
     
  ## Update vertex attributes
     nw%v%"art.status" <- art.status #16oct14
     nw%v%"art.type" <- art.type #16oct14

  ## Update new edge attributes
     ## nw%e%"longest.ptshp" <- longest.ptshp
     set.edge.attribute(nw, "known.sdp",
                        1, e=known.sdp)
                        
     return(nw)
   }
##############################################################

