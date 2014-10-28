##############################################################
### Test and Treat Sero-discordant Partners: Wrapper function 
##############################################################


##############################################################
### Progression of versions
##############################################################
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
                                  sdp.testing.coverage,
                                  sdp.art.at.cd4,
                                  sdp.art.coverage,
                                  time,
                                  ...
                                  ){
       

  ## Extract cross-sectional network
     nw <- network.extract(nw, at=time,
                           retain.all.vertices=TRUE)
     
  ## Extract relevant node attributes
     inf.status <- nw%v%"inf.status"
     cd4.count.today <- nw%v%"cd4.count.today"
     art.status <- nw%v%"art.status"

  ## Extract relevant edge attributes
     longest.ptshp <- nw%e%"longest.ptshp"
     known.sdp <- nw%e%"known.sdp"

  ## Extract edgelist, convert to matrix form
     nw.el <- as.edgelist(nw, retain.all.vertices = T)
     status.el <- matrix((nw %v% "inf.status")[nw.el],
                          ncol = 2)

  ## identify couples eligible for test-and-treat
     partner.cum.status <- rowSums(status.el)
     longest.ptshp.check <- intersect(which(partner.cum.status == 1),
                                    which(longest.ptshp == 1))
     longest.ptshp.check <- intersect(longest.ptshp.check,
                                    which(known.sdp) != 1)
                                    ) #if a couple is already "known SDP",
                                      #then test&treat routine does not apply

  ## test-sdp 
     test.el <- matrix(0,
                       nrow=nrow(status.el),
                       ncol=ncol(status.el))

     for(i in 1:length(longest.ptshp.check)){
       couple <- longest.ptshp.check[i]
       test.el[couple, 1] <- rbinom(1, 1, #first ptn in sdp
                                    sdp.testing.coverage)
       test.el[couple, 2] <- rbinom(1, 1, #sec ptn in sdp
                                    sdp.testing.coverage)
       
     }

     known.sdp <- which(rowSums(test.el) == 2)


   ## treat-sdp
       for (i in 1:length(known.sdp)){
         pos.partner <- which(status.el[known.sdp[i],] == 1)
         pos.ind <- nw.el[known.sdp[i],pos.partner]
         if (cd4.count.today[pos.ind] <= sdp.art.at.cd4){
           if (art.status[pos.ind] != 1){
             art.status[pos.ind] <- rbinom(1, 1,
                                           sdp.art.coverage)
           }
         }
       }

  ## Update vertex attributes
     nw%v%"art.status" <- art.status #16oct14

  ## Update new edge attributes
     ## nw%e%"longest.ptshp" <- longest.ptshp
     set.edge.attribute(nw, "known.sdp",
                        1, e=known.sdp)
                        
     return(nw)
   }
##############################################################

