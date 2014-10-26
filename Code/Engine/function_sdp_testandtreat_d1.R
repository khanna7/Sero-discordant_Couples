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

    ## c. check below if "primary.sdp.check" should be intersected
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
     ## nw <- network.extract(nw, at=time,
     ##                       retain.all.vertices=TRUE)
     
  ## Extract relevant node attributes
     inf.status <- nw%v%"inf.status"
     cd4.count.today <- nw%v%"cd4.count.today"
     art.status <- nw%v%"art.status"
     art.type <- nw%v%"art.type"

  ## Extract relevant edge attributes
     primary.sdp <- nw%e%"primary.sdp"
     known.sdp <- nw%e%"known.sdp"

  ## Extract edgelist, convert to matrix form
     nw.el <- as.edgelist(nw, retain.all.vertices = T)
     status.el <- matrix((nw %v% "inf.status")[nw.el],
                          ncol = 2)

  ## identify couples eligible for test-and-treat
     partner.cum.status <- rowSums(status.el)
     primary.sdp.check <- intersect(which(partner.cum.status == 1),
                                    which(primary.sdp == 1))
     primary.sdp.check <- intersect(primary.sdp.check,
                                    which(known.sdp != 1))
                                      #if a couple is already "known SDP",
                                      #then test&treat routine does not apply

  ## test-sdp 
     test.el <- matrix(0,
                       nrow=nrow(status.el),
                       ncol=ncol(status.el))

     for(i in 1:length(primary.sdp.check)){
       couple <- primary.sdp.check[i]
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
             coin.flip <- rbinom(1, 1, sdp.art.coverage)
             art.status[pos.ind] <- coin.flip)
             if (coin.flip == 1){
               art.type[pos.ind] <- 1
             } 
           }
         }
       }

  ## Update vertex attributes
     nw%v%"art.status" <- art.status #16oct14
     nw%v%"art.type" <- art.type #16oct14

  ## Update new edge attributes
     ## nw%e%"primary.sdp" <- primary.sdp
     set.edge.attribute(nw, "known.sdp",
                        1, e=known.sdp)
                        
     return(nw)
   }
##############################################################

