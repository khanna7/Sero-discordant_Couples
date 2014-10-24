# Data to run simulations for SDP interventions.

   **Datasets needed** 
   ---------------
  - [ ] Uganda Burnin data: With relevant edge attributes added   
  - [ ] South African data: With relevant edge attributes added    
        Edge attributes are:      
          - Primary SDP: ```set.edge.attribute(nw, "primary.sdp", 0) ```       
          - Known SDP:```set.edge.attribute(nw, "known.sdp", 0) ```      
  - [ ] Parameter files for Uganda and South Africa: With any new parameters for South Africa and Uganda   
          *Identify SDPs* (new)   
           ```nw=nw```   
           ```verbose=TRUE```   
           ```sdp.coverage=0.80```    
           ```time=time```   
          *Test and treat function* (new)   
            ```nw=nw```      
            ```verbose=TRUE```              
            ```sdp.testing.coverage=0.80```      
            ```sdp.art.at.coverage=350```   
            ```time=time```   
          *Transmission function* (modified from before)      
             - Decline in unprotected intercourse for known SDPs: ```decline.ui=0.63```
          

  
       
