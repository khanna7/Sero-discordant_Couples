# Notes for computation

## Identify serodiscordant couples

### Follow the steps below.

Before any intervention runs,  
  * Add placeholder for edge-attribute "primary_sdp" for primary serodiscordant partnerships.  
    
```R
     set.edge.attribute(x, attrname, value, e=seq_along(x$mel))
     set.edge.value(x, attrname, value, e=seq_along(x$mel))
```

At time of HBHCT,   
  * Identify all serodiscordant partnerships  
  * Check which actors have multiple partnerships  
  * Identify which partnerships have   
  * Add indicator for serodiscordant partneships  
    
