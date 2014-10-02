# Notes for computation

## Identify serodiscordant couples

### Follow the steps below.

*Before any intervention runs*,  
  * Add placeholder for edge-attribute "primary_sdp" for primary serodiscordant partnerships.  
    
```R
     set.edge.attribute(x, attrname, value, e=seq_along(x$mel))
     set.edge.value(x, attrname, value, e=seq_along(x$mel))
     ## x is a network object
```

At time of HBHCT,   
  * Identify all serodiscordant partnerships  
  * Check which HIV-infected actors in serodiscordant
    partnerships have >1 partnership at that time  
  * Of these partnerships, idenfify the longest running partnership 
  * Set indicator for primary serodiscordant 
    partneships  (primary_sdp = 1) for these partnerships.
    
