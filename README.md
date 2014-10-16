## How to install required packages

To reinstall needed packages, in MOSIX3, go to:

~/Rlibrary/src (i.e. /homes/Rlibrary/src), and then

R CMD INSTALL ergm_3.1-0.tar.gz  
R CMD INSTALL tergm_3.1.1.tar.gz ....
and so on...

## Scenario Table


|    Population                                           | **ALL**                 | **ALL**              | **ALL**                      | **NOT SDP**               | **NOT SDP**      | **NOT SDP**                 | **SDP**                   | **SDP**          | **SDP**                     | **SDP**                              |
|---------------------------------------------------------|-------------------------|----------------------|-----------------------------|---------------------------|------------------|-----------------------------|---------------------------|------------------|-----------------------------|--------------------------------------|
|    Scenario                                             |    Testing Frequency    |    Testing Uptake    |    PMTCT                    |    CD4 ART eligibility    |    ART uptake    |    % Virally Suppressed     |    ART CD4 eligibility    |    ART uptake    |    % Virally Suppressed     |    % Reduction in unprotected sex    |
|    Baseline                                             |    N/A                  |    N/A               |    Option A, current        |    350                    |    40%           |    UG: 88%   SA: 85%        |    350                    |    40%           |    UG: 88%   SA: 85%        |    N/A                               |
|    HB-HCT Only (Baseline 2)                             |    3 years              |    80%               |    Option A, current        |    350                    |    58.4%         |    UG: 88%   SA: 85%        |    350                    |    58.4%         |    UG: 88%   SA: 85%        |    63%                               |
|    HB-HCT with immediate ART for SDC (Main Analysis)    |    3 years              |    80%               |    Option A, current        |    350                    |    58.4%         |    UG: 88%   SA: 85%        |    Any CD4                |    90%           |    UG: 88%   SA: 85%        |    63%                               |


**Serodiscordant Partnership (SDP) Intervention**   
  * If both partners have tested and are serodiscordant:    
    a. initiate behavior change (all)    
    b. initiate ART (at scenario-specific uptake level (58.4% or 90%) and 
       eligibility criteria (CD4 count<350 or any CD4 count)  
  
  * If only one partner has tested   
    a. do nothing (treat the same as the rest of the population who are 
       not in a known SDC)
       
**Limitations**    
   * We did not model increased rates of relationship dissolution in SDCs   
   * We assumed the probability of each partner in a relationship getting tested  
     for HIV was independent of the other partner getting tested  
   * We assume disclosure between partners occurs 100% of the time that both partners test  

**Notes and Definition**
  Please see detailed chart [here]().

   
   
