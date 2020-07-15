# syntrial
## synthesize clinical trial data
From an clinical trial new synthetic data are generated. Synthetic patients are created by 

* randomly selecting a fixed number, default 3, of real source patients
* assign random weights to each source patient  
such that no source patients gets more weight than a certain limit, default 2/3
* take weighted means for continuous variables
* select master source patient for categorical variables,  
optionally adding noise per observation.

## SDTM
Clinical trial data are expected to be in [SDTM](https://www.cdisc.org/standards/foundational/sdtm)-like
format without RELREC tables. Adaptation to [ADAM](https://www.cdisc.org/standards/foundational/adam)
datasets should be straightforward.