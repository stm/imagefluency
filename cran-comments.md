## Test environments
* local OS X install, R 3.5.1
* local windows install, R version 3.6.1
* ubuntu 14.04 (on travis-ci), R version 3.5.1
* win-builder (devel and release)

## R CMD check results
There were no ERRORs or WARNINGs. 

There was 1 NOTE:

* checking R code for possible problems ... NOTE
  img_self_similarity: no visible binding for global variable '.x'
   Undefined global functions or variables:
     .x

  This is a spurious NOTE due to the (optional, default: FALSE) log-log plot
  in the img_self_similarity function. This function optionally displays a ggplot
  with log ticks at both axes using the scales package. The syntax in the scales
  package uses this undefined global variable .x
  (see https://ggplot2.tidyverse.org/reference/annotation_logticks.html). 

