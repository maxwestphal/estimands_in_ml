###
#   Project: 
#   Estimands in ML algorithm evaluation (estimands_in_ml)
###
#   Author: 
#   Max Westphal (max.westphal@mevis.fraunhofer.de)
###
#   Date: 
#   2022-03-15
###
#   Script:
#   simulation_study_exp.R
###
#   Remarks:
#
###

library(dplyr)


## big scenario
# regions, sites, years, processes, n per strata
c(4, 6, 5, 2, 250) %>% prod

# small scenario
c(1, 4, 5, 3, 250) %>% prod


# Experiments ---------------------------------------------------------------------------------




