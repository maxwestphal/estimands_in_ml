###
#   Project: 
#   Estimands in ML algorithm evaluation (estimands_in_ml)
###
#   Author: 
#   Max Westphal (max.westphal@mevis.fraunhofer.de)
###
#   Date: 
#   2022-03-14
###
#   Script:
#   literature_review_annotation.R
###
#   Remarks:
#
###



# Setup files for annotation ------------------------------------------------------------------
query_grd <- readr::read_csv(file.path(dir$main, "query_grd.csv"))

for(yyyy in year_list){
  message("Setup for year ", yyyy, "...")
  setup_annotation(query_grd, yyyy=yyyy, dir=dir)
}






