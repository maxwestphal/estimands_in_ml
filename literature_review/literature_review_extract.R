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
#   literature_review_extract.R
###
#   Remarks:
#
###



# PubMed extraction ---------------------------------------------------------------------------

retmax <- 10000

## query all individual findings:
for(i in 1:nrow(query_grd)){
  message(paste("Start: Processing query", i, "out of", nrow(query_grd), "..."))
  query <- compose_terms(incl, query_grd[i, "search"], query_grd[i, "period"], link=" AND ")

  target_file <- file.path(dir$extract, paste0("pubmed_extract_query_", int_to_char(query_grd[i, "query_id"]), ".csv"))
  query_to_table(query, retstart=0, retmax=retmax, target_file=target_file)
}



# query_grd %>% filter(year==2021, search_word=="")
# 
# i = 2251
# i = 2376











