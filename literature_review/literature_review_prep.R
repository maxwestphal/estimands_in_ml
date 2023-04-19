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
#   literature_review_prep.R
###
#   Remarks:
#
###



# Definition of terms -------------------------------------------------------------------------

## define inclusion terms:
incl_1a <- c("clinical prediction", "clinical risk", "clinical risk prediction",
             "diagnosis", "diagnostic", "prognosis", "prognostic")
incl_1b <- c("rule", "model", "score", "algorithm")
incl_1 <- build_terms(incl_1a, incl_1b, end="[Title]", link=" OR ") 


incl_2a <- incl_1a %>% 
  build_terms(end="[Title]", link=" OR ")
incl_2b <- c("machine learning", "deep learning", "artificial intelligence") %>% 
  build_terms(end="[Title]", link=" OR ")
incl_2 <- compose_terms(incl_2a, incl_2b, link=" AND ")

incl <- compose_terms(incl_1, incl_2, link=" OR ") 
incl


## define search terms:
search_0 <- ""

search_1 <-  
  c("generalizability", "transportability", "transferability") %>% 
  build_terms_list()

search_2a <- c("intended", "designated", "planned")
search_2b <- c("context", "setting", 
               "use", "usage", "utilization", "application", "implementation")
search_2 <- build_terms_list(search_2a, search_2b)

search_3a <- c("external",
               "temporal", 
               "methodological",
               "spatial", "regional", "geographic", "locational", 
               "multicentre", "multi-centre", "multicenter", "multi-center")
search_3b <- c("validation",
               "CV", "cross validation", "cross-validation",  
               "hold-out validation", "holdout validation", 
               "evaluation", "assessment",
               "validity")
search_3 <- build_terms_list(search_3a, search_3b)

search_4 <- c("estimand") %>% 
  build_terms_list()

search_list <- c(search_0,
                 search_1,
                 search_2,
                 search_3,
                 search_4)
length(search_list)

search_full <- search_list[-1] %>% 
  as.character() %>% 
  {do.call(paste, c(., list(sep = " OR ")))} %>% 
  paste0("(", ., ")")
search_full

## define time period:
#period <- '(2002/1/1:2021/12/31[pdat])'
period <- '(("2002/01/01"[Date - Publication] : "2021/12/31"[Date - Publication]))'
year_list <- 2021:2022 # TODO; 2001:2022
period_list <- sapply(year_list, year_to_period)


## compile list of all queries (= inclusion criteria + single query+ period)
query_list <- sapply(search_list, function(s) compose_terms(incl, s, period, link=" AND "))
str(query_list, 1)

## query for inclusion criteria only:
query_incl <- compose_terms(incl, period, link=" AND ")
writeClipboard(query_incl)

## full query (any of the search terms):
query_full <- compose_terms(incl, search_full, period, link=" AND ")
writeClipboard(query_full)


## set retmax (=upper bound for next searches):


tags <-  list(incl = search_0, grp1 = search_1, grp2 = search_2,
              grp3 = search_3, grp4 = search_4)

## define grid of metadata for all queries
query_grd <- 
  expand.grid(search_id = 1:length(search_list),
              year = year_list) %>% 
  mutate(period = year_to_period(year),
         search = search_list[search_id],
         search_word = stringr::str_match(search, '"(.*?)\"')[,2],
         search_word = tidyr::replace_na(search_word, ""),
         search_tag = sapply(search, get_search_tag, tags = tags),
         query_id = 1:n())

head(query_grd)
dim(query_grd)

readr::write_csv(query_grd, file.path(dir$main, "query_grd.csv"))
