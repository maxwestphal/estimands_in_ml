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
#   literature_review_funs.R
###
#   Remarks:
#   TODO: convert to R package
###



# Custom functions ----------------------------------------------------------------------------
build_terms <- function(..., end = "[Text Word]", link= " OR "){
  do.call(expand.grid, list(...)) %>% 
    #do.call(expand.grid, input) %>%
    apply(1, paste0, collapse=' ') %>% 
    expand.grid('("', ., '"', end, ')') %>% 
    apply(1, paste0, collapse='') %>%
    paste0(collapse = link) %>% 
    paste0('(', ., ')')
}

build_terms_list <- function(..., end = "[Text Word]"){
  do.call(expand.grid, list(...)) %>% 
    apply(1, paste0, collapse=' ') %>% 
    expand.grid('("', ., '"', end, ')') %>% 
    apply(1, paste0, collapse='') %>%
    as.list()
}

compose_terms <- function(..., link=" AND "){
  list(...) %>% 
    #input %>% 
    expand.grid() %>% 
    apply(1, paste0, collapse = link) %>% 
    paste0('(', ., ')')
}

query_to_table <- function(query, retstart=0, retmax=100, target_file=NULL){
  
  pm_ids <- get_pubmed_ids(query)
  
  if(pm_ids$Count != "0"){
    pm_data <- fetch_pubmed_data(pm_ids, 
                                 retstart=retstart, retmax=retmax,
                                 format="xml", encoding="UTF8")
    pm_table <- table_articles_byAuth(pm_data, 
                                      included_authors = "first", max_chars=1000, 
                                      autofill = FALSE, dest_file = NULL,
                                      getKeywords = FALSE, encoding="UTF8")
    
    message(paste0("Done: Extracted ", nrow(pm_table), " entries from PubMed."))
    
    if(is.character(target_file)){
      readr::write_csv(pm_table, target_file)
      return(invisible())
    }
  
  }else{
    message(paste0("Done: Extracted 0 entries from PubMed."))
    pm_table <- NULL
  }
  
  return(pm_table)
  
}

year_to_period <- function(year=2021, end='[Date - Publication]'){
  paste0('(("', year, '/01/01"', end, ' : "', year, '/12/31"', end, '))')
}

int_to_char <- function(i = 1, digits=5){
  paste0(c(rep("0", digits-nchar(as.character(i))), i), collapse = "")
}

## TODO:
get_search_tag <- function(search, 
                           tags = list(incl = search_0, grp1 = search_1, grp2 = search_2,
                                       grp3 = search_3, grp4 = search_4)){
  tag <- names(which(sapply(tags, function(x) any(x %in% search))))
  stopifnot(length(tag) %in% 0:1)
  if(length(tag)==0){tag <- NA}
  return(tag)
} 

ids_str_to_words <- function(ids_str, max_hits=10, grid_pos, digits=3){
  
  words <- grid_pos %>% filter(search_id %in% ids_str_to_search_ids(ids_str)) %>% {.$search_word} 
  
  df <- c(rep("", max_hits-length(words)), rev(words)) %>% t() %>% as.data.frame()
  names(df) <- paste0("match", sapply(max_hits:1, int_to_char, digits=3))
  df    
}

ids_str_to_search_ids <- function(ids_str, sep="|"){
  strsplit(ids_str, sep, fixed=TRUE)[[1]] %>% as.integer()
}


read_extract <- function(id, extdir, prefix="pubmed_extract_query_"){
  readr::read_csv(file.path(extdir, paste0(prefix, int_to_char(id), ".csv")))
}


setup_annotation <- function(grid, 
                             yyyy = 2004, 
                             dir = dir){
  
  ## filter for year
  grid_yyyy <- grid %>% filter(year == yyyy) %>% 
    mutate(search_word = tidyr::replace_na(search_word, ""))
  
  ## inlcusion (all) vs. found specific search term (pos)
  grid_all <- grid_yyyy %>% filter(search_word=="")
  grid_pos <- grid_yyyy %>% filter(search_word!="")
  
  stopifnot(nrow(grid_all) == 1)
  
  df_all <- read_extract(grid_all$query_id, dir$extract) %>% 
    select(-keywords, - address, -email) 
  
  found_ids <- list.files(dir$extract) %>% stringr::str_match("[0-9]+") %>% as.numeric()
  found_ids_yyyy <- intersect(grid_pos$query_id, found_ids)
  
  df_pos <- lapply(found_ids_yyyy, function(id){
    read_extract(id, dir$extract) %>% 
      mutate(search_id = grid %>% filter(query_id == id) %>% select(search_id) %>% as.integer(),
             year_ctrl = grid %>% filter(query_id == id) %>% select(year) %>% as.integer()) 
  }) %>% 
    do.call(rbind, .) %>% 
    group_by(across(c(-search_id))) %>% 
    summarize(search_ids=paste0(search_id, collapse = "|")) %>% 
    mutate(num_hits = length(ids_str_to_search_ids(search_ids))) %>% 
    mutate(ids_str_to_words(search_ids, max_hits=max(num_hits), grid_pos, 3)) %>% 
    mutate(exclude = "", no_pdf="", 
           val_only = "", 
           unconditional="",
           estimand ="",
           esti_site = "", esti_region="", esti_time = "", esti_process="", esti_setting="",
           estimator = "",
           remark = "",
           done = "")
  
  ## negative hits, i.e. included but not search term hit (neg)
  df_neg <- df_all %>% 
    filter( !(pmid %in% df_pos$pmid) ) %>% 
    mutate(exclude = "", no_pdf="")
  
  ## consistency check:
  stopifnot(nrow(df_all) == nrow(df_neg)+nrow(df_pos))
  
  ## setup and create directories:
  save_dir <- file.path(dir$annotation, yyyy)
  match_dir <- file.path(save_dir, "match")
  nomatch_dir <- file.path(save_dir, "nomatch")
  
  dir.create(save_dir)
  dir.create(match_dir)
  dir.create(nomatch_dir)
  
  ## save csv files for annotation
  readr::write_csv(df_pos, file.path(match_dir, paste0("annotation_", yyyy, "_match", ".csv")))
  readr::write_csv2(df_pos, file.path(match_dir, paste0("annotation_", yyyy, "_match_2", ".csv")))
  
  readr::write_csv(df_neg, file.path(nomatch_dir, paste0("annotation_", yyyy, "_nomatch", ".csv")))
  readr::write_csv2(df_neg, file.path(nomatch_dir, paste0("annotation_", yyyy, "_nomatch_2", ".csv")))
  
  
  ## save queries to txt files for pubmed searches:
  period <- year_to_period(yyyy)
  
  query_incl <- compose_terms(incl, period, link=" AND ") # TODO: SHOULD be 1117 but is only 999
  query_pos <- compose_terms(incl, search_full, period, link=" AND ")  # TODO: 86
  query_neg <- compose_terms(incl, period, link=" AND ") %>% compose_terms(search_full, link=" NOT ") # TODO: 1031
  
  readr::write_file(query_incl, file.path(save_dir, paste0("query_", yyyy, "_incl.txt")))
  readr::write_file(query_pos, file.path(match_dir, paste0("query_", yyyy, "_match.txt")))
  readr::write_file(query_neg, file.path(nomatch_dir, paste0("query_", yyyy, "_nomatch.txt")))
  
  return(invisible())
}
