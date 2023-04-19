# Preparation ---------------------------------------------------------------------------------
source("_setup.R")
source(file.path("real_world_data", "0_rwd_functions.R"))

library(dplyr)


# Functions -----------------------------------------------------------------------------------
## split_data return list of trn/val splits
split_data <- function(data, split_type = "cv_10", seed=NULL){
  set.seed(seed)
  do.call(paste0("split_data_", split_type), args=list(data=data))
}

split_data_cv <- function(data, k){
  n <- nrow(data)
  folds <- caret::createFolds(1:n, k = k)
  splits <- lapply(folds, function(x) list(trn = setdiff(1:n, x), val=x))
  names(splits) <- NULL
  return(splits)
}

split_data_cv_10 <- function(data){
  split_data_cv(data=data, k=10)
}

split_data_cv_5 <- function(data){
  split_data_cv(data=data, k=5)
}

split_data_cv_4 <- function(data){
  split_data_cv(data=data, k=4)
}

split_data_country_diff <- function(data){
  folds <- lapply(unique(data$COUNTRY), function(x) which(data$COUNTRY == x))
  #which(sapply(folds, length) == 0)
  
  splits <- lapply(folds, function(x) list(trn = setdiff(1:nrow(data), x), val=x)) 
  return(splits)
}

split_data_year_diff <- function(data){
  folds <- lapply(unique(data$YEAR), function(x) which(data$YEAR == x))
  splits <- lapply(folds, function(x) list(trn = setdiff(1:nrow(data), x), val=x))
  return(splits)
}

split_data_year_lag1 <- function(data){
  lapply(unique(data$YEAR), function(x) {
    list(trn = which(data$YEAR == x - 1), val = which(data$YEAR == x))
  }) 
}

split_data_uk <- function(data, yearlag = c(1,2), sitediff = TRUE){ 
  
  grd <- expand.grid(YEAR = 1993:1996,
                     HOSPNUM = c(1,  11,   4,   6,  33,  73,  85,  74,  34,  89,  32,  72,
                                 39,  78,  83, 119,  81, 134, 406))
  
  splits <- lapply(1:nrow(grd), function(i){
    y <- grd[i, "YEAR"]; 
    hn <- grd[i, "HOSPNUM"];
    
    if( sitediff){sitelogi <- data$HOSPNUM != hn};
    if(!sitediff){sitelogi <- data$HOSPNUM == hn};
    
    list(trn = which(((y - data$YEAR) %in% yearlag) & sitelogi & data$COUNTRY == "UK"), 
         val = which(data$YEAR == y & data$HOSPNUM == hn & data$COUNTRY == "UK"))
  }) %>% 
    filter_splits()
  
  return(splits)
}

split_data_uk_cv_4 <- function(data){ 
  
  split_data_cv_4(data %>% filter(COUNTRY == "UK"))
  
}

split_data_uk_cv_5 <- function(data){ 
  
  split_data_cv_5(data %>% filter(COUNTRY == "UK"))
  
}

split_data_uk_cv_10 <- function(data){ 
  
  split_data_cv_10(data %>% filter(COUNTRY == "UK"))
  
}

split_data_uk_ss_y1 <- function(data){ 
  
  split_data_uk(data, yearlag = c(1), sitediff = FALSE)
  
}

split_data_uk_ss_y1 <- function(data){ 
  
  split_data_uk(data, yearlag = c(1), sitediff = FALSE)
  
}

split_data_uk_ss_y2 <- function(data){
  
  split_data_uk(data, yearlag = c(1,2), sitediff = FALSE)
  
}

split_data_uk_ss_y3 <- function(data){
  
  split_data_uk(data, yearlag = c(1,2,3), sitediff = FALSE)
  
}

split_data_uk_sd_y1 <- function(data){ 
  
  split_data_uk(data, yearlag = c(1), sitediff = TRUE)
  
}

split_data_uk_sd_y2 <- function(data){
  
  split_data_uk(data, yearlag = c(1,2), sitediff = TRUE)
  
}

split_data_uk_sd_y3 <- function(data){
  
  split_data_uk(data, yearlag = c(1,2, 3), sitediff = TRUE)
  
}


split_data_uk_sd_y0 <- function(data){
  
  split_data_uk(data, yearlag = c(0), sitediff = TRUE)
  
  return(splits)
}






# split_data_country5_site_diff <- function(){
#   cc <- c("UK", "ITAL", "SWIT", "POLA", "NETH")
#   
#   d <- data %>%
#     filter(COUNTRY %in% cc) %>%
#     group_by(HOSPNUM) %>%
#     filter(n() >= 150) %>%
#     select(COUNTRY, HOSPNUM, YEAR) %>% 
#     group_by(COUNTRY) %>% 
#     summarize(nn=n())
#   
#   d
#   unique(d$COUNTRY)
# }
# 
# split_data_country5_site_same <- function(){
#   
# }


# Data Splits -------------------------------------------------------------

# TODO(RA): check/understand functions for data splitting in 0_rwd_functions.R
# (Idea: splits_desc only needs only 1 line update for new estimand + function def above)
# TODO(RA): check/test functions above

splits_desc <- list(
  list(split_type = "cv_4", seed=123, suffix="a"),
  list(split_type = "cv_5", seed=123, suffix="a"),
  list(split_type = "cv_10", seed=123, suffix="a"),
  list(split_type = "uk_cv_4"),
  list(split_type = "uk_cv_5"),
  list(split_type = "uk_cv_10"),
  list(split_type = "uk_sd_y1"),
  list(split_type = "uk_sd_y2"),
  list(split_type = "uk_sd_y3"),
  list(split_type = "uk_ss_y1"),
  list(split_type = "uk_ss_y2"),
  list(split_type = "uk_ss_y3"),
  list(split_type = "uk_sd_y0"),
  list(split_type = "country_diff"),
  list(split_type = "year_lag1")
)

## TODO (MW) 
## add split_data_ function above and entry in splits_desc for 
## (a) "year_lag1_country_diff": 1 year time lag, different country
## (b) "year_lag1_country_same": 1 year time lag, same country (for trn, val)
## (c) "year_lag1_site_diff": see (a) but for site (hospital)
## (d) "year_lag1_site_same": see (b) but for site (hospital)



## generate and save splits for all missings:

## (1) complete case (cc) analysis:
save_splits_list(rwd_dir, "ID14", "cc", splits_desc)
save_splits_list(rwd_dir, "FDEAD", "cc", splits_desc)

## (2) cc analysis after removing columns with > 5% missing data:
save_splits_list(rwd_dir, "ID14", "cc05", splits_desc)
save_splits_list(rwd_dir, "FDEAD", "cc05", splits_desc)

## (3) mode/median imputation:
save_splits_list(rwd_dir, "ID14", "mode", splits_desc)
save_splits_list(rwd_dir, "FDEAD", "mode", splits_desc)



# Test area -----------------------------------------------------------------------------------

### test individual functions:
# data <- import_data("FDEAD", "cc", rwd_dir)
# split_data(data, "cv_10", seed=123)
# split_data(data, "country_diff")
# split_data(data, "year_diff")
# split_data(data, "year_lag1")




# temporal-geographical split
## How? in first years not all countries, so use all available and in testing 
## excluding those countries? Or only one country in testing? For each country in test separately?
# c91 <- unique(as.character(df2[df2$YEAR == 91, "COUNTRY"])) # 5/36
# c92 <- unique(as.character(df2[df2$YEAR == 92, "COUNTRY"])) # 9/36
# c93 <- unique(as.character(df2[df2$YEAR == 93, "COUNTRY"])) # 23/36
# c94 <- unique(as.character(df2[df2$YEAR == 94, "COUNTRY"])) # 29/36
# c95 <- unique(as.character(df2[df2$YEAR == 95, "COUNTRY"])) # 35/36
# c96 <- unique(as.character(df2[df2$YEAR == 96, "COUNTRY"])) # 34/36



