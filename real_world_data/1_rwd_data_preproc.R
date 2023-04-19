# Preparation ---------------------------------------------------------------------------------
source("_setup.R")

library(dplyr)
library(missMethods)


# Data import ---------------------------------------------------------------------------------

# data source: https://datashare.ed.ac.uk/handle/10283/124 
# -> "Download all files" to file.path(rwd_dir, "data_raw")

## get path (do not change)
rwd_data_raw_file <- file.path(rwd_dir, "data_raw", "IST_corrected.csv")

# "" = NA, "U"/"u"/"UNKNOWN" = unknown, "C" = can't assess
# in RCONSC "U" = unconscious (and no missing values)
data_raw <- readr::read_csv(rwd_data_raw_file, na = c("", "U", "u", "UNKNOWN", "C")) %>% 
  mutate(RCONSC = replace(RCONSC, is.na(RCONSC), "U"))

# TODO (MW): implement this via opendata..

# Data processing -----------------------------------------------------------------------------

## variable selection:
data <- data_raw %>% 
  ## select variables:
  select(
    c(## Hostpital ID (for site splitting):
      1,
      ## country (for geographic splitting):
      82, 
      ## date (month and year) of randomization (only up to 48 hours after stroke, so used for temporal splitting):
      22, 
      ## prediction target 1: indicator of death within 14 days
      94,
      ## prediction target 2: indicator of death within 6 months
      70,
      
      ## features for prediction of death within 14 days and 6 months (collected at randomization):
      3, # conscious state at randomization
      4, # sex
      5, # age
      6, # symptoms noted on waking
      7, # atrial fibrillation
      8, # CT before randomization
      9, # infarct visible on CT
      10, 11, # heparin within 24 hours and aspirin within 3 days prior to randomization
      12, # systolic blood pressure at randomization
      13:20, # indicator of different deficits
      21, # stroke subtype
      26, 27, # indicator trial aspirin and dose trial heparin allocated
      
      ## features only for prediction of death within 6 months (collected on 14 days/discharge):
      28, # aspirin given for 14 days or till death or discharge
      29, # discharged on long term aspirin
      30:32, # low dose, medium dose during study and medium dose during pilot phase of heparin given for 14 days or till death or discharge
      33, # estimate of days on trial treatment (0-14)
      34:43, # indicator of non-trial anticoagulants an other blood thinning and risk if stroke reducing medicine and procedures
      #44, 47, # indicator of major non-cerebral haemorrhage and other side effect
      50:53, # final diagnosis of initial event
      #55, 57, 59, 61, # indicators of different recurrent stroke types and pulmonary embolism within 14 days
      86, 87) # compliant for aspirin and heparin
  ) %>% 
  ## 156 patients whose death status is not known at 14 days or 6 months are omitted:
  filter(!(is.na(ID14)|is.na(FDEAD))) %>%
  ## transform characters to factors:
  mutate_if(is.character, as.factor) %>% 
  as.data.frame()


## excluded variables (ordered by appearance in IST_variables.pdf):
# - delay between stroke and randomization, weekday and time in hours and minutes of randomization 
# - dates of and/or comments on: major non-cerebral haemorrhage, other side effects, 
#     final diagnosis not a stroke, all types of recurrent strokes within 14 days,
#     pulmonary embolism within 14 days and the outcome
#     (all found in categories "... within/on 14 days", but their dates are between 0 and over 390...) 
    # TODO (RA/MW): check whether dates are in days after randomization and (some) features can be included 
    # (currently commented out in the variable selection above)
# - discharge destination, alive/dead on discharge with its date, cause and comment of death 
# - all data collected at 6 months (prediction latest at 14 days)
# - info about discharge form, numeric country code
# - coding of compliance (compliance to aspirin and heparin included, but for coding lots of missing values)
# - indicator for death (overall) and time of censoring
# - predicted probabilities of death at 14 days and 6 months
# - known to be dead after 14 days (as outcome assumes knowledge), 6 month outcome and long term aspirin
# - all indicator variables for specific causes of death (again, only interested in whether dead or alive after 14 days/6 months)

## as recommended in IST_variables.pdf: 
# merge DHH14 and DHM14 as they measure the same but DHH14 was used only during pilot phase
# and change "H" to "M" in trial heparin allocated, as "H" is used in pilot phase and "M" afterwards
data <- data  %>% 
  mutate(DMH14 = recode(ifelse(is.na(DMH14), DHH14, DMH14), "1"="N", "2"="Y")) %>% 
  select(-DHH14) %>% 
  mutate(RXHEP = recode(RXHEP, H="M")) 

# for "thrombolysis Y/N" only 14 entries Y, so often leads to errors during prediction
# => exclusion
data <- select(data, -DTHROMB)

# Convert boolean variables to logical --------------------------------------------------------
detect_logical <- function(x, true="Y", false="N"){
  all(x %in% c(true, false) | is.na(x))
}

convert_to_logical <- function(x, true="Y", false="N"){
  stopifnot(detect_logical(x, true=true, false=false))
  return(x == true)
}

## detect logical variables:
apply(data, 2, detect_logical)

## investigate remaining variables:
summary(data %>% select(where(~ ! detect_logical(.x))))
# -> DASP14 coding error: one observation each with "n"/"y" instead of "N"/"Y"
# -> ID14 coded as 0/1 and not "N"/"Y"

## convert to logical:
data <- data %>% 
  mutate(DASP14 = recode(DASP14, n="N", y="Y")) %>% 
  mutate_if(detect_logical, convert_to_logical) %>% 
  mutate(ID14 = convert_to_logical(as.character(ID14), true = "1", false= "0"))


# Convert RDATE to YEAR, MONTH ----------------------------------------------------------------


## split date into year and month according to
# january = styczeń, february = luty, march = marzec, april = kwiecień ,
# may = maj, june = czerwiec, july = lipiec, august = sierpień,
# september = wrzesień , october = październik, november = listopad 
# december = grudzień (translated from Polish)

months_polish <- c("sty", "lut", "mar", "kwi", "maj", "cze", "lip", "sie", "wrz", "paz", "lis", "gru")

data <- data %>% # 
  # process as characters:
  mutate(RDATE = as.character(RDATE)) %>% 
  # get rid of strange character:
  mutate(RDATE = sub("pa\x9f", "paz", data$RDATE)) %>% 
  # separate date into month, year:
  tidyr::separate(RDATE, into=c("MONTH", "YEAR"), sep="-", remove=FALSE) %>% 
  mutate(YEAR = paste0("19", YEAR), .before=RDATE) %>% 
  mutate(YEAR = as.numeric(YEAR)) %>% 
  mutate(MONTH = sapply(MONTH, function(x) which(months_polish == x)), .before=RDATE) %>% 
  # remove RDATE:
  select(-RDATE) %>% 
  # remove initial two years ('pilot' phase)
  dplyr::filter(YEAR >= 1993) %>% 
  dplyr::mutate(HOSPNUM = trimws(HOSPNUM)) %>% 
  dplyr::mutate(MONTH = trimws(MONTH))

  

## TODO (RA): please check once more, that all vars have correct class (then, line below can be deleted)
## currently all good, but not finished discussion which variables to include
cbind(names(data), sapply(seq_len(ncol(data)), function(j) class(data[,j])))


# Data export for outcome ID14 --------------------------------------------------------------------------
## convention: name data.frames data_[outcome]_[missing]



data_ID14 <- select(data, -c(6, 28:ncol(data)))
## missing data per column
apply(data_ID14, 2, function(x) sum(is.na(x)))
apply(data_ID14, 2, function(x) mean(is.na(x)))*100 # in percent

## (1) complete case analysis
# especially, all 981 patients left from pilot phase (year 91, 92 and beginning of 93) 
# will be omitted, because not all features were measured yet
# from 19279 patients to 12983 patients
# all 26 columns stay
# from 6 to 4 years
data_ID14_cc <- na.omit(data_ID14)
dim(data_ID14_cc)
table(data_ID14_cc$YEAR)
export_data("data_ID14_cc")



## (2) complete case analysis after removing columns with > 5% missing data
# from 19279 patients to 18012 patients
# from 26 columns to 20 columns
# from 6 to 5 years
data_ID14_cc05 <- data_ID14 %>% 
  select(which(apply(data_ID14, 2, function(x) mean(is.na(x))) < 0.05)) %>% 
  na.omit()
dim(data_ID14_cc05)
table(data_ID14_cc05$YEAR)
export_data("data_ID14_cc05")


## (3) median/mode imputation
# all 19279 patients, 26 columns and 6 years stay
data_ID14_mode <- data_ID14 %>% 
  mutate_if(~is.numeric(.x), ~ifelse(is.na(.x), median(.x, na.rm = T), .x)) %>% 
  impute_mode()
table(data_ID14_mode$YEAR)
export_data("data_ID14_mode")


# Data export for outcome FDEAD --------------------------------------------------------------------------
data_FDEAD <- select(data, -"ID14")
## missing data per column
apply(data_FDEAD, 2, function(x) sum(is.na(x)))
apply(data_FDEAD, 2, function(x) mean(is.na(x)))*100 # in percent

## (1) complete case analysis
# especially, all 981 patients left from pilot phase (year 91, 92 and beginning of 93) 
# will be omitted, because not all features were measured yet
# from 19279 patients to 12712 patients
# all 48 columns stay
data_FDEAD_cc <- na.omit(data_FDEAD)
dim(data_FDEAD_cc)
export_data("data_FDEAD_cc")


## (2) complete case analysis after removing columns with > 5% missing data
# from 19279 patients to 17615 patients
# from 48 columns to 41 columns
data_FDEAD_cc05 <- data_FDEAD %>% 
  select(which(apply(data_FDEAD, 2, function(x) mean(is.na(x))) < 0.05)) %>% 
  na.omit()
dim(data_FDEAD_cc05)
export_data("data_FDEAD_cc05")


## (3) median/mode imputation
# all 19279 patients and 48 columns stay
data_FDEAD_mode <- data_FDEAD %>% 
  mutate_if(~is.numeric(.x), ~ifelse(is.na(.x), median(.x, na.rm = T), .x)) %>% 
  impute_mode() 
export_data("data_FDEAD_mode")

