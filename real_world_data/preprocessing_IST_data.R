library(dplyr)
library(caret)

# data also used by Takada et al. 2021
# setwd("//gaia/fme/home/ralpers/public/Estimands/IST")
#setwd("E:\\estimands_in_ml\\data\\IST")

# df <- read.csv("IST_corrected.csv", 
#                na.strings = c("", "U", "u", "UNKNOWN", "C")) # "" = NA, "U"/"u"/"UNKNOWN" = unknown, "C" = can't assess

df <- read.csv(file.path(rwd_dir, "data_raw", "IST_corrected.csv"),
               na.strings = c("", "U", "u", "UNKNOWN", "C")) # "" = NA, "U"/"u"/"UNKNOWN" = unknown, "C" = can't assess)
df$RCONSC <- replace(df$RCONSC, is.na(df$RCONSC), "U") # in RCONSC "U" = unconscious (and no missing values)



# Preprocessing -----------------------------------------------------------

# choice of variables
df2 <- df[!(is.na(df$ID14)), # 2 patients whose death status is not known are omitted
          c(82, # country (used for geographic splitting)
            22, # date (month and year) of randomization (only up to 48 hours after stroke, so used for temporal splitting)
            3:21, 26:44, 47, 50:53, 55, 57, 59, 61, 86, 87, # features for prediction
            94)] %>% # indicator of death at 14 days
  mutate_at(c(1, 3, 4, 6:11, 13:28, 30:52), as.factor) # factors for all non-numeric features

#
# review: NA filtering should be done with filter(!is.na(ID14)) for better code readability
# review: column exclusion should be done with select, e.g. (select(-82))
# review: mutate_at can be done simpler & less error-prone with mutate_if (e.g. mutate_if(class() %in% ...))

# excluded variables (ordered by appearance in IST_variables.pdf):
# - hospital number (would lead to error in geographic split case)
# - delay between stroke and randomization, weekday and time in hours and minutes of randomization 
# - dates of and/or comments on: major non-cerebral haemorrhage, other side effects, 
#     final diagnosis not a stroke, all types of recurrent strokes within 14 days,
#     pulmonary embolism within 14 days and the outcome
# - discharge destination, alive/dead on discharge with its date, cause and comment of death (all after 14 days) 
#     (as only interested in whether dead or alive after 14 days)
# - all data collected at 6 months (happens after outcome of interest)
# - info about discharge form, numeric country code
# - coding of compliance (compliance to aspirin and heparin included, but for coding lots of missing values)
# - indicator for death (overall) and time of censoring
# - predicted probabilities of death at 14 days and 6 months
# - known to be dead after 14 days (as outcome assumes knowledge), 6 month outcome and long term aspirin
# - all indicator variables for specific causes of death (again, only interested in whether dead or alive after 14 days)


# as recommended in IST_variables.pdf: 
# merge DHH14 and DHM14 as they measure the same but DHH14 was used only during pilot phase
df2$DMH14[1:984] <- df2$DHH14[1:984] 
df2 <- select(df2, -DHH14) 
# and change "H" to "M" in trial heparin allocated, as "H" is used in pilot phase and "M" afterwards
df2$RXHEP <- replace(df2$RXHEP, which(df2$RXHEP == "H"), "M")


# split date into year and month according to
# january = styczeń, february = luty, march = marzec, april = kwiecień ,
# may = maj, june = czerwiec, july = lipiec, august = sierpień,
# september = wrzesień , october = październik, november = listopad 
# december = grudzień (translated from Polish)
datesplit <- strsplit(df2$RDATE, "-")
year <- character()
month <- character()
for (i in 1:length(datesplit)) {
  month <- c(month, datesplit[[i]][1])
  year <- c(year, datesplit[[i]][2])
}
for (i in 1:length(month)){
  if (month[i] == "sty")
    month[i] = 1
  else if (month[i] == "lut")
    month[i] = 2
  else if (month[i] == "mar")
    month[i] = 3
  else if (month[i] == "kwi")
    month[i] = 4
  else if (month[i] == "maj")
    month[i] = 5
  else if (month[i] == "cze")
    month[i] = 6
  else if (month[i] == "lip")
    month[i] = 7
  else if (month[i] == "sie")
    month[i] = 8
  else if (month[i] == "wrz")
    month[i] = 9
  else if (month[i] == "pa<9f>")
    month[i] = 10
  else if (month[i] == "lis")
    month[i] = 11
  else if (month[i] == "gru")
    month[i] = 12
}

# review: this large if-else block can be simplfied, e.g. via switch (see ?switch)
# review: even simpler: sapply(month, which(month == c("sty", "lut", ...)), 

df2 <- mutate(df2, "YEAR" = year,
       "MONTH" = month, .before = RDATE) %>% 
  select(-RDATE)


# only one entry written as "n" and one as "y", assuming "N" and "Y" are meant
df2[323, "DASP14"] <- "N" 
df2[65, "DASP14"] <- "Y"


# for "thrombolysis Y/N" only 14 entries Y, so often leads to errors during prediction
df2 <- select(df2, -DTHROMB) 


# do complete case analysis
# from 19435 patients to 12815 patients
# all 984 patients in pilot phase (year 91 and 92) will be omitted because not all features were measured yet
df2 <- na.omit(df2)



write.csv2(df2, "BatchtoolSimulationsMW/IST_final_data.csv")

# Data Splits -------------------------------------------------------------

# random split
set.seed(927)
random_split_test <- createFolds(df2$ID14, k = 10)
random_split_train <- lapply(random_split_test, function(x) c(1:nrow(df2))[-x])

saveRDS(random_split_test, "random_split_test.RDS")
saveRDS(random_split_train, "random_split_train.RDS")

# geographic split
geographic_split_test <- lapply(unique(df2$COUNTRY), function(x) which(df2$COUNTRY == x))
geographic_split_train <- lapply(geographic_split_test, function(x) c(1:nrow(df2))[-x])
saveRDS(geographic_split_test, "geographic_split_test.RDS")
saveRDS(geographic_split_train, "geographic_split_train.RDS")

# temporal split
temporal_split_test <- lapply(94:96, function(x) which(df2$YEAR == x))
temporal_split_train <- lapply(93:95, function(x) which(df2$YEAR == x))
saveRDS(temporal_split_test, "temporal_split_test.RDS")
saveRDS(temporal_split_train, "temporal_split_train.RDS")

# temporal-geographical split
## How? in first years not all countries, so use all available and in testing 
## excluding those countries? Or only one country in testing? For each country in test separately?
# c91 <- unique(as.character(df2[df2$YEAR == 91, "COUNTRY"])) # 5/36
# c92 <- unique(as.character(df2[df2$YEAR == 92, "COUNTRY"])) # 9/36
# c93 <- unique(as.character(df2[df2$YEAR == 93, "COUNTRY"])) # 23/36
# c94 <- unique(as.character(df2[df2$YEAR == 94, "COUNTRY"])) # 29/36
# c95 <- unique(as.character(df2[df2$YEAR == 95, "COUNTRY"])) # 35/36
# c96 <- unique(as.character(df2[df2$YEAR == 96, "COUNTRY"])) # 34/36



# Exploring Data ----------------------------------------------------------

# Hospital IDs
length(unique(df$HOSPNUM)) # 466
table(df$HOSPNUM) # 1-524 patients each, mean 42

# Countries
length(unique(df$COUNTRY)) # 36 from different continents
table(df$COUNTRY) # 2-6257, mean 540

# Dates of Randomisation (month+year)
length(unique(df$RDATE)) # 65
table(df$RDATE) # year 91-96, 12 months in Polish (I guess), but one month called pa\x9f??

# Time Between Stroke and Randomisation in Hours
length(unique(df$RDELAY)) # 48
table(df$RDELAY) # 1-48 hours

# Subtype of Stroke
length(unique(df$STYPE)) # 5
table(df$STYPE) # 57 (for "other") - 7855, mean 3887

# Final Diagnosis of Initial Event
## Ischaemic Stroke
table(df$DDIAGISC) # Y = 17398, N = 1995, "" = 23  and "U" = 16 + "u" = 3??
## Haemorrhagic Stroke
table(df$DDIAGHA) # Y = 599, N = 18804, "" = 26  and "U" = 6
## Indeterminate Stroke
table(df$DDIAGUN) # Y = 992, N = 18414, "" =  23 and "U" = 6
## Not a Stroke
table(df$DNOSTRK) # Y = 420, N = 18983, "" = 26 and "U" = 6


# Potential Outcomes
## after 14 days: recurrent stroke, pulmonary embolism, death 

## after 6 months: dead (can find out date and cause), dependent, not recovered, recovered, unknown
table(df$OCCODE) # if known 3296-7884

