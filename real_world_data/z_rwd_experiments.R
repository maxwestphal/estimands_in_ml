## TODO: delete this file


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

# experiments for 1_rwd_data_preproc ----------------------------------------------------------

all(as.data.frame(df2) == as.data.frame(data))


names(df2) == names(data_test)

sapply(1:ncol(df2), function(i) class(df2[,i]))

cbind(names(df2), 
      sapply(1:ncol(df2), function(i) class(df2[,i])), 
      sapply(1:ncol(df2), function(i) class(data[,i]))) 

dim(df2); dim(data)
dim(na.omit(data_test))
dim(na.omit(df2))

sapply(names(df2), function(n) all(as.character(df2[,n]) == as.character(data_test[,n]))) ## TODO

sapply(names(df2), function(n) all(levels(df2[,n]) == levels(data_test[,n]))) ## TODO


levels(df2$RXHEP)
levels(data$RXHEP)

all(as.character(df2$RXHEP) == as.character(data$RXHEP))


sapply(names(df2), function(n) table(df2[,n], data_test[,n], useNA = "ifany"))

table(data$DMH14, data$DHH14, useNA="ifany")
