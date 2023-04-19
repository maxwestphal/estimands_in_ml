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
#   literature_review_init.R
###
#   Remarks:
#
###



# Preparation ---------------------------------------------------------------------------------

## easyPubMed vignette:
# https://cran.r-project.org/web/packages/easyPubMed/vignettes/getting_started_with_easyPubMed.html

# load packages
library(dplyr)
library(easyPubMed)

## define list of directory:
dir <- list()
dir$main <- "1_literature_review"
dir$files <- "E:/estimands_in_ml_files"
dir$extract <- file.path(dir$files, "extract")
dir$annotation <- file.path(dir$files, "annotation")

## create all directories:
sapply(dir, dir.create)

## load customs functions:
source(file.path(dir$main, "literature_review_funs.R"))