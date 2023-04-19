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
#   literature_review_pdfscan.R
###
#   Remarks:
###




# PDF scan ------------------------------------------------------------------------------------

dir$pdf <- "C:/Users/maxwe/Desktop/testpdf"


fl <- list.files(dir$pdf, full.names = TRUE, recursive=TRUE)


?pdftools::pdf_text

i <- 1
path <- fl[i]

pt <- pdftools::pdf_text(path)
pt <- paste(pt, collapse = " ") 


pi <- pdftools::pdf_info(path)
pi$keys$doi
pi$keys$Author
pi$keys$Title

query_grd$search_word %>% unique()

## TODO
# rewrite extract workflow as follows:
#   based on INCL-only, download PDF
#   then, BASED PDF TEXT: match vs. nomatch 

?stringr::str_match()
