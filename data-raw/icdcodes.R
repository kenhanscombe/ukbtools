
# Make ICD datasets -------------------------------------------------------

library(XML)

ukb7232_tables <- readHTMLTable(
  "inst/extdata/ukb7232.html",
  stringsAsFactors = FALSE)


icd9 <- ukb7232_tables[[7]]
icd9codes <- icd9[, c("Code", "Meaning")]
names(icd9codes) <- tolower(names(icd9codes))

icd10 <- ukb7232_tables[[6]]
icd10codes <- icd10[, c("Code", "Meaning")]
names(icd10codes) <- tolower(names(icd10codes))

rm(ukb7232_tables)


devtools::use_data(icd9codes, overwrite = TRUE)
devtools::use_data(icd10codes, overwrite = TRUE)
