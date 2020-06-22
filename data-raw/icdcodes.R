
# Make ICD datasets -------------------------------------------------------

library(XML)

ukb7232_tables <- readHTMLTable(
  "../ukbiobank/activity2/data-raw/ukb7232.html",
  stringsAsFactors = FALSE
)


icd9 <- ukb7232_tables[[7]]
icd9codes <- icd9[, c("Code", "Meaning")]
names(icd9codes) <- tolower(names(icd9codes))

icd10 <- ukb7232_tables[[6]]
icd10codes <- icd10[, c("Code", "Meaning")]
names(icd10codes) <- tolower(names(icd10codes))

rm(ukb7232_tables)


# Replace non-ASCII characters

showNonASCII(icd10codes$meaning)
# 12926: T36.1 Cefalosporins and other <c2><af>-lactam antibiotics
# 13007: T44.5 Predominantly <c2><af>-adrenoreceptor agonists, not elsewhere classified
# 13009: T44.7 <c2><af>-Adrenoreceeptor antagonists, not elsewhere classified
# 16818: Y51.5 Predominantly <c2><af>-adrenoreceptor agonists, not elsewhere classified
# 16820: Y51.7 <c2><af>-Adrenoreceptor antagonists, not elsewhere classified

icd10codes$meaning <- iconv(icd10codes$meaning, "UTF-8", "ASCII", sub = "beta")
icd10codes$meaning <- str_replace_all(x, "betabeta", "beta")
icd10codes$meaning[c(12926, 13007, 13009, 16818, 16820)]
# [1] "T36.1 Cefalosporins and other beta-lactam antibiotics"
# [2] "T44.5 Predominantly beta-adrenoreceptor agonists, not elsewhere classified"
# [3] "T44.7 beta-Adrenoreceeptor antagonists, not elsewhere classified"
# [4] "Y51.5 Predominantly beta-adrenoreceptor agonists, not elsewhere classified"
# [5] "Y51.7 beta-Adrenoreceptor antagonists, not elsewhere classified"

# icd10codes$meaning <- stringi::stri_trans_general(icd10codes$meaning, "latin-ascii")


usethis::use_data(icd9codes, overwrite = TRUE)
usethis::use_data(icd10codes, overwrite = TRUE)
