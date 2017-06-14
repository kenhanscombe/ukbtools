## ----setup, include=FALSE------------------------------------------------

knitr::opts_chunk$set(
  fig.retina = 2,
  eval = FALSE,
  warning = FALSE,
  message = FALSE,
  comment = NA
)


## ------------------------------------------------------------------------
#  
#  my_ukb_data <- ukb_df("ukbxxxx")
#  

## ------------------------------------------------------------------------
#  
#  my_ukb_data <- ukb_df("ukbxxxx", path = "/full/path/to/my/fileset")
#  

## ------------------------------------------------------------------------
#  
#  ukbxxxx_data <- ukb_df("ukbxxxx")
#  ukbyyyy_data <- ukb_df("ukbyyyy")
#  ukbzzzz_data <- ukb_df("ukbzzzz")
#  
#  # Merge with your preferred method
#  my_ukb_data <- plyr::join_all(
#    list(ukbxxxx_data, ukbyyyy_data, ukbzzzz_data),
#    by = "eid",
#    type = "full"
#  )
#  

## ------------------------------------------------------------------------
#  
#  ukb_names <- c(
#    names(ukbxxxx_data),
#    names(ukbyyyy_data),
#    names(ukbzzzz_data)
#  )
#  
#  count_ukb_names <- data.frame(table(ukb_names))
#  dim(count_ukb_names[count_names$Freq > 1, ])
#  

## ------------------------------------------------------------------------
#  
#  ukb_context(my_ukb_data, nonmiss.var = "my_variable_of_interest")
#  

## ------------------------------------------------------------------------
#  
#  subgroup_of_interest <- (my_ukb_data$bmi > 40 & my_ukb_data$age < 50)
#  ukb_context(my_ukb_data, subset.var = subgroup_of_interest)
#  

## ------------------------------------------------------------------------
#  
#  ukb_icd_diagnosis(my_ukb_data, id = "0000000", icd.version = 10)
#  

## ------------------------------------------------------------------------
#  
#  ukb_icd_code_meaning(icd.code = "I74", icd.version = 10)
#  

## ------------------------------------------------------------------------
#  
#  ukb_icd_keyword("cardio", icd.version = 10)
#  

## ------------------------------------------------------------------------
#  
#  # ICD-10 code I74, Arterial embolism and thrombosis
#  ukb_icd_prevalence(my_ukb_data, icd.version = 10, icd.diagnosis = "I74")
#  
#  # ICD-10 chapter 9, disease block I00–I99, Diseases of the circulatory system
#  ukb_icd_prevalence(my_ukb_data, icd.version = 10, icd.diagnosis = "I")
#  
#  # ICD-10 chapter 2, C00-D49, Neoplasms
#  ukb_icd_prevalence(my_ukb_data, icd.version = 10, icd.diagnosis = "C|D[0-4].")
#  

## ------------------------------------------------------------------------
#  
#  my_gen_meta <- ukb_gen_meta(my_ukb_data)
#  my_gen_pcs <- ukb_gen_pcs(my_ukb_data)
#  

## ------------------------------------------------------------------------
#  
#  ukb_gen_excl(my_ukb_data)
#  ukb_gen_het(my_ukb_data)
#  

## ------------------------------------------------------------------------
#  
#  ukb_gen_het(my_ukb_data, all.het = TRUE)
#  

## ------------------------------------------------------------------------
#  
#  my_gen_rel <- ukb_gen_rel(my_ukb_data)
#  
#  # To get a count and plot of degree of relatedness
#  ukb_gen_rel_count(my_ukb_data, plot = TRUE)
#  

