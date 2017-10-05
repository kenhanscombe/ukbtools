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
#  library(ukbtools)
#  
#  my_ukb_data <- ukb_df("ukbxxxx")
#  

## ------------------------------------------------------------------------
#  
#  my_ukb_data <- ukb_df("ukbxxxx", path = "/full/path/to/my/fileset")
#  

## ------------------------------------------------------------------------
#  
#  my_ukb_key <- ukb_df_field("ukbxxxx", path = "/full/path/to/my/fileset")
#  

## ------------------------------------------------------------------------
#  
#  ukbxxxx_data <- ukb_df("ukbxxxx")
#  ukbyyyy_data <- ukb_df("ukbyyyy")
#  ukbzzzz_data <- ukb_df("ukbzzzz")
#  
#  ukb_df_full_join(ukbxxxx_data, ukbyyyy_data, ukbzzzz_data)
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
#  # To plot the frequency of the default ICD codes with respect to BMI
#  ukb_icd_freq_by(my_ukb_data, reference.var = "body_mass_index_bmi_0_0", freq.plot = TRUE)
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
#  ukb_gen_rel_count(my_gen_rel, plot = TRUE)
#  

## ---- eval = FALSE-------------------------------------------------------
#  
#  # Read .sample file supplied with bulk genetic data
#  my_sample_file <- ukb_gen_read_sample("path/to/sample_file")
#  
#  # Write a BGENIE format phenotype or covariate file
#  ukb_gen_write_bgenie(
#    my_ukb_data,
#    path = "path/to/bgenie_input_file",
#    ukb.sample = my_sample_file,
#    ukb.variables = c("variable1", "variable2", "variable3")
#  )
#  

## ---- eval = FALSE-------------------------------------------------------
#  
#  # Write a PLINK format phenotype or covariate file
#  ukb_gen_write_plink(
#    my_ukb_data,
#    path = "path/to/plink_input_file",
#    ukb.variables = c("variable1", "variable2", "variable3")
#  )
#  

## ---- eval = FALSE-------------------------------------------------------
#  
#  my_ukb_data$height_excl_na <- ukb_gen_excl_to_na(my_ukb_data, x = "height")
#  

## ---- eval = FALSE-------------------------------------------------------
#  
#  ukb_gen_write_plink_excl("path/to/plink_input_file")
#  

