## ----setup, include=FALSE------------------------------------------------

knitr::opts_chunk$set(
  fig.retina = 2,
  eval = FALSE,
  warning = FALSE,
  message = FALSE,
  comment = NA
)


## ------------------------------------------------------------------------
#  # Install from CRAN
#  install.packages("ukbtools")
#  
#  # Install latest development version
#  devtools::install_github("kenhanscombe/ukbtools", build_vignettes = TRUE, dependencies = TRUE)

## ------------------------------------------------------------------------
#  library(ukbtools)
#  
#  my_ukb_data <- ukb_df("ukbxxxx")

## ------------------------------------------------------------------------
#  my_ukb_data <- ukb_df("ukbxxxx", path = "/full/path/to/my/ukb/fileset/data")

## ------------------------------------------------------------------------
#  my_ukb_key <- ukb_df_field("ukbxxxx", path = "/full/path/to/my/ukb/fileset/data")

## ------------------------------------------------------------------------
#  ukbxxxx_data <- ukb_df("ukbxxxx")
#  ukbyyyy_data <- ukb_df("ukbyyyy")
#  ukbzzzz_data <- ukb_df("ukbzzzz")
#  
#  ukb_df_full_join(ukbxxxx_data, ukbyyyy_data, ukbzzzz_data)

## ------------------------------------------------------------------------
#  # To load the example data
#  df <- ukb_df("ukbxxxx", path = "inst/extdata")
#  
#  # To create a field code to name key
#  df_field <- ukb_df_field("ukbxxxx", path = "inst/extdata")

## ------------------------------------------------------------------------
#  ukb_context(my_ukb_data, nonmiss.var = "my_variable_of_interest")

## ------------------------------------------------------------------------
#  subgroup_of_interest <- (my_ukb_data$body_mass_index_bmi_0_0 >= 25)
#  ukb_context(my_ukb_data, subset.var = subgroup_of_interest)

## ------------------------------------------------------------------------
#  ukb_icd_diagnosis(my_ukb_data, id = "0000000", icd.version = 10)

## ------------------------------------------------------------------------
#  ukb_icd_code_meaning(icd.code = "I74", icd.version = 10)

## ------------------------------------------------------------------------
#  ukb_icd_keyword("cardio", icd.version = 10)

## ------------------------------------------------------------------------
#  # ICD-10 code I74, Arterial embolism and thrombosis
#  ukb_icd_prevalence(my_ukb_data, icd.version = 10, icd.diagnosis = "I74")
#  
#  # ICD-10 chapter 9, disease block I00–I99, Diseases of the circulatory system
#  ukb_icd_prevalence(my_ukb_data, icd.version = 10, icd.diagnosis = "I")
#  
#  # ICD-10 chapter 2, C00-D49, Neoplasms
#  ukb_icd_prevalence(my_ukb_data, icd.version = 10, icd.diagnosis = "C|D[0-4].")

## ------------------------------------------------------------------------
#  ukb_icd_freq_by(my_ukb_data, reference.var = "body_mass_index_bmi_0_0", freq.plot = TRUE)
#  ukb_icd_freq_by(my_ukb_data, reference.var = "sex_0_0", freq.plot = TRUE)

## ------------------------------------------------------------------------
#  # With ukb_sqc_v2.txt read into the dataframe my_sqc_data
#  my_sqc_data <- ukb_gen_sqc_names(my_sqc_data)
#  
#  # For a character vector of column names
#  ukb_gen_sqc_names(col_names_only = TRUE)

## ------------------------------------------------------------------------
#  # With ukbA_rel_sP.txt read into the dataframe my_relatedness_data
#  ukb_gen_rel_count(my_relatedness_data)

## ------------------------------------------------------------------------
#  ukb_gen_rel_count(ukb_relatedness, plot = TRUE)

## ------------------------------------------------------------------------
#  # With ukbA_rel_sP.txt read into the dataframe my_relatedness_data
#  # and an integer vector samples_with_phenotype of samples who have
#  # data on the phenotype of interest
#  
#  ukb_gen_related_with_data(my_relatedness_data, ukb_with_data = samples_with_phenotype)
#  ukb_gen_samples_to_remove(my_relatedness_data, ukb_with_data = samples_with_phenotype)

## ------------------------------------------------------------------------
#  # Read .sample file supplied with bulk genetic data
#  my_sample_file <- ukb_gen_read_sample("path/to/sample_file")
#  
#  # Write a BGENIE format phenotype or covariate file
#  ukb_gen_write_bgenie(
#      my_ukb_data,
#      path = "path/to/bgenie_input_file",
#      ukb.sample = my_sample_file,
#      ukb.variables = c("variable1", "variable2", "variable3")
#  )

## ------------------------------------------------------------------------
#  # Write a PLINK format phenotype or covariate file
#  
#  ukb_gen_write_plink(
#      my_ukb_data,
#      path = "path/to/plink_input_file",
#      ukb.variables = c("variable1", "variable2", "variable3")
#  )

