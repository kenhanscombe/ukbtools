## ---- eval = FALSE-------------------------------------------------------
#  
#  my_ukb_data <- ukb_df("ukbxxxx")
#  

## ---- eval = FALSE-------------------------------------------------------
#  
#  my_ukb_data <- ukb_df("ukbxxxx", path = "/full/path/to/my/data")
#  

## ----eval = FALSE--------------------------------------------------------
#  
#  ukbxxxx_data <- ukb_df("ukbxxxx")
#  ukbyyyy_data <- ukb_df("ukbyyyy")
#  ukbzzzz_data <- ukb_df("ukbzzzz")
#  
#  my_ukb_data <- plyr::join_all(
#    list(
#      ukbxxxx_data,
#      ukbyyyy_data,
#      ukbzzzz_data),
#    by = "eid",
#    type = "full")
#  

## ---- eval = FALSE-------------------------------------------------------
#  
#  ukb_names <- c(
#    names(ukbxxxx_data),
#    names(ukbyyyy_data),
#    names(ukbzzzz_data)
#  )
#  count_ukb_names <- data.frame(table(ukb_names))
#  dim(count_ukb_names[count_names$Freq > 1, ])
#  

## ---- eval = FALSE-------------------------------------------------------
#  
#  icd_diagnosis(my_ukb_data, id = "0000000", icd.version = 10)
#  

## ---- eval = FALSE-------------------------------------------------------
#  
#  icd_code(icd.code = "H282", icd.version = 10)
#  

## ---- eval = FALSE-------------------------------------------------------
#  
#  icd_chapter(icd.version = 10)
#  

## ---- eval = FALSE-------------------------------------------------------
#  
#  ukb_context(
#    my_ukb_data,
#    age.var = "year_of_birth_0_0",
#    sample.ref = sample(c(T,F), nrow(my_ukb_data), replace = TRUE)
#    )
#  

## ---- eval = FALSE-------------------------------------------------------
#  
#  my_gen_meta <- ukb_gen_meta(my_ukb_data)
#  my_gen_pcs <- ukb_gen_pcs(my_ukb_data)
#  

## ---- eval = FALSE-------------------------------------------------------
#  
#  ukb_gen_excl(my_ukb_data)
#  ukb_gen_het(my_ukb_data)
#  

## ---- eval = FALSE-------------------------------------------------------
#  
#  ukb_gen_het(my_ukb_data, all.het = TRUE)
#  

## ---- eval = FALSE-------------------------------------------------------
#  
#  my_gen_rel <- ukb_gen_rel(my_ukb_data)
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
#  ukb_gen_write_plink_excl()
#  

