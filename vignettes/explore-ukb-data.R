## ---- eval = FALSE-------------------------------------------------------
#  
#  my_ukb_data <- ukb_df("ukbxxxx")
#  

## ---- eval = FALSE-------------------------------------------------------
#  
#  my_ukb_data <- ukb_df("ukbxxxx", path = "./data/")
#  

## ----eval = FALSE--------------------------------------------------------
#  
#  ukbxxxx_data <- ukb_df("ukbxxxx", path = "./data/")
#  ukbyyyy_data <- ukb_df("ukbyyyy", path = "./data/")
#  ukbzzzz_data <- ukb_df("ukbzzzz", path = "./data/")
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
#  my_gen_meta <- ukb_meta(my_ukb_data)
#  my_gen_pcs <- ukb_gen_pcs(my_ukb_data)
#  

## ---- eval = FALSE-------------------------------------------------------
#  
#  ukb_gen_excl(my_ukb_data)
#  ukb_gen_het(my_ukb_data)
#  

## ---- eval = FALSE-------------------------------------------------------
#  
#  my_gen_rel <- ukb_gen_rel(my_ukb_data)
#  

