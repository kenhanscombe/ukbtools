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

