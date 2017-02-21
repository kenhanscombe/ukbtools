
# Make UKB centre dataset -------------------------------------------------

centre <- read_csv("data-raw/centre.csv", col_names = c("code", "centre"))
ukbcentre <- as.data.frame(centre)
devtools::use_data(ukbcentre, overwrite = TRUE)
