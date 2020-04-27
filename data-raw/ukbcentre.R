
# Make UKB centre dataset -------------------------------------------------
library(tidyverse)

# centre <- readr::read_csv("data-raw/centre.csv",
#                           col_names = c("code", "centre"))

centre <- readr::read_tsv("data-raw/coding10.tsv",
                          col_names = TRUE,
                          col_types = "ic")

ukbcentre <- as.data.frame(centre)

usethis::use_data(ukbcentre, overwrite = TRUE)
