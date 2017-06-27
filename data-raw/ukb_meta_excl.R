
# Genetic metadata exclusions (pre-computed internal data) ----------------

library(ukbtools)

load("../ukbiobank/activity2/data/ukb.rda")
ukb <- tbl_df(ukb)
recommend_excl <- ukb_gen_excl(ukb)
het_excl <- ukb_gen_het(ukb)
rel_excl <- ukb_gen_rel(ukb)


recommended_exclusions <- data_frame(FID = recommend_excl, IID = recommend_excl)
heterozygosity_exclusions <- data_frame(FID = het_excl, IID = het_excl)
genetic_ethnic_exclusions <- ukb %>%
  filter(is.na(ukb$genetic_ethnic_grouping_0_0)) %>%
  mutate(FID = eid) %>%
  select(FID, IID = eid)


# Related individuals
# KING robust estimator kinship coefficient
# Duplicate/MZ: > 0.354;  1st: > 0.177;  2nd: > 0.088;  3rd: > 0.044

# Retain IDs not in pair
ukb_unpaired <- as.numeric(names(table(rel_excl$pair)[table(rel_excl$pair)!=2]))
ukb_unpaired_id <- rel_excl[rel_excl$pair == ukb_unpaired, "eid"]


rel_pairs <- rel_excl[!(rel_excl$pair %in% ukb_unpaired), ]
rel_pairs <- rel_pairs[order(rel_pairs$pair), ]

# Select a random member of each pair to exclude
rel_excl_index <- vector(mode = "logical")
for (i in 1:(nrow(rel_pairs)/2)){
  rel_excl_index <- append(
    rel_excl_index,
    sample(c(T,F), 2, replace = FALSE)
  )
}

related_exclusions <- tbl_df(rel_pairs) %>%
  filter(rel_excl_index) %>%
  mutate(FID = eid, IID = eid) %>%
  select(FID, IID)


# Combine sample exclusions
ukb_exclusions <- recommended_exclusions %>%
  bind_rows(heterozygosity_exclusions) %>%
  bind_rows(related_exclusions) %>%
  bind_rows(genetic_ethnic_exclusions) %>%
  unique()


lookup <- function(data, key, value) {
  df <- as.data.frame(data)
  l <- df[, value]
  names(l) <- df[, key]
  return(l)
}


# 2-column data frame of genetic metadata exclusions: FID IID
ukb_meta_excl_plink <- ukb_exclusions

ukb_meta_excl_table <- data_frame(
  ukb_id = ukb$eid,
  gen_meta_exclude = ukb$eid %in% ukb_exclusions$FID
)

# Named logical vector indicating genetic metadata exclusions
ukb_meta_excl_lookup <- lookup(ukb_meta_excl_table, key = "ukb_id", value = "gen_meta_exclude")


devtools::use_data(ukb_meta_excl_plink, ukb_meta_excl_lookup, internal = TRUE, overwrite = TRUE)
