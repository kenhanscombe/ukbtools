
#' Reads an Oxford format sample file
#'
#' This is a wrapper for \code{read_table} that reads an Oxford format .sample file. If you use the unedited sample file as supplied with your genetic data, you should aonly need to specifiy the first argument, file.
#'
#' @param file A path to a sample file.
#' @param col.names A character vector of column names. Default: c("id_1", "id_2", "missing")
#' @param row.skip Number of lines to skip before reading data.
#'
#' @export
#'
ukb_gen_read_sample <- function(
  file, col.names = c("id_1", "id_2", "missing"), row.skip = 2) {
  read_table(file, skip = row.skip, col_names = col.names)
}




#' Reads a plink format fam file
#'
#' This is wrapper for read_table that reads a basic plink fam file. For plink hard-called data, it may be useful to use the fam file ids as a filter for your phenotype and covariate data.
#'
#' @param file A path to a fam file.
#' @param col.names A character vector of column names. Default: c("FID", "IID", "paternalID", "maternalID", "sex", "phenotype")
#' @param na.strings Character vector of strings to use for missing values. Default "-9". Set this option to character() to indicate no missing values.
#'
#' @seealso
#'
#' @export
#'
ukb_gen_read_fam <- function(
  file, col.names = c("FID", "IID", "paternalID", "maternalID", "sex", "phenotype"), na.strings = "-9") {
  read_table(file, col_names = col.names, na = na.strings)
}




#' Writes a plink format phenotype or covariate file
#'
#' This function writes a space-delimited file with header, with the obligatory first two columns FID and IID. Use this function to write phenotype and covariate files for downstream genetic analysis in \href{https://www.cog-genomics.org/plink2}{plink} - the format is the same.
#'
#' @param x A data frame to write to disk.
#' @param path A path to a file.
#' @param ukb.variables A character vector of either the phenotypes for a plink phenotype file, or covariates for a plink covariate file.
#' @param ukb.id The id variable name (default = "eid").
#' @param na.strings String used for missing values. Defaults to NA.
#'
  #' @details The function writes the id variable in your dataset to the first two columns of the output file with the names FID and IID - you do not need to have two id columns in the data.frame passed to the argument \code{x}. Use the \code{--pheno-name} and \code{--covar-name} plink flags to select columns by name. See the plink documentation for the \code{--pheno}, \code{--mpheno}, \code{--pheno-name}, and \code{--covar}, \code{--covar-name}, \code{--covar-number} flags.
#'
#' @seealso  \code{\link{ukb_gen_read_sample}} to read a sample file, and \code{\link{ukb_gen_write_bgenie}} to write phenotype and covariate files to BGENIE format.
#'
#' @export
#'
ukb_gen_write_plink <- function(x, path, ukb.variables, ukb.id = "eid", na.strings = "NA") {

  x %>%
    mutate_(FID = ukb.id, IID = ukb.id) %>%
    select_("FID", "IID", .dots = ukb.variables) %>%
    write_delim(path = path, na = na.strings, col_names = TRUE)
}




#' Writes a plink format file for combined exclusions
#'
#' For exclusion of individuals from a genetic analysis, the plink flag \code{--remove} accepts a space/tab-delimited text file with family IDs in the first column and within-family IDs in the second column (i.e., FID IID), without a header. This function writes a combined exclusions file including UKB recommended exclusions, heterozygosity exclusions (+/- 3*sd from mean), genetic ethnicity exclusions (based on the UKB genetic ethnic grouping variable, field 1002), and relatedness exclusions (a randomly-selected member of each related pair).
#'
#' @param data A UKB dataset (or subset of) created with \code{\link{ukb_df}}.
#' @param recommend.excl An integer vector of UKB ids created with \link{\code{ukb_gen_excl}}.
#' @param het.excl An integer vector of UKB ids created with \link{\code{ukb_gen_het}}.
#' @param gen.excl Default value "genetic_ethnic_grouping_0_0".
#' @param rel.excl A data.frame created with \link{\code{ukb_gen_rel}}.
#'
#' @details  \strong{Note.} The exclusion list for related individuals is created as a random selection of one member in each pair. Set a random number generation seed with \link{\code{set.seed}} if you think you may write exclusions out again and would like to replicate the same list of relateds to remove.
#'
#' @seealso \link{\code{ukb_gen_meta}}, \link{\code{ukb_gen_pcs}} which retrieve variables to be included in a covariate file. \link{\code{ukb_gen_write_plink}}, \link{\code{ukb_gen_write_bgenie}}
#'
#' @export
#'
ukb_gen_write_plink_excl <- function(data, ukb.path, recommend.excl, het.excl, gen.excl = "genetic_ethnic_grouping_0_0", rel.excl) {

  recommended_exclusions <- data_frame(FID = recommend.excl, IID = recommend.excl)
  heterozygosity_exclusions <- data_frame(FID = het.excl, IID = het.excl)
  genetic_ethnic_exclusions <- data %>%
    filter(is.na(data[, gen.excl])) %>%
    mutate(FID = eid) %>%
    select(FID, IID = eid)

  # Related individuals
  # KING robust estimator kinship coefficient
  # Duplicate/MZ: > 0.354;  1st: > 0.177;  2nd: > 0.088;  3rd: > 0.044

  # Retain IDs not in pair
  ukb_unpaired <- as.numeric(names(table(rel.excl$pair)[table(rel.excl$pair)!=2]))
  ukb_unpaired_id <- rel.excl[rel.excl$pair == ukb_unpaired, "eid"]
  if (length(ukb_unpaired_id) >= 1) {message(paste("Unpaired related individuals (not excluded):", ukb_unpaired_id))}

  rel_pairs <- rel.excl[!(rel.excl$pair %in% ukb_unpaired), ]
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

  write.table(
    ukb_exclusions,
    file = ukb.path,
    quote = FALSE,
    row.names = FALSE,
    col.names = FALSE
  )
}




#' Writes a BGENIE format phenotype or covariate file.
#'
#' Writes a space-delimited file with a header, missing character set to "-999", and observations (i.e. UKB subject ids) in sample file order. Use this function to write phenotype and covariate files for downstream genetic analysis in \href{https://jmarchini.org/bgenie/}{BGENIE} - the format is the same.
#'
#' @param x A data frame to write to disk.
#' @param path A path to a file.
#' @param ukb.sample Path to the UKB sample file.
#' @param ukb.variables A character vector of either the phenotypes for a BGENIE phenotype file, or covariates for a BGENIE covariate file.
#' @param ukb.id The eid variable name (default = "eid").
#'
#' @details See [BGENIE usage](https://jmarchini.org/bgenie-usage/) for descriptions of the \code{--pheno} and \code{--covar} flags to read phenotype and covariate data into BGENIE.
#'
#' @seealso \code{\link{ukb_gen_read_sample}} to read a sample file, and \code{\link{ukb_gen_write_plink}} to write phenotype and covariate files to plink format.
#'
#' @export
#'
ukb_gen_write_bgenie <- function(x, ukb.sample, path, ukb.variables,
                                 ukb.id = "eid", na.strings = "-999") {
  names(ukb_sample)[1] <- ukb.id

  ukb_sample %>%
    left_join(ukb, by = ukb.id) %>%
    select_(.dots = ukb.variables) %>%
    write_delim(path = path, na = na.strings, col_names = TRUE)
}
