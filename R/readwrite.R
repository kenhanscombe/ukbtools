
#' Reads an Oxford format sample file
#'
#' This is a wrapper for \code{read_table} that reads an Oxford format .sample file. If you use the unedited sample file as supplied with your genetic data, you should aonly need to specifiy the first argument, file.
#'
#' @param file A path to a sample file.
#' @param col.names A character vector of column names. Default: c("id_1", "id_2", "missing")
#' @param row.skip Number of lines to skip before reading data.
#'
#' @seealso \code{\link{ukb_gen_read_fam}} to read a fam file
#'
#' @import readr
#' @export
#'
ukb_gen_read_sample <- function(
  file, col.names = c("id_1", "id_2", "missing"), row.skip = 2) {
  sample <- readr::read_table(file, skip = row.skip, col_names = col.names)
  as.data.frame(sample)
}




#' Reads a plink format fam file
#'
#' This is wrapper for read_table that reads a basic plink fam file. For plink hard-called data, it may be useful to use the fam file ids as a filter for your phenotype and covariate data.
#'
#' @param file A path to a fam file.
#' @param col.names A character vector of column names. Default: c("FID", "IID", "paternalID", "maternalID", "sex", "phenotype")
#' @param na.strings Character vector of strings to use for missing values. Default "-9". Set this option to character() to indicate no missing values.
#'
#' @seealso \code{\link{ukb_gen_read_sample}} to read a sample file
#'
#' @import readr
#' @export
#'
ukb_gen_read_fam <- function(
  file, col.names = c("FID", "IID", "paternalID", "maternalID", "sex", "phenotype"), na.strings = "-9") {
  fam <- readr::read_table(file, col_names = col.names, na = na.strings)
  as.data.frame(fam)
}




#' Writes a plink format phenotype or covariate file
#'
#' This function writes a space-delimited file with header, with the obligatory first two columns FID and IID. Use this function to write phenotype and covariate files for downstream genetic analysis in \href{https://www.cog-genomics.org/plink2}{plink} - the format is the same.
#'
#' @param x A UKB dataset.
#' @param path A path to a file.
#' @param ukb.variables A character vector of either the phenotypes for a plink phenotype file, or covariates for a plink covariate file.
#' @param ukb.id The id variable name (default = "eid").
#' @param na.strings String used for missing values. Defaults to NA.
#'
  #' @details The function writes the id variable in your dataset to the first two columns of the output file with the names FID and IID - you do not need to have two id columns in the data.frame passed to the argument \code{x}. Use the \code{--pheno-name} and \code{--covar-name} plink flags to select columns by name. See the plink documentation for the \code{--pheno}, \code{--mpheno}, \code{--pheno-name}, and \code{--covar}, \code{--covar-name}, \code{--covar-number} flags.
#'
#' @seealso  \code{\link{ukb_gen_read_sample}} to read a sample file, and \code{\link{ukb_gen_write_bgenie}} to write phenotype and covariate files to BGENIE format.
#'
#' @import dplyr readr
#' @importFrom magrittr "%>%"
#' @export
#'
ukb_gen_write_plink <- function(x, path, ukb.variables, ukb.id = "eid", na.strings = "NA") {

  ids <- dplyr::transmute_(x, FID = ukb.id, IID = ukb.id)
  vars <- dplyr::select_(x, .dots = ukb.variables)

  readr::write_delim(cbind(ids, vars), path = path, na = na.strings, col_names = TRUE)
}




#' Writes a PLINK format file for combined exclusions
#'
#' Writes a combined exclusions file including UKB recommended exclusions, heterozygosity exclusions (+/- 3*sd from mean), genetic ethnicity exclusions (based on the UKB genetic ethnic grouping variable, field 1002), and relatedness exclusions (a randomly-selected member of each related pair). For exclusion of individuals from a genetic analysis, the PLINK flag \code{--remove} accepts a space/tab-delimited text file with family IDs in the first column and within-family IDs in the second column (i.e., FID IID), without a header.
#'
#' @param path A path to a file.
#'
#' @seealso \code{\link{ukb_gen_meta}}, \code{\link{ukb_gen_pcs}} which retrieve variables to be included in a covariate file. \code{\link{ukb_gen_excl_to_na}} to update a phenotype with NAs for samples to-be-excluded based on genetic metadata, and \code{\link{ukb_gen_write_plink}} and \code{\link{ukb_gen_write_bgenie}}
#'
#' @importFrom utils write.table
#' @export
#'
ukb_gen_write_plink_excl <- function(path) {

  utils::write.table(
    ukb_meta_excl_plink,
    file = path,
    quote = FALSE,
    row.names = FALSE,
    col.names = FALSE
  )
}




#' Writes a BGENIE format phenotype or covariate file.
#'
#' Writes a space-delimited file with a header, missing character set to "-999", and observations (i.e. UKB subject ids) in sample file order. Use this function to write phenotype and covariate files for downstream genetic analysis in \href{https://jmarchini.org/bgenie/}{BGENIE} - the format is the same.
#'
#' @param x A UKB dataset.
#' @param path A path to a file.
#' @param ukb.sample Path to the UKB sample file.
#' @param ukb.variables A character vector of either the phenotypes for a BGENIE phenotype file, or covariates for a BGENIE covariate file.
#' @param ukb.id The eid variable name (default = "eid").
#' @param na.strings Characer string to be used for missing value in output file. Default = "-999"
#'
#' @details See href{https://jmarchini.org/bgenie-usage/}{BGENIE usage} for descriptions of the \code{--pheno} and \code{--covar} flags to read phenotype and covariate data into BGENIE.
#'
#' @seealso \code{\link{ukb_gen_read_sample}} to read a sample file, \code{\link{ukb_gen_excl_to_na}} to update a phenotype with NAs for samples to-be-excluded based on genetic metadata, and \code{\link{ukb_gen_write_plink}} to write phenotype and covariate files to PLINK format.
#'
#' @import dplyr readr
#' @importFrom magrittr "%>%"
#' @export
#'
ukb_gen_write_bgenie <- function(x, ukb.sample, path, ukb.variables,
                                 ukb.id = "eid", na.strings = "-999") {
  names(ukb.sample)[1] <- ukb.id

  ukb.sample %>%
    dplyr::left_join(x, by = ukb.id) %>%
    dplyr::select_(.dots = ukb.variables) %>%
    readr::write_delim(path = path, na = na.strings, col_names = TRUE)
}
