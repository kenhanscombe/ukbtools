
#' Reads an Oxford format sample file
#'
#' This is a wrapper for \code{read_table} that reads an Oxford format .sample file. If you use the unedited sample file as supplied with your genetic data, you should aonly need to specifiy the first argument, file.
#'
#' @param file A path to a file.
#' @param col.names A character vector of column names.
#' @param row.skip Number of lines to skip before reading data.
#'
#' @export
#'
ukb_gen_read_sample <- function(file, col.names = c("id_1", "id_2", "missing"),
                                row.skip = 2) {
  read_table(file, skip = row.skip, col_names = col.names)
}

#ukb_gen_read_fam <- function(file, col.names) {}




# Write plink files -------------------------------------------------------
# All plink output files should be FID IID ...


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




# Write BGENIE files ------------------------------------------------------


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




# Matthew’s requests ------------------------------------------------------

# 1. Generate a phenotype based on certain criteria from the genetic metadata, e.g., stroke cases versus controls in europeans after removing suggested exclusions and all related individuals

# ukb_gen_make_pheno <- function(x, ukb.excl) {
#   x[ukb.excl] <- NA
# }


# 2. how to generate a set of related individuals to remove from analyses (not sure if this is optimal):

# data<-data[order(data$RelID1),]
# data<-subset(data,!(duplicated(data$RelID1) & !is.na(data$RelID1)))
# data<-subset(data,!(duplicated(data$RelID2) & !is.na(data$RelID2)))
# data<-subset(data,!(duplicated(data$RelID3) & !is.na(data$RelID3)))
# data<-subset(data,!(duplicated(data$RelID4) & !is.na(data$RelID4)))
