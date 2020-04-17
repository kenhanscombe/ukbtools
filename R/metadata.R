
defunct_message <- "The genetic metadata functions were written to retrieve genetic metadata from the phenotype file for the interim genotype release. The fields retrieved became obselete when the full genotyping results (500K individuals) were released at the end of 2017. With the release of the full genotyping results, sample QC (ukb_sqc_v2.txt) and marker QC (ukb_snp_qc.txt) data are now supplied as separate files. The contents of these files, along with all other genetic files are described fully in UKB Resource 531."


#' Genetic metadata
#'
#' @description \lifecycle{defunct}
#'
#' UKB have published \href{http://www.ukbiobank.ac.uk/wp-content/uploads/2014/04/UKBiobank_genotyping_QC_documentation-web.pdf}{full details of genotyping and quality control} for the interim genotype data. This function retrieves UKB assessment centre codes and assessment centre names, genetic ethnic grouping, genetically-determined sex, missingness, UKB recommended genomic analysis exclusions, BiLeve unrelatedness indicator, and BiLeve Affymetrix and genotype quality control.
#'
#' @param data A UKB dataset created with \code{\link{ukb_df}}.
#' @import dplyr
#' @importFrom magrittr "%>%"
#' @importFrom lifecycle deprecate_stop
#' @export
ukb_gen_meta <-  function(data) {
  lifecycle::deprecate_stop("0.10", "ukb_gen_meta()",
                            details = defunct_message)
  # centre_lookup <- lookup(ukbtools::ukbcentre, "code", "centre")
  # data$ukb_centre_name = centre_lookup[as.factor(data[["uk_biobank_assessment_centre_0_0"]])]
  # data$bileve_chip = ifelse(!is.na(data[["ukbileve_affymetrix_quality_control_for_samples_0_0"]]), 1, 0)
  #
  # data %>%
  #   dplyr::select_(
  #     "eid",
  #     "uk_biobank_assessment_centre_0_0",
  #     "ukb_centre_name",
  #     "genetic_ethnic_grouping_0_0",
  #     "genetic_sex_0_0",
  #     "missingness_0_0",
  #     "recommended_genomic_analysis_exclusions_0_0",
  #     "ukbileve_unrelatedness_indicator_0_0",
  #     "ukbileve_affymetrix_quality_control_for_samples_0_0",
  #     "ukbileve_genotype_quality_control_for_samples_0_0",
  #     "bileve_chip"
  #   ) %>%
  #   as.data.frame()
}




#' Genetic principal components
#'
#' @description \lifecycle{defunct}
#'
#'  These are the principal components derived on the UK Biobank subsample with interim genotype data. UKB have published \href{http://www.ukbiobank.ac.uk/wp-content/uploads/2014/04/UKBiobank_genotyping_QC_documentation-web.pdf}{full details of genotyping and quality control} for the interim genotype data.
#'
#' @param data A UKB dataset created with \code{\link{ukb_df}}.
#' @import dplyr
#' @importFrom magrittr "%>%"
#' @importFrom lifecycle deprecate_stop
#' @export
ukb_gen_pcs <- function(data) {
  lifecycle::deprecate_stop("0.10", "ukb_gen_pcs()",
                            details = defunct_message)
  # data %>%
  #   dplyr::select(
  #     eid,
  #     matches("genetic_principal_components")) %>%
  #   as.data.frame()
}




#' Sample exclusions
#'
#' @description \lifecycle{defunct}
#'
#'  This list of sample exclusions includes UKB's "recommended", "affymetrix quality control", and "genotype quality control" exclusions. UKB have published \href{http://www.ukbiobank.ac.uk/wp-content/uploads/2014/04/UKBiobank_genotyping_QC_documentation-web.pdf}{full details of genotyping and quality control} for the interim genotype data.
#'
#' @param data A UKB dataset created with \code{\link{ukb_df}}.
#' @import dplyr
#' @importFrom magrittr "%>%"
#' @importFrom lifecycle deprecate_stop
#' @export
#' @examples
#' \dontrun{
#' # For a vector of IDs
#' recommended_excl_ids <- ukb_gen_excl(my_ukb_df)
#' }
ukb_gen_excl <- function(data) {
  lifecycle::deprecate_stop("0.10", "ukb_gen_excl()",
                            details = defunct_message)
  # data %>%
  #   dplyr::filter(
  #     !is.na(data[["recommended_genomic_analysis_exclusions_0_0"]]) |
  #       (data[["ukbileve_affymetrix_quality_control_for_samples_0_0"]] == "Fail" &
  #          !is.na(data[["ukbileve_affymetrix_quality_control_for_samples_0_0"]])
  #       ) |
  #       (data[["ukbileve_genotype_quality_control_for_samples_0_0"]] == "Fail" &
  #          !is.na(data[["ukbileve_genotype_quality_control_for_samples_0_0"]])
  #       )
  #   ) %>%
  #   pull("eid")
}




#' Creates a table of related individuals
#'
#' @description \lifecycle{defunct}
#'
#' Makes a data.frame containing all related individuals with columns UKB ID, pair ID, \href{http://people.virginia.edu/~wc9c/KING/manual.html}{KING kinship coefficient}, and proportion of alleles IBS = 0. UKB have published \href{http://www.ukbiobank.ac.uk/wp-content/uploads/2014/04/UKBiobank_genotyping_QC_documentation-web.pdf}{full details of genotyping and quality control} including details on relatedness calculations for the interim genotype data.
#'
#' @param data A UKB dataset created with \code{\link{ukb_df}}.
#' @seealso \code{\link{ukb_gen_rel_count}}
#' @import dplyr
#' @importFrom magrittr "%>%"
#' @importFrom stats na.omit
#' @importFrom lifecycle deprecate_stop
#' @export
ukb_gen_rel <- function(data) {
  lifecycle::deprecate_stop("0.10", "ukb_gen_rel()",
                            details = defunct_message)
  # rbind(
  #   data %>%
  #     dplyr::select_(
  #       "eid",
  #       pair = "genetic_relatedness_pairing_0_0",
  #       kinship = "genetic_relatedness_factor_0_0",
  #       ibs0 = "genetic_relatedness_ibs0_0_0"
  #     ),
  #   data %>%
  #     dplyr::select_(
  #       "eid",
  #       pair = "genetic_relatedness_pairing_0_1",
  #       kinship = "genetic_relatedness_factor_0_1",
  #       ibs0 = "genetic_relatedness_ibs0_0_1"
  #     ),
  #   data %>%
  #     dplyr::select_(
  #       "eid",
  #       pair = "genetic_relatedness_pairing_0_2",
  #       kinship = "genetic_relatedness_factor_0_2",
  #       ibs0 = "genetic_relatedness_ibs0_0_2"
  #     ),
  #   data %>%
  #     dplyr::select_(
  #       "eid",
  #       pair = "genetic_relatedness_pairing_0_3",
  #       kinship = "genetic_relatedness_factor_0_3",
  #       ibs0 = "genetic_relatedness_ibs0_0_3"
  #     ),
  #   data %>%
  #     dplyr::select_(
  #       "eid",
  #       pair = "genetic_relatedness_pairing_0_4",
  #       kinship = "genetic_relatedness_factor_0_4",
  #       ibs0 = "genetic_relatedness_ibs0_0_4"
  #     )
  # ) %>%
  #   stats::na.omit() %>%
  #   dplyr::tbl_df() %>%
  #   dplyr::arrange(pair) %>%
  #   as.data.frame()
}




#' Heterozygosity outliers
#'
#' @description \lifecycle{defunct}
#'
#' Heterozygosity outliers are typically removed from genetic association analyses. This function returns either a vector of heterozygosity outliers to remove (+/- 3sd from mean heterozygosity), or a data frame with heterozygosity scores for all samples.
#'
#' @param data A UKB dataset created with \code{\link{ukb_df}}.
#' @param all.het Set \code{all.het = TRUE} for heterozygosity scores for all samples. By default \code{all.het = FALSE} returns a vector of sample IDs for individuals +/-3SD from the mean heterozygosity.
#' @return A vector of IDs if \code{all.het = FALSE} (default), or a dataframe with ID, heterozygosity and PCA-corrected heterozygosity if \code{all.het = TRUE}.
#' @details UKB have published \href{http://www.ukbiobank.ac.uk/wp-content/uploads/2014/04/UKBiobank_genotyping_QC_documentation-web.pdf}{full details of genotyping and quality control} for the interim genotype data.
#' @import dplyr
#' @importFrom magrittr "%>%"
#' @importFrom stats sd
#' @importFrom lifecycle deprecate_stop
#' @export
#' @examples
#' \dontrun{
#' #' # Heterozygosity outliers (+/-3SD)
#' outlier_het_ids <- ukb_gen_het(my_ukb_data)
#'
#' # Retrieve all raw and pca-corrected heterozygosity scores
#' ukb_het <- ukb_gen_het(my_ukb_data, all.het = TRUE)
#' }
ukb_gen_het <- function(data, all.het = FALSE) {
  lifecycle::deprecate_stop("0.10", "ukb_gen_het()",
                            details = defunct_message)
  # if (all.het) {
  #   return(
  #     data %>%
  #       dplyr::select_("eid", "heterozygosity_0_0", "heterozygosity_pca_corrected_0_0") %>%
  #       as.data.frame()
  #   )
  # } else {
  #   data %>%
  #     dplyr::filter(
  #       heterozygosity_0_0 < (mean(heterozygosity_0_0, na.rm = TRUE) - (3 * stats::sd(heterozygosity_0_0, na.rm = TRUE))) |
  #         heterozygosity_0_0 > (mean(heterozygosity_0_0, na.rm = TRUE) + (3 * stats::sd(heterozygosity_0_0, na.rm = TRUE)))
  #     ) %>%
  #     pull(eid)
  # }
}




#' Inserts NA into phenotype for genetic metadata exclusions
#'
#' @description \lifecycle{defunct}
#'
#' Replaces data values in a vector (a UKB phenotype) with \code{NA} where the sample is to-be-excluded, i.e., is either a UKB recommended exclusion, a heterozygosity outlier, a genetic ethnicity outlier, or a randomly-selected member of a related pair.
#'
#' @param data A UKB dataset created with \code{\link{ukb_df}}.
#' @param x The phenotype to be updated (as it is named in \code{data}) e.g. "height"
#' @param ukb.id The name of the ID variable in \code{data}. Default is "eid"
#' @param data.frame A logical vector indicating whether to return a vector or a data.frame (header: id, meta_excl, pheno, pheno_meta_na) containing the original and updated variable. Default = FALSE returns a vector.
#' @seealso \code{\link{ukb_gen_write_plink_excl}}
#' @importFrom lifecycle deprecate_stop
#' @export
#' @examples
#' \dontrun{
#' my_ukb_data$height_excl_na <- ukb_gen_excl_to_na(my_ukb_data, x = "height")
#' }
ukb_gen_excl_to_na <- function(data, x, ukb.id = "eid", data.frame = FALSE) {
  lifecycle::deprecate_stop("0.10", "ukb_gen_excl_to_na()",
                            details = defunct_message)
  # ids <- as.character(data[[ukb.id]])
  # meta_excl <- ukb_meta_excl_lookup[ids]
  # pheno_meta_na <- data[[x]]
  # pheno_meta_na[meta_excl] <- NA
  #
  # if (data.frame) {
  #   data.frame(id = ids, meta_excl = meta_excl, pheno = data[[x]], pheno_meta_na = pheno_meta_na)
  # } else {
  #   return(pheno_meta_na)
  # }
}




#' Writes a PLINK format file for combined exclusions
#'
#' @description \lifecycle{defunct}
#
#' Writes a combined exclusions file including UKB recommended exclusions, heterozygosity exclusions (+/- 3*sd from mean), genetic ethnicity exclusions (based on the UKB genetic ethnic grouping variable, field 1002), and relatedness exclusions (a randomly-selected member of each related pair). For exclusion of individuals from a genetic analysis, the PLINK flag \code{--remove} accepts a space/tab-delimited text file with family IDs in the first column and within-family IDs in the second column (i.e., FID IID), without a header.
#'
#' @param path A path to a file.
#'
#' @seealso \code{\link{ukb_gen_meta}}, \code{\link{ukb_gen_pcs}} which retrieve variables to be included in a covariate file. \code{\link{ukb_gen_excl_to_na}} to update a phenotype with NAs for samples to-be-excluded based on genetic metadata, and \code{\link{ukb_gen_write_plink}} and \code{\link{ukb_gen_write_bgenie}}
#'
#' @importFrom utils write.table
#' @importFrom lifecycle deprecate_stop
#' @export
#' @examples
#' \dontrun{
#' # Supply name of a file to write PLINK format combined exclusions
#' ukb_gen_write_plink_excl("combined_exclusions.txt")
#' }
#'
ukb_gen_write_plink_excl <- function(path) {
  lifecycle::deprecate_stop("0.10", "ukb_gen_write_plink_excl()",
                            details = defunct_message)
  # utils::write.table(
  #   ukb_meta_excl_plink,
  #   file = path,
  #   quote = FALSE,
  #   row.names = FALSE,
  #   col.names = FALSE
  # )
}