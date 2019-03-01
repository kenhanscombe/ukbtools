
#' ukbtools: Manipulate and Explore UK Biobank Data
#'
#' @description A set of tools to create a \href{https://www.ukbiobank.ac.uk/}{UK Biobank} dataset
#' from a UKB fileset (.tab, .r, .html), visualize primary demographic data for
#' a sample subset, query ICD diagnoses, retrieve genetic metadata, read and
#' write standard file formats for genetic analyses.
#'
#' @section UKB Dataframe:
#' Functions to wrangle the UKB data into a dataframe with meaningful column
#' names.
#' \itemize{
#' \item \code{\link{ukb_df}}
#' \item \code{\link{ukb_df_field}}
#' \item \code{\link{ukb_df_full_join}}
#' \item \code{\link{ukb_df_duplicated_name}}
#' \item \code{\link{ukb_centre}}
#' \item \code{\link{ukb_context}}
#' }
#'
#' @section Genetic Metadata:
#' Functions to query the associated genetic sample QC information.
#' \itemize{
#' \item \code{\link{ukb_gen_read_fam}}
#' \item \code{\link{ukb_gen_read_sample}}
#' \item \code{\link{ukb_gen_rel_count}}
#' \item \code{\link{ukb_gen_related_with_data}}
#' \item \code{\link{ukb_gen_samples_to_remove}}
#' \item \code{\link{ukb_gen_sqc_names}}
#' \item \code{\link{ukb_gen_write_bgenie}}
#' \item \code{\link{ukb_gen_write_plink}}
#' }
#'
#' @section Disease Diagnoses:
#' Functions to query the UKB hospital episodes statistics.
#' \itemize{
#' \item \code{\link{ukb_icd_code_meaning}}
#' \item \code{\link{ukb_icd_diagnosis}}
#' \item \code{\link{ukb_icd_freq_by}}
#' \item \code{\link{ukb_icd_keyword}}
#' \item \code{\link{ukb_icd_prevalence}}
#' }
#'
#' @section Datasets:
#' \itemize{
#' \item \code{\link{ukbcentre}}
#' \item \code{\link{icd10chapters}}
#' \item \code{\link{icd10codes}}
#' \item \code{\link{icd9chapters}}
#' \item \code{\link{icd9codes}}
#' }
#'
#' @docType package
#' @name ukbtools
NULL
