
#' Genetic metadata
#'
#' UKB have published \href{http://www.ukbiobank.ac.uk/wp-content/uploads/2014/04/UKBiobank_genotyping_QC_documentation-web.pdf}{full details of genotyping and quality control} for the interim genotype data.
#'
#' @export
#' @param data A UKB dataset created with \code{\link{ukb_df}}.
#'
ukb_gen_meta <-  function(data) {
  data %>%
    select(
      eid,
      uk_biobank_assessment_centre_0_0,
      genetic_ethnic_grouping_0_0,
      average_x_chromosome_intensities_for_determining_sex_0_0,
      average_y_chromosome_intensities_for_determining_sex_0_0,
      genetic_sex_0_0,
      missingness_0_0,
      recommended_genomic_analysis_exclusions_0_0,
      ukbileve_unrelatedness_indicator_0_0,
      ukbileve_affymetrix_quality_control_for_samples_0_0,
      ukbileve_genotype_quality_control_for_samples_0_0
    ) %>%
    mutate(
      bileve_chip = ifelse(
        !is.na(ukbileve_affymetrix_quality_control_for_samples_0_0), 1, 0
      )
    ) %>%
    as.data.frame()
}



#' Genetic principal components
#'
#' These are the principal components derived on the UK Biobank subsample with interim genotype data. UKB have published \href{http://www.ukbiobank.ac.uk/wp-content/uploads/2014/04/UKBiobank_genotyping_QC_documentation-web.pdf}{full details of genotyping and quality control} for the interim genotype data.
#'
#' @export
#' @param data A UKB dataset created with \code{\link{ukb_df}}.
#'
ukb_gen_pcs <- function(data) {
  data %>%
    select(
      eid,
      matches("genetic_principal_components")) %>%
    as.data.frame()
}



#' Sample exclusions
#'
#' This list of sample exclusions includes UKB's "recommended", "affymetrix quality control", and "genotype quality control" exclusions. UKB have published \href{http://www.ukbiobank.ac.uk/wp-content/uploads/2014/04/UKBiobank_genotyping_QC_documentation-web.pdf}{full details of genotyping and quality control} for the interim genotype data.
#'
#' @export
#' @param data A UKB dataset created with \code{\link{ukb_df}}.
#'
ukb_gen_excl <- function(data) {
  data %>%
    filter(
      !is.na(recommended_genomic_analysis_exclusions_0_0) |
        (ukbileve_affymetrix_quality_control_for_samples_0_0 == "Fail" &
           !is.na(ukbileve_affymetrix_quality_control_for_samples_0_0)
        ) |
        (ukbileve_genotype_quality_control_for_samples_0_0 == "Fail" &
           !is.na(ukbileve_genotype_quality_control_for_samples_0_0)
        )
    ) %>%
    pull(eid)
}



#' Relatedness exclusions
#'
#' UKB have published \href{http://www.ukbiobank.ac.uk/wp-content/uploads/2014/04/UKBiobank_genotyping_QC_documentation-web.pdf}{full details of genotyping and quality control} for the interim genotype data.
#'
#' @export
#' @param data A UKB dataset created with \code{\link{ukb_df}}.
#'
ukb_gen_rel <- function(data) {
  rbind(
    data %>%
      select(
        eid,
        pair = genetic_relatedness_pairing_0_0,
        kinship = genetic_relatedness_factor_0_0
      ),
    data %>%
      select(
        eid,
        pair = genetic_relatedness_pairing_0_1,
        kinship = genetic_relatedness_factor_0_1
      ),
    data %>%
      select(
        eid,
        pair = genetic_relatedness_pairing_0_2,
        kinship = genetic_relatedness_factor_0_2
      ),
    data %>%
      select(
        eid,
        pair = genetic_relatedness_pairing_0_3,
        kinship = genetic_relatedness_factor_0_3
      ),
    data %>%
      select(
        eid,
        pair = genetic_relatedness_pairing_0_4,
        kinship = genetic_relatedness_factor_0_4
      )
  ) %>%
    na.omit() %>%
    as_tibble() %>%
    arrange(pair) %>%
    as.data.frame()
}



#' Heterozygosity outliers
#'
#' UKB have published \href{http://www.ukbiobank.ac.uk/wp-content/uploads/2014/04/UKBiobank_genotyping_QC_documentation-web.pdf}{full details of genotyping and quality control} for the interim genotype data.
#'
#' @export
#' @param data A UKB dataset created with \code{\link{ukb_df}}.
#'
ukb_gen_het <- function(data) {
  data %>%
    filter(
      heterozygosity_0_0 < (mean(heterozygosity_0_0, na.rm = TRUE) - (3 * sd(heterozygosity_0_0, na.rm = TRUE))) |
        heterozygosity_0_0 > (mean(heterozygosity_0_0, na.rm = TRUE) + (3 * sd(heterozygosity_0_0, na.rm = TRUE)))
    ) %>%
    pull(eid)
}


#' Inserts UKB centre names into data
#'
#' Useful if your UKB centre variable \code{uk_biobank_assessment_centre_0_0} has not been populated with named levels.
#'
#' @export
#' @param data A UKB dataset created with \code{\link{ukb_df}}.
#' @return A dataframe with an additional column \code{ukb_centre} - UKB assessment centre names
#'
ukb_centre <- function(data){
  centre_lookup <- .lookup(ukbcentre, "code", "centre")
  data$ukb_centre <- centre_lookup[as.factor(data$uk_biobank_assessment_centre_0_0)]

  return(data)
}
