
#' Genetic metadata
#'
#' UKB have published \href{http://www.ukbiobank.ac.uk/wp-content/uploads/2014/04/UKBiobank_genotyping_QC_documentation-web.pdf}{full details of genotyping and quality control} for the interim genotype data. This function retrieves UKB assessment centre codes and assessment centre names, genetic ethnic grouping, genetically-determined sex, missingness, UKB recommended genomic analysis exclusions, BiLeve unrelatedness indicator, and BiLeve Affymetrix and genotype quality control.
#'
#' @export
#' @param data A UKB dataset created with \code{\link{ukb_df}}.
#'
ukb_gen_meta <-  function(data) {

  centre_lookup <- lookup(ukbcentre, "code", "centre")

  data %>%
    mutate(
      bileve_chip = ifelse(!is.na(ukbileve_affymetrix_quality_control_for_samples_0_0), 1, 0),
      ukb_centre_name = centre_lookup[as.factor(uk_biobank_assessment_centre_0_0)]
    ) %>%
    select(
      eid,
      uk_biobank_assessment_centre_0_0,
      ukb_centre_name,
      genetic_ethnic_grouping_0_0,
      genetic_sex_0_0,
      missingness_0_0,
      recommended_genomic_analysis_exclusions_0_0,
      ukbileve_unrelatedness_indicator_0_0,
      ukbileve_affymetrix_quality_control_for_samples_0_0,
      ukbileve_genotype_quality_control_for_samples_0_0,
      bileve_chip
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



#' Creates a table of related individuals
#'
#' Makes a data.frame containing all related individuals with columns UKB ID, pair ID, \href{http://people.virginia.edu/~wc9c/KING/manual.html}{KING kinship coefficient}, and proportion of alleles IBS = 0. UKB have published \href{http://www.ukbiobank.ac.uk/wp-content/uploads/2014/04/UKBiobank_genotyping_QC_documentation-web.pdf}{full details of genotyping and quality control} including details on relatedness calculations for the interim genotype data.
#'
#' @export
#' @param data A UKB dataset created with \code{\link{ukb_df}}.
#'
#' @seealso \code{\link{ukb_gen_rel_count}}
#'
ukb_gen_rel <- function(data) {
  rbind(
    data %>%
      select(
        eid,
        pair = genetic_relatedness_pairing_0_0,
        kinship = genetic_relatedness_factor_0_0,
        ibs0 = genetic_relatedness_ibs0_0_0
      ),
    data %>%
      select(
        eid,
        pair = genetic_relatedness_pairing_0_1,
        kinship = genetic_relatedness_factor_0_1,
        ibs0 = genetic_relatedness_ibs0_0_1
      ),
    data %>%
      select(
        eid,
        pair = genetic_relatedness_pairing_0_2,
        kinship = genetic_relatedness_factor_0_2,
        ibs0 = genetic_relatedness_ibs0_0_2
      ),
    data %>%
      select(
        eid,
        pair = genetic_relatedness_pairing_0_3,
        kinship = genetic_relatedness_factor_0_3,
        ibs0 = genetic_relatedness_ibs0_0_3
      ),
    data %>%
      select(
        eid,
        pair = genetic_relatedness_pairing_0_4,
        kinship = genetic_relatedness_factor_0_4,
        ibs0 = genetic_relatedness_ibs0_0_4
      )
  ) %>%
    na.omit() %>%
    as_tibble() %>%
    arrange(pair) %>%
    as.data.frame()
}



#' Relatedness count
#'
#' Creates a summary count table of the number of individuals and pairs at each at each degree of relatedness that occurs in the UKB sample, and an optional plot.
#'
#' @export
#' @param data A dataframe of UKB ID, pair ID, \href{http://people.virginia.edu/~wc9c/KING/manual.html}{KING kinship coefficient}, and proportion of alleles IBS = 0 created with \code{\link{ukb_gen _rel}}.
#' @return If \code{plot = FALSE} (default), a count of individuals and pairs at each level of relatedness. If \code{plot = TRUE}, reproduces the scatterplot of genetic relatedness against proportion of SNPs shared IBS=0 (each point representing a pair of related UKB individuals) from the \href{http://www.ukbiobank.ac.uk/wp-content/uploads/2014/04/UKBiobank_genotyping_QC_documentation-web.pdf}{genotyping and quality control} documentation.
#'
#' @seealso \code{\link{ukb_gen_rel}}
#'
ukb_gen_rel_count <- function(data, plot = FALSE) {

  relatedness <- data %>%
    mutate(
      category_related = cut(
        kinship,
        breaks = rev(c(0.044, 0.088, 0.177, 0.354, Inf)),
        labels = rev(c("Duplicates/MZ twins","1st-degree","2nd-degree","3rd-degree"))
      ),
      ped_related = ifelse(
        !(category_related %in% "1st"),
        as.character(category_related),
        ifelse(ibs0 < 0.0020,
               "Parent-offspring",
               "Full siblings")
      )
    )

  if (plot) {
    relatedness %>%
      ggplot(aes(ibs0, kinship, color = ped_related)) +
      geom_jitter() +
      labs(
        x = "Proportion of SNPs IBS = 0",
        y = "KING kinship coefficient",
        color = "Relatedness"
      ) +
      theme(legend.position = "bottom")
  } else {
    relatedness %>%
      count(ped_related) %>%
      mutate(pairs = round(n/2)) %>%
      rename(
        relationship = ped_related,
        individuals = n
      ) %>%
      as.data.frame()
  }
}



#' Heterozygosity outliers
#'
#' Heterozygosity outliers are typically removed from genetic association analyses. This function returns either a vector of heterozygosity outliers to remove (+/- 3sd from mean heterozygosity), or a data frame with heterozygosity scores for all samples.
#'
#' @export
#' @param data A UKB dataset created with \code{\link{ukb_df}}.
#' @param all.het Set \code{all.het = TRUE} for heterozygosity scores for all samples. By default \code{all.het = FALSE} returns a vector of sample IDs for individuals +/-3SD from the mean heterozygosity.
#'
#' @return A vector of IDs if \code{all.het = FALSE} (default), or a dataframe with ID, heterozygosity and PCA-corrected heterozygosity if \code{all.het = TRUE}.
#'
#' @details UKB have published \href{http://www.ukbiobank.ac.uk/wp-content/uploads/2014/04/UKBiobank_genotyping_QC_documentation-web.pdf}{full details of genotyping and quality control} for the interim genotype data.
#'
ukb_gen_het <- function(data, all.het = FALSE) {
  if (all.het) {
    return(
      data %>%
        select(eid, heterozygosity_0_0, heterozygosity_pca_corrected_0_0) %>%
        as.data.frame()
    )
  } else {
    data %>%
      filter(
        heterozygosity_0_0 < (mean(heterozygosity_0_0, na.rm = TRUE) - (3 * sd(heterozygosity_0_0, na.rm = TRUE))) |
          heterozygosity_0_0 > (mean(heterozygosity_0_0, na.rm = TRUE) + (3 * sd(heterozygosity_0_0, na.rm = TRUE)))
      ) %>%
      pull(eid)
  }
}


#' Inserts UKB centre names into data
#'
#' Inserts a column with centre name, \code{ukb_centre}, into the supplied data.frame. Useful if your UKB centre variable \code{uk_biobank_assessment_centre_0_0} has not been populated with named levels.
#'
#' @export
#' @param data A UKB dataset created with \code{\link{ukb_df}}.
#' @return A dataframe with an additional column \code{ukb_centre} - UKB assessment centre names
#'
ukb_centre <- function(data) {
  centre_lookup <- lookup(ukbcentre, "code", "centre")
  data$ukb_centre <- centre_lookup[as.factor(data$uk_biobank_assessment_centre_0_0)]

  return(data)
}
