
# UKB Resource 664: Accessing Genetic Data within UK Biobank
# http://biobank.ctsu.ox.ac.uk/crystal/refer.cgi?id=664

# UKB Resource 531: Description of genetic data types
# https://biobank.ctsu.ox.ac.uk/crystal/refer.cgi?id=531


# Sample QC ---------------------------------------------------------------
# File: ukb_sqc_v2.txt

#


#' Sample QC column names
#'
#' @description The UKB sample QC file has no header on it.
#'
#' @param data The UKB ukb_sqc_v2.txt data as dataframe. (Not necessary if column names only are required)
#' @param col_names_only If \code{TRUE} returns a character vector of column names (\code{data} argument not required). Useful if you would like to supply as header when reading in your sample QC data. If \code{FALSE} (Default), returns the supplied dataframe with column names (Checks number of columns in supplied data. See Details.).
#'
#' @return A sample QC dataframe with column names, or a character vector of column names if \code{col_names_only = TRUE}.
#' @details From \href{https://biobank.ctsu.ox.ac.uk/crystal/refer.cgi?id=531}{UKB Resource 531}: There are currently 2 versions of this file (UKB ukb_sqc_v2.txt) in circulation. The newer version is described below and contains column headers on the first row. The older (deprecated) version lacks the column headers and has two additional Affymetrix internal values prefixing the columns listed below.
#' @import stringr
#' @export
ukb_gen_sqc_names <- function(data, col_names_only = FALSE) {

  sqc_col_names <- stringr::str_replace_all(
    tolower(c(
      "x1",
      "x2",
      "genotyping.array",
      "Batch",
      "Plate.Name",
      "Well",
      "Cluster.CR",
      "dQC",
      "Internal.Pico..ng.uL.",
      "Submitted.Gender",
      "Inferred.Gender",
      "X.intensity",
      "Y.intensity",
      "Submitted.Plate.Name",
      "Submitted.Well",
      "sample.qc.missing.rate",
      "heterozygosity",
      "heterozygosity.pc.corrected",
      "het.missing.outliers",
      "putative.sex.chromosome.aneuploidy",
      "in.kinship.table",
      "excluded.from.kinship.inference",
      "excess.relatives",
      "in.white.British.ancestry.subset",
      "used.in.pca.calculation",
      paste("pc", 1:40, sep = ""),
      "in.Phasing.Input.chr1_22",
      "in.Phasing.Input.chrX",
      "in.Phasing.Input.chrXY"
    )),
    "[[:punct:]]", "_"
  )

  if (col_names_only) {
    message("If the number of columns in your sample QC file is not 68, drop the first two values `x1` and `x2`. See Details for excerpt from UKB Resource 531.")
    sqc_col_names
  }

  if (ncol(data) == 68)  {
    names(data) <- sqc_col_names
    return(data)
  } else if (ncol(data) == 66) {
    names(data) <- sqc_col_names[-c(1,2)]
    return(data)
  }
}




# Relatedness -------------------------------------------------------------

#' Relatedness count
#'
#' @description Creates a summary count table of the number of individuals and pairs at each degree of relatedness that occurs in the UKB sample, and an optional plot.
#'
#' @param data A dataframe of the genetic relatedness data including \href{http://people.virginia.edu/~wc9c/KING/manual.html}{KING kinship coefficient}, and proportion of alleles IBS = 0. See Details.
#' @return If \code{plot = FALSE} (default), a count of individuals and pairs at each level of relatedness. If \code{plot = TRUE}, reproduces the scatterplot of genetic relatedness against proportion of SNPs shared IBS=0 (each point representing a pair of related UKB individuals) from the \href{http://www.ukbiobank.ac.uk/wp-content/uploads/2014/04/UKBiobank_genotyping_QC_documentation-web.pdf}{genotyping and quality control} documentation.
#' @param plot Logical indicating whether to plot relatedness figure. Default = FALSE.
#'
#' @details Use UKB supplied program `ukbgene` to retrieve genetic relatedness data file ukbA_rel_sP.txt. See \href{http://biobank.ctsu.ox.ac.uk/crystal/refer.cgi?id=664}{UKB Resource 664}. The count and plot include individuals with IBS0 >= 0.
#'
#' @import dplyr ggplot2
#' @importFrom magrittr "%>%"
#' @seealso \code{\link{ukb_gen_related_with_data}}, \code{\link{ukb_gen_samples_to_remove}}
#' @export
#' @examples
#' \dontrun{
#' # Use UKB supplied program `ukbgene` to retrieve genetic relatedness file ukbA_rel_sP.txt.
#' See \href{http://biobank.ctsu.ox.ac.uk/crystal/refer.cgi?id=664}{UKB Resource 664}.
#' With the whitespace delimited file read into R as e.g. ukb_relatedness,
#' generate a dataframe of counts or a plot as follows:
#'
#' ukb_gen_rel_count(ukb_relatedness)
#' ukb_gen_rel_count(ukb_relatedness, plot = TRUE)
#' }
ukb_gen_rel_count <- function(data, plot = FALSE) {
  relatedness <- data %>%
    dplyr::rename_all(tolower) %>%
    dplyr::filter(ibs0 >= 0) %>%
    dplyr::mutate(
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
      theme(
        legend.position = "bottom",
        panel.grid = element_blank()
      )
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




#' Subset of the UKB relatedness dataframe with data
#'
#' @param data The UKB relatedness data as a dataframe (header: ID1, ID2, HetHet, IBS0, Kinship)
#' @param ukb_with_data A character vector of ukb eids with data on the phenotype of interest
#' @param cutoff KING kingship coefficient cutoff (default 0.0884 includes pairs with greater than 3rd-degree relatedness)
#'
#' @return A dataframe (header: ID1, ID2, HetHet, IBS0, Kinship) for the subset of individuals with data.
#' @import tidyr dplyr
#' @seealso \code{\link{ukb_gen_rel_count}}, \code{\link{ukb_gen_samples_to_remove}}
#' @export
ukb_gen_related_with_data <- function(data, ukb_with_data, cutoff = 0.0884) {
  data %>%
    dplyr::filter(
      Kinship > cutoff &
             !(!(ID1 %in% ukb_with_data) | !(ID2 %in% ukb_with_data))
      )
}




#' Related samples (with data on the variable of interest) to remove
#'
#' @description There are many ways to remove related individuals from phenotypic data for genetic analyses. You could simply exclude all individuals indicated as having "excess relatedness" and include those "used in pca calculation" (these variables are included in the sample QC data, ukb_sqc_v2.txt) - see details. This list is based on the complete dataset, and possibly removes more samples than you need to for your phenotype of interest. Ideally, you want a maximum independent set, i.e., to remove the minimum number of individuals with data on the phenotype of interest, so that no pair exceeds some cutoff for relatedness. \code{ukb_gen_samples_to_remove} returns a list of samples to remove in to achieve a maximal set of unrelateds for a given phenotype.
#'
#' @details Trims down the UKB relatedness data before selecting individuals to exclude, using the algorithm: step 1. remove pairs below KING kinship coefficient 0.0884 (3rd-degree or less related, by default. Can be set with \code{cutoff} argument), and any pairs if either member does not have data on the phenotype of interest. The user supplies a vector of samples with data. step 2. count the number of "connections" (or relatives) each participant has and add to "samples to exclude" the individual with the most connections. This is the greedy part of the algorithm. step 3. repeat step 2 till all remaining participants only have 1 connection, then add one random member of each remaining pair to "samples to exclude" (adds all those listed under ID2)
#'
#' \emph{Another approach from the UKB email distribution list:}
#'
#' To: UKB-GENETICS@JISCMAIL.AC.UK
#' Date:    Wed, 26 Jul 2017 17:06:01 +0100
#' \strong{Subject: A list of unrelated samples}
#'
#' (...) you could use the list of samples which we used to calculate the PCs,
#' which is a (maximal) subset of unrelated participants after applying some QC
#' filtering. Please read supplementary Section S3.3.2 for details. You can
#' find the list of samples using the "used.in.pca.calculation" column in the
#' sample-QC file (ukb_sqc_v2.txt) (...). Note that this set contains diverse
#' ancestries. If you take the intersection with the white British ancestry
#' subset you get ~337,500 unrelated samples.
#'
# What does KING kinship coefficient = -1 mean?
#'
#' @param data The UKB relatedness data as a dataframe (header: ID1, ID2, HetHet, IBS0, Kinship)
#' @param ukb_with_data A character vector of ukb eids with data on the phenotype of interest
#' @param cutoff KING kingship coefficient cutoff (default 0.0884 includes pairs with greater than 3rd-degree relatedness)
#'
#' @return An integer vector of UKB IDs to remove.
#' @seealso \code{\link{ukb_gen_rel_count}}, \code{\link{ukb_gen_related_with_data}}
#' @import tidyr dplyr
#' @export
ukb_gen_samples_to_remove <- function(data, ukb_with_data, cutoff = 0.0884) {

  data <- ukb_gen_related_with_data(data, ukb_with_data = ukb_with_data, cutoff = cutoff)
  remove_samples <- vector(mode = "integer")

  connections <- data %>%
    tidyr::gather(key = "Label", value = "ID", -HetHet, -IBS0, -Kinship) %>%
    dplyr::count(ID, sort = TRUE)

  while(!all(connections[["n"]] == 1)) {
    df_id_long <- data %>%
      tidyr::gather(key = "Label", value = "ID", -HetHet, -IBS0, -Kinship)

    remove_samples <- df_id_long %>%
      dplyr::left_join(count(df_id_long, ID), by = "ID") %>%
      dplyr::arrange(desc(n)) %>%
      utils::head(n = 1) %>%
      .[["ID"]] %>%
      append(remove_samples, values = .)

    data <- data %>%
      dplyr::filter(!(ID1 %in% remove_samples | ID2 %in% remove_samples))

    connections <- data %>%
      tidyr::gather(key = "Label", value = "ID", -HetHet, -IBS0, -Kinship) %>%
      dplyr::count(ID, sort = TRUE)
  }

  remove_samples <- data %>%
      .[["ID2"]] %>%
      append(remove_samples, values = .)

  return(remove_samples)
}
