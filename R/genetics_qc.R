
# UKB Resource 664: Accessing Genetic Data within UK Biobank
# http://biobank.ctsu.ox.ac.uk/crystal/refer.cgi?id=664

# UKB Resource 531: Description of genetic data types
# https://biobank.ctsu.ox.ac.uk/crystal/refer.cgi?id=531


# Sample QC ---------------------------------------------------------------
# File: ukb_sqc_v2.txt

# Column names
# UKB NOTE: There are currently 2 versions of this file in circulation. The newer version is described below and contains column headers on the first row. The older (deprecated) version lacks the column headers and has two additional Affymetrix internal values prefixing the columns listed below.

# sqc_col_names <- stringr::str_replace_all(
#   tolower(c(
#     "x1",
#     "x2",
#     "genotyping.array",
#     "Batch",
#     "Plate.Name",
#     "Well",
#     "Cluster.CR",
#     "dQC",
#     "Internal.Pico..ng.uL.",
#     "Submitted.Gender", # 10
#     "Inferred.Gender", # 11
#     "X.intensity",
#     "Y.intensity",
#     "Submitted.Plate.Name",
#     "Submitted.Well",
#     "sample.qc.missing.rate",
#     "heterozygosity",
#     "heterozygosity.pc.corrected",
#     "het.missing.outliers", # 19
#     "putative.sex.chromosome.aneuploidy",
#     "in.kinship.table",
#     "excluded.from.kinship.inference",
#     "excess.relatives",
#     "in.white.British.ancestry.subset",
#     "used.in.pca.calculation",
#     paste("pc", 1:40, sep = ""), # 26:65
#     "in.Phasing.Input.chr1_22",
#     "in.Phasing.Input.chrX",
#     "in.Phasing.Input.chrXY"
#   )),
#   "[[:punct:]]", "_"
# )




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
#' @export
#' @examples
#' \dontrun{
#' # Use UKB supplied program `ukbgene` to retrieve genetic relatedness file ukbA_rel_sP.txt. See \href{http://biobank.ctsu.ox.ac.uk/crystal/refer.cgi?id=664}{UKB Resource 664}. With the whitespace delimited file read into R as e.g. ukb_relatedness, generate a dataframe of counts or a plot as follows:
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




# Maximal set of unrelateds for a given phenotype

# PLINK tries to maximize the final sample size, but this maximum independent set problem is NP-hard, so we use a greedy algorithm which does not guarantee an optimal result. In practice, PLINK --rel-cutoff does yield maximal sets whenever there aren't too many intertwined close relations, and it outperforms GCTA --grm-cutoff when there are (we chose our greedy algorithm carefully); but if you want to try to beat both programs, use the --make-rel and --keep/--remove flags and patch your preferred approximation algorithm in between. (We may add one or two levels of backtracking to our --rel-cutoff if its level of imperfection becomes problematic.)


# Re. relatedness filtering,
# To: UKB-GENETICS@JISCMAIL.AC.UK
# Date:    Wed, 26 Jul 2017 17:06:01 +0100
# From:    Clare Bycroft <clare@WELL.OX.AC.UK>
#
# Subject: A list of unrelated samples
#
# (...) you could use the list of samples which we used to calculate the PCs,
# which is a (maximal) subset of unrelated participants after applying some QC
# filtering. Please read supplementary Section S 3.3.2 for details. You can
# find the list of samples using the “used.in.pca.calculation" column in the
# sample-QC file (ukb_sqc_v2.txt) (...). **Note that this set contains diverse
# ancestries. If you take the intersection with the white British ancestry
# subset you get ~337,500 unrelated samples.**

# IBD = 0.25 (second degree relatives e.g. grandparent-grandchild)
# IBD = 0.125 (third degree relatives e.g. full cousins)

# Weale (2010)
# (...) empirical threshold for QC set at half-way between second degree and third degree relatives (i.e., at IBD = 0.1875). For related pairs or family groups above this threshold, the usual QC step is to leave one individual in the dataset and drop the other or others, based for example on the one with the least missingness.
# duplicated/ related excl >0.1875


# KING kinship coefficient:
# >0.354 duplicate/MZ twin
# [0.177, 0.354] 1st-degree
# [0.0884, 0.177] 2nd-degree
# [0.0442, 0.0884] 3rd-degree

# What does KING kinship coefficient = -1 mean?




#' Subset of the UKB relatedness dataframe with data
#'
#' @param data The UKB relatedness data as a dataframe (header: ID1, ID2, HetHet, IBS0, Kinship)
#' @param ukb_with_data A character vector of ukb eids with data on the phenotype of interest
#' @param cutoff KING kingship coefficient cutoff (default 0.0884 includes pairs with greater than 3rd-degree relatedness)
#'
#' @return A dataframe (header: ID1, ID2, HetHet, IBS0, Kinship) for the subset of individuals with data.
#' @import tidyr dplyr
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
#' @param data The UKB relatedness data as a dataframe (header: ID1, ID2, HetHet, IBS0, Kinship)
#' @param ukb_with_data A character vector of ukb eids with data on the phenotype of interest
#' @param cutoff KING kingship coefficient cutoff (default 0.0884 includes pairs with greater than 3rd-degree relatedness)
#'
#' @return A dataframe (header: ID1, ID2, HetHet, IBS0, Kinship) for the subset of individuals with data.
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
      head(n = 1) %>%
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
