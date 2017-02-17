
#' Demographics of a sample subset, or an individual
#'
#' Describes a subset of UKB sample, or an individual, relative to the full UK Biobank sample, or a specified UKB subset. This function is intended as an exploratory data analysis and quality control tool, and as such only provides summary statistics and graphical context for individual's data.
#'
#' @param data A UKB dataset.
#' @param sample_sub A vector of UKB ids, or logical or row index, that defines the subset of interest.
#' @param sample_ref A vector of UKB ids (or a logical or row index), that defines an alternative reference population. By default the reference is the full sample.
#' @param individual A UKB individual id ("eid").
#'
ukb_context <- function(data, id = "eid", sample_sub, sample_ref = TRUE, individual = NULL, comparison_var) {

  # Resolve:
  # 1. reference sample
  # 2. subset sample or individual
  # 3. query variables (default demographic set)

}
