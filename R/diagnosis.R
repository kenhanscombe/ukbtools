
#' Retrieves diagnoses for an individual.
#'
#' @param data A UKB dataset (or subset) created with \code{\link{ukb_df}}.
#' @param id An individual's id, i.e., their unique eid reference number.
#' @param icd.version The ICD version (or revision) number, 9 or 10.
#'
#' @seealso \code{\link{ukb_df}}, \code{\link{ukb_icd_code_meaning}}, \code{\link{ukb_icd_keyword}}, \code{\link{ukb_icd_prevalence}}
#'
#' @import dplyr
#' @importFrom magrittr "%>%"
#' @export
#'
ukb_icd_diagnosis <- function(data, id, icd.version = NULL) {
  if (!is.null(icd.version) && !(icd.version %in% 9:10)) {
    stop(
      "`icd.version` is an invalid ICD revision number.
      Enter 9 for ICD9, or 10 for ICD10",
      call. = FALSE
    )
  }

  icd <- if (icd.version == 9) {
    ukbtools::icd9codes
  } else if (icd.version == 10){
    ukbtools::icd10codes
  }

  individual_codes <- data %>%
    dplyr::filter(eid == id) %>%
    dplyr::select(matches(paste("^diagnoses.*icd", icd.version, sep = ""))) %>%
    dplyr::select_if(colSums(!is.na(.)) > 0) %>%
    t() %>%
    as.vector()

  if(sum(!is.na(individual_codes)) < 1) {
    message(paste("ID", id, "has no ICD", icd.version, "diagnoses", sep = " "))
  } else {
    message(paste("ID", id, "has ICD", icd.version, "diagnoses:", sep = " "))
    print(ukb_icd_code_meaning(c(individual_codes), icd.version))
    cat("\n")
  }
}



#' Retrieves description for a ICD code.
#'
#' @param icd.version The ICD version (or revision) number, 9 or 10.
#' @param icd.code The ICD diagnosis code to be looked up.
#'
#' @seealso \code{\link{ukb_icd_diagnosis}}, \code{\link{ukb_icd_keyword}}, \code{\link{ukb_icd_prevalence}}
#'
#' @import dplyr
#' @importFrom magrittr "%>%"
#' @export
#'
ukb_icd_code_meaning <- function(icd.code, icd.version) {
  icd <- if (icd.version == 9) {
    ukbtools::icd9codes
  } else if (icd.version == 10){
    ukbtools::icd10codes
  }

  if(is.name(substitute(icd.code))) {
    char_code <- deparse(substitute(icd.code))
    icd %>%
      dplyr::filter(code %in% char_code)
  } else if (is.character(icd.code)){
    icd %>%
      dplyr::filter(code %in% icd.code)
  }
}



#' Retrieves diagnoses containing a description.
#'
#' @param icd.version The ICD version (or revision) number, 9 or 10.
#' @param description A regular expression to be looked up in the ICD descriptions, e.g., "cardiovascular"
#'
#' @seealso \code{\link{ukb_icd_diagnosis}}, \code{\link{ukb_icd_code_meaning}}, \code{\link{ukb_icd_prevalence}}
#'
#' @import dplyr
#' @importFrom magrittr "%>%"
#' @export
#'
ukb_icd_keyword <- function(icd.version, description) {
  icd <- if (icd.version == 9) {
    ukbtools::icd9codes
  } else if (icd.version == 10){
    ukbtools::icd10codes
  }

  icd %>%
    dplyr::filter(grepl(description, .$meaning))
}



#' Returns the prevalence for an ICD diagnosis
#'
#' @param data A UKB dataset (or subset) created with \code{\link{ukb_df}}.
#' @param icd.version The ICD version (or revision) number, 9 or 10.
#' @param icd.code An ICD disease code e.g. "I74". Use a regular expression to specify a broader set of diagnoses, e.g. "I" captures all Diseases of the circulatory system, I00-I99, "C|D[0-4]." captures all Neoplasms, C00-D49.
#'
#' @seealso \code{\link{ukb_icd_diagnosis}}, \code{\link{ukb_icd_code_meaning}}, \code{\link{ukb_icd_keyword}}
#'
#' @import dplyr
#' @importFrom magrittr "%>%"
#' @importFrom purrr map_df
#' @export
#'
ukb_icd_prevalence <- function(data, icd.version, icd.code) {

  n_observations <- nrow(data)

  ukb_case <- data %>%
    dplyr::select(matches(paste("^diagnoses.*icd", icd.version, sep = ""))) %>%
    purrr::map_df(~ grepl(icd.code, .)) %>%
    (function(x) apply(x, 1, any))(.)

  sum(ukb_case)/n_observations
}
