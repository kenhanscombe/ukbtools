
#' Retrieves diagnoses for an individual.
#'
#' @export
#' @param data A UKB dataset created with \code{\link{ukb_df}}.
#' @param id An individual's id, i.e., their unique eid reference number.
#' @param icd.version The ICD version (or revision) number, 9 or 10.
#'
#' @seealso \code{\link{ukb_df}}
#'
icd_diagnosis <- function(data, id, icd.version = NULL) {
  if (!is.null(icd.version) && !(icd.version %in% 9:10)) {
    stop(
      "`icd.version` is an invalid ICD revision number.
      Enter 9 for ICD9, or 10 for ICD10",
      call. = FALSE
    )
  }

  icd <- if (icd.version == 9) {
    icd9codes
  } else if (icd.version == 10){
    icd10codes
  }

  individual_codes <- data %>%
    filter(eid == id) %>%
    select(
      matches(paste("^diagnoses.*icd", icd.version, sep = ""))
    ) %>%
    unlist()

  icd_code(icd.version, individual_codes)
}



#' Retrieves description for a ICD code.
#'
#' @export
#' @param icd.version The ICD version (or revision) number, 9 or 10.
#' @param icd.code The ICD diagnosis code to be looked up.
#'
icd_code <- function(icd.code, icd.version) {
  icd <- if (icd.version == 9) {
    icd9codes
  } else if (icd.version == 10){
    icd10codes
  }

  if(is.name(substitute(icd.code))) {
    char_code <- deparse(substitute(icd.code))
    icd %>%
      filter(code %in% char_code)
  } else if (is.character(icd.code)){
    icd %>%
      filter(code %in% icd.code)
  }
}



#' Displays a table for the International Classification of Diseases (ICD)
#'
#' @export
#' @param icd.version The ICD icd.version (or revision) number, 9 or 10.
#'
icd_chapter <- function(icd.version) {
  if (icd.version == 9) {
    cat("\nICD-10 codes\n\n")
    icd9chapters
  } else if (icd.version == 10) {
    cat("\nICD-10 codes (xxx.yyy z)\n\n")
    cat("xxx = Category;  yyy = Etiology, anatomical site, severity;  z = Extension\n\n")
    icd10chapters
  }
}
