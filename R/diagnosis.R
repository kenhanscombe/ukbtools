
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
#' @importFrom purrr map
#' @importFrom tibble as_tibble
#' @export
#' @examples
#' \dontrun{
#' ukb_icd_diagnosis(my_ukb_data, id = "123456", icd.version = 10)
#' }
#'
ukb_icd_diagnosis <- function(data, id, icd.version = NULL) {

  if (!all(id %in% data$eid)) {
    stop(
      "Invalid UKB sample id. Check all ids are included in the supplied data",
      call. = FALSE
    )
  }

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
    dplyr::filter(eid %in% id) %>%
    dplyr::select(matches(paste("^diagnoses.*icd", icd.version, sep = ""))) %>%
    dplyr::select_if(colSums(!is.na(.)) > 0) %>%
    t() %>%
    tibble::as_tibble()

  colnames(individual_codes) <- id

  if(ncol(individual_codes) == 1 & sum(!is.na(individual_codes[[1]])) < 1) {
    message(paste("ID", id, "has no ICD", icd.version, "diagnoses", sep = " "))
  } else {

    d <- individual_codes %>%
      purrr::map(~ ukb_icd_code_meaning(c(.), icd.version)) %>%
      dplyr::bind_rows(.id = "sample")

    no_icd <- id[!(id %in% unique(d$sample))]
    if(length(no_icd) > 0) message("ID(s) ", paste(no_icd, " "), "have no ICD ", icd.version, " diagnoses.")

    return(d)

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
#' @examples
#' ukb_icd_code_meaning(icd.code = "I74", icd.version = 10)
#'
ukb_icd_code_meaning <- function(icd.code, icd.version = 10) {
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
#' Returns a dataframe of ICD code and descriptions for all entries including any supplied keyword.
#'
#' @param icd.version The ICD version (or revision) number, 9 or 10. Default = 10.
#' @param description A character vector of one or more keywords to be looked up in the ICD descriptions, e.g., "cardio", c("cardio", "lymphoma"). Each keyword can be a regular expression, e.g. "lymph*".
#'
#' @seealso \code{\link{ukb_icd_diagnosis}}, \code{\link{ukb_icd_code_meaning}}, \code{\link{ukb_icd_prevalence}}
#'
#' @import dplyr
#' @importFrom magrittr "%>%"
#' @export
#' @examples
#' ukb_icd_keyword("cardio", icd.version = 10)
#'
ukb_icd_keyword <- function(description, icd.version = 10) {
  icd <- if (icd.version == 9) {
    ukbtools::icd9codes
  } else if (icd.version == 10){
    ukbtools::icd10codes
  }

  icd %>%
    dplyr::filter(grepl(paste(description, collapse = "|"), .$meaning, perl = TRUE))
}



#' Returns the prevalence for an ICD diagnosis
#'
#' @param data A UKB dataset (or subset) created with \code{\link{ukb_df}}.
#' @param icd.version The ICD version (or revision) number, 9 or 10. Default = 10.
#' @param icd.code An ICD disease code e.g. "I74". Use a regular expression to specify a broader set of diagnoses, e.g. "I" captures all Diseases of the circulatory system, I00-I99, "C|D[0-4]." captures all Neoplasms, C00-D49.
#'
#' @seealso \code{\link{ukb_icd_diagnosis}}, \code{\link{ukb_icd_code_meaning}}, \code{\link{ukb_icd_keyword}}
#'
#' @import dplyr
#' @importFrom magrittr "%>%"
#' @importFrom purrr map_df
#' @export
#' @examples
#' \dontrun{
#' # ICD-10 code I74, Arterial embolism and thrombosis
#' ukb_icd_prevalence(my_ukb_data, icd.version = 10, icd.diagnosis = "I74")
#'
#' # ICD-10 chapter 9, disease block I00–I99, Diseases of the circulatory system
#' ukb_icd_prevalence(my_ukb_data, icd.version = 10, icd.diagnosis = "I")
#'
#' # ICD-10 chapter 2, C00-D49, Neoplasms
#' ukb_icd_prevalence(my_ukb_data, icd.version = 10, icd.diagnosis = "C|D[0-4].")
#' }
#'
ukb_icd_prevalence <- function(data, icd.code, icd.version = 10) {

  ukb_case <- data %>%
    dplyr::select(matches(paste("^diagnoses.*icd", icd.version, sep = ""))) %>%
    purrr::map_df(~ grepl(icd.code, ., perl = TRUE)) %>%
    rowSums() > 0

  sum(ukb_case, na.rm = TRUE) / length(ukb_case)
}



#' Frequency of an ICD diagnosis by a target variable
#'
#' @param data A UKB dataset (or subset) created with \code{\link{ukb_df}}.
#' @param icd.code ICD disease code(s) e.g. "I74". Use a regular expression to specify a broader set of diagnoses, e.g. "I" captures all Diseases of the circulatory system, I00-I99, "C|D[0-4]." captures all Neoplasms, C00-D49. Default is the WHO top 3 causes of death globally in 2015, see \url{http://www.who.int/healthinfo/global_burden_disease/GlobalCOD_method_2000_2015.pdf?ua=1}.
#' @param reference.var UKB ICD frequencies will be calculated by levels of this variable. If continuous, by default it is cut into 10 intervals of approximately equal size (set with n.groups).
#' @param n.groups Number of approximately equal-sized groups to split a continuous variable into.
#' @param icd.version The ICD version (or revision) number, 9 or 10.
#' @param freq.plot If TRUE returns a plot of ICD diagnosis by target variable. If FALSE (default) returns a dataframe.
#' @param legend.col Number of columns for the legend. (Default = 1).
#' @param legend.pos Legend position, default = "right".
#' @param icd.labels Character vector of ICD labels for the plot legend. Default = V1 to VN.
#' @param plot.title Title for the plot. Default describes the default icd.codes, WHO top 6 cause of death 2015.
#'
#' @import dplyr ggplot2 parallel
#' @importFrom magrittr "%>%"
#' @importFrom stats complete.cases
#' @importFrom tidyr gather
#' @importFrom readr parse_factor
#' @importFrom scales percent
#' @importFrom foreach foreach "%:%" "%dopar%"
#' @importFrom doParallel registerDoParallel
#' @export
ukb_icd_freq_by <- function(
  data, reference.var, n.groups = 10,
  icd.code = c("^(I2[0-5])", "^(I6[0-9])", "^(J09|J1[0-9]|J2[0-2]|P23|U04)"),
  icd.labels = c("coronary artery disease", "cerebrovascular disease",
                 "lower respiratory tract infection"),
  plot.title = "", legend.col = 1, legend.pos = "right", icd.version = 10,
  freq.plot = FALSE) {

  data <- data %>%
    dplyr::filter(stats::complete.cases(.[[reference.var]])) %>%
    dplyr::select(reference.var, matches(paste("^diagnoses.*icd",
                                               icd.version, sep = "")))

  # Include categorical variable
  if (is.factor(data[[reference.var]]) | is.ordered(data[[reference.var]])) {
    categorized_var <- data[[reference.var]]
  } else {
    categorized_var <- factor(
      ggplot2::cut_number(data[[reference.var]], n = n.groups),
      ordered = TRUE
    )
  }

  l <- split(
    dplyr::select(data, matches(paste("^diagnoses.*icd", icd.version, sep = ""))),
    categorized_var
  )

  cl <- parallel::makeCluster(parallel::detectCores())
  doParallel::registerDoParallel(cl)
  x <- foreach::foreach(i = seq_along(l), .combine = "cbind") %:%
    foreach::foreach(j = seq_along(icd.code), .combine = "c") %dopar% {
      ukb_icd_prevalence(l[[i]], icd.version = 10, icd.code = icd.code[j])
    }
  stopImplicitCluster()
  stopCluster(cl)

  x <- as.data.frame(t(x)) %>%
    mutate(x, group = names(l))

  rm(l)


  if(is.numeric(data[[reference.var]])) {
    group_order <- order(as.numeric(gsub("[\\(\\[]", "", gsub(",.*$", "", x$group))))
    group_levels = x$group[group_order]
    x$group <- readr::parse_factor(x$group, levels = group_levels, ordered = TRUE,
                            include_na = FALSE)
  }


  # plot
  if(freq.plot) {
    p <- with(x, tidyr::gather(x, icd_code, frequency, -group, factor_key = TRUE)) %>%
      ggplot2::ggplot(aes_string(x = "group", y = "frequency",
                                 color = "icd_code", fill = "icd_code", group = "icd_code")) +
      labs(x = "Reference variable", y = "UKB frequency", title = plot.title,
           color = "", fill = "") +
      theme(title = element_text(face = "bold"), panel.grid = element_blank(),
            panel.background = element_rect(color = "grey97"),
            legend.key = element_blank(), legend.position = legend.pos,
            axis.ticks.x = element_blank()) +
      scale_y_continuous(labels = scales::percent)

    if(is.numeric(data[[reference.var]])){
      p +
        geom_point(size = 2) +
        geom_line(size = 0.5) +
        scale_x_discrete(labels = c("Low", rep("", n.groups - 2), "High")) +
        guides(color = guide_legend(ncol = legend.col), size = FALSE,
               fill = FALSE) +
        scale_color_discrete(labels = icd.labels)
    } else {
      p +
        geom_bar(stat = "identity", position = "dodge") +
        guides(fill = guide_legend(ncol = legend.col), size = FALSE,
               color = FALSE) +
        scale_fill_discrete(labels = icd.labels)
    }
  } else {
    return(x)
  }
}
