
#' Demographics of a UKB sample subset
#'
#' Describes a subset of the UKB sample, relative to a reference subsample, on the \href{http://biobank.ctsu.ox.ac.uk/crystal/label.cgi?id=1001}{UKB primary demographics} (sex, age, ethnicity, socioeconomic status). The "reference" and "comparison" samples are defined either by a variable of interest (\code{comparison.var} - those with data form the "comparison" subset and samples with missing data are the "reference" sample), or a logical vector (\code{sample.ref} - where \code{TRUE} values define the "comparison" and \code{FALSE} the "reference" sample) . This function is intended as an exploratory data analysis and quality control tool, and as such only provides summary statistics and graphical context for an individual's data.
#'
#' @param data A UKB dataset constructed with \code{\link{ukb_df}}.
#' @param comparison.var The variable of interest which defines the "comparison" (samples with data) and "reference" (samples without data, i.e. NA) samples.
#' @param age.var The demographic to be use for age, either "age_when_attended_assessment_centre_0_0" (default) or "year_of_birth_0_0" (age calculated as 2010 - "year_of_birth_0_0").
#' @param sample.ref A logical vector defining a "comparison" subset (TRUE) and "reference" subset (FALSE). Length must equal the number of rows in your \code{data}.
#'
#' @seealso \code{\link{ukb_df}}
#'
#' @import grid
#' @export
#'

ukb_context <- function(data, age.var = "age_when_attended_assessment_centre_0_0", comparison.var = NULL, sample.ref = NULL) {

  data$age <- if (age.var == "age_when_attended_assessment_centre_0_0") {
    age.var
  } else if (age.var == "year_of_birth_0_0") {
    2010 - data[, age.var]
  }

  data <- data %>%
    mutate(
      data_avail = if (is.null(comparison.var) && is.null(sample.ref)) {
        stop("Either supply a comparison variable (comparison.var), or a logical vector (sample.ref) to define reference and comparison samples", call. = FALSE)
      } else if (!is.null(comparison.var) && !is.null(sample.ref)) {
        stop("Either supply a comparison variable (comparison.var), or a logical vector (sample.ref) to define reference and comparison samples", call. = FALSE)
      } else if (is.null(sample.ref)) {
        !is.na(data[, comparison.var])
      } else if (!is.null(sample.ref) && (length(sample.ref) != nrow(data))) {
        stop("Length of sample.ref must equal the number of rows in your data", call. = FALSE)
      } else if (!is.null(sample.ref)) {
        sample.ref
      }
    ) %>%
    select(
      sex_0_0,
      age,
      townsend_deprivation_index_at_recruitment_0_0,
      ethnic_background_0_0,
      data_avail
    )


  # sex
  gender <- data %>%
    ggplot(
      aes(
        x = sex_0_0,
        fill = data_avail
      )) +
    geom_bar(position = "stack", na.rm = TRUE) +
    scale_fill_manual(
      values = c("grey35", "hotpink"),
      labels = c("Reference", "Subset")) +
    theme(legend.position = "top") +
    labs(
      x = "Sex",
      fill = "SAMPLE")

  # age
  years_old <- data %>%
    ggplot(
      aes(
        x = age,
        fill = data_avail,
        color = data_avail)) +
    geom_density(na.rm = TRUE) +
    scale_fill_manual(values = c("grey35", NA)) +
    scale_color_manual(values = c("grey35", "hotpink")) +
    theme(legend.position = "none") +
    labs(x = "Age")


  # ethnicity
  ethnicity <- data %>%
    ggplot(
      aes(
        x = ethnic_background_0_0,
        fill = data_avail
      )) +
    geom_bar(position = "fill", na.rm = TRUE) +
    scale_fill_manual(values = c("grey35", "hotpink")) +
    coord_flip() +
    theme(legend.position = "none") +
    labs(
      x = "Ethnic Background",
      y = "Proportion"
      )

  # ses
  socioeconomic <- data %>%
    ggplot(
      aes(
        x = townsend_deprivation_index_at_recruitment_0_0,
        fill = data_avail,
        color = data_avail)) +
    geom_density(na.rm = TRUE) +
    scale_fill_manual(values = c("grey35", NA)) +
    scale_color_manual(values = c("grey35", "hotpink")) +
    theme(legend.position = "none") +
    labs(x = "Townsend deprivation index")

  multiplot(gender, years_old, ethnicity, socioeconomic, cols = 2)
}
