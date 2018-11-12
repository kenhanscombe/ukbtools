
#' Demographics of a UKB sample subset
#'
#' Describes a subset of the UKB sample, relative to a reference subsample, on the \href{http://biobank.ctsu.ox.ac.uk/crystal/label.cgi?id=1001}{UKB primary demographics} (sex, age, ethnicity, Townsend deprivation) and assessment centre and current employment status. The "subset" and "reference" samples are defined either by a variable of interest (\code{nonmiss.var} - those with data form the "subset" of interest and samples with missing data are the "reference" sample), or a logical vector (\code{subset.var} - where \code{TRUE} values define the "subset" and \code{FALSE} the "reference" samples) . This function is intended as an exploratory data analysis and quality control tool.
#'
#' @param data A UKB dataset constructed with \code{\link{ukb_df}}.
#' @param nonmiss.var The variable of interest which defines the "subset" (samples with data) and "reference" (samples without data, i.e., NA) samples.
#' @param subset.var A logical vector defining a "subset" (\code{TRUE}) and "reference" subset (\code{FALSE}). Length must equal the number of rows in your \code{data}.
#' @param bar.position This argument is passed to the \code{position} in \code{geom_bar}. The default value is \code{"fill"} which shows reference and subset of interest as proportions of the full dataset. Useful alternatives are \code{"stack"} for counts and \code{"dodge"} for side-by-side bars.
#' @param sex.var The variable to be used for sex. Default value is the regular expression "^sex.*0_0".
#' @param age.var The variable to be use for age. Default value is the regular expression "^age_when_attended_assessment_centre.*0_0".
#' @param socioeconomic.var The variable to be used for socioeconomic status. Default value is deprivation at baseline, the regular expression "^townsend_deprivation_index_at_recruitment.*0_0".
#' @param ethnicity.var The variable to be used for ethnicity. Default value is the regular expression "^ethnic_background.*0_0".
#' @param employment.var The variable to be used for employment status. Default value is employment status at baseline "^current_employment_status.*0_0".
#' @param centre.var The variable to be used for assessment centre. Default value is the regular expression "^uk_biobank_assessment_centre.*0_0".
#'
#' @seealso \code{\link{ukb_df}}
#'
#' @import grid ggplot2 dplyr
#' @importFrom magrittr "%>%"
#' @export
#' @examples
#' \dontrun{
#' # Compare those with data to those without
#' ukb_context(my_ukb_data, nonmiss.var = "my_variable_of_interest")
#'
#' # Define a subset of interest as a logical vector
#' subgroup_of_interest <- (my_ukb_data$bmi > 40 & my_ukb_data$age < 50)
#' ukb_context(my_ukb_data, subset.var = subgroup_of_interest)
#' }
#'
ukb_context <- function(
  data, nonmiss.var = NULL, subset.var = NULL, bar.position = "fill",
  sex.var = "^sex.*0_0",
  age.var = "^age_when_attended_assessment_centre.*0_0",
  socioeconomic.var = "^townsend_deprivation_index_at_recruitment.*0_0",
  ethnicity.var = "^ethnic_background.*0_0",
  employment.var = "^current_employment_status.*0_0",
  centre.var = "^uk_biobank_assessment_centre.*0_0") {

  if (is.null(nonmiss.var) & is.null(subset.var)) {
    stop("Either supply a variable of interest (nonmiss.var),
         or a logical vector (subset.var) to define reference and comparison samples", call. = FALSE)
  }

  fill.var <- if (!is.null(nonmiss.var)) {
    !is.na(data[, nonmiss.var])
  } else {
    subset.var
  }

  sex.var <- data %>% select(matches(sex.var)) %>% names()
  age.var <- data %>% select(matches(age.var)) %>% names()
  socioeconomic.var <- data %>% select(matches(socioeconomic.var)) %>% names()
  ethnicity.var <- data %>% select(matches(ethnicity.var)) %>% names()
  employment.var <- data %>%
    select(matches(employment.var), -contains("correct")) %>%
    names()
  centre.var <- data %>% select(matches(centre.var)) %>% names()

  centre_lookup <- lookup(ukbtools::ukbcentre, "code", "centre")
  data$centre <-  centre_lookup[as.character(
    data %>% select(matches("uk_biobank_assessment_centre.*0_0")) %>% .[[1]]
    )]


  if (bar.position == "fill") {
    format_cnt <- function(x) format(round(x / 1000))
    count_lab <- "count (1000s)"
  } else {
    format_cnt <- function(x) x
    count_lab <- "count"
  }

  multiplot(

    ggplot2::ggplot(data, aes_string(sex.var, fill = fill.var)) +
      geom_bar(position = bar.position, na.rm = TRUE, width = .5) +
      scale_fill_manual(values = c("grey35", "hotpink"),
                        labels = c("Reference", "Subset"), na.value = "grey65") +
      scale_y_continuous(labels = format_cnt) +
      theme(legend.position = "top",
            axis.title.y = element_text(face = "bold"),
            panel.grid = element_blank()) +
      labs(x = "Sex", y = count_lab, fill = "") +
      coord_flip(),

    ggplot2::ggplot(data, aes_string(age.var, fill = fill.var, color = fill.var)) +
      geom_density(na.rm = TRUE) +
      scale_fill_manual(values = c("grey35", NA)) +
      scale_color_manual(values = c("grey35", "hotpink")) +
      theme(legend.position = "none", axis.title.x = element_text(face = "bold"),
            panel.grid = element_blank()) +
      labs(x = "Age"),

    ggplot2::ggplot(data, aes_string(socioeconomic.var, fill = fill.var,
                                     color = fill.var)) +
      geom_density(na.rm = TRUE) +
      scale_fill_manual(values = c("grey35", NA)) +
      scale_color_manual(values = c("grey35", "hotpink")) +
      theme(legend.position = "none", axis.title.x = element_text(face = "bold"),
            panel.grid = element_blank()) +
      labs(x = "Townsend deprivation index"),

    ggplot2::ggplot(data, aes_string(ethnicity.var, fill = fill.var)) +
      geom_bar(position = bar.position, na.rm = TRUE, width = .7) +
      scale_fill_manual(values = c("grey35", "hotpink"), na.value = "grey65") +
      scale_y_continuous(labels = format_cnt) +
      theme(legend.position = "none", axis.title.y = element_text(face = "bold"),
            panel.grid = element_blank()) +
      labs(x = "Ethnic Background", y = count_lab) +
      coord_flip(),

    ggplot2::ggplot(data, aes_string("centre", fill = fill.var)) +
      geom_bar(position = bar.position, na.rm = TRUE, width = .6) +
      scale_fill_manual(values = c("grey35", "hotpink"), na.value = "grey65") +
      scale_y_continuous(labels = format_cnt) +
      theme(legend.position = "none", axis.title.y = element_text(face = "bold"),
            panel.grid = element_blank()) +
      labs(x = "Assessment Centre", y = count_lab) +
      coord_flip(),

    ggplot2::ggplot(data, aes_string(employment.var, fill = fill.var)) +
      geom_bar(position = bar.position, na.rm = TRUE, width = .3) +
      scale_fill_manual(values = c("grey35", "hotpink"), na.value = "grey65") +
      scale_y_continuous(labels = format_cnt) +
      theme(legend.position = "none", axis.title.y = element_text(face = "bold"),
            panel.grid = element_blank()) +
      labs(x = "Employment Status", y = count_lab) +
      coord_flip(),

    cols = 2
  )
}




#' Inserts UKB centre names into data
#'
#' Inserts a column with centre name, \code{ukb_centre}, into the supplied data.frame. Useful if your UKB centre variable \code{uk_biobank_assessment_centre_0_0} has not been populated with named levels.
#'
#' @param data A UKB dataset created with \code{\link{ukb_df}}.
#' @param centre.var The UKB column containing numerically coded assessment centre. The default is a regular expression \code{"^uk_biobank_assessment_centre.*0_0"}.
#' @return A dataframe with an additional column \code{ukb_centre} - UKB assessment centre names
#' @export
ukb_centre <- function(data, centre.var = "^uk_biobank_assessment_centre.*0_0") {
  centre.var <- dplyr::select(data, matches(centre.var)) %>% names()
  centre.var <- as.factor(data[[centre.var]])
  centre_lookup <- lookup(ukbtools::ukbcentre, "code", "centre")
  data$ukb_centre <- centre_lookup[centre.var]

  data %>%
    dplyr::select(eid, ukb_centre, everything()) %>%
    return()
}
