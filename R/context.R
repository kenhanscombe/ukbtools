
#' Demographics of a sample subset, or an individual
#'
#' Describes a subset of UKB sample, or an individual, relative to the full UK Biobank sample, or a specified UKB subset. This function is intended as an exploratory data analysis and quality control tool, and as such only provides summary statistics and graphical context for individual's data.
#'
#' @param data A UKB dataset.
#' @param sample.sub A vector of UKB ids, or logical or row index, that defines the subset of interest.
#' @param sample.ref A vector of UKB ids (or a logical or row index), that defines an alternative reference population. By default the reference is the full sample.
#'
ukb_context <- function(data, comparison.var, sample.sub, sample.ref) {
  df <- data %>%
    select(
      sex_0_0,
      year_of_birth_0_0,
      townsend_deprivation_index_at_recruitment_0_0,
      ethnic_background_0_0,
      overall_acceleration_average_0_0) %>%
    mutate(age = 2010 - year_of_birth_0_0)

  # sex
  sex <- df %>%
    ggplot(aes(sex_0_0)) +
    geom_bar(fill = "grey35") +
    geom_bar(
      data = df %>%
        filter(!is.na(my.variable)),
      fill = "hotpink"
    ) +
    labs(x = "Sex")

  # age
  age <- df %>%
    ggplot(aes(age)) +
    geom_density(color = "grey35", fill = "grey35") +
    geom_density(
      data = df %>%
        filter(!is.na(my.variable)),
      color = "hotpink"
    ) +
    labs(x = "Age")

  # ethnicity
  eth <- df %>%
    ggplot(aes(ethnic_background_0_0)) +
    geom_bar(fill = "grey35") +
    geom_bar(
      data = data %>%
        filter(!is.na(my.variable)),
      fill = "hotpink") +
    coord_flip() +
    labs(x = "Ethnic Background")

  # ses
  ses <- df %>%
    ggplot(aes(townsend_deprivation_index_at_recruitment_0_0)) +
    geom_density(color = "grey35", fill = "grey35") +
    geom_density(
      data = df %>%
        filter(!is.na(my.variable)),
      color = "hotpink"
    ) +
    labs(x = "Townsend deprivation index")

  .multiplot(sex, age, eth, ses, col = 2)
}
