
globalVariables(
  c(".", "i", "j", "eid", "pair", "ibs0", "kinship", "category_related",
    "ped_related", "code", "heterozygosity_0_0", "field.tab", "field.showcase",
    "field.html", "col.type", "variable", "HetHet", "IBS0", "ID", "ID1", "ID2",
    "Kinship", "categorized_var", "dx", "freq", "tile_range", "lower", "upper",
    "mid", "frequency", "disease"))


#' Reads a UK Biobank phenotype fileset and returns a single dataset.
#'
#' A UK Biobank \emph{fileset} includes a \emph{.tab} file containing the raw data with field codes instead of variable names, an \emph{.r} (\emph{sic}) file containing code to read raw data (inserts categorical variable levels and labels), and an \emph{.html} file containing tables mapping field code to variable name, and labels and levels for categorical variables.
#'
#' @param fileset The prefix for a UKB fileset, e.g., ukbxxxx (for ukbxxxx.tab, ukbxxxx.r, ukbxxxx.html)
#' @param path The path to the directory containing your UKB fileset. The default value is the current directory.
#' @param n_threads Either "max" (uses the number of cores, `parallel::detectCores()`), "dt" (default - uses the data.table default, `data.table::getDTthreads()`), or a numerical value (in which case n_threads is set to the supplied value, or `parallel::detectCores()` if it is smaller).
#' @param data.pos Locates the data in your .html file. The .html file is read into a list; the default value data.pos = 2 indicates the second item in the list. (The first item in the list is the title of the table). You will probably not need to change this value, but if the need arises you can open the .html file in a browser and identify where in the file the data is.
#' @param temporary Should the `R` file be copied to a temporary directory?
#' Useful for permissions issues, especially on computing clusters.
#'
#' @details The \strong{index} and \strong{array} from the UKB field code are preserved in the variable name, as two numbers separated by underscores at the end of the name e.g. \emph{variable_index_array}. \strong{index} refers the assessment instance (or visit). \strong{array} captures multiple answers to the same "question". See UKB documentation for detailed descriptions of \href{http://biobank.ctsu.ox.ac.uk/crystal/instance.cgi?id=2}{index} and \href{http://biobank.ctsu.ox.ac.uk/crystal/help.cgi?cd=array}{array}.
#'
#' @return A dataframe with variable names in snake_case (lowercase and separated by an underscore).
#'
#' @seealso \code{\link{ukb_df_field}} \code{\link{ukb_df_full_join}}
#'
#' @import stringr
#' @importFrom data.table fread
#' @export
#'
#' @examples
#' \dontrun{
#' # Simply provide the stem of the UKB fileset.
#' # To read ukb1234.tab, ukb1234.r, ukb1234.html
#'
#' my_ukb_data <- ukb_df("ukb1234")
#'
#'
#' If you have multiple UKB filesets, read each then join with your preferred
#' method (ukb_df_full_join is
#' a thin wrapper around dplyr::full_join applied recursively with
#' purrr::reduce).
#'
#' ukb1234_data <- ukb_df("ukb1234")
#' ukb2345_data <- ukb_df("ukb2345")
#' ukb3456_data <- ukb_df("ukb3456")
#'
#' ukb_df_full_join(ukb1234_data, ukb2345_data, ukb3456_data)
#' }
#'
ukb_df <- function(fileset, path = ".", n_threads = "dt", data.pos = 2,
                   temporary = FALSE) {

  fileset = stringr::str_replace(fileset, "[.](r|html|tab)$", "")

  # Check files exist
  html_file <- stringr::str_interp("${fileset}.html")
  r_file <- stringr::str_interp("${fileset}.r")
  tab_file <- stringr::str_interp("${fileset}.tab")

  # Column types as described by UKB
  # http://biobank.ctsu.ox.ac.uk/crystal/help.cgi?cd=value_type
  col_type <- c(
    "Sequence" = "integer",
    "Integer" = "integer",
    "Categorical (single)" = "character",
    "Categorical (multiple)" = "character",
    "Continuous" = "double",
    "Text" = "character",
    "Date" = "character",
    "Time" = "character",
    "Compound" = "character",
    "Binary object" = "character",
    "Records" = "character",
    "Curve" = "character"
  )

  ukb_key <- ukb_df_field(fileset, path = path) %>%
    mutate(fread_column_type = col_type[col.type])

  bad_col_type <- is.na(ukb_key$fread_column_type)

  if (any(bad_col_type)) {
    bad_types <- sort(unique(ukb_key$col.type[bad_col_type])) %>%
      stringr::str_c(bad_types, collapse = ", ")
    warning(
      stringr::str_c(
        "Unknown column types ",
        bad_types,
        " encountered, setting them to type character."
      )
    )
    ukb_key$fread_column_type[bad_col_type] <- "character"
  }

  # Comment out .r read of .tab
  # Read .tab file from user named path with data.table::fread
  # Include UKB-generated categorical variable labels
  bd <- read_ukb_tab(fileset,
                     column_type = ukb_key$fread_column_type,
                     path,
                     n_threads = n_threads,
                     temporary = temporary)
  if (temporary) {
    r_file = file.path(tempdir(), basename(r_file))
  } else {
    r_file = file.path(path, r_file)
  }
  source(r_file, local = TRUE)

  names(bd) <- ukb_key$col.name[match(names(bd), ukb_key$field.tab)]
  return(bd)
}




#' Makes a UKB data-field to variable name table for reference or lookup.
#'
#' Makes either a table of Data-Field and description, or a named vector handy for looking up descriptive name by column names in the UKB fileset tab file.
#'
#' @param fileset The prefix for a UKB fileset, e.g., ukbxxxx (for ukbxxxx.tab, ukbxxxx.r, ukbxxxx.html)
#' @param path The path to the directory containing your UKB fileset. The default value is the current directory.
#' @param data.pos Locates the data in your .html file. The .html file is read into a list; the default value data.pos = 2 indicates the second item in the list. (The first item in the list is the title of the table). You will probably not need to change this value, but if the need arises you can open the .html file in a browser and identify where in the file the data is.
#' @param as.lookup If set to TRUE, returns a named \code{vector}. The default \code{as.look = FALSE} returns a dataframe with columns: field.showcase (as used in the UKB online showcase), field.data (as used in the tab file), name (descriptive name created by \code{\link{ukb_df}})
#'
#' @return Returns a data.frame with columns \code{field.showcase}, \code{field.html}, \code{field.tab}, \code{names}. \code{field.showcase} is how the field appears in the online \href{http://biobank.ctsu.ox.ac.uk/crystal/}{UKB showcase}; \code{field.html} is how the field appears in the html file in your UKB fileset; \code{field.tab} is how the field appears in the tab file in your fileset; and \code{names} is the descriptive name that \code{\link{ukb_df}} assigns to the variable. If \code{as.lookup = TRUE}, the function returns a named character vector of the descriptive names.
#'
#' @seealso \code{\link{ukb_df}}
#'
#' @importFrom stringr str_interp str_c str_replace_all
#' @importFrom xml2 read_html xml_find_all
#' @importFrom rvest html_table
#' @importFrom tibble tibble
#' @export
#' @examples
#' \dontrun{
#' # UKB field-to-description for ukb1234.tab, ukb1234.r, ukb1234.html
#'
#' ukb_df_field("ukb1234")
#' }
#'
ukb_df_field <- function(fileset, path = ".", data.pos = 2, as.lookup = FALSE) {
  fileset = stringr::str_replace(fileset, "[.](r|html|tab)$", "")

  html_file <- stringr::str_interp("${fileset}.html")
  html_internal_doc <- xml2::read_html(file.path(path, html_file))
  html_table_nodes <- xml2::xml_find_all(html_internal_doc, "//table")
  html_table <- rvest::html_table(html_table_nodes[[data.pos]])

  df <- fill_missing_description(html_table)
  lookup <- description_to_name(df)
  old_var_names <- paste("f.", gsub("-", ".", df[, "UDI"]), sep = "")

  if (as.lookup) {
    names(lookup) <- old_var_names
    return(lookup)
  } else {
    lookup.reference <- tibble::tibble(
      field.showcase = gsub("-.*$", "", df[, "UDI"]),
      field.html = df[, "UDI"],
      field.tab = old_var_names,
      col.type = df[, "Type"],
      col.name = ifelse(
        field.showcase == "eid",
        "eid",
        stringr::str_c(
          lookup, "_f",
          stringr::str_replace_all(field.html, c("-" = "_", "\\." = "_"))
        )
      )
    )

    return(lookup.reference)
  }
}




# Fills Description and Type columns where missing at follow-up assessments.
#
# @param data Field-to-description table from html file
#
fill_missing_description <-  function(data) {
  udi <- gsub(pattern = "-.*$", "", data[, "UDI"])
  for (i in 2:nrow(data)) {
    if (udi[i] == udi[i-1] & is.na(data[, "Description"][i])) {
      data[i, "Type"] <- data[i-1, "Type"]
      data[i, "Description"] <- data[i-1, "Description"]
    }
  }
  return(data)
}



# Creates a variable name from the field description.
#
# @param data Field-to-description table from html file
#
description_to_name <-  function(data) {

  name <- tolower(data[, "Description"]) %>%
    gsub(" - ", "_", x = .) %>%
    gsub(" ", "_", x = .) %>%
    gsub("uses.*data.*coding.*simple_list.$", "", x = .) %>%
    gsub("uses.*data.*coding.*hierarchical_tree.", "", x = .) %>%
    gsub("uses.*data.*coding_[0-9]*", "", x = .) %>%
    gsub("[^[:alnum:][:space:]_]", "", x = .) %>%
    gsub("__*", "_", x = .)

  return(name)
}



# Corrects path to tab file in R source
#
# In particular, if you have moved the fileset from the directory containing the foo.enc file on which you called gconv. NB. gconv writes absolute path to directory containing foo.enc, into foo.r read.table() call
#
# @param fileset prefix for UKB fileset
# @param path The path to the directory containing your UKB fileset. The default value is the current directory.
#
read_ukb_tab <- function(fileset, column_type, path = ".",
                         n_threads = "max",
                         temporary = FALSE) {
  fileset = stringr::str_replace(fileset, "[.](r|html|tab)$", "")

  r_file <- stringr::str_interp("${fileset}.r")
  tab_file <- stringr::str_interp("${fileset}.tab")

  # Update path to tab file in R source
  tab_location <- file.path(path, tab_file)
  r_location <- file.path(path, r_file)

  edit_date <- Sys.time()

  f <- stringr::str_replace(
    readLines(r_location),
    pattern = "bd *<-" ,
    replacement = stringr::str_interp(
      "# Read function edited by ukbtools ${edit_date}\n# bd <-")
  )
  if (temporary) {
    r_location = file.path(tempdir(), basename(r_location))
  }
  cat(f, file = r_location, sep = "\n")

  bd <- data.table::fread(
    input = tab_location,
    sep = "\t",
    header = TRUE,
    colClasses = stringr::str_c(column_type),
    data.table = FALSE,
    showProgress = TRUE,
    nThread = if(n_threads == "max") {
      parallel::detectCores()
    } else if (n_threads == "dt") {
      data.table::getDTthreads()
    } else if (is.numeric(n_threads)) {
      min(n_threads, parallel::detectCores())
    }
  )

  return(bd)
}



#' Recursively join a list of UKB datasets
#'
#' A thin wrapper around \code{purrr::reduce} and \code{dplyr::full_join} to merge multiple UKB datasets.
#'
#' @param ... Supply comma separated unquoted names of to-be-merged UKB datasets (created with \code{\link{ukb_df}}). Arguments are passed to \code{list}.
#' @param by Variable used to merge multiple dataframes (default = "eid").
#'
#' @details The function takes a comma separated list of unquoted datasets. By explicitly setting the join key to "eid" only (Default value of the \code{by} parameter), any additional variables common to any two tables will have ".x" and ".y" appended to their names. If you are satisfied the additional variables are identical to the original, the copies can be safely deleted. For example, if \code{setequal(my_ukb_data$var, my_ukb_data$var.x)} is \code{TRUE}, then my_ukb_data$var.x can be dropped. A \code{dplyr::full_join} is like the set operation union in that all observations from all tables are included, i.e., all samples are included even if they are not included in all datasets.
#'
#' NB. \code{ukb_df_full_join} will fail if any variable names are repeated **within** a single UKB dataset. This is unlikely to occur, however, \code{ukb_df} creates variable names by combining a snake_case descriptor with the variable's **index** and **array**. If an index_array combination is incorrectly repeated, this will result in a duplicated variable. If the join fails, you can use \code{\link{ukb_df_duplicated_name}} to find duplicated names. See \code{vignette(topic = "explore-ukb-data", package = "ukbtools")} for further details.
#'
#' @seealso \code{\link{ukb_df_duplicated_name}}
#'
#' @importFrom purrr reduce
#' @importFrom dplyr full_join
#' @export
#'
#' @examples
#' \dontrun{
#' # If you have multiple UKB filesets, tidy then merge them.
#'
#' ukb1234_data <- ukb_df("ukb1234")
#' ukb2345_data <- ukb_df("ukb2345")
#' ukb3456_data <- ukb_df("ukb3456")
#'
#' my_ukb_data <- ukb_df_full_join(ukb1234_data, ukb2345_data, ukb3456_data)
#' }
#'
ukb_df_full_join <- function(..., by = "eid") {
  purrr::reduce(
    list(...),
    dplyr::full_join,
    by = by
  )
}



#' Checks for duplicated names within a UKB dataset
#'
#' @param data A UKB dataset created with \code{\link{ukb_df}}.
#'
#' @return Returns a named list of numeric vectors, one for each duplicated variable name. The numeric vectors contain the column indices of duplicates.
#'
#' @details Duplicates *within* a UKB dataset are unlikely to occur, however, \code{ukb_df} creates variable names by combining a snake_case descriptor with the variable's **index** and **array**. If an index_array combination is incorrectly repeated in the original UKB data, this will result in a duplicated variable name. . See \code{vignette(topic = "explore-ukb-data", package = "ukbtools")} for further details.
#'
#' @importFrom purrr map
#' @export
#'
ukb_df_duplicated_name <- function(data) {
  dup_names <- names(data)[base::duplicated(names(data))]
  dup_pos <- purrr::map(dup_names, ~ grep(paste(., collapse = "|"), names(data), perl = TRUE))
  names(dup_pos) <- dup_names
  if (length(dup_pos) > 0) {
    return(dup_pos)
  } else {
    message("No duplicated variable names")
  }
}
