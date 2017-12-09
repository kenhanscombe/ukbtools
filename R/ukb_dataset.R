
globalVariables(c(".", "eid", "pair", "ibs0", "kinship", "category_related", "ped_related", "code", "heterozygosity_0_0"))

#' Reads a UK Biobank phenotype fileset and returns a single dataset.
#'
#' A UK Biobank \emph{fileset} includes a \emph{.tab} file containing the raw data with field codes instead of variable names, an \emph{.r} (\emph{sic}) file containing code to read raw data (inserts categorical variable levels and labels), and an \emph{.html} file containing tables mapping field code to variable name, and labels and levels for categorical variables.
#'
#' @param fileset The prefix for a UKB fileset, e.g., ukbxxxx (for ukbxxxx.tab, ukbxxxx.r, ukbxxxx.html)
#' @param path The path to the directory containing your UKB fileset. The default value is the current directory.
#' @param data.pos Locates the data in your .html file. The .html file is read into a list; the default value data.pos = 2 indicates the second item in the list. (The first item in the list is the title of the table). You will probably not need to change this value, but if the need arises you can open the .html file in a browser and identify where in the file the data is.
#'
#' @details The \strong{index} and \strong{array} from the UKB field code are preserved in the variable name, as two numbers separated by underscores at the end of the name e.g. \emph{variable_index_array}. \strong{index} refers the assessment instance (or visit). \strong{array} captures multiple answers to the same "question". See UKB documentation for detailed descriptions of \href{http://biobank.ctsu.ox.ac.uk/crystal/instance.cgi?id=2}{index} and \href{http://biobank.ctsu.ox.ac.uk/crystal/help.cgi?cd=array}{array}.
#'
#' @return A dataframe with variable names in snake_case (lowercase and separated by an underscore).
#'
#' @seealso \code{\link{ukb_df_field}}
#'
#' @import XML
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
#' If you have multiple UKB filesets, tidy then merge.
#'
#' ukb1234_data <- ukb_df("ukb1234")
#' ukb2345_data <- ukb_df("ukb2345")
#' ukb3456_data <- ukb_df("ukb3456")
#'
#' my_ukb_data <- plyr::join_all(
#'   list(ukb1234_data, ukb2345_data, ukb3456_data),
#'   by = "eid",
#'   type = "full"
#' )
#' }
#'
ukb_df <- function(fileset, path = ".", data.pos = 2) {
  html_file <- sprintf("%s.html", fileset)
  r_file <- sprintf("%s.r", fileset)
  tab_file <- sprintf("%s.tab", fileset)

  .update_tab_path(fileset, path)

  source(
    if (path == ".") {
      file.path(getwd(), r_file)
    } else {
      file.path(path, r_file)
    },
    local = TRUE
  )

  html_internal_doc <- XML::htmlParse(file.path(path, html_file))
  html_table_nodes <- XML::getNodeSet(html_internal_doc, "//table")
  html_table = XML::readHTMLTable(
    html_table_nodes[[data.pos]],
    as.data.frame = TRUE,
    stringsAsFactors = FALSE,
    colClasses = c("integer", "character", "integer", "character", "character")
  )

  variable_names <- .column_name_lookup(html_table)
  names(bd) <- variable_names[names(bd)]
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
#' @import XML
#' @export
#' @examples
#' \dontrun{
#' # UKB field-to-description for ukb1234.tab, ukb1234.r, ukb1234.html
#'
#' ukb_df_field("ukb1234")
#' }
#'
ukb_df_field <- function(fileset, path = ".", data.pos = 2, as.lookup = FALSE) {
  html_file <- sprintf("%s.html", fileset)
  html_internal_doc <- XML::htmlParse(file.path(path, html_file))
  html_table_nodes <- XML::getNodeSet(html_internal_doc, "//table")
  html_table = XML::readHTMLTable(
    html_table_nodes[[data.pos]],
    as.data.frame = TRUE,
    stringsAsFactors = FALSE,
    colClasses = c("integer", "character", "integer", "character", "character")
  )

  df <- .fill_missing_description(html_table)
  lookup <- .description_to_name(df)
  old_var_names <- paste("f.", gsub("-", ".", df[, "UDI"]), sep = "")

  if (as.lookup) {
    names(lookup) <- old_var_names
    return(lookup)
  } else {
    lookup.reference <- data.frame(
      field.showcase = gsub("-.*$", "", df[, "UDI"]),
        field.html = df[, "UDI"],
      field.tab = old_var_names,
      names = lookup)
    return(lookup.reference)
  }
}



# Fills Description and Type columns where missing at follow-up assessments.
#
# @param data Field-to-description table from html file
#
.fill_missing_description <-  function(data) {
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
.description_to_name <-  function(data) {

  name <- tolower(data[, "Description"])
  name <- gsub(" - ", "_", name)
  name <- gsub(" ", "_", name)
  name <- gsub("uses_data-coding.*simple_list.$", "", name)
  name <- gsub("uses_data-coding.*hierarchical_tree.", "", name)
  name <- gsub("[^[:alnum:][:space:]_]", "", name)

  ukb_index_array <- gsub("^.*-", "", data[, "UDI"])
  ukb_index_array <- gsub("\\.", "_", ukb_index_array)
  name_index_array <- ifelse(
    ukb_index_array == "eid",
    "eid",
    paste(name, ukb_index_array, sep = "_")
  )
  return(name_index_array)
}



# Matches field (as in tab file) to variable name
#
# @param data Field-to-description table from html file
# @return A named character vector. Names are fields, values are variable names made from description
#
.column_name_lookup <-  function(data){
  df <- .fill_missing_description(data)
  lookup <- .description_to_name(df)
  names(lookup) <- paste("f.", gsub("-", ".", df[, "UDI"]), sep = "")
  return(lookup)
}



# Corrects path to tab file in R source
#
# In particular, if you have moved the fileset from the directory containing the foo.enc file on which you called gconv. NB. gconv writes absolute path to directory containing foo.enc, into foo.r read.table() call
#
# @param fileset prefix for UKB fileset
# @param path The path to the directory containing your UKB fileset. The default value is the current directory.
#
.update_tab_path <- function(fileset, path = ".") {
  r_file <- sprintf("%s.r", fileset)
  tab_file <- sprintf("%s.tab", fileset)

  # Update path to tab file in R source
  if(path == ".") {
    tab_location <- file.path(getwd(), tab_file)
    r_location <- file.path(getwd(), r_file)
  } else {
    tab_location <- file.path(path, tab_file)
    r_location <- file.path(path, r_file)
  }

  f <- gsub(
    "^read\\.delim.*$" ,
    sprintf("read.delim('%s')", tab_location),
    readLines(r_location))
  cat(f, file = r_location, sep = "\n")
}



#' Recursively join a list of UKB datasets
#'
#' A thin wrapper around \code{purrr::reduce} and \code{dplyr::full_join} to merge multiple UKB datasets.
#'
#' @param ... Supply comma separated unquoted names of to-be-merged UKB datasets (created with \code{\link{ukb_df}}). Arguments are passed to \code{list}.
#' @param by Variable used to merge multiple dataframes (default = "eid").
#'
#' @details The function takes a comma separated list of unquoted datasets. By explicitly setting the join key to "eid" only (Default value of the \code{by} parameter), any additional variables common to any two tables will have ".x" and ".y" appended to their names. If you are satisfied the additional variables are identical to the original, the copies can be safely deleted. For example, if \code{setequal(my_ukb_data$var, my_ukb_data$var.x)} is \code{TRUE}, then my_ukb_data$var.x can be dropped. A \code{dlyr::full_join} is like the set operation union in that all abservation from all tables are included, i.e., all samples are included even if they are not included in all datasets.
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
#' @return Returns a named list of numeric vectors, one for each duplicated variable name. The numeric vectors contain the column indeces of duplicates.
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
