
#' Reads a UK Biobank phenotype fileset and returns a single dataset.
#'
#' A UK Biobank \emph{fileset} includes a \emph{.tab} file containing the raw data with field codes instead of variable names, an \emph{.r} (\emph{sic}) file containing code to read raw data (inserts categorical variable levels and labels), and an \emph{.html} file containing tables mapping field code to variable name, and labels and levels for categorical variables.
#'
#' @export
#' @param fileset The prefix for a UKB fileset, e.g., ukbxxxx (for ukbxxxx.tab, ukbxxxx.r, ukbxxxx.html)
#' @param path The relative path to the directory containing your UKB fileset.
#' @param data.pos Locates the data in your .html file. The .html file is read into a list; the default value data.pos = 2 indicates the second item in the list. (The first item in the list is the title of the table). You will probably not need to change this value, but if the need arises you can open the .html file in a browser and identify where in the file the data is.
#'
#' @details The \strong{index} and \strong{array} from the UKB field code are preserved in the variable name, as two numbers separated by underscores at the end of the name e.g. \emph{variable_index_array}. \strong{index} refers the assessment instance (or visit). \strong{array} captures multiple answers to the same "question". See UKB documentation for detailed descriptions of \href{http://biobank.ctsu.ox.ac.uk/crystal/instance.cgi?id=2}{index} and \href{http://biobank.ctsu.ox.ac.uk/crystal/help.cgi?cd=array}{array}.
#'
#' @return A dataframe with variable names in snake_case (lowercase and separated by an underscore).
#'
#' @seealso \code{\link{ukb_field}}
#'
ukb_df <- function(fileset, path = './', data.pos = 2) {
  html_file <- sprintf("%s.html", fileset)
  r_file <- sprintf("%s.r", fileset)
  tab_file <- sprintf("%s.tab", fileset)

  .update_tab_path(fileset, path)

  source(paste(path, r_file, sep = ""))

  tables <- readHTMLTable(
    doc = paste(path, html_file, sep = ""),
    stringsAsFactors = FALSE
  )

  variable_names <- .column_name_lookup(tables[[data.pos]])
  names(bd) <- variable_names[names(bd)]
  return(bd)
}



#' Makes a UKB Data-Field to variable name table for reference or lookup.
#'
#' Makes either a table of Data-Field and description, or a named vector handy for looking up descriptive name by column names in the UKB fileset tab file.
#'
#' @export
#' @param fileset The prefix for a UKB fileset, e.g., ukbxxxx (for ukbxxxx.tab, ukbxxxx.r, ukbxxxx.html)
#' @param path The relative path to the directory containing your UKB fileset.
#' @param data.pos Locates the data in your .html file. The .html file is read into a list; the default value data.pos = 2 indicates the second item in the list. (The first item in the list is the title of the table). You will probably not need to change this value, but if the need arises you can open the .html file in a browser and identify where in the file the data is.
#' @param as.lookup If set to TRUE, returns a named \code{vector}. The default \code{as.look = FALSE} returns a dataframe with columns: field.showcase (as used in the UKB online showcase), field.data (as used in the tab file), name (descriptive name created by \code{\link{ukb_df}})
#'
ukb_field <- function(fileset, path = './', data.pos = 2, as.lookup = FALSE) {
  html_file <- sprintf("%s.html", fileset)
  tables <- readHTMLTable(
    doc = paste(path, html_file, sep = ""),
    stringsAsFactors = FALSE
  )

  df <- .fill_missing_description(tables[[data.pos]])
  lookup <- .description_to_name(df)
  old_var_names <- .paste("f.", gsub("-", ".", df[, "UDI"]), sep = "")

  if (as.lookup) {
    names(lookup) <- old_var_names
    return(lookup)
  } else {
    lookup.reference <- data.frame(
      field.showcase = df[, "UDI"],
      field.data = old_var_names,
      names = lookup)
    return(lookup.reference)
  }
}



#' Fills Description and Type columns where missing at follow-up assessments.
#'
#' @param data Field-to-description table from html file
#'
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



#' Creates a variable name from the field description.
#'
#' @param data Field-to-description table from html file
#'
.description_to_name <-  function(data) {

  name <- tolower(data[, "Description"])
  name <- gsub(" - ", "_", name)
  name <- gsub(" ", "_", name)
  name <- gsub("uses_data-coding.*simple_list.$", "", name)
  name <- gsub("uses_data-coding.*hierarchical_tree.", "", name)
  name <- gsub(",", "", name)
  name <- gsub("\\(", "", name)
  name <- gsub("\\)", "", name)

  ukb_index_array <- gsub("^.*-", "", data[, "UDI"])
  ukb_index_array <- gsub("\\.", "_", ukb_index_array)
  name_index_array <- ifelse(
    ukb_index_array == "eid",
    "eid",
    paste(name, ukb_index_array, sep = "_")
  )
  return(name_index_array)
}



#' Matches field (as in tab file) to variable name
#'
#' @param data Field-to-description table from html file
#' @return A named character vector. Names are fields, values are variable names made from description
#'
.column_name_lookup <-  function(data){
  df <- .fill_missing_description(data)
  lookup <- .description_to_name(df)
  names(lookup) <- paste("f.", gsub("-", ".", df[, "UDI"]), sep = "")
  return(lookup)
}



#' Corrects path to tab file in R source
#'
#' In particular, if you have moved the fileset from the directory containing the foo.enc file on which you called gconv. NB. gconv writes absolute path to directory containing foo.enc, into foo.r read.table() call
#'
#' @param fileset prefix for UKB fileset
#' @param path relative path to directory containing the fileset
#'
.update_tab_path <- function(fileset, path = './') {
  r_file <- sprintf("%s.r", fileset)
  tab_file <- sprintf("%s.tab", fileset)

  # Update path to tab file in R source
  tab_location <- paste(path, tab_file, sep = "")
  r_location <- paste(path, r_file, sep = "")

  f <- gsub(
    "read.*$" ,
    sprintf("read.delim('%s')", tab_location) ,
    readLines(r_location))
  cat(f, file = r_location, sep = "\n")
}
