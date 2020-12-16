
os_type <- function() {
  .Platform$OS.type
}


sys_type <- function() {
  if (os_type() == "windows") {
    "windows"
  } else if (Sys.info()["sysname"] == "Darwin") {
    "macos"
  } else if (Sys.info()["sysname"] == "Linux") {
    "linux"
  } else if (os_type() == "unix") {
    # "unix"
    "linux"
  } else {
    stop("Unknown OS")
  }
}


#' Downloads individual UKB utlities and file handlers.
#'
#' @param util Name of the utility. Must be one of \code{"ukbmd5"}, \code{"ukbconv"}, \code{"ukbunpack"}, \code{"ukbfetch"}, \code{"ukblink"}, \code{"ukbgene"}, \code{"encoding.ukb"}. For a description of the UKB utilies and file handlers, see \href{http://biobank.ndph.ox.ac.uk/showcase/download.cgi}{UKB Downloads}.
#' @param download Should the utility be downloaded if not found? Default is \code{TRUE}.
#' @param out_dir The output directory to download the UKB utility to. Default \code{tempdir()} - UKB utility and temporary directory will be deleted at the end of the current session.
#' @return Path to the downloaded utility.
#' @export
#'
#' @examples
#' md5 = ukb_util_get("ukbmd5")
#' file.remove(md5)
ukb_util_get <- function(
  util = c("ukbmd5", "ukbconv", "ukbunpack",
           "ukbfetch", "ukblink", "ukbgene",
           "encoding.ukb"),
  download = TRUE,
  out_dir = tempdir()) {

  util = match.arg(util)
  st = sys_type()
  stopifnot(st %in% c("windows", "linux", "macos"))

  ext = ""
  if (st == "windows") {
    ext = ".exe"
  }

  exec_path = Sys.which(util)
  if (nzchar(exec_path)) {
    return(exec_path)
  }

  if (download) {
    util_url = paste0("http://biobank.ndph.ox.ac.uk/showcase/util/",
                 util, ext)
    dest_file = file.path(out_dir, basename(util_url))

    if (!file.exists(dest_file)) {
      utils::download.file(util_url, destfile = dest_file,
                           mode = "wb")
    }

    if (st %in% "macos") {
      noah = Sys.which("noah")
      if (!nzchar(noah)) {
        warning(
          paste0(
            "You may need noah to use this UKB utility ",
            "(a linux executable) on Mac OSX.",
            " See https://github.com/linux-noah/noah"),
          call. = FALSE
        )
      }
    }

    Sys.chmod(dest_file)
    return(dest_file)

  } else {

    stop(
      paste0("Cannot find tool: ", util,
             ", you may need to modify your PATH",
             "so that Sys.which('", util, "')",
             "returns the path to the utility.")
    )
  }
}




#' Downloads encoding dictionaries for use with ukb_util_conv.
#'
#' @param out_dir The output directory to download the UKB utility to. Default \code{tempdir()}.
#'
# #' @rdname ukb_util_get
#' @export
ukb_util_encoding <- function(out_dir = tempdir()) {
  res = ukb_util_get(util = "encoding.ukb",
                      download = TRUE,
                      out_dir = out_dir)
}




#' Calculates size and MD5 of a UKB utlity file.
#'
#' @param file Path to file to run MD5 utility on.
#' @param ... Additional arguments to pass to
#' \code{\link{ukb_util_get}}.
#'
#' @return A character string
#'
#' @export
ukb_util_md5 <- function(file, ...) {
  path = ukb_util_get("ukbmd5", ...)
  out = system2(path, args = file, stdout = TRUE)
  out = out[grepl("MD5=", out)]
  out = sub(".*MD5=", "", out)
  return(out)
}




#' Unpacks (decrypts and decompresses) UKB data.
#'
# #' @rdname ukb_util_md5
#' @param file Path to file to unpack/decrypt.
#' @param key file to key to unpack/decrypt file
#' @param ... Additional arguments to pass to
#' \code{\link{ukb_util_get}}.
#' @export
ukb_util_unpack <- function(file, key, ...) {
  path = ukb_util_get("ukbunpack", ...)
  out = system2(path, c(file, key))
  if (out != 0) {
    warning("Unpacking did not seem to complete successfully")
  }
  out = paste0(file, "_ukb")
  return(out)
}




#' Converts unpacked UKB data to other formats.
#'
# #' @rdname ukb_util_md5
#' @param file Path to decrypted file to convert.
#' @param type Type of conversion to do.
#' @param encoding_file encoding file to map for `ukbconv`.  If want no
#' encoding, set to \code{NULL}
#' @param ... Additional arguments to pass to
#' \code{\link{ukb_util_get}}.
#' @export
ukb_util_conv <- function(file,
                    type = c("r", "docs",
                             "csv", "sas",
                             "stata",
                             "lims", "bulk",
                             "txt"),
                    encoding_file = "encoding.ukb",
                    ...) {
  type = match.arg(type)
  if (!is.null(encoding_file)) {
    url = paste0("http://biobank.ndph.ox.ac.uk/showcase/util/",
                 "encoding.ukb")
    if (!file.exists(encoding_file)) {
      utils::download.file(url, destfile = encoding_file,
                           mode = "wb")
    }
  }
  path = ukb_util_get("ukbconv", ...)
  args = c(file, type)
  # if not default file
  if (!is.null(encoding_file) && encoding_file != "encoding.ukb") {
    args = c(args, "-E", encoding_file)
  }
  out = system2(path, args)

  if (out != 0) {
    warning("Convert did not seem to complete successfully")
  }

  return(out)
}




#' Downloads approved bulk data files.
#'
# #' @rdname ukb_util_md5
#' @param file Path to bulk file
#' @param key Path to key file.
#' @param start start of the fetching, 1-indexed
#' @param out_dir output directory of download
#' @param ... Additional arguments to pass to
#' \code{\link{ukb_util_get}}.
#' @export
ukb_util_fetch <- function(file, key, start = NULL, out_dir = NULL, ...) {
  stopifnot(file.exists(file))

  file = normalizePath(file, mustWork = TRUE, winslash = "/")
  key = normalizePath(key, mustWork = TRUE, winslash = "/")

  if (nchar(file) > 64) {
    warning("File may be too long > 64 characters")
  }

  if (nchar(key) > 64) {
    warning("Key file may be too long > 64 characters")
  }

  owd = getwd()
  if (!is.null(out_dir)) {
    setwd(out_dir)
    on.exit({
      setwd(owd)
    }, add = TRUE)
  }

  n_max = 1000
  if (is.null(start)) {
    x = readLines(file)
    n = length(x)
    if (n > n_max) {
      start = (seq(0, ceiling(n / n_max) -1) * n_max) + 1
    } else {
      start = 1
    }
  }

  path = ukb_util_get("ukbfetch", ...)
  bfile = paste0("-b", file)
  akey = paste0("-a", key)

  starts = paste0("-s", start)
  x = starts[1]
  num = paste0("-m", n_max)
  res = sapply(starts, function(x) {
    out = system2(path, c(bfile, akey, x, num))
    if (out != 0) {
      warning("Convert did not seem to complete successfully")
    }
    out
  })
  return(res)
}
