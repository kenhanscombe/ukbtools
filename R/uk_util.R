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

#' Get Path to UKB Utilitiy
#'
#' @param util Name of the utility
#' @param download Should the utility be downloaded if not found?
#' @param outdir The output directory to download the utility
#'
#' @return A path to the utility
#' @export
#'
#' @examples
#' md5 = ukb_util_path("ukbmd5")
#' file.remove(md5)
ukb_util_path = function(
  util = c("ukbmd5", "ukbconv", "ukbunpack",
           "ukbfetch", "ukblink", "ukbgene",
           "encoding.ukb"),
  download = TRUE,
  outdir = tempdir()) {

  util = match.arg(util)
  st = sys_type()
  stopifnot(st %in% c("windows", "linux", "macos"))

  ext = ""
  if (st == "windows") {
    ext = ".exe"
  }

  tool_path = Sys.which(util)
  if (nzchar(tool_path)) {
    return(tool_path)
  }

  if (download) {

    if (st %in% "macos") {
      noah = Sys.which("noah")
      if (!nzchar(noah)) {
        warning(
          paste0(
            "You may need noah to use these ",
            "linux execs on Mac OSX,",
            " See https://github.com/linux-noah/noah ")
        )
      }
    }

    url = paste0("http://biobank.ndph.ox.ac.uk/showcase/util/",
                 util, ext)
    destfile = file.path(outdir, basename(url))
    if (!file.exists(destfile)) {
      utils::download.file(url, destfile = destfile,
                           mode = "wb")
    }
    Sys.chmod(destfile)
    return(destfile)
  } else {
    stop(
      paste0("Cannot find tool: ", util,
             ", may need to modify PATH",
             "so that Sys.which('", util, "')",
             "returns the path to the tool")
    )
  }
}

#' @rdname ukb_util_path
#' @export
ukb_encoding = function(  outdir = tempdir()) {
  res = ukb_util_path(util = "encoding.ukb",
                      download = TRUE,
                      outdir = outdir)
}

#' UKB MD5 Checksum
#'
#' @param file name of file to run utility on
#' @param ... additional arguments to pass to
#' \code{\link{ukb_util_path}}
#'
#' @return A character string
#'
#' @export
ukb_md5 = function(file, ...) {
  path = ukb_util_path("ukbmd5", ...)
  out = system2(path, file, stdout = TRUE)
  out = out[grepl("MD5=", out)]
  out = sub(".*MD5=", "", out)
  return(out)
}


#' @rdname ukb_md5
#' @param key file to key to unpack/decrypt file
#' @export
ukb_unpack = function(file, key, ...) {
  path = ukb_util_path("ukbunpack", ...)
  out = system2(path, c(file, key))
  if (out != 0) {
    warning("Unpacking did not seem to complete successfully")
  }
  out = paste0(file, "_ukb")
  return(out)
}


#' @rdname ukb_md5
#' @param type type of conversion to do
#' @export
ukb_conv = function(file,
                    type = c("r", "docs",
                             "csv", "sas",
                             "stata",
                             "lims", "bulk",
                             "txt"), ...) {
  type = match.arg(type)
  path = ukb_util_path("ukbconv", ...)
  out = system2(path, c(file, type))
  if (out != 0) {
    warning("Convert did not seem to complete successfully")
  }
  return(out)
}

#' @rdname ukb_md5
#' @param start start of the fetching, 1-indexed
#' @export
ukb_fetch_bulk = function(
  file,
  key,
  start = NULL,
  ...) {
  stopifnot(file.exists(file))

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

  path = ukb_util_path("ukbfetch", ...)
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
