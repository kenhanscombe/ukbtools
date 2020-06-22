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
            "You may need noah to use these linux execs on Mac OSX,",
            " See https://github.com/linux-noah/noah ")
        )
      }
    }

    url = paste0("http://biobank.ndph.ox.ac.uk/showcase/util/",
                 util, ext)
    destfile = file.path(outdir, basename(url))
    if (!file.exists(destfile)) {
      download.file(url, destfile = destfile,
                    mode = "wb")
    }
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

ukb_md5 = function(filename, checksum, ...) {
  path = ukb_util_path("ukbmd5", ...)
  out = system2(path, filename)
}
