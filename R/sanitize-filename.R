#' Sanitizes file name
#'
#' Sanitizes file name (without the extension part).
#' Ported and modified from `fs::path_sanitize()`.
#'
#' @param filename TBA
#' @param replacement TBA
#'
#' @return TBA
#'
#' @export sanitize_filename
#'
#' @examples
#' x <- " znul/zzz.z>z/\\z "
#' paste0(sanitize_filename(x), ".rds")
sanitize_filename <- function(filename, replacement = "-") {
  illegal <- "[/\\?<>\\:*|\":]"
  control <- "[[:cntrl:]]"
  reserved <- "^[.]+$"
  windows_reserved <- "^(con|prn|aux|nul|com[0-9]|lpt[0-9])([.].*)?$"
  windows_trailing <- "[. ]+$"

  filename <- trimws(filename)

  filename <- gsub(illegal, replacement, filename)
  filename <- gsub(control, replacement, filename)
  filename <- gsub(reserved, replacement, filename)
  filename <- gsub(windows_reserved, replacement, filename, ignore.case = TRUE)
  filename <- gsub(windows_trailing, replacement, filename)

  # TODO: this substr should really be unicode aware, so it doesn't chop a
  # multibyte code point in half.
  filename <- substr(filename, 1, 255)
  if (replacement == "") {
    return(filename)
  }
  sanitize_filename(filename, "")
}
