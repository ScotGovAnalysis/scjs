#' Import data downloaded from the UKDS
#'
#' @param path A file path to where files downloaded from the UKDS are.
#'
#' @return A string
#' @export
#'
#' @examples
#' x <- "path/to/data"
#' scjs_load_ukds(x)
scjs_load_ukds <- function(path=getwd()) {
  return(paste("path to UKDS data:", path))
}
