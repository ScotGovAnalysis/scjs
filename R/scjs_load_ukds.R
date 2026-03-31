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

my_path <- Sys.getenv("USER_DATA_PATH")

folder <- ukds_lookup[[1,3]]
cur_path <- paste0(my_path, folder)

found_path <- paste0(
  cur_path,
  "/",
  list.files(cur_path, pattern = ukds_lookup[[1,4]], recursive = TRUE)
)

new_df <- haven::read_sav(found_path)
