#' Extract file information with path
#'
#' Basically a wrapper for base::file.info
#'
#' @param file list of file paths
#'
#' @return
#' @export
#'
#' @examples file_info(c("1.csv", "2.csv", "3.csv"))
file_info = function(file){
  data = file.info(file, extra_cols = T) %>%
    dplyr::mutate(path = file) %>%
    dplyr::relocate(path, .before = dplyr::everything())
  return(data)
}
