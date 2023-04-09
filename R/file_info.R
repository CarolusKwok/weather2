#' Extract file information with path
#'
#' This is a wrapper for `base::file.info`, but with an additional column of the path provided by user.
#'
#' @param file File paths as a `list` or a `vector`
#'
#' @return
#' For __Windows__: A tibble, containing the following columns.
#' * size: File size in bytes. In `numeric`.
#' * isdir: Is the file a directory. In `logical`.
#' * mode: File permissions, printed in octal. In `Integer`.
#' * mtime: File modification time. In `POSIXct`.
#' * ctime: File last status change time. In `POSIXct`.
#' * atime: File last access time. In `POSIXct`.
#' * exe: Is the file an executable file (.exe)? If so, what kind of executable ("no", "msdos", "win16", "win32", "win64", or "unknown")? In `character`.
#'
#' @export
#'
#' @examples file_info(c("1.csv", "2.csv", "3.csv"))
file_info = function(file){
  data = tibble::tibble(path = file) %>%
    dplyr::bind_cols(file.info(file, extra_cols = T))
  return(data)
}
