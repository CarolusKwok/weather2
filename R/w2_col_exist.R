#' System tool: Check if a column exist in data
#'
#' @param data Data
#' @param col Column name. Doesn't need to be in "".
#'
#' @return
#' @export
#'
#' @examples w2_col_exist(data, info)
w2_col_exist = function(data, col){
  flag = F
  check = tryCatch(dplyr::select(data, {{col}}), error = function(e){tibble::tibble(.rows = 0)})
  if(ncol(check) != 0){flag = T}
  return(flag)
}
