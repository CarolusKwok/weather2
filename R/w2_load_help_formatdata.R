#' System tools: Format data with exist and size column in any w2_load_... functions
#'
#' @param data Data frame containing columns "URL", "DIR", "Info"
#'
#' @return
#' @export
#'
#' @examples w2_load_help_formatdata(data)
w2_load_help_formatdata = function(data){
  data = dplyr::mutate(data,
                       exist = file.exists(DIR),
                       size = file.size(DIR),
                       size = ifelse(is.na(size), 0, size))
  return(data)
}
