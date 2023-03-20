#' System tools: Get a non-clashing column name within a dataframe, by adding a numeric suffix ("_x")
#'
#' @param value Column name you are expecting to use. Can be a symbol or a character.
#' @param data The dataframe you are using
#'
#' @return A character
#' @export
#'
#' @examples sys.tl.data_get.colname(x, data = tibble::tibble(x = 0)) #Returns "x_1"
sys.tl.data_get.colname = function(value, data){
  #Check ####
  if(weather2:::sys.help_hasArg(value, value_name = "value")){return(T)}
  if(weather2:::sys.help_hasArg(data, value_name = "data")){return(T)}
  #Work ####
  value = weather2:::sys.help_sym2chr({{value}})
  colnames_data = colnames(data)
  name_trial = value
  n = 0
  while(name_trial %in% colnames_data){
    n = n + 1
    name_trial = paste0(value, "_", n)
  }
  return(name_trial)
}
