#' System tool: Check if the input satisfy the w2_load_fileset... requirements
#'
#' @param data Data frame containing columns "URL", "DIR", "Info", "Set"
#' @param title Title of the downloaded data
#' @param attempt Attempts to be made per download.
#' @param worker Numbers of sessions to be open
#' @param list_fail List failed-to-download items
#' @param threshold Threshold of file size, when compared to the largest item to be installed in the list
#'
#' @return
#' @export
#'
#' @examples
w2_check_load_set = function(data, title, attempt, worker, list_fail, threshold){
  flag = F
  #Start checking!
  if(!weather2::w2_check_internet()){flag = T}
  if(weather2::w2_check_type_dataframe(value = data, value_name = "data")){
    flag = T
  } else {
    if(weather2::w2_check_col_exist(data = data, data_name = "data", value = DIR, value_name = "DIR")){flag = T}
    if(weather2::w2_check_col_exist(data = data, data_name = "data", value = URL, value_name = "URL")){flag = T}
    if(weather2::w2_check_col_exist(data = data, data_name = "data", value = Info, value_name = "Info")){flag = T}
    if(weather2::w2_check_col_exist(data = data, data_name = "data", value = Set, value_name = "Set")){flag = T}
  }
  if(weather2::w2_check_type_character(value = title, value_name = "title")){flag = T}
  if(weather2::w2_check_type_integer(value = attempt, value_name = "attempt")){flag = T}
  if(weather2::w2_check_type_integer(value = worker, value_name = "worker")){flag = T}
  if(weather2::w2_check_type_logical(value = list_fail, value_name = "list_fail")){flag = T}
  if(weather2::w2_check_type_numeric(value = threshold, value_name = "threshold")){flag = T}
  return(flag)
}
