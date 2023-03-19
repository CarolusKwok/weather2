#' System tool: Check if the input satisfy the sys.load_file requirements
#'
#' @param data Data frame containing columns "URL", "DIR", "Info"
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
sys.cf_sys.load_file = function(data, title, attempt, worker, list_fail, threshold){
  if(weather2::sys.ck.class_data.frame(value = data, value_name = "data")){return(T)}
  if(weather2::w2_check_col_exist(data = data, data_name = "data", value = DIR, value_name = "DIR")){return(T)}
  if(weather2::w2_check_col_exist(data = data, data_name = "data", value = URL, value_name = "URL")){return(T)}
  if(weather2::w2_check_col_exist(data = data, data_name = "data", value = Info, value_name = "Info")){return(T)}
  if(weather2::sys.ck.class_character(value = title, value_name = "title")){return(T)}
  if(weather2::sys.ck.class_integer(value = attempt, value_name = "attempt")){return(T)}
  if(weather2::sys.ck.class_integer(value = worker, value_name = "worker")){return(T)}
  if(weather2::sys.ck.class_logical(value = list_fail, value_name = "list_fail")){return(T)}
  if(weather2::sys.ck.class_numeric(value = threshold, value_name = "threshold")){return(T)}
  return(F)
}

#' System tool: Check if the input satisfy the sys.load_fileset requirements
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
sys.cf_sys.load_fileset = function(data, title, attempt, worker, list_fail, threshold){
  if(weather2::sys.ck.class_data.frame(value = data, value_name = "data")){return(T)}
  if(weather2::w2_check_col_exist(data = data, data_name = "data", value = DIR, value_name = "DIR")){return(T)}
  if(weather2::w2_check_col_exist(data = data, data_name = "data", value = URL, value_name = "URL")){return(T)}
  if(weather2::w2_check_col_exist(data = data, data_name = "data", value = Info, value_name = "Info")){return(T)}
  if(weather2::w2_check_col_exist(data = data, data_name = "data", value = Set, value_name = "Set")){return(T)}
  if(weather2::sys.ck.class_character(value = title, value_name = "title")){return(T)}
  if(weather2::sys.ck.class_integer(value = attempt, value_name = "attempt")){return(T)}
  if(weather2::sys.ck.class_integer(value = worker, value_name = "worker")){return(T)}
  if(weather2::sys.ck.class_logical(value = list_fail, value_name = "list_fail")){return(T)}
  if(weather2::sys.ck.class_numeric(value = threshold, value_name = "threshold")){return(T)}
  return(F)
}
