#' System tool: Check if the input satisfy the sys_load_file requirements
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
sys_ckf_SysLoadFile = function(data, title, attempt, worker, list_fail, threshold){
  if(weather2::sys_ckc_dataframe(value = data, value_name = "data")){return(T)}
  if(weather2::sys_ckd_colexist(value = DIR, value_name = "DIR", data = data, data_name = "data")){return(T)}
  if(weather2::sys_ckd_colexist(value = URL, value_name = "URL", data = data, data_name = "data")){return(T)}
  if(weather2::sys_ckd_colexist(value = Info, value_name = "Info", data = data, data_name = "data")){return(T)}
  if(weather2::sys_ckc_character(value = title, value_name = "title")){return(T)}
  if(weather2::sys_ckc_integer(value = attempt, value_name = "attempt")){return(T)}
  if(weather2::sys_ckc_integer(value = worker, value_name = "worker")){return(T)}
  if(weather2::sys_ckc_logical(value = list_fail, value_name = "list_fail")){return(T)}
  if(weather2::sys_ckc_numeric(value = threshold, value_name = "threshold")){return(T)}
  return(F)
}

#' System tool: Check if the input satisfy the sys_load_fileset requirements
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
sys_ckf_SysLoadFileset = function(data, title, attempt, worker, list_fail, threshold){
  if(weather2::sys_ckc_dataframe(value = data, value_name = "data")){return(T)}
  if(weather2::sys_ckd_colexist(value = DIR, value_name = "DIR", data = data, data_name = "data")){return(T)}
  if(weather2::sys_ckd_colexist(value = URL, value_name = "URL", data = data, data_name = "data")){return(T)}
  if(weather2::sys_ckd_colexist(value = Info, value_name = "Info", data = data, data_name = "data")){return(T)}
  if(weather2::sys_ckd_colexist(value = Set, value_name = "Set", data = data, data_name = "data")){return(T)}
  if(weather2::sys_ckc_character(value = title, value_name = "title")){return(T)}
  if(weather2::sys_ckc_integer(value = attempt, value_name = "attempt")){return(T)}
  if(weather2::sys_ckc_integer(value = worker, value_name = "worker")){return(T)}
  if(weather2::sys_ckc_logical(value = list_fail, value_name = "list_fail")){return(T)}
  if(weather2::sys_ckc_numeric(value = threshold, value_name = "threshold")){return(T)}
  return(F)
}




#' System tool: Check if the input satisfy the `calc_smooth_` requirements
#'
#' @param data The dataframe to be smoothened
#' @param based The column name of the explanatory variable
#' @param value The column name of the response variable
#'
#' @return
#' @export
#'
#' @examples
sys_ckf_CalcSmooth = function(data, based, value){
  if(weather2::sys_ckc_dataframe(value = data, value_name = "data")){return(T)}
  if(weather2::sys_ckd_colexist(value = {{based}}, value_name = "based", data = data, data_name = "data")){return(T)}
  if(weather2::sys_ckd_colexist(value = {{value}}, value_name = "value", data = data, data_name = "data")){return(T)}

  data0 = dplyr::select(data, x = {{based}})$x
  if(weather2::sys_ckl_hasNA(list = data0, list_name = "based", mode = "exclude")){return(T)}
  if(weather2::sys_ckl_numericable(list = data0, list_name = "based")){return(T)}
  if(weather2::sys_ckl_ItemUnique(list = data0, list_name = "based")){return(T)}

  data0 = dplyr::select(data, x = {{value}})$x
  if(weather2::sys_ckl_hasNA(list = data0, list_name = "based", mode = "include")){return(T)}
  if(weather2::sys_ckl_numericable(list = data0, list_name = "value")){return(T)}
  return(F)
}
