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
#' @param threshold Threshold of line size. Any downloaded lines with number less than the threshold will be considered as failure and will be reattempted.
#'
#' @return
#' @export
#'
#' @examples sys_ckf_SysLoadFileset(data, "title", 5L, 20L, TRUE, 1)
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


#' System tool: Check if the input satisfy the `sys_load_line` requirements
#'
#' @param data Dataframe containing columns `URL` and `Info`.
#' @param title Title of the downloaded data
#' @param attempt Attempts to be made per download URL to be attempted.
#' @param worker Numbers of sessions to be open.
#' @param list_fail List failed-to-download items
#' @param threshold
#'
#' @return
#' @export
#'
#' @examples sys_ckf_SysLoadLine(data, "title", 5L, 20L, TRUE, 1L)
sys_ckf_SysLoadLine = function(data, title, attempt, worker, list_fail, threshold){
  if(weather2::sys_ckc_dataframe(value = data, value_name = "data")){return(T)}
  if(weather2::sys_ckd_colexist(value = URL, value_name = "URL", data = data, data_name = "data")){return(T)}
  if(weather2::sys_ckd_colexist(value = Info, value_name = "Info", data = data, data_name = "data")){return(T)}
  if(weather2::sys_ckc_character(value = title, value_name = "title")){return(T)}
  if(weather2::sys_ckc_integer(value = attempt, value_name = "attempt")){return(T)}
  if(weather2::sys_ckc_integer(value = worker, value_name = "worker")){return(T)}
  if(weather2::sys_ckc_logical(value = list_fail, value_name = "list_fail")){return(T)}
  if(weather2::sys_ckc_integer(value = threshold, value_name = "threshold")){return(T)}
  return(F)
}

#' System tool: Check if the input satisfy the `calc_smooth_` requirements
#'
#' @param data The dataframe to be smoothened
#' @param based The column name of the explanatory variable
#' @param value The column name of the response variable
#' @param check_na Should NA values be excluded from the based and value column? Default as `TRUE`
#'
#' @return
#' A logical value. `TRUE` if an error occured. `FALSE` if no error occured. A text message is provided if `silent` is `FALSE`.
#' @export
#'
#' @examples
sys_ckf_CalcSmooth = function(data, based, value, check_na = T){
  if(weather2::sys_ckc_dataframe(value = data, value_name = "data")){return(T)}
  if(weather2::sys_ckd_colexist(value = {{based}}, value_name = "based", data = data, data_name = "data")){return(T)}
  if(weather2::sys_ckd_colexist(value = {{value}}, value_name = "value", data = data, data_name = "data")){return(T)}
  if(weather2::sys_ckd_grouped(data = data, data_name = "data")){return(T)}

  data0 = dplyr::select(data, x = {{based}})$x
  if(check_na){
    if(weather2::sys_ckl_hasNA(list = data0, list_name = "based", mode = "exclude")){return(T)}
  }
  if(weather2::sys_ckl_numericable(list = data0, list_name = "based")){return(T)}
  if(weather2::sys_ckl_ItemUnique(list = data0, list_name = "based")){return(T)}

  data0 = dplyr::select(data, x = {{value}})$x
  if(check_na){
    if(weather2::sys_ckl_hasNA(list = data0, list_name = "based", mode = "include")){return(T)}
  }
  if(weather2::sys_ckl_numericable(list = data0, list_name = "value")){return(T)}
  return(F)
}


#' System tool: Check if the input satisfy the `sys_tld_FormatReturn` requirements
#'
#' @param name_as Names of the newly added columns into the dataframe. The keyword "*del*" is used here.
#' @param overwrite Let the new column names to overwrite the original dataframe columns?
#' @param expected Expected length of `name_as`, in integer.
#'
#' @return A logical value, with `TRUE` as encountering an issue with input, and `FALSE` as successful. Prints error message when `TRUE`.
#' @export
#'
#' @examples sys_ckf_NameAsReturn("temp2", TRUE, 1L)
sys_ckf_NameAsReturn = function(name_as, overwrite, expected){
  if(weather2::sys_ckc_character(value = name_as, value_name = "name_as")){return(T)}
  if(weather2::sys_ckc_logical(value = overwrite, value_name = "overwrite")){return(T)}
  if(weather2::sys_ckc_integer(expected, "expected")){return(T)}

  if(weather2::sys_ckl_length(list = name_as, list_name = "name_as", expected = expected, mode = "==")){return(T)}
  return(F)
}
