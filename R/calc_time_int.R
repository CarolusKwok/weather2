#' Calculate time interval, between previous or next row
#'
#' This function arranges the data, ascendingly, and calculates the duration between 2 observations - either comparing 1 row before or 1 row after.
#'
#'
#' @param data The dataframe itself.
#' @param time The column name of time
#' @param type Type of operation, accepts `"before"` or `"after"` only.
#' @param unit Unit of time interval. Default as `"minute"`.
#' @param name_as Names of the 1 new columns, i.e. time interval between the 2 rows. Default as `NULL`, i.e. the column name of `time` with a suffix of `"_int"`. Keyword `"*del*"` is supported.
#' @param overwrite Let the new column names to overwrite the original dataframe columns? Default as `FALSE`.
#'
#' @return The same dataframe as `data`, with 1 additional column with column name defined in `name_as`.
#' @export
#'
#' @examples data.frame(time = as.POSIXct(c("2023/01/01 12:00:00", "2022/12/01 12:00:00", "2022/11/01 12:00:00"))) %>%
#' calc_time_int(time, type = "after")
calc_time_int = function(data, time, type = "before", unit = "minute", name_as = NULL, overwrite = F){
  #Check ####
  if(weather2::sys_ckc_dataframe(data, "data")){return()}
  if(weather2::sys_ckd_colexist(value = time, value_name = "time", data = data, data_name = "data")){return()}
  if(weather2::sys_ckl_ItemIn(type, list_name = "type", expected = c("before", "after"), mode = "in")){return()}
  if(weather2::sys_ckl_length(list = type, list_name = "type", expected = 1L)){return()}
  if(weather2::sys_ckl_ItemIn(unit, list_name = "unit", expected = c("second", "minute", "hour", "day"))){return()}
  if(weather2::sys_ckl_length(list = unit, list_name = "unit", expected = 1L)){return()}

  if(is.null(name_as)){name_as = paste0(weather2:::sys_hp_sym2chr({{time}}), "_int")}
  if(weather2::sys_ckf_NameAsReturn(name_as = name_as,
                                    overwrite = overwrite,
                                    expected = 1L)){return()}


  #Format the data ####
  data = dplyr::arrange(data, {{time}})

  #Start to analysis ####
  data2 = dplyr::select(data, time1 = {{time}}) %>%
    dplyr::mutate(time1 = lubridate::with_tz(time = time1, tzone = "UTC"))

  if(type == "before"){
    data2 = dplyr::mutate(data2, time2 = dplyr::lag(time1))
  } else {
    data2 = dplyr::mutate(data2, time2 = dplyr::lead(time1))
  }
  data2 = dplyr::mutate(data2,
                        time_int = as.numeric(lubridate::as.duration(abs(time2 - time1)), unit = unit))

  #Return the data ####
  data = weather2::sys_tld_FormatReturn(data,
                                        name_as = name_as,
                                        value = list(data2$time_int),
                                        overwrite = overwrite)
  return(data)
}
