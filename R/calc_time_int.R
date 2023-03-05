#' Calculate time interval, between previous or next row
#'
#' @param data Dataframe or Weather containing POSIXct time
#' @param time Column name of the time
#' @param type Type of operation, accepts "before" or "after" only.
#' @param unit Unit of time interval. Default as "minute".
#' @param name_as Column name of the time interval. If not provided, default as time with suffix "_int".
#'
#' @return
#' @export
#'
#' @examples calc_time_int(data, time)
calc_time_int = function(data, time, type = "before", unit = "minute", name_as){
  #Format the data ####
  if(class(data) == "weather"){
    data1 = data$data
    unit1 = data$unit}
  else {data1 = data}
  data1 = dplyr::arrange(data1, {{time}})
  #Give name_as if missing ####
  if(missing(name_as)){name_as = paste0(colnames(dplyr::select(data1, {{time}})), "_int")}

  #Start to analysis ####
  data2 = dplyr::select(data1, {{time}}) %>%
    dplyr::rename(time1 = {{time}}) %>%
    dplyr::mutate(time1 = lubridate::with_tz(time = time1, tzone = "UTC"))
  if(type == "before"){list = append(as.POSIXct(NA_character_, tz = "UTC"), data2$time1[1:nrow(data2)-1])}
  if(type == "after"){list = append(data2$time1[2:nrow(data2)], as.POSIXct(NA_character_, tz = "UTC"))}
  data2 = dplyr::mutate(data2,
                        time2 = list,
                        time_int = as.numeric(lubridate::as.duration(abs(time2 - time1)), unit = unit)) %>%
    dplyr::select(time_int)
  data1 = dplyr::mutate(data1, "{name_as}" := data2$time_int)

  #Return the data ####
  if(class(data) == "weather"){
     data$data = data1
     data$unit = dplyr::mutate(unit1, "{name_as}" := unit)
  } else {data = data1}
  return(data)
}
