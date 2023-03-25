#' Tool: To select date time by a value
#'
#' @param time A sequence of POSIXct
#' @param by A character string of "sec", "min", "hour", "day", "month", or "year"
#' @param value Value of time selected by "by". Can be a vector.
#'
#' @return
#' @export
#'
#' @examples tool_datetime_select(time, "min", 0)
tool_datetime_select = function(time, by, value){
  #Check
  if(weather2::sys_ckc_POSIXct(time, "time")){return(invisible())}

  #Start
  data = tibble::as_tibble(time) %>% dplyr::rename(time = value)

  if("sec" %in% by){
    data = dplyr::mutate(data, tvalue = floor(lubridate::second(time))) %>%
      dplyr::mutate(filter = base::ifelse(tvalue %in% value, T, F)) %>%
      dplyr::filter(filter == T)
  }
  if("min" %in% by){
    data = dplyr::mutate(data, tvalue = floor(lubridate::minute(time))) %>%
      dplyr::mutate(filter = base::ifelse(tvalue %in% value, T, F)) %>%
      dplyr::filter(filter == T)
  }
  if("hour" %in% by){
    data = dplyr::mutate(data, tvalue = floor(lubridate::hour(time))) %>%
      dplyr::mutate(filter = base::ifelse(tvalue %in% value, T, F)) %>%
      dplyr::filter(filter == T)
  }
  if("day" %in% by){
    data = dplyr::mutate(data, tvalue = floor(lubridate::day(time))) %>%
      dplyr::mutate(filter = base::ifelse(tvalue %in% value, T, F)) %>%
      dplyr::filter(filter == T)
  }
  if("month" %in% by){
    data = dplyr::mutate(data, tvalue = floor(lubridate::month(time))) %>%
      dplyr::mutate(filter = base::ifelse(tvalue %in% value, T, F)) %>%
      dplyr::filter(filter == T)
  }
  if("year" %in% by){
    data = dplyr::mutate(data, tvalue = floor(lubridate::year(time))) %>%
      dplyr::mutate(filter = base::ifelse(tvalue %in% value, T, F)) %>%
      dplyr::filter(filter == T)
  }
  data = data$time
  return(data)
}
