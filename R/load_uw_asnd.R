#' Download atmospheric sounding data
#'
#' Understanding the sounding is important for understanding the atmospheric condition, especially for extreme weather.
#' This catches the sounding data provided by the University of Wyoming.
#'
#' @param time A list of time. If said time does not have a sounding, it will try to adjust to the last sounding possible. Only accepts POSIXct.
#' @param station Weather station code, according to the University of Wyoming website.
#' @param frost Include frost point calculation in dataframe. Default as `TRUE`.
#' @param attempt Attempts of downloads to try per time. Default as `5L`.
#' @param list_fail Should failed to download items be listed out? Default as `TRUE`.
#' @param worker Numbers of sessions to be open. Default as `1L`.
#'
#' @return
#' @export
#'
#' @examples load_uw_asnd()
load_uw_asnd = function(time = Sys.time(), station = "45004", frost = T, attempt = 5L, worker = 1L, list_fail = T){
  #Preset function ####
  magic = function(data, frost){
    filter = ifelse(frost,
                    "   PRES   HGHT   TEMP   DWPT   FRPT   RELH   RELI   MIXR   DRCT   SKNT   THTA   THTE   THTV",
                    "   PRES   HGHT   TEMP   DWPT   RELH   MIXR   DRCT   SKNT   THTA   THTE   THTV")
    cols = trimws(filter) %>% stringr::str_split(pattern = "   ") %>% unlist() %>% stringr::str_to_lower()
    Line = data$Line
    Info = data$Info
    ##Part 0: Check the data ####
    if(tryCatch(nrow(Line), error = function(e){0}) <= 28){
      return(tibble::tibble(.rows = 0))
    }
    ##Part 1: Get the station information ####
    Info_lle = Line %>%
      dplyr::mutate(lat = stringr::str_detect(string = value, pattern = "Station latitude"),
                    lon = stringr::str_detect(string = value, pattern = "Station longitude"),
                    ele = stringr::str_detect(string = value, pattern = "Station elevation")) %>%
      dplyr::filter(lat|lon|ele) %>%
      dplyr::mutate(loc = stringr::str_locate(string = value, pattern = ":")[,"start"],
                    value = stringr::str_sub(string = value, start = loc+1, end = -1),
                    value = trimws(value))
    lat = as.numeric(dplyr::filter(Info_lle, lat)$value[1])
    lon = as.numeric(dplyr::filter(Info_lle, lon)$value[1])
    ele = as.numeric(dplyr::filter(Info_lle, ele)$value[1])
    loc = stringr::str_locate(string = Info, pattern = "\\-")[1,1]
    station = stringr::str_sub(Info, start = loc+1, end = -1)
    time = stringr::str_sub(Info, start = 1, end = (loc-1)) %>% as.POSIXct(tz = "UTC", format = "%Y%m%d%H")

    ##Part 2: Format the data ####
    slice = match(x = filter, table = Line$value)
    data = Line %>%
      dplyr::slice(slice:dplyr::n()) %>%
      dplyr::slice(c(1, 4:dplyr::n()))

    filter = "</PRE><H3>Station information and sounding indices</H3><PRE>"
    slice = match(x = filter, table = data$value)
    data = dplyr::slice(data, 2:(slice-1))
    data = readr::read_fwf(I(data$value), show_col_types = FALSE)
    colnames(data) = cols
    data = dplyr::mutate(data,
                         time = time,
                         station = station,
                         lat = lat,
                         lon = lon,
                         ele = ele) %>%
      dplyr::relocate(time, station, lat, lon, ele, .before = dplyr::everything())
    return(data)
  }
  #Check ####
  if(weather2::sys_ckc_POSIXct(time, "time")){return()}
  if(weather2::sys_ckc_character(station, "station")){return()}
  if(weather2::sys_ckc_integer(attempt, "attempt")){return()}
  if(weather2::sys_ckc_integer(worker, "worker")){return()}
  if(weather2::sys_ckc_logical(list_fail, "list_fail")){return()}
  if(weather2::sys_ckc_logical(frost, "frost")){return()}

  #Create Dataframe #####
  URL = tibble::tibble(time = time) %>%
    dplyr::mutate(time_UTC = lubridate::with_tz(time, tzone = "UTC"),
                  diff = lubridate::hour(time_UTC) %% 12,
                  Ltime_UTC = time_UTC - lubridate::hours(diff),
                  Year = sprintf("%04d", as.numeric(lubridate::year(Ltime_UTC))),
                  Month = sprintf("%02d", as.numeric(lubridate::month(Ltime_UTC))),
                  DayHour = paste0(sprintf("%02d", as.numeric(lubridate::day(Ltime_UTC))),
                                   sprintf("%02d", as.numeric(lubridate::hour(Ltime_UTC)))),
                  URL = paste0("https://weather.uwyo.edu/cgi-bin/sounding?region=seasia&TYPE=TEXT%3ALIST",
                               "&YEAR=", Year, "&MONTH=", Month, "&FROM=", DayHour, "&TO=", DayHour,"&STNM=", station,
                               "&ICE=", as.numeric(frost)),
                  Info = paste0(Year, Month, DayHour,"-", station)) %>%
    dplyr::select(URL, Info) %>%
    dplyr::distinct()

  #Grab the list of data ####
  if(worker <= 0){
    return(URL)
  }

  list = weather2::sys_load_line(data = URL,
                                 title = "UW - Atmospheric Soundings",
                                 attempt = attempt,
                                 threshold = 29L,
                                 list_fail = list_fail,
                                 worker = worker,
                                 check = T)
  list = lapply(list, FUN = magic, frost = frost)
  list = do.call(what = dplyr::bind_rows, list)
  return(list)
}
