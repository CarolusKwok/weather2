#' MO weather - Download tidal images from https://www.smg.gov.mo/zh
#'
#' @param time Date/time to download
#' @param dir Directory of downloaded data.
#' @param attempt Attempts to be made per file to download.
#' @param subattempt Attempts to be made per attempt to download.
#'
#' @return
#' @export
#'
#' @examples mo_load_tide()
mo_load_tide = function(time = weather2::tool_datetime(end = Sys.time(), duration = "91 hour", by = "1 min") %>% weather2::tool_datetime_select(by = "min", value = 0),
                        dir = getwd(), attempt = 900, subattempt = 1, worker = 1){
  #Check
  if(!weather2::w2_check_internet(silent = T)){return(invisible())}
  if(weather2::w2_check_posixct(value = time, value_name = "time")){return(invisible())}
  if(weather2::w2_check_int(value = as.integer(attempt), value_name = "attempt")){return(invisible())}
  if(weather2::w2_check_int(value = as.integer(subattempt), value_name = "subattempt")){return(invisible())}

  #Additional variables
  dit = 15

  #Force time to be HKT
  time = lubridate::with_tz(time, tzone = "HongKong")

  #Format
  URL = tibble::tibble(time = time) %>%
    dplyr::mutate(Year = lubridate::year(time),
                  Month= lubridate::month(time),
                  Day = lubridate::day(time),
                  Hour = lubridate::hour(time),
                  Min = lubridate::minute(time),
                  com = Min %% dit,
                  Ltime = ISOdatetime(Year, Month, Day, Hour, Min, 00, tz = "Asia/Macau") - lubridate::minutes(com),
                  set = seq(1, dplyr::n(), 1)) %>%
    dplyr::select(time, Ltime) %>%
    dplyr::mutate(LDate = paste0(sprintf("%04d", lubridate::year(Ltime)),
                                 sprintf("%02d", lubridate::month(Ltime)),
                                 sprintf("%02d", lubridate::day(Ltime))),
                  LHour = paste0(sprintf("%02d", lubridate::hour(Ltime)),
                                 sprintf("%02d", lubridate::minute(Ltime))),
                  Ztime = as.numeric(difftime(lubridate::with_tz(Ltime,tzone = "UTC"),
                                              ISOdatetime(1970, 1, 1, 0, 0, 0, tz = "UTC"),
                                              units = "sec")))

  URLt = tibble::tibble(Set = NA_real_, URL = NA_character_, DIR = NA_character_, Info = NA_character_, .rows = 0)
  for(i in 1:nrow(URL)){
    URL2 = tibble::tibble(Set = i,
                          time = URL$time[i],
                          Ltime = URL$Ltime[i],
                          LDate = URL$LDate[i],
                          LHour = URL$LHour[i],
                          Ztime = URL$Ztime[i],
                          seq = seq(0: attempt)) %>%
      dplyr::mutate(URL = paste0("https://cms.smg.gov.mo/uploads/backup/WL/WL_",
                                 (Ztime+seq),
                                 ".png"),
                    Info = paste0(LDate, "-", LHour),
                    DIR = paste0(dir,
                                 "/", "MO_Data",
                                 "/", "TIDE",
                                 "/", substr(LDate, 1, 4),
                                 "/", substr(LDate, 1, 6),
                                 "/", LDate,
                                 "/", "MO_TIDE", LDate, "-", LHour, ".png")) %>%
      dplyr::select(Set, URL, DIR, Info)
    URLt = dplyr::bind_rows(URLt, URL2)
  }

  #Start
  weather2::w2_load_fileset(data = URLt, title = "MO Tidal Height", subattempt = subattempt, worker = worker)
}
