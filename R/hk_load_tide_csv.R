#' HK weather - Downloading tidal height data from https://data.gov.hk/en/help/api-spec
#'
#' @param time Date/time to download
#' @param lan Language to download. Accepts "en", "tc", or "sc"
#' @param type Type of Data to download. Accepts "hko", "md", or both.
#' @param list_fail List failed-to-download items
#' @param dir Directory of downloaded data.
#' @param attempt Attempts to be made per file to download
#' @param worker Numbers of sessions to be open
#'
#' @return
#' @export
#'
#' @examples hk_load_tide_csv()
hk_load_tide_csv = function(time = weather2::tool_datetime(end = Sys.time(), by = "5 min", duration = "7 day"), lan = "en", type = c("hko", "md"),
                            list_fail = F, dir = getwd(), attempt = 10, worker = 1){
  #Check
  if(!weather2::w2_check_internet(silent = T)){return(invisible())}
  if(weather2::w2_check_posixct(value = time, value_name = "time")){return(invisible())}
  if(weather2::w2_check_lan(value = lan, value_name = "lan")){return(invisible())}
  if(weather2::w2_check_int(value = as.integer(attempt), value_name = "attempt")){return(invisible())}
  #Additional variables
  nlan = ifelse(lan == "en", "en",
                ifelse(lan == "tc", "tc",
                       ifelse(lan == "sc", "sc", NA)))
  #Force time to be HKT
  time = lubridate::with_tz(time, tzone = "HongKong")
  #Split operations
  URL = tidyr::crossing(time = time, type = type) %>%
    dplyr::mutate(dit = ifelse(type == "hko", 5, 10),
                  year = lubridate::year(time),
                  month = lubridate::month(time),
                  day = lubridate::day(time),
                  hour = lubridate::hour(time),
                  min = lubridate::minute(time),
                  com = min %% dit,
                  Ltime = ISOdatetime(year, month, day, hour, min - com, 00, tz = "HongKong"),
                  LDate = paste0(sprintf("%04d", lubridate::year(Ltime)),
                                 sprintf("%02d", lubridate::month(Ltime)),
                                 sprintf("%02d", lubridate::day(Ltime))),
                  LHour = paste0(sprintf("%02d", lubridate::hour(Ltime)),
                                 sprintf("%02d", lubridate::minute(Ltime))),
                  Info = paste0(LDate, "-", LHour),
                  URL = paste0("https://api.data.gov.hk/v1/historical-archive/get-file?url=https%3A%2F%2Fdata.weather.gov.hk%2FweatherAPI%2Fhko_data%2Ftide%2FALL_",
                               nlan, ".csv&time=", LDate, "-", LHour),

                  Ptime = Ltime - lubridate::minutes(5),
                  PDate = paste0(sprintf("%04d", lubridate::year(Ptime)),
                                 sprintf("%02d", lubridate::month(Ptime)),
                                 sprintf("%02d", lubridate::day(Ptime))),
                  PHour = paste0(sprintf("%02d", lubridate::hour(Ptime)),
                                 sprintf("%02d", lubridate::minute(Ptime))),

                  DIR = paste0(dir,
                               "/", "HK_Data",
                               "/", "TIDE",
                               "/", "TIDE(hko)", lan,
                               "/", substr(PDate, 1, 4),
                               "/", substr(PDate, 1, 6),
                               "/", PDate,
                               "/", "TIDE(hko)", lan, "-", PDate, "-", PHour, ".csv")) %>%
    dplyr::arrange(Ltime) %>%
    dplyr::select(Info, URL, DIR) %>%
    dplyr::distinct()

  title = paste0("Tidal Height_(",
                 stringr::str_flatten(type, collapse = ","), ")")
  weather2::w2_load_file(data = URL, attempt = attempt, title = title, list_fail = list_fail, worker = worker)
}
