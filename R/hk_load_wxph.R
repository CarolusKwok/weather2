#' HK weather - Downloading weather photo from https://www.hko.gov.hk/en/index.html
#'
#' @param time Date/time to download
#' @param magn Satellite image magnification. Accepts 2/ 4/ 8, or any combinations
#' @param type Type of satellite image. Accepts "tc" (True color), "ir" (Infrared), "dc" (Deep convection), or any combinations.
#' @param list_fail List failed-to-download items
#' @param dir Directory of downloaded data.
#' @param attempt Attempts to be made per file to download.
#' @param worker Numbers of sessions to be open
#'
#' @return
#' @export
#'
#' @examples hk_load_wxph()
hk_load_wxph = function(time = weather2::tool_datetime(end = Sys.time(), by = "10 min", duration = "3 day"),
                        station = "all", list_fail = F, dir = getwd(), attempt = 10, worker = 1){
  #Check
  if(!weather2::w2_check_internet(silent = T)){return(invisible())}
  if(weather2::w2_check_posixct(time, "time")){return(invisible())}
  if(weather2::w2_check_int(value = as.integer(attempt), value_name = "attempt")){return(invisible())}

  #Force time to be HKT
  time = lubridate::with_tz(time, tzone = "HongKong")

  #Check if station = "all", and Force station to be lower
  if("all" %in% station | "ALL" %in% station){station = weather2::tool_hk_wxph()$code}
  station = unique(tolower(station))

  #Additional variables
  dit = 5
  comb = station

  for(i in 1:length(comb)){
    station = comb[i]

    #Format
    URL = tibble::tibble(time = time) %>%
      dplyr::mutate(year = lubridate::year(time),
                    month = lubridate::month(time),
                    day = lubridate::day(time),
                    hour = lubridate::hour(time),
                    min = lubridate::minute(time),
                    com = min %% dit,
                    Ltime = ISOdatetime(year, month, day, hour, min - com, 00, tz = "HongKong")) %>%
      dplyr::select(time, Ltime) %>%
      dplyr::mutate(LDate = paste0(sprintf("%04d", lubridate::year(Ltime)),
                                   sprintf("%02d", lubridate::month(Ltime)),
                                   sprintf("%02d", lubridate::day(Ltime))),
                    LHour = paste0(sprintf("%02d", lubridate::hour(Ltime)),
                                   sprintf("%02d", lubridate::minute(Ltime))),

                    Info = paste0(LDate, "-", LHour),
                    URL = paste0("http://www.weather.gov.hk/wxinfo/aws/hko_mica/",
                                 tolower(station),
                                 "/img", toupper(station), "_", substr(LDate, 3, 8), "_", LHour, ".jpg"),
                    DIR = paste0(dir,
                                 "/", "HK_Data",
                                 "/", "WXPH",
                                 "/", "WXPH(", station, ")",
                                 "/", substr(LDate, 1, 4),
                                 "/", substr(LDate, 1, 6),
                                 "/", LDate,
                                 "/", "WXPH(", station, ")_", LDate, "_", LHour, ".jpg")) %>%
      dplyr::select(Info, URL, DIR) %>%
      dplyr::distinct()
    #Start to download
    #return(URL)
    weather2::w2_load_file(data = URL, attempt = attempt, title = paste0("Weather Photo_", station, " (HKO)"), list_fail = list_fail, worker = worker)
  }
}
