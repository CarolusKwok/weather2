#' Download METAR data
#'
#' Meteorological Aviation Report, or METAR, are weather reports issued at airports for aviators.
#' These reports have a high temporal resolution, updating at least once per hour.
#'
#' @param time Date to be download (UTC time)
#' @param station ICAO code
#' @param attempt Attempts of downloads to try per time. Default as 50.
#' @param decode Decode the METAR code automatically. Default as T.
#' @param CAVOK If decode is T, supress CAVOK and convert all CAVOK into vsby = 9999. Default as T
#'
#' @return
#' @export
#'
#' @examples load_metar()
load_metar = function(time = tool_date(end = as.Date(lubridate::with_tz(Sys.time(), tzone = "UTC")), by = "1 days", duration = "1 days"),
                      station = "VHHH", attempt = 50, decode = T, CAVOK = T){
  #Force time to be Dates ####
  time = as.Date(time)
  #Create dataframe #####
  source = "iowa"

  URL = tibble::tibble(time = time) %>%
    dplyr::mutate(Atime = time + lubridate::days(1)) %>%
    dplyr::mutate(Year = sprintf("%04d", as.numeric(lubridate::year(time))),
                  Month = sprintf("%02d", as.numeric(lubridate::month(time))),
                  Day = sprintf("%02d", as.numeric(lubridate::day(time))),
                  Date = paste0(Year,Month,Day),

                  AYear = sprintf("%04d", as.numeric(lubridate::year(Atime))),
                  AMonth = sprintf("%02d", as.numeric(lubridate::month(Atime))),
                  ADay = sprintf("%02d", as.numeric(lubridate::day(Atime))),
                  ADate = paste0(AYear,AMonth,ADay))
  if(source == "wyoming"){
    URL = dplyr::mutate(URL, URL = paste0("https://weather.uwyo.edu/cgi-bin/wyowx.fcgi?TYPE=metar&DATE=", ADate, "&HOUR=00&UNITS=M&STATION=", station))
  }
  if(source == "iowa"){
    URL = dplyr::mutate(URL, URL = paste0("https://mesonet.agron.iastate.edu/cgi-bin/request/asos.py?station=", station,"&data=metar&",
                                          "year1=", Year, "&month1=", Month, "&day1=", Day, "&",
                                          "year2=", Year, "&month2=", Month, "&day2=", Day, "&tz=Etc%2FUTC&format=onlycomma&latlon=no&elev=no&missing=empty&trace=T&direct=no&report_type=1&report_type=3&report_type=4"))
  }

  #Start reading #####
  data = "No data"
  data_final = tibble::tibble(station = NA_character_, time = as.POSIXct(NA), code = NA_character_) %>% tidyr::drop_na()

  ##Reading iowa#####
  cli::cli_text(cli::style_bold(cli::col_green("______Initiate download process______")))
  cli::cli_text("Info")
  cli::cli_alert_info("Start Time: {Sys.time()}")
  cli::cli_alert_info("Attempts: {nrow(URL)}")
  cli::cli_text(cli::style_bold(cli::col_grey("_____________________________________")))

  attp_sum = 0
  defaultW = getOption("warn")
  options(warn = -1)


     for(i in 1:nrow(URL)){
       read_attp = 0
       cli::cli_progress_step("Attempt for {URL$time[i]}: {read_attp}/{attempt}", spinner = T,
                              msg_failed = "Loading fail for {URL$time[i]}",
                              msg_done = "Attempt for {URL$time[i]}: {read_attp}/{attempt}")

       for(j in 1:attempt){
         read_attp = read_attp + 1
         cli::cli_progress_update()
         data = readr::read_csv(URL$URL[i], show_col_types = F, progress = F)
         if(tibble::is_tibble(data)){
           colnames(data) = c("station", "time", "code")
           data = dplyr::mutate(data, time = as.POSIXct(time, tz = "UTC"))
           data_final = dplyr::bind_rows(data_final, data)
           cli::cli_progress_done()
           attp_sum = attp_sum + read_attp
           read_attp = 0
           break
         }
         data = "No data"
       }
     }

  #Return #####
  data_final = dplyr::arrange(data_final, time) %>%
    dplyr::distinct()
  options(warn = defaultW)
  cli::cli_text(cli::style_bold(cli::col_red("__________Download complete__________")))
  cli::cli_text("Info")
  cli::cli_alert_info("End Time: {Sys.time()}")
  cli::cli_alert_info("Total attemps: {attp_sum}")

  ## Run decode if neccessary ####
  if(decode == T & (nrow(data_final) != 0)){
    data_final = weather2::tool_metar(data = data_final, code = code, CAVOK = CAVOK)
  }
  class(data_final) = "weather"
  data_final$type = "metar"
  return(data_final)
}
