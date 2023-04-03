#' Download atmospheric sounding data
#'
#' Understanding the sounding is important for understanding the atmospheric condition, especially for extreme weather.
#' This catches the sounding data provided by the University of Wyoming.
#'
#' @param time A list of time. If said time does not have a sounding, it will try to adjust to the last sounding possible. Only accepts POSIXct.
#' @param station Weather station code, according to the University of Wyoming website.
#' @param frost Include frost point calculation in dataframe. Only accepts logical value.
#' @param attempt Attempts of downloads to try per time. Default as 50.
#'
#' @return
#' @export
#'
#' @examples load_asnd()

load_asnd = function(time = Sys.time(), station = 45004, frost = T, attempt = 50){
  #Create filter by frost #####
  filter_line = ifelse(frost == T,
                       "   PRES   HGHT   TEMP   DWPT   FRPT   RELH   RELI   MIXR   DRCT   SKNT   THTA   THTE   THTV",
                       "   PRES   HGHT   TEMP   DWPT   RELH   MIXR   DRCT   SKNT   THTA   THTE   THTV")

  #Create Dataframe #####
  URL = tibble::tibble(time = time) %>%
    dplyr::mutate(time_UTC = lubridate::with_tz(time, tzone = "UTC")) %>%
    dplyr::mutate(diff = lubridate::hour(time_UTC) %% 12) %>%
    dplyr::mutate(Ltime_UTC = time_UTC - lubridate::hours(diff)) %>%
    dplyr::mutate(Year = sprintf("%04d", as.numeric(lubridate::year(Ltime_UTC))),
                  Month = sprintf("%02d", as.numeric(lubridate::month(Ltime_UTC))),
                  DayHour = paste0(sprintf("%02d", as.numeric(lubridate::day(Ltime_UTC))),
                                   sprintf("%02d", as.numeric(lubridate::hour(Ltime_UTC))))) %>%
    dplyr::mutate(URL = paste0("https://weather.uwyo.edu/cgi-bin/sounding?region=seasia&TYPE=TEXT%3ALIST",
                               "&YEAR=", Year, "&MONTH=", Month, "&FROM=", DayHour, "&TO=", DayHour,"&STNM=", station,
                               "&ICE=", as.numeric(frost)))

  #Start reading Wyoming #####
  data_final = tibble::tibble(station = NA_character_, time = as.POSIXct(NA_character_), lat = NA_real_, lon = NA_real_, ele = NA_real_,
                              pres = NA_real_, hght = NA_real_, temp = NA_real_, dwpt = NA_real_, frpt = NA_real_,
                              relh = NA_real_, reli = NA_real_, mixr = NA_real_, drct = NA_real_, sped = NA_real_,
                              thta = NA_real_, thte = NA_real_, thtv = NA_real_) %>% tidyr::drop_na()
  cli::cli_text(cli::style_bold(cli::col_green("______Initiate download process______")))
  cli::cli_text("Info")
  cli::cli_alert_info("Start Time: {Sys.time()}")
  cli::cli_alert_info("Attempts: {nrow(URL)}")
  cli::cli_text(cli::style_bold(cli::col_grey("_____________________________________")))

  attp_sum = 0
  defaultW = getOption("warn")
  options(warn = -1)
  for(i in 1:nrow(URL)){
    read_line = T
    read_attp = 0
    line = NA

    cli::cli_progress_step("Attempt for {URL$time_UTC[i]}: {read_attp}/{attempt}", spinner = T,
                           msg_failed = "Loading fail for {URL$time_UTC[i]}",
                           msg_done = "Attempt for {URL$time_UTC[i]}: {read_attp}/{attempt}")
    while(read_line == T & read_attp <= attempt){
      read_attp = read_attp + 1
      cli::cli_progress_update()
      line = tryCatch(readLines(URL$URL[i]), error = function(e){NA})

      if(filter_line %in% line){
        read_line = F

        #Summary processing
        start = which(line=="</PRE><H3>Station information and sounding indices</H3><PRE>", arr.ind = T)
        end = which(line=="<P>Description of the ", arr.ind = T)
        meta_data = tibble::as_tibble(line) %>%
          dplyr::slice((start+1):(end-2)) %>%
          dplyr::mutate(value = trimws(value),
                        split = as.numeric(gregexpr(":", value))-1,
                        para = substr(value, 1, split),
                        value = (substr(value, split+2, 999999))) %>%
          dplyr::select(-split) %>%
          dplyr::filter(para == "Station number"    |
                        para == "Observation time"  |
                        para == "Station latitude"  |
                        para == "Station longitude" |
                        para == "Station elevation")

        Station = dplyr::filter(meta_data, para == "Station number") %>% dplyr::select(value) %>% trimws()
        Time = dplyr::filter(meta_data, para == "Observation time") %>% dplyr::select(value) %>% trimws() %>% as.POSIXct(tz = "UTC", format = "%y%m%d/%H%M")
        Lat = dplyr::filter(meta_data, para == "Station latitude") %>% dplyr::select(value) %>% trimws()
        Lon = dplyr::filter(meta_data, para == "Station longitude") %>% dplyr::select(value) %>% trimws()
        Ele = dplyr::filter(meta_data, para == "Station elevation") %>% dplyr::select(value) %>% trimws()

        start = which(line == filter_line, arr.ind = T)
        end = which(line=="</PRE><H3>Station information and sounding indices</H3><PRE>", arr.ind= T)

        data_1 = dplyr::slice(tibble::as_tibble(line), (start+3):(end-1))
        if(frost == T){
          data_2 = readr::read_fwf(file = I(data_1$value), show_col_types = F,
                                   readr::fwf_empty(file = I(data_1$value),
                                                    col_names = c("pres", "hght", "temp", "dwpt", "frpt", "relh", "reli", "mixr", "drct", "sped", "thta", "thte", "thtv")))
        }
        if(frost == F){
          data_2 = readr::read_fwf(file = I(data_1$value), show_col_types = F,
                                   readr::fwf_empty(file = I(data_1$value),
                                                    col_names = c("pres", "hght", "temp", "dwpt", "relh", "mixr", "drct", "sped", "thta", "thte", "thtv")))
        }
        data_2 = dplyr::mutate(data_2, station = Station,
                                       time = Time,
                                       lat = as.numeric(Lat),
                                       lon = as.numeric(Lon),
                                       ele = as.numeric(Ele)) %>%
          dplyr::relocate(station, time, lat, lon, ele)
        data_final = dplyr::bind_rows(data_final, data_2)
        cli::cli_progress_done()
      }
    }
    if(read_attp > attempt){
      cli::cli_process_failed()
    }
    read_line = T
    attp_sum = attp_sum + read_attp
    read_attp = 0
  }
  data_final = dplyr::mutate(data_final,
                  uwnd = -sped * sin(drct * pi / 180),
                  vwnd = -sped * cos(drct * pi / 180))

  options(warn = defaultW)
  cli::cli_text(cli::style_bold(cli::col_red("__________Download complete__________")))
  cli::cli_text("Info")
  cli::cli_alert_info("End Time: {Sys.time()}")
  cli::cli_alert_info("Total attemps: {attp_sum}")
  return(data_final)
}
load_asnd(Sys.time())
