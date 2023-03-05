#' Download atmospheric sounding data metrics
#'
#' Understanding the sounding is important for understanding the atmospheric condition, especially for extreme weather.
#' This catches the metrics provided by the University of Wyoming.
#'
#' @param time A list of time. If said time does not have a sounding, it will try to adjust to the last sounding possible. Only accepts POSIXct.
#' @param station Weather station code, according to the University of Wyoming website.
#' @param attempt Attempts of downloads to try per time. Default as 50.
#'
#' @return
#' @export
#'
#' @examples load_asnd_metr()

load_asnd_metr = function(time = tool_datetime(end = Sys.time(), duration = "7 days", by = "12 hours"), station = 45004, attempt = 50){
  #Create Dataframe
  URL = tibble::tibble(time = time) %>%
    dplyr::mutate(time_UTC = lubridate::with_tz(time, tzone = "UTC")) %>%
    dplyr::mutate(diff = lubridate::hour(time_UTC) %% 12) %>%
    dplyr::mutate(Ltime_UTC = time_UTC - lubridate::hours(diff)) %>%
    dplyr::mutate(Year = sprintf("%04d", as.numeric(lubridate::year(Ltime_UTC))),
                  Month = sprintf("%02d", as.numeric(lubridate::month(Ltime_UTC))),
                  DayHour = paste0(sprintf("%02d", as.numeric(lubridate::day(Ltime_UTC))),
                                   sprintf("%02d", as.numeric(lubridate::hour(Ltime_UTC))))) %>%
    dplyr::mutate(URL = paste0("https://weather.uwyo.edu/cgi-bin/sounding?region=seasia&TYPE=TEXT%3ALIST",
    "&YEAR=", Year, "&MONTH=", Month, "&FROM=", DayHour, "&TO=", DayHour,"&STNM=", station))

  #Start reading Wyoming
  data_final = tibble::tibble(station = NA_character_, time = as.POSIXct(NA_character_), lat = NA_real_, lon = NA_real_, ele = NA_real_,
                              metric = NA_character_, value = NA_real_) %>% tidyr::drop_na()
  cli::cli_text(cli::style_bold(cli::col_green("______Initiate download process______")))
  cli::cli_text("Info")
  cli::cli_alert_info("Starting Time: {Sys.time()}")
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


      if("   PRES   HGHT   TEMP   DWPT   RELH   MIXR   DRCT   SKNT   THTA   THTE   THTV" %in% line){
        read_line = F

        #Summary Processing
        start = which(line=="</PRE><H3>Station information and sounding indices</H3><PRE>", arr.ind = T)
        end = which(line=="<P>Description of the ", arr.ind = T)

        data1 = tibble::as_tibble(line) %>%
          dplyr::slice((start+1):(end-2)) %>%
          dplyr::mutate(split = as.numeric(gregexpr(":", value))) %>%
          dplyr::mutate(metric = trimws(substr(value, 1, split-1)),
                        value = trimws(substr(value, split+1, 999999))) %>%
          dplyr::select(-split)

        Station = dplyr::filter(data1, metric == "Station number") %>% dplyr::select(value) %>% as.character()
        Time = dplyr::filter(data1, metric == "Observation time") %>% dplyr::select(value) %>% as.character() %>% as.POSIXct(format = "%y%m%d/%H%M", tz = "UTC")
        Lat = dplyr::filter(data1, metric == "Station latitude") %>% dplyr::select(value) %>% as.numeric()
        Lon = dplyr::filter(data1, metric == "Station longitude") %>% dplyr::select(value) %>% as.numeric()
        Ele = dplyr::filter(data1, metric == "Station elevation") %>% dplyr::select(value) %>% as.numeric()

        data2 = dplyr::filter(data1, metric != "Station number" &
                               metric != "Observation time" &
                               metric != "Station latitude" &
                               metric != "Station longitude" &
                               metric != "Station elevation") %>%
          dplyr::mutate(value = as.numeric(value),
                        station = Station,
                        time = Time,
                        lat = Lat,
                        lon = Lon,
                        ele = Ele) %>%
          dplyr::relocate(station, time, lat, lon, ele, metric, value)

        data_final = dplyr::bind_rows(data_final, data2)
        cli::cli_progress_done()
      }
    }
    #Process fail and Restart
    if(read_attp > attempt){
      cli::cli_process_failed()
    }
    read_line = T
    attp_sum = attp_sum + read_attp
    read_attp = 0
  }
  options(warn = defaultW)
  cli::cli_text(cli::style_bold(cli::col_red("__________Download complete__________")))
  cli::cli_text("Info")
  cli::cli_alert_info("Ending Time: {Sys.time()}")
  cli::cli_alert_info("Attemps: {attp_sum}")


  data_final = data_final %>%
    dplyr::left_join(weather2::tool_asnd_dict()) %>%
    dplyr::mutate(metric = abbr) %>%
    dplyr::select(-abbr) %>%
    tidyr::pivot_wider(names_from = metric)

  data_final = list(data = data_final,
                    unit = tibble::tibble(show = "C",
                                          lift = "C",
                                          lifv = "C",
                                          #swet = "",
                                          kinx = "C",
                                          ctot = "C",
                                          vtot = "C",
                                          tott = "C",
                                          cape = "jkg",
                                          capv = "jkg",
                                          cins = "jkg",
                                          cinv = "jkg",
                                          #brch = "",
                                          #brcv = "",
                                          lclt = "K",
                                          lclp = "hPa",
                                          lcth = "K",
                                          mlth = "K",
                                          mlmr = "gkg",
                                          thtk = "m",
                                          pwat = "mm"))
  class(data_final) = "weather"
  return(data_final)
}
