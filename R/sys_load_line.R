
#' System tools: Download lines from website
#'
#' @param data Data frame containing columns `URL` and `Info`.
#' @param title Title of the downloaded data
#' @param attempt Attempts to be made per download URL to be attempted. Default as a maximum of `5` times.
#' @param worker Numbers of sessions to be open.
#' @param list_fail List failed-to-download items. Default as `TRUE`.
#' @param threshold Threshold of line size. Any downloaded lines with number less than the threshold will be considered as failure and will be reattempted. Default as a minimum of `1` line.
#' @param check Check the all of the parameters before execution. Default as `TRUE`.
#'
#' @return
#' @export
#'
#' @examples sys_load_line(data, title = "Wyoming Atmospheric Sounding Data")
sys_load_line = function(data, title = "test", attempt = 5, worker = 0, list_fail = T, threshold = 1, check = T){
  #Format download process information and CHECK ####
  worker = as.integer(worker)
  attempt = as.integer(attempt)
  if(check){
    if(weather2::sys_ckf_SysLoadLine(data = data,
                                     title = title,
                                     attempt = attempt,
                                     worker = worker,
                                     list_fail = list_fail,
                                     threshold = threshold)){return(invisible())}
  }

  #Start ####
  if(worker <= 0){
    return(data)
  } else if(worker == 1){
    weather2:::sys_load_lineSEQ(data = data,
                                title = title,
                                list_fail = list_fail,
                                attempt = attempt,
                                threshold = threshold)
  } else {
    weather2:::sys_load_lineSTM(data = data,
                                title = title,
                                attempt = attempt,
                                worker = worker,
                                list_fail = list_fail,
                                threshold = threshold)
  }
}






#' System tools: Download lines from website
#'
#' __Warning: This is not a "checked" function. Your input, if incorrect, may damage your system.__
#' __Please use the sys_load_line() function instead.__
#'
#' @param data Data frame containing columns `URL` and `Info`.
#' @param title Title of the downloaded data
#' @param attempt Attempts to be made per download URL to be attempted. Default as a maximum of `5` times.
#' @param threshold Threshold of line size. Any downloaded lines with number less than the threshold will be considered as failure and will be reattempted. Default as a minimum of `1` line.
#' @param list_fail List failed-to-download items. Default as `TRUE`.
#'
#' @keywords internal
#'
#' @examples sys_load_lineSEQ(data, title = "Wyoming Atmospheric Sounding Data")
sys_load_lineSEQ = function(data, title = "test_seq", attempt = 5, threshold = 1, list_fail = T){
  if(!weather2::sys_ck_internet()){return(invisible())}
  #Format download process information ####
  data = dplyr::distinct(data) %>%
    dplyr::mutate(loc = 1:dplyr::n(),
                  RUN = T)
  line_f = tryCatch((data$Info[1]), error = function(e){"NA"})
  line_n = ifelse(nrow(data) == 0, "NA",
                  tryCatch((data$Info[nrow(data)]), error = function(e){"NA"}))

  #Download panel ####
  time_start = Sys.time()
  weather2:::sys_ldhp_panelstart(title = title,
                                 time_start = time_start,
                                 data_1 = line_f,
                                 data_n = line_n,
                                 data_num = nrow(data))

  #Start to download ####
  defaultW = getOption("warn")
  options(warn = -1)
  attp_sum = 0
  max_size = 0

  if(nrow(data) != 0){
    ## Initiate a list of X slots ####
    list = as.list(data$Info)
    list = lapply(list, FUN = function(x){return(list(Info = x,
                                                      Line = ""))})
    names(list) = data$Info
    ## Copy data start downloading ####
    temp_data = data
    read = 1
    read_URL = temp_data$URL[1]
    read_Info = temp_data$Info[1]
    for(i in 1:attempt){
      cli::cli_alert_warning(text = "Loading data. Doing attempt {i}.")
      if(nrow(temp_data) != 0){
        cli::cli_progress_bar(format = "{cli::pb_spin} Loading data. Doing {read}/{nrow(temp_data)}: {read_Info}",
                              total = nrow(temp_data), auto_terminate = T)
        for(read in 1:nrow(temp_data)){
          read_URL = temp_data$URL[read]
          read_Info = temp_data$Info[read]
          read_loc = temp_data$loc[read]
          lines = tryCatch(readLines(read_URL), error = function(e){"ReadLines Unsuccessful"}) %>% tibble::as_tibble()
          list[[read]]$Line = lines
          if(lines$value[1] == "ReadLines Unsuccessful" | nrow(lines) <= threshold){
            data$RUN[read_loc] = T
          } else {
            data$RUN[read_loc] = F
          }
          cli::cli_progress_update(force = T)
        }
        temp_data = dplyr::filter(data, RUN == T)
        attp_sum = attp_sum + read
      }
    }
  }
  options(warn = defaultW)
  #Check the downloaded data ####
  check = function(data, threshold){
    Line = data$Line

    if(nrow(Line) <= threshold){
      return(NULL)
    } else {
      return(data)
    }
  }
  list = lapply(list, check, threshold = threshold)

  #Return download process information ####
  success = length(list)
  fail    = nrow(data) - success
  time_end = Sys.time()
  list_of_fail = dplyr::filter(data, !(Info %in% names(list)))$Info
  weather2:::sys_ldhp_panelend(time_start = time_start,
                               time_end = time_end,
                               attempts = attp_sum,
                               success = success,
                               fail = fail,
                               list_fail = list_fail,
                               list_of_fail = list_of_fail)
  return(list)
}



#' System tools: Download lines from website
#'
#' __Warning: This is not a "checked" function. Your input, if incorrect, may damage your system.__
#' __Please use the sys_load_line() function instead.__
#'
#' @param data Data frame containing columns `URL` and `Info`.
#' @param title Title of the downloaded data.
#' @param attempt Attempts to be made per download URL to be attempted. Default as a maximum of `5` times.
#' @param worker Numbers of sessions to be open.
#' @param threshold Threshold of line size. Any downloaded lines with number less than the threshold will be considered as failure and will be reattempted. Default as a minimum of `1` line.
#' @param list_fail List failed-to-download items. Default as `TRUE`.
#'
#' @keywords internal
#'
#' @examples sys_load_lineSEQ(data, title = "Wyoming Atmospheric Sounding Data")
sys_load_lineSTM = function(data, title = "test_stm", attempt = 5, worker = 20, threshold = 1, list_fail = T){
  if(!weather2::sys_ck_internet()){return(invisible())}
  #Pre-set function ####
  download_template = function(.x, threshold){
    Stat = .x$Stat
    if(Stat){return(.x)}
    Line = tryCatch(readLines(.x$URL), error = function(e){"ReadLines Unsuccessful"}) %>%
      tibble::as_tibble()
    Stat = (nrow(Line) > threshold)
    return(list(Info = .x$Info,
                URL = .x$URL,
                Line = Line,
                Stat = Stat))
  }
  #Format download process information ####
  data = dplyr::distinct(data) %>% dplyr::mutate(loc = 1:dplyr::n())
  line_f = tryCatch(data$Info[1], error = function(e){"NA"})
  line_n = ifelse(nrow(data) == 0, "NA",
                  tryCatch(data$Info[nrow(data)], error = function(e){"NA"}))

  #Download panel ####
  time_start = Sys.time()
  weather2:::sys_ldhp_panelstart(title = title,
                                 time_start = time_start,
                                 data_1 = line_f,
                                 data_n = line_n,
                                 data_num = nrow(data),
                                 storm = T)
  #Start to download ####
  defaultW = getOption("warn")
  options(warn = -1)
  max_size = 0
  attp_sum = 0

  if(nrow(data) > 0){
    ## Initiate a list of X slots ####
    magic = function(loc, data){
      data = data[loc,]
      return(list(Info = data$Info,
                  URL = data$URL,
                  Line = "",
                  Stat = F))
    }
    list = as.list(data$loc)
    list = lapply(list, FUN = magic, data = data)
    names(list) = data$Info
    ## Copy data start downloading ####
    temp_data = data
    temp_work = worker
    i = 1
    cli::cli_progress_bar(format = "{cli::pb_spin} Loading data. Doing Attempt {i}. Worker: {temp_work}", auto_terminate = F)
    future::plan("future::multisession", workers = temp_work)
    for(i in 1:attempt){
      if(nrow(temp_data) < worker){
        temp_work = nrow(temp_data)
        temp_work = ifelse(temp_work == 0, 1, temp_work)
      } else {temp_work = worker}
      if(future::nbrOfWorkers() != temp_work){future::plan("future::multisession", workers = temp_work)}
      cli::cli_progress_update(force = T)

      attp_sum = attp_sum + nrow(temp_data)
      list = furrr::future_map(.x = list, .f = download_template, threshold = 1)
    }
    future::plan("future::sequential")
  }
  options(warn = defaultW)

  #Check the downloaed data ####
  check = function(data, threshold){
    Line = data$Line %>% tibble::as_tibble()
    Stat = data$Stat
    if((tryCatch(nrow(Line), error = function(e){return(0)}) <= threshold) | (Stat == F)){
      return(NULL)
    } else {
      return(list(Info = data$Info,
                  URL = data$URL,
                  Line = data$Line))
    }
  }
  list = lapply(list, check, threshold = threshold)
  list = list[!sapply(list, is.null)]
  #Return download process information ####
  success = length(list)
  fail    = nrow(data) - success
  time_end = Sys.time()
  list_of_fail = dplyr::filter(data, !(Info %in% names(list)))$Info
  weather2:::sys_ldhp_panelend(time_start = time_start,
                               time_end = time_end,
                               attempts = attp_sum,
                               success = success,
                               fail = fail,
                               list_fail = list_fail,
                               list_of_fail = list_of_fail)
  return(list)
}
