#' System tools: Download files from website
#'
#' @param data Data frame containing columns "URL", "DIR", "Info"
#' @param title Title of the downloaded data
#' @param list_fail List failed-to-download items
#' @param attempt Attempts to be made per download.
#' @param worker Numbers of sessions to be open
#' @param threshold Threshold of file size, when compared to the largest item to be installed in the list
#' @param check Check the all of the parameters before execution, default as T
#'
#' @return
#' @export
#'
#' @examples sys_load_file(data, title = "Global nc data", attempt = 10, worker = 10, list_fail = T, threshold = 0.5)
sys_load_file = function(data, title = "test", attempt = 5, worker = 0, list_fail = T, threshold = 0.5, check = T){
  #Format download process information and CHECK ####
  worker = as.integer(worker)
  attempt = as.integer(attempt)
  if(check != F){
    if(weather2::sys_ckf_SysLoadFile(data = data,
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
    weather2:::sys_load_fileSEQ(data = data,
                                 title = title,
                                 list_fail = list_fail,
                                 attempt = attempt,
                                 threshold = threshold)
  } else {
    weather2:::sys_load_fileSTM(data = data,
                                 title = title,
                                 attempt = attempt,
                                 worker = worker,
                                 list_fail = list_fail,
                                 threshold = threshold)
  }
}

#' System tools: Download files from website
#'
#' Warning: This is not a "checked" function. Your input, if incorrect, may damage your system.
#' Please use the sys_load_file() function instead.
#'
#' @param data Data frame containing columns "URL", "DIR", "Info"
#' @param title Title of the downloaded data
#' @param attempt Attempts to be made per download.
#' @param threshold Threshold of file size, when compared to the largest item to be installed in the list
#' @param list_fail List failed-to-download items
#'
#' @keywords internal
#'
#' @examples sys_load_fileSEQ(data, title = "Global nc data")
sys_load_fileSEQ = function(data, title = "test_seq", attempt = 5, threshold = 0.5, list_fail = T){
  if(!weather2::sys_ck_internet()){return(invisible())}

  #Format download process information ####
  data = weather2:::sys_load_formatdata(data) %>%
    dplyr::filter(exist == F) %>%
    dplyr::distinct()
  file_f = tryCatch(basename(data$DIR[1]), error = function(e){"NA"})
  file_n = ifelse(nrow(data) == 0, "NA",
                  tryCatch(basename(data$DIR[nrow(data)]), error = function(e){"NA"}))

  #Download panel ####
  time_start = Sys.time()
  weather2:::sys_ldhp_panelstart(title = title,
                                 time_start = time_start,
                                 data_1 = file_f,
                                 data_n = file_n,
                                 data_num = nrow(data))

  #Start to download ####
  defaultW = getOption("warn")
  options(warn = -1)
  attp_sum = 0
  max_size = 0

  if(nrow(data) != 0){
    temp_data = data
    read = 1
    read_URL = temp_data$URL[1]
    read_DIR = temp_data$DIR[1]
    read_Info = temp_data$Info[1]
    for(i in 1:attempt){
      cli::cli_alert_warning(text = "Loading data. Doing attempt {i}.")
      if(nrow(temp_data) != 0){
        cli::cli_progress_bar(format = "{cli::pb_spin} Loading data. Doing {read}/{nrow(temp_data)}: {read_Info}",
                              total = nrow(temp_data), auto_terminate = T)
        for(read in 1:nrow(temp_data)){
          read_URL = temp_data$URL[read]
          read_DIR = temp_data$DIR[read]
          read_Info = temp_data$Info[read]
          cli::cli_progress_update(force = T)

          dir.create(path = dirname(read_DIR), showWarnings = F, recursive = T)
          tryCatch(download.file(url = read_URL,
                                 destfile = read_DIR,
                                 mode = "wb", quiet = T),
                   error = function(e){})
        }
        attp_sum = attp_sum + read
      }
      data = weather2:::sys_load_formatdata(data)
      max_size = max(c(max_size, data$size), na.rm = T)
      temp_data = dplyr::filter(data, size <= (max_size * threshold))
    }
  }
  options(warn = defaultW)

  #Return download process information ####
  data = weather2:::sys_load_formatdata(data)
  success = dplyr::filter(data, exist == T)
  fail    = dplyr::filter(data, exist == F)
  time_end = Sys.time()
  time_diff= round(as.numeric(difftime(time_end, time_start, units = "secs")), digits = 3)

  weather2:::sys_ldhp_panelend(time_start = time_start,
                               time_end = time_end,
                               attempts = attp_sum,
                               success = nrow(success),
                               fail = nrow(fail),
                               list_fail = list_fail,
                               list_of_fail = fail$Info)
}

#' System tools: Download files from website using STORM mode
#'
#' Warning: This is not a "checked" function. Your input, if incorrect, may damage your system.
#' Please use the sys_load_file() function instead.
#'
#' @param data Data frame containing columns "URL", "DIR", "Info"
#' @param title Title of the downloaded data
#' @param attempt Attempts to be made per download.
#' @param worker Numbers of sessions to be open
#' @param threshold Threshold of file size, when compared to the largest item to be installed in the list
#' @param list_fail List failed-to-download items
#'
#' @keywords internal
#'
#' @examples sys_load_fileSTM(data, title = "Global nc data")
sys_load_fileSTM = function(data, title = "test_stm", attempt = 5, worker = 20, threshold = 0.5, list_fail = T){
  if(!weather2::sys_ck_internet()){return(invisible())}

  #Pre-set function ####
  download_template = function(.x, .y){
    dir.create(path = dirname(.y), showWarnings = F, recursive = T)
    tryCatch(download.file(url = .x, destfile = .y, mode = "wb", quiet = T), error = function(e){})
  }

  #Format data ####
  data = weather2:::sys_load_formatdata(data) %>%
    dplyr::filter(exist == F) %>%
    dplyr::distinct()
  file_f = tryCatch(basename(data$DIR[1]), error = function(e){"NA"})
  file_n = ifelse(nrow(data) == 0, "NA",
                  tryCatch(basename(data$DIR[nrow(data)]), error = function(e){"NA"}))

  #Download panel ####
  time_start = Sys.time()
  weather2:::sys_ldhp_panelstart(title = title,
                                 time_start = time_start,
                                 data_1 = file_f,
                                 data_n = file_n,
                                 data_num = nrow(data),
                                 storm = T)

  #Start to download ####
  defaultW = getOption("warn")
  options(warn = -1)
  max_size = 0
  attp_sum = 0

  if(nrow(data) > 0){
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
      furrr::future_map2(.x = temp_data$URL, .y = temp_data$DIR, .f = download_template)

      data = weather2:::sys_load_formatdata(data)
      max_size = max(max_size, data$size, na.rm = T)
      temp_data = dplyr::filter(data, size <= (max_size * threshold))
    }
    future::plan("future::sequential")
  }
  options(warn = defaultW)
  #Return download process information ####
  data = weather2:::sys_load_formatdata(data)
  success = dplyr::filter(data, exist == T)
  fail    = dplyr::filter(data, exist == F)
  time_end = Sys.time()
  weather2:::sys_ldhp_panelend(time_start = time_start,
                               time_end = time_end,
                               attempts = attp_sum,
                               success = nrow(success),
                               fail = nrow(fail),
                               list_fail = list_fail,
                               list_of_fail = fail$Info)
}
