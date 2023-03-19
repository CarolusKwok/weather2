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
#' @examples sys.load_file(data, title = "Global nc data", attempt = 10, worker = 10, list_fail = T, threshold = 0.5)
sys.load_file = function(data, title = "test", attempt = 5, worker = 0, list_fail = T, threshold = 0.5, check = T){
  #Format download process information and CHECK ####
  worker = as.integer(worker)
  attempt = as.integer(attempt)
  if(check != F){
    if(weather2::sys.cf_sys.load_file(data = data,
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
    weather2:::sys.load_file.seq(data = data,
                                 title = title,
                                 list_fail = list_fail,
                                 attempt = attempt,
                                 threshold = threshold)
  } else {
    weather2:::sys.load_file.stm(data = data,
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
#' Please use the sys.load_file() function instead.
#'
#' @param data Data frame containing columns "URL", "DIR", "Info"
#' @param title Title of the downloaded data
#' @param attempt Attempts to be made per download.
#' @param threshold Threshold of file size, when compared to the largest item to be installed in the list
#' @param list_fail List failed-to-download items
#'
#' @keywords internal
#'
#' @examples sys.load_file.seq(data, title = "Global nc data")
sys.load_file.seq = function(data, title = "test_seq", attempt = 5, threshold = 0.5, list_fail = T){
  if(!weather2::sys.ck.internet()){return(invisible())}

  #Format download process information ####
  data = weather2:::sys.load.formatdata(data) %>%
    dplyr::filter(exist == F) %>%
    dplyr::distinct()
  file_f = tryCatch(basename(data$DIR[1]), error = function(e){"NA"})
  file_n = ifelse(nrow(data) == 0, "NA",
                  tryCatch(basename(data$DIR[nrow(data)]), error = function(e){"NA"}))

  #Download panel ####
  time_start = Sys.time()
  cli::cli_text(cli::style_bold(cli::col_green("____________Initiate download process____________")))
  cli::cli_text("Info")
  cli::cli_alert_info("Downloading {.var {title}}")
  cli::cli_alert_info("Start Time: {time_start}")
  cli::cli_alert_info("First File: {file_f}")
  cli::cli_alert_info("Last  File: {file_n}")
  cli::cli_alert_info("     Files: {nrow(data)}")
  cli::cli_text(cli::style_bold(cli::col_grey("_________________________________________________")))

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
      data = weather2:::sys.load.formatdata(data)
      max_size = max(c(max_size, data$size), na.rm = T)
      temp_data = dplyr::filter(data, size <= (max_size * threshold))
    }
  }
  options(warn = defaultW)

  #Return download process information ####
  data = weather2:::sys.load.formatdata(data)
  success = dplyr::filter(data, exist == T)
  fail    = dplyr::filter(data, exist == F)
  time_end = Sys.time()
  time_diff= round(as.numeric(difftime(time_end, time_start, units = "secs")), digits = 3)
  cli::cli_text(cli::style_bold(cli::col_red("________________Download complete________________")))
  cli::cli_text("Info")
  cli::cli_alert_info("End Time: {time_end}")
  cli::cli_alert_info("Eclipsed: {time_diff} sec")
  cli::cli_alert_info(" Attemps: {attp_sum}")
  cli::cli_alert_info(" Success: {nrow(success)}")
  cli::cli_alert_info("    Fail: {nrow(fail)}")
  if(list_fail){weather2:::sys.load.listfail(fail$Info)}
  cli::cli_text("")
}

#' System tools: Download files from website using STORM mode
#'
#' Warning: This is not a "checked" function. Your input, if incorrect, may damage your system.
#' Please use the sys.load_file() function instead.
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
#' @examples sys.load_file.stm(data, title = "Global nc data")
sys.load_file.stm = function(data, title = "test_stm", attempt = 5, worker = 20, threshold = 0.5, list_fail = T){
  if(!weather2::sys.ck.internet()){return(invisible())}

  #Pre-set function ####
  download_template = function(.x, .y){
    dir.create(path = dirname(.y), showWarnings = F, recursive = T)
    tryCatch(download.file(url = .x, destfile = .y, mode = "wb", quiet = T), error = function(e){})
  }

  #Format data ####
  data = weather2:::sys.load.formatdata(data) %>%
    dplyr::filter(exist == F) %>%
    dplyr::distinct()
  file_f = tryCatch(basename(data$DIR[1]), error = function(e){"NA"})
  file_n = ifelse(nrow(data) == 0, "NA",
                  tryCatch(basename(data$DIR[nrow(data)]), error = function(e){"NA"}))

  #Download panel ####
  time_start = Sys.time()
  cli::cli_text(cli::style_bold(cli::col_green("____________Initiate download process____________")))
  cli::cli_text(cli::style_bold(cli::bg_br_yellow(cli::col_black("______________________STORM______________________"))))
  cli::cli_text("Info")
  cli::cli_alert_info("Downloading {.var {title}}")
  cli::cli_alert_info("Start Time: {time_start}")
  cli::cli_alert_info("First File: {file_f}")
  cli::cli_alert_info("Last  File: {file_n}")
  cli::cli_alert_info("     Files: {nrow(data)}")
  cli::cli_text(cli::style_bold(cli::col_grey("_________________________________________________")))

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

      data = weather2:::sys.load.formatdata(data)
      max_size = max(max_size, data$size, na.rm = T)
      temp_data = dplyr::filter(data, size <= (max_size * threshold))
    }
    future::plan("future::sequential")
  }
  options(warn = defaultW)
  #Return download process information ####
  data = weather2:::sys.load.formatdata(data)
  success = dplyr::filter(data, exist == T)
  fail    = dplyr::filter(data, exist == F)
  time_end = Sys.time()
  time_diff= round(
    as.numeric(
      difftime(time_end, time_start, units = "secs")), digits = 3)
  cli::cli_text(cli::style_bold(cli::col_red("________________Download complete________________")))
  cli::cli_text("Info")
  cli::cli_alert_info("End Time: {time_end}")
  cli::cli_alert_info("Eclipsed: {time_diff} sec")
  cli::cli_alert_info(" Attemps: {attp_sum}")
  cli::cli_alert_info(" Success: {nrow(success)}")
  cli::cli_alert_info("    Fail: {nrow(fail)}")
  if(list_fail){weather2:::sys.load.listfail(fail$Info)}
  cli::cli_text("")
}
