#' System tools: Download files from website using STORM mode
#'
#' Warning: This is not a "checked" function. Your input, if incorrect, may damage your system.
#' Please use the w2_load_fileset() function instead.
#'
#' @param data Data frame containing columns "URL", "DIR", "Info", "Set"
#' @param title Title of the downloaded data
#' @param attempt Attempts to be made per download URL to be attempted.
#' @param worker Numbers of sessions to be open
#' @param threshold Threshold of file size, when compared to the largest item to be installed in the list
#' @param list_fail List failed-to-download items
#'
#' @return
#' @export
#'
#' @examples w2_load_fileset_stm(data, title = "Macao tidal data", attempt = 3, worker = 10, threshold = 0, list_fail = T)
#'
w2_load_fileset_stm = function(data, title = "test_set_stm", attempt = 5, worker = 20, threshold = 0.5, list_fail = T){
  if(!weather2::w2_check_internet()){return(invisible())}
  #Preset functions ####
  download_list = function(.x, .y){
    data_t = dplyr::filter(.x, Set == .y)
    list_t = list(data_t)
    return(list_t)
  }

  download_template = function(.x){
    data = .x[[1]]
    for(i in 1:nrow(data)){
      read_URL = data$URL[i]
      read_DIR = data$DIR[i]
      dir.create(path = dirname(read_DIR), showWarnings = F, recursive = T)
      tryCatch(download.file(url = read_URL, destfile = read_DIR, mode = "wb", quiet = T),
               error = function(e){})
      if(file.exists(read_DIR)){break}
    }
    return(i)
  }

  set_filter = function(data){
    sets = as.list(unique(data$Set))
    return(sets)
  }

  #Format the data ####
  data = dplyr::filter(weather2::w2_load_help_formatdata(data), exist == F)
  data_f = data
  sets = set_filter(data = data_f)

  if(length(sets) < worker){
    temp_work = length(sets)
    temp_work = ifelse(temp_work == 0, 1, temp_work)
  } else{temp_work = worker}
  if(future::nbrOfWorkers() != temp_work){future::plan("future::multisession", workers = temp_work)}

  future::plan("future::multisession", workers = temp_work)
  list = furrr::future_map2(.x = list(data), .y = sets, .f = download_list)
  file_f = tryCatch(basename(data_f$DIR[1]), error = function(e){"NA"})
  file_n = ifelse(nrow(data_f) == 0, "NA",
                  tryCatch(basename(data_f$DIR[nrow(data_f)]), error = function(e){"NA"}))

  #Start download process ####
  time_start = Sys.time()
  cli::cli_text(cli::style_bold(cli::col_green("____________Initiate download process____________")))
  cli::cli_text(cli::style_bold(cli::bg_br_yellow(cli::col_black("______________________STORM______________________"))))
  cli::cli_text("Info")
  cli::cli_alert_info("Downloading {.var {title}}")
  cli::cli_alert_info("Start Time: {Sys.time()}")
  cli::cli_alert_info("First File: {file_f}")
  cli::cli_alert_info("Last  File: {file_n}")
  cli::cli_alert_info("      Sets: {length(sets)}")
  cli::cli_text(cli::style_bold(cli::col_grey("_________________________________________________")))

  #Start download ####
  defaultW = getOption("warn")
  options(warn = -1)
  attp_sum = 0
  attp = 0
  if(nrow(data_f) != 0){
    cli::cli_progress_bar(format = "{cli::pb_spin} Loading data. Doing Attempt {attp}. Worker: {temp_work}", auto_terminate = F)
    for(attp in 1:attempt){
      if(length(sets) < worker){
        temp_work = length(sets)
        temp_work = ifelse(temp_work == 0, 1, temp_work)
      } else{temp_work = worker}
      if(future::nbrOfWorkers() != temp_work){future::plan("future::multisession", workers = temp_work)}

      cli::cli_progress_update(force = T)
      if(nrow(data_f) != 0){
        attp_temp = furrr::future_map(.x = list, .f = download_template)
        attp_sum  = attp_sum + sum(unlist(attp_temp))
        data = weather2::w2_load_help_formatdata(data)
        max_size = max(data$size, na.rm = T)
        data_f = dplyr::filter(data, size <= (max_size * threshold))
        sets = set_filter(data = data_f)
        list = furrr::future_map2(.x = list(data_f), .y = sets, .f = download_list)
      }
    }
  }
  future::plan("future::sequential")
  options(warn = defaultW)
  #Return download process information ####
  data = weather2::w2_load_help_formatdata(data)
  success = dplyr::select(data, DIR, Info, exist) %>%
    dplyr::distinct() %>%
    dplyr::filter(exist == T)
  fail = dplyr::select(data, DIR, Info, exist) %>%
    dplyr::distinct() %>%
    dplyr::filter(exist == F)
  time_end = Sys.time()
  time_diff= round(
    as.numeric(
      difftime(time_end, time_start, units = "secs")), digits = 3)
  cli::cli_text(cli::style_bold(cli::col_red("________________Download complete________________")))
  cli::cli_text("Info")
  cli::cli_alert_info("End Time: {Sys.time()}")
  cli::cli_alert_info("Eclipsed: {time_diff} sec")
  cli::cli_alert_info(" Attempt: {attp_sum}")
  cli::cli_alert_info(" Success: {nrow(success)}")
  cli::cli_alert_info("    Fail: {nrow(fail)}")
  if(list_fail){weather2::w2_load_help_listfail(fail$Info)}
  cli::cli_text("")
}
