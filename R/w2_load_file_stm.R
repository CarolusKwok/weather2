#' System tools: Download files from website using STORM mode
#'
#' @param data Data frame containing columns "URL", "DIR", "Info"
#' @param title Title of the downloaded data
#' @param attempt Attempts to be made per download.
#' @param worker Numbers of sessions to be open
#' @param threshold Threshold of file size, when compared to the largest item to be installed in the list
#' @param list_fail List failed-to-download items
#'
#' @return
#' @export
#'
#' @examples w2_load_file_stm(data, title = "Global nc data", attempt = 10, worker = 10)
w2_load_file_stm = function(data, title = "test", attempt = 10, worker = 20, threshold = 0.5, list_fail = F){
  #Pre-set function ####
  download_template = function(.x, .y){
    dir.create(path = dirname(.y), showWarnings = F, recursive = T)
    tryCatch(download.file(url = .x, destfile = .y, mode = "wb", quiet = T), error = function(e){})
  }

  format_data = function(data){
    data = dplyr::mutate(data,
                         exist = file.exists(DIR),
                         size = file.size(DIR),
                         size = ifelse(is.na(size), 0, size))
    return(data)
  }

  #Format data ####
  data = format_data(data) %>%
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

      data = format_data(data)
      max_size = max(max_size, data$size, na.rm = T)
      temp_data = dplyr::filter(data, size <= (max_size * 0.5))
    }
    future::plan("future::sequential")
  }
  options(warn = defaultW)
  #Return download process information ####
  data = format_data(data)
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
  if(list_fail){
    failed_items = fail$Info %>%
      stringr::str_flatten(collapse = ", ")
    cli::cli_alert_info("Failed Items as follow:")
    cli::cli_bullets(c(" " = "{failed_items}"))
  }
  cli::cli_text("")
}
