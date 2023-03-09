#' System tools: Download files from website
#'
#' Warning: This is not a "checked" function. Your input, if incorrect, may damage your system.
#' Please use the w2_load_file() function instead.
#'
#' @param data Data frame containing columns "URL", "DIR", "Info"
#' @param title Title of the downloaded data
#' @param attempt Attempts to be made per download.
#' @param threshold Threshold of file size, when compared to the largest item to be installed in the list
#' @param list_fail List failed-to-download items
#'
#' @return
#' @export
#'
#' @examples w2_load_file_seq(data, title = "Global nc data")
w2_load_file_seq = function(data, title = "test_seq", attempt = 5, threshold = 0.5, list_fail = T){
  #Format download process information ####
  data = weather2::w2_load_help_formatdata(data) %>%
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
      data = weather2::w2_load_help_formatdata(data)
      max_size = max(c(max_size, data$size), na.rm = T)
      temp_data = dplyr::filter(data, size <= (max_size * threshold))
    }
  }
  options(warn = defaultW)

  #Return download process information ####
  data = weather2::w2_load_help_formatdata(data)
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
  if(list_fail){weather2::w2_load_help_listfail(fail$Info)}
  cli::cli_text("")
}
