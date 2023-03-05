#' System tools: Download files from website
#'
#' @param data Data frame containing columns "URL", "DIR", "Info"
#' @param title Title of the downloaded data
#' @param list_fail List failed-to-download items
#' @param attempt Attempts to be made per download.
#'
#' @return
#' @export
#'
#' @examples w2_load_file_seq(data, title = "Global nc data", attempt = 10)
w2_load_file_seq = function(data, title, list_fail = F, attempt = 10){
  #Check ####
  if(!weather2::w2_check_internet(silent = T)){return(invisible())}
  if(weather2::w2_check_str(value = title, value_name = "title")){return(invisible())}
  if(weather2::w2_check_col_exist(data, DIR, "data-DIR")){return(invisible())}
  if(weather2::w2_check_col_exist(data, URL, "data-URL")){return(invisible())}
  if(weather2::w2_check_col_exist(data, Info, "data-Info")){return(invisible())}
  if(weather2::w2_check_int(value = as.integer(attempt), value_name = "attempt")){return(invisible())}

  #Format download process information ####
  data = dplyr::mutate(data, exist = file.exists(DIR)) %>%
    dplyr::filter(exist == F) %>%
    dplyr::distinct()
  file_f = tryCatch(basename(data$DIR[1]), error = function(e){"NA"})
  file_n = ifelse(nrow(data) == 0, "NA",
                  tryCatch(basename(data$DIR[nrow(data)]), error = function(e){"NA"}))

  #Download panel ####
  cli::cli_text(cli::style_bold(cli::col_green("____________Initiate download process____________")))
  cli::cli_text("Info")
  cli::cli_alert_info("Downloading {.var {title}}")
  cli::cli_alert_info("Start Time: {Sys.time()}")
  cli::cli_alert_info("First File: {file_f}")
  cli::cli_alert_info("Last  File: {file_n}")
  cli::cli_alert_info("     Files: {nrow(data)}")
  cli::cli_text(cli::style_bold(cli::col_grey("_________________________________________________")))

  #Start to download ####
  defaultW = getOption("warn")
  options(warn = -1)
  attp_sum = 0
  attp_read = 0
  read = 1
  read_URL = data$URL[read]
  read_DIR = data$DIR[read]
  read_Info = data$Info[read]
  cli::cli_progress_bar(format = "{cli::pb_spin} Loading data. Doing {read}/{nrow(data)}: {read_Info}[{attp_read}]",
                        total = nrow(data), auto_terminate = F)
  if(nrow(data) != 0){
    for(read in 1:nrow(data)){
      attp_read = 0
      read_URL = data$URL[read]
      read_DIR = data$DIR[read]
      read_Info = data$Info[read]
      dir.create(path = dirname(read_DIR), showWarnings = F, recursive = T)
      for(attp_read in 1:attempt){
        cli::cli_progress_update(force = TRUE)
        tryCatch(download.file(url = read_URL, destfile = read_DIR, mode = "wb", quiet = T), error = function(e){})
        if(file.exists(read_DIR)){break}
      }
      if(!file.exists(read_DIR) & list_fail){
        cli::cli_alert_danger("[{read}] Fail to download {read_Info}")
      }
      attp_sum = attp_sum + attp_read
    }
  }
  options(warn = defaultW)

  #Return download process information ####
  success = dplyr::mutate(data, exist = file.exists(DIR)) %>%
    dplyr::filter(exist == T) %>%
    nrow()
  fail = dplyr::mutate(data, exist = file.exists(DIR)) %>%
    dplyr::filter(exist == F) %>%
    nrow()
  cli::cli_text(cli::style_bold(cli::col_red("________________Download complete________________")))
  cli::cli_text("Info")
  cli::cli_alert_info("End Time: {Sys.time()}")
  cli::cli_alert_info(" Attemps: {attp_sum}")
  cli::cli_alert_info(" Success: {success}")
  cli::cli_alert_info("    Fail: {fail}")
  cli::cli_text("")
}
