#' System tools: Download files from website using STORM mode
#'
#' @param data Data frame containing columns "URL", "DIR", "Info"
#' @param title Title of the downloaded data
#' @param attempt Attempts to be made per download.
#' @param worker Numbers of sessions to be open
#'
#' @return
#' @export
#'
#' @examples w2_load_file_stm(data, title = "Global nc data", attempt = 10, worker = 10)
w2_load_file_stm = function(data, title = "test", attempt = 10, worker = 20){
  data = dplyr::mutate(data, exist = file.exists(DIR)) %>%
    dplyr::filter(exist == F) %>%
    dplyr::distinct()
  file_f = tryCatch(basename(data$DIR[1]), error = function(e){"NA"})
  file_n = ifelse(nrow(data) == 0, "NA",
                  tryCatch(basename(data$DIR[nrow(data)]), error = function(e){"NA"}))

  #Download panel ####
  cli::cli_text(cli::style_bold(cli::col_green("____________Initiate download process____________")))
  cli::cli_text(cli::style_bold(cli::bg_br_yellow(cli::col_black("______________________STORM______________________"))))
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
  read = 1

  download_template = function(.x, .y){
    dir.create(path = dirname(.y), showWarnings = F, recursive = T)
    tryCatch(download.file(url = .x, destfile = .y, mode = "wb", quiet = T), error = function(e){})
  }
  if(nrow(data) > 0){
    temp = data
    i = 1
    cli::cli_progress_bar(format = "{cli::pb_spin} Loading data. Doing Attempt {i}", auto_terminate = F)
    future::plan("future::multisession", workers = worker)
    for(i in 1:attempt){
      cli::cli_progress_update(force = T)
      temp = dplyr::mutate(temp, exist = file.exists(DIR)) %>% dplyr::filter(exist == F)
      attp_sum = attp_sum + nrow(temp)
      furrr::future_map2(.x = temp$URL, .y = temp$DIR, .f = download_template)
    }
    future::plan("future::sequential")
  }
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
}
