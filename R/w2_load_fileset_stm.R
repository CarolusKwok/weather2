#' System tools: Download files from website using STORM mode
#'
#' @param data Data frame containing columns "URL", "DIR", "Info"
#' @param title Title of the downloaded data
#' @param subattempt Attempts to be made per download URL to be attempted.
#' @param worker Numbers of sessions to be open
#'
#' @return
#' @export
#'
#' @examples w2_load_fileset_stm(data, title = "Macao tidal data", subattempt = 10, worker = 10)
w2_load_fileset_stm = function(data, title, subattempt = 1, worker = 20){
  #Check ####
  if(!weather2::w2_check_internet(silent = T)){return(invisible())}
  if(weather2::w2_check_str(value = title, value_name = "title")){return(invisible())}
  if(weather2::w2_check_col_exist(data, Set, "data-Set")){return(invisible())}
  if(weather2::w2_check_col_exist(data, DIR, "data-DIR")){return(invisible())}
  if(weather2::w2_check_col_exist(data, URL, "data-URL")){return(invisible())}
  if(weather2::w2_check_col_exist(data, Info, "data-Info")){return(invisible())}
  if(weather2::w2_check_int(value = as.integer(subattempt), value_name = "subattempt")){return(invisible())}

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
      if(file.exists(read_DIR) & (file.info(read_DIR)$size > 10)){break}
    }
    return(i)
  }
  set_filter = function(data){
    sets = dplyr::select(data, Set)%>%
      dplyr::distinct() %>%
      .$Set %>%
      as.list()
    return(sets)
  }

  #Format the data ####
  data = dplyr::filter(data, !file.exists(DIR))
  data_f = data
  future::plan("future::multisession", workers = worker)
  sets = set_filter(data = data_f)
  list = furrr::future_map2(.x = list(data), .y = sets, .f = download_list)

  #Start download process
  file_f = tryCatch(basename(data_f$DIR[1]), error = function(e){"NA"})
  file_n = ifelse(nrow(data_f) == 0, "NA",
                  tryCatch(basename(data_f$DIR[nrow(data_f)]), error = function(e){"NA"}))

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
  j = 0
  if(nrow(data_f) != 0){
    cli::cli_progress_bar(format = "{cli::pb_spin} Loading data. Doing Attempt {j}", auto_terminate = F)
    for(j in 1:subattempt){
      cli::cli_progress_update(force = T)
      if(nrow(data_f) != 0){
        attp_temp = furrr::future_map(.x = list, .f = download_template)
        attp_sum  = attp_sum + sum(unlist(attp_temp))
        data_f = dplyr::filter(data_f, !file.exists(DIR))
        sets = set_filter(data = data_f)
        list = furrr::future_map2(.x = list(data_f), .y = sets, .f = download_list)
      }
    }
  }
  future::plan("future::sequential")
  options(warn = defaultW)
  #Return download process information ####
  success = dplyr::select(data, DIR) %>%
      dplyr::distinct() %>%
      dplyr::mutate(exist = file.exists(DIR)) %>%
      dplyr::filter(exist == T) %>%
      nrow()
  fail = dplyr::select(data, DIR) %>%
      dplyr::distinct() %>%
      dplyr::mutate(exist = file.exists(DIR)) %>%
      dplyr::filter(exist == F) %>%
      nrow()
  cli::cli_text(cli::style_bold(cli::col_red("________________Download complete________________")))
  cli::cli_text("Info")
  cli::cli_alert_info("  End Time: {Sys.time()}")
  cli::cli_alert_info("Subattempt: {attp_sum}")
  cli::cli_alert_info("   Success: {success}")
  cli::cli_alert_info("      Fail: {fail}")
  cli::cli_text("")
}
