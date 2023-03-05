#' System tools: Download files from website
#'
#' @param data Data frame containing columns "URL", "DIR", "Info"
#' @param title Title of the downloaded data
#' @param subattempt Attempts to be made per download URL to be attempted.
#'
#' @return
#' @export
#'
#' @examples w2_load_fileset_seq(data, title = "Macao tidal data", subattempt = 10)
w2_load_fileset_seq = function(data, title, subattempt = 1){
  #Check ####
  if(!weather2::w2_check_internet(silent = T)){return(invisible())}
  if(weather2::w2_check_str(value = title, value_name = "title")){return(invisible())}
  if(weather2::w2_check_col_exist(data, Set, "data-Set")){return(invisible())}
  if(weather2::w2_check_col_exist(data, DIR, "data-DIR")){return(invisible())}
  if(weather2::w2_check_col_exist(data, URL, "data-URL")){return(invisible())}
  if(weather2::w2_check_col_exist(data, Info, "data-Info")){return(invisible())}
  if(weather2::w2_check_int(value = as.integer(subattempt), value_name = "subattempt")){return(invisible())}

  #Format the data ####
  data = dplyr::filter(data, !file.exists(DIR))
  sets = dplyr::select(data, Set) %>% dplyr::distinct() %>% .$Set
  info = dplyr::select(data, Info) %>% dplyr::distinct() %>% .$Info

  #Start download process
  file_f = tryCatch(basename(data$DIR[1]), error = function(e){"NA"})
  file_n = ifelse(nrow(data) == 0, "NA",
                  tryCatch(basename(data$DIR[nrow(data)]), error = function(e){"NA"}))

  cli::cli_text(cli::style_bold(cli::col_green("____________Initiate download process____________")))
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
  sattp = 0
  read = 1
  read_set = sets[read]
  read_info = info[read]

  if(nrow(data) != 0){
    cli::cli_progress_bar(format = "{cli::pb_spin} Loading data. Doing {read}/{length(sets)}: {read_info}[{attp};{sattp}]",
                          auto_terminate = F)
    cli::cli_progress_update(force = T)

    for(read in 1:length(sets)){
      read_set = sets[read]
      read_info = info[read]
      data2 = dplyr::filter(data, Set == read_set)

      read_DIR = data2$DIR[1]
      dir.create(path = dirname(read_DIR), showWarnings = F, recursive = T)
      for (attp in 1:nrow(data2)){
        read_URL = data2$URL[attp]

        for (sattp in 1:subattempt){
          cli::cli_progress_update(force = T)
          tryCatch(download.file(url = read_URL, destfile = read_DIR, mode = "wb", quiet = T),
                   error = function(e){})
          if(file.exists(read_DIR) & (file.info(read_DIR)$size > 10)){break}
        }
        if(file.exists(read_DIR) & (file.info(read_DIR)$size > 10)){break}
      }
      attp_sum = attp_sum + (attp * subattempt - 1)
      attp = 0
      if(!file.exists(read_DIR)){cli::cli_alert_danger("Fail to download {read_info}")}
    }
  }
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
