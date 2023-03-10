#' System tools: Download files from website
#'
#' Warning: This is not a "checked" function. Your input, if incorrect, may damage your system.
#' Please use the w2_load_fileset() function instead.
#'
#' @param data Data frame containing columns "URL", "DIR", "Info", "Set"
#' @param title Title of the downloaded data
#' @param attempt Attempts to be made per download URL to be attempted.
#' @param threshold Threshold of file size, when compared to the largest item to be installed in the list
#' @param list_fail List failed-to-download items
#'
#' @return
#' @export
#'
#' @examples w2_load_fileset_seq(data, title = "Macao tidal data")
w2_load_fileset_seq = function(data, title = "test_set_seq", attempt = 5, threshold = 0.5, list_fail = T){
  if(!weather2::w2_check_internet()){return(invisible())}
  #Pre-set function ####
  format_sets = function(data){
    sets = unique(data$Set)
    return(sets)
  }

  format_info = function(data){
    info = unique(data$Info)
    return(info)
  }

  #Format the data ####
  data = weather2::w2_load_help_formatdata(data) %>%
    dplyr::filter(exist == F) %>%
    dplyr::distinct()
  sets = format_sets(data = data)
  info = format_info(data = data)
  file_f = tryCatch(basename(data$DIR[1]), error = function(e){"NA"})
  file_n = ifelse(nrow(data) == 0, "NA",
                  tryCatch(basename(data$DIR[nrow(data)]), error = function(e){"NA"}))

  #Start download process
  time_start = Sys.time()
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

  if(nrow(data) != 0){
    sattp = 0
    read = 1
    attp = 1
    read_set = sets[1]
    read_info = info[1]
    for(sattp in 1:attempt){
      cli::cli_alert_warning(text = "Loading data. Doing attempt {sattp}.")
      cli::cli_progress_bar(format = "{cli::pb_spin} Loading data. Doing {read}/{length(sets)}: {read_info}[{attp}]",
                            auto_terminate = T)
      if(length(sets) != 0){
        for(read in 1:length(sets)){
          read_set = sets[read]
          temp_data = dplyr::filter(data, Set == read_set)
          for(attp in 1:nrow(temp_data)){
            read_info= temp_data$Info[attp]
            read_DIR = temp_data$DIR[attp]
            read_URL = temp_data$URL[attp]
            cli::cli_progress_update(force = T)

            dir.create(path = dirname(read_DIR), showWarnings = F, recursive = T)
            tryCatch(download.file(url = read_URL, destfile = read_DIR, mode = "wb", quiet = T),
                     error = function(e){})
            if(file.exists(read_DIR)){break}
          }
          attp_sum = attp_sum + attp
        }
      }
      data = weather2::w2_load_help_formatdata(data)
      max_size = max(data$size, na.rm = T)
      sets = dplyr::filter(data, size <= (max_size * threshold)) %>% format_sets()
      info = dplyr::filter(data, size <= (max_size * threshold)) %>% format_info()
    }
  }
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
  cli::cli_alert_info("  End Time: {Sys.time()}")
  cli::cli_alert_info("  Eclipsed: {time_diff} sec")
  cli::cli_alert_info("   Attempt: {attp_sum}")
  cli::cli_alert_info("   Success: {nrow(success)}")
  cli::cli_alert_info("      Fail: {nrow(fail)}")
  if(list_fail){weather2::w2_load_help_listfail(fail$Info)}
  cli::cli_text("")
}
