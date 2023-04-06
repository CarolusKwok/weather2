#' System tools: Format data with exist and size column in any sys_load_... functions
#'
#' @param data Data frame containing columns "URL", "DIR", "Info"
#'
#' @keywords internal
#' @noRd
#'
#' @examples sys_load_formatdata(data)
sys_load_formatdata = function(data){
  data = dplyr::mutate(data,
                       exist = file.exists(DIR),
                       size = file.size(DIR),
                       size = ifelse(is.na(size), 0, size))
  return(data)
}

#' System tools: Print out the download initiation panel
#'
#' @param title Title of the downloaded items
#' @param time_start Start of time
#' @param data_1 The first item to be downloaded
#' @param data_n The last item to be downloaded
#' @param data_num Number of items to download
#' @param length The length of the panel to be drawn. Default as `51` characters
#'
#' @keywords internal
#' @noRd
#'
#' @examples sys_ldhp_panelstart("test", Sys.time(), "1", "n", 999)
sys_ldhp_panelstart = function(title, time_start, data_1, data_n, data_num, storm = F, length = 51){
  line_s1 = stringr::str_flatten(rep("_", floor((length - 25)/2)))
  line_s2 = stringr::str_flatten(rep("_", ceiling((length - 25)/2)))
  line_s = stringr::str_c(line_s1, "initiate download process", line_s2)
  line_e = stringr::str_flatten(rep("_", length))

  if(storm){
    line_m = stringr::str_c(stringr::str_flatten(rep("_", floor((length - 5)/2))),
                            "STORM",
                            stringr::str_flatten(rep("_", ceiling((length - 5)/2))))
    cli::cli_text(cli::style_bold(cli::bg_yellow(cli::col_br_black("{line_m}"))))
  }
  cli::cli_text(cli::style_bold(cli::col_green("{line_s}")))
  cli::cli_alert_info("Info")
  cli::cli_alert("Downloading {.var {title}}")
  cli::cli_alert("Start Time: {time_start}")
  cli::cli_alert("First Data: {data_1}")
  cli::cli_alert("Last  Data: {data_n}")
  cli::cli_alert("    Number: {data_num}")
  cli::cli_text(cli::style_bold(cli::col_grey("{line_e}")))
}




#' System tools: Print out the download complete panel
#'
#' @param time_start Time of the download process starts
#' @param time_end Time of the download process ends
#' @param attempts Total attempts made to download all the items
#' @param success Total number of success
#' @param fail Total number of fails
#' @param list_fail Should you list the failed-to-download items?
#' @param list_of_fail The list of failed to download items.
#' @param length The length of the panel to be drawn. Default as `51` characters
#'
#' @keywords internal
#' @noRd
#'
#' @examples sys_ldhp_panelend(Sys.time(), Sys.time(), 999, 0, 9, TRUE, "fail")
sys_ldhp_panelend = function(time_start, time_end, attempts, success, fail, list_fail, list_of_fail, length = 51){
  time_diff = round(as.numeric(difftime(time_end, time_start, units = "mins")), digits = 3)
  line_s1 = stringr::str_flatten(rep("_", floor((length - 17)/2)))
  line_s2 = stringr::str_flatten(rep("_", ceiling((length - 17)/2)))
  line_s = stringr::str_c(line_s1, "download complete", line_s2)


  cli::cli_text(cli::style_bold(cli::col_red("{line_s}")))
  cli::cli_alert_info("Info")
  cli::cli_alert("End Time: {time_end}")
  cli::cli_alert("Eclipsed: {time_diff} mins")
  cli::cli_alert(" Attempt: {attempts}")
  cli::cli_alert(" Success: {success}")
  cli::cli_alert("    Fail: {fail}")
  if(list_fail){
    num = length(list_of_fail)
    if(num > 0){
      failed_items = stringr::str_flatten(list_of_fail, collapse = ", ")
      cli::cli_alert_danger("Failed items as follow:")
      cli::cli_bullets(c(" " = "{failed_items}"))
    } else {
      cli::cli_alert_success("There are no failed items!")
    }
  }
  cli::cli_text("")
}
