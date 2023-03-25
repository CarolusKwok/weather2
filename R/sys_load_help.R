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

#' System tools: Message failed items in any sys_load_... functions
#'
#' @param list_of_fail List of the failed items, in a character vector
#'
#' @keywords internal
#' @noRd
#'
#' @examples sys_load_listfail(fail$Info)
sys_load_listfail = function(list_of_fail){
  num = length(list_of_fail)
  if(num > 0){
    failed_items = stringr::str_flatten(list_of_fail, collapse = ", ")
    cli::cli_alert_danger("Failed items as follow:")
    cli::cli_bullets(c(" " = "{failed_items}"))
  } else {
    cli::cli_alert_success("There are no failed items!")
  }
}
