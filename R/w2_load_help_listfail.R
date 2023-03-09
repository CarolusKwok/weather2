#' System tools: Message failed items in any w2_load_... functions
#'
#' @param list_of_fail List of the failed items, in a character vector
#'
#' @return
#' @export
#'
#' @examples w2_load_help_listfail(fail$Info)
w2_load_help_listfail = function(list_of_fail){
  num = length(list_of_fail)
  if(num > 0){
    failed_items = stringr::str_flatten(list_of_fail, collapse = ", ")
    cli::cli_alert_danger("Failed items as follow:")
    cli::cli_bullets(c(" " = "{failed_items}"))
  } else {
    cli::cli_alert_success("There are no failed items!")
  }
}
