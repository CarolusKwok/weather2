#' System tool: Check if a value is a language flag
#'
#' @param value value
#' @param value_name name of the value
#'
#' @return
#' @export
#'
#' @examples w2_check_lan(lan, "lan")
w2_check_lan = function(value, value_name){
  flag = F
  if(length(value) != 1){
    flag = T
    cli::cli_text('Error: {.var {value_name}} can only have 1 value.')
    cli::cli_bullets(c("x" = 'You supplied {length(value)} values.'))
  }
  if(!(value %in% c("en", "tc", "sc"))){
    flag = T
    cli::cli_text('Error: {.var {value_name}} must be "en", "tc", or "sc".')
    cli::cli_bullets(c("x" = 'You supplied {.var {value}}.'))
  }
  return(flag)
}
