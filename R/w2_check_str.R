#' System tool: Check if a value is a string
#'
#' @param value value
#' @param value_name name of the value
#'
#' @return
#' @export
#'
#' @examples w2_check_str(title, "title")
w2_check_str = function(value, value_name){
  flag = F
  if(!is.character(value)){
    flag = T
    cli::cli_text('Error: {.var {value_name}} can only be a string.')
    cli::cli_bullets(c("x" = 'You supplied a {.var {class(value)}}.'))
  }
  return(flag)
}
