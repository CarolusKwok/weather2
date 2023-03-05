#' System tool: Check if a value is a integer
#'
#' @param value value
#' @param value_name name of the value
#'
#' @return
#' @export
#'
#' @examples w2_check_int(attempt, "attempt")
w2_check_int = function(value, value_name){
  flag = F
  if(!is.integer(value)){
    flag = T
    cli::cli_text('Error: {.var {value_name}} can only be an integer.')
    cli::cli_bullets(c("x" = 'You supplied {.var {class(value)}} values.'))
  }
  return(flag)
}
