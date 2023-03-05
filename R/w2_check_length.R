#' System tool: Check if a value has a certain length
#'
#' @param value value
#' @param length expected length of the value
#' @param value_name name of the value
#'
#' @return
#' @export
#'
#' @examples w2_check_length(c("sec", "min"), 1, "by")
w2_check_length = function(value, length, value_name){
  flag = F
  if(length(value) != length){
    flag = T
    cli::cli_text('Error: {.var {value_name}} can only be {length} item long')
    cli::cli_bullets(c("x" = 'You supplied a {.var {length(value)}}.'))
  }
  return(flag)
}
