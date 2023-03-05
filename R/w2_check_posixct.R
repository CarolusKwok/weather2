#' System tool: Check if a value is a POSIXct
#'
#' @param value value
#' @param value_name name of the value
#'
#' @return
#' @export
#'
#' @examples w2_check_posixct(time, "time")
w2_check_posixct = function(value, value_name){
  flag = F
  if(!lubridate::is.POSIXct(value)){
    flag = T
    cli::cli_text('Error: {.var {value_name}} must be a POSIXct value.')
    cli::cli_bullets(c("x" = 'You supplied a {.var {class(value)}}'))
  }
  return(flag)
}
