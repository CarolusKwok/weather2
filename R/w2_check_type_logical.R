#' System tool: Check if an object is a logical
#'
#' @param value value
#' @param value_name name of the value
#'
#' @return
#' @export
#'
#' @examples
w2_check_type_logical = function(value, value_name){
  flag = F
  if(!hasArg(value)){
    cli::cli_text('Error: {.var value} must be supplied.')
    cli::cli_bullets(c("x" = 'You supplied nothing!'))
    flag = T
  } else if(!hasArg(value_name)){
    cli::cli_text('Error: {.var value_name} must be supplied.')
    cli::cli_bullets(c("x" = 'You supplied nothing!'))
    flag = T
  } else {
    if(!is.logical(value)){
      cli::cli_text('Error: {.var {value_name}} must be a logical')
      cli::cli_bullets(c("x" = 'You supplied a {.var {class(value)}}!'))
      flag = T
    }
  }
  return(flag)
}
