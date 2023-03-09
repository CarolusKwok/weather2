#' System tool: Check if a list contains NA
#'
#' @param data data
#' @param data_name name of the data
#'
#' @return
#' @export
#'
#' @examples
w2_check_list_na = function(data, data_name){
  flag = F
  if(!hasArg(data)){
    cli::cli_text('Error: {.var data} must be supplied.')
    cli::cli_bullets(c("x" = 'You supplied nothing!'))
    flag = T
  } else if(!hasArg(data_name)){
    cli::cli_text('Error: {.var data_name} must be supplied.')
    cli::cli_bullets(c("x" = 'You supplied nothing!'))
    flag = T
  } else {
    if(anyNA(data)){
      cli::cli_text('Error: {.var {data_name}} must not contain NA.')
      cli::cli_bullets(c("x" = 'You supplied some NAs!'))
      flag = T
    }
  }
  return(flag)
}
