#' System tool: Check if a list can be turned into a numeric
#'
#' @param data data
#' @param data_name name of the data
#'
#' @return
#' @export
#'
#' @examples
w2_check_list_numericable = function(data, data_name){
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
    data = data[!is.na(data)]
    data = as.numeric(data)
    if(anyNA(data)){
      cli::cli_text('Error: {.var {data_name}} must be numeric-able.')
      cli::cli_bullets(c("x" = 'You supplied some values that can not be numerics!'))
      flag = T
    }
  }
  return(flag)
}
