#' System tool: Check if a list contains NA
#'
#' @param data data
#' @param data_name name of the data
#' @param NAs check if it should contains NAs(T) or not contain NAs(F)
#'
#' @return
#' @export
#'
#' @examples
w2_check_list_na = function(data, data_name, NAs = F){
  flag = F
  if(!hasArg(data)){
    cli::cli_text('Error: {.var data} must be supplied.')
    cli::cli_bullets(c("x" = 'You supplied nothing!'))
    flag = T
  } else if(!hasArg(data_name)){
    cli::cli_text('Error: {.var data_name} must be supplied.')
    cli::cli_bullets(c("x" = 'You supplied nothing!'))
    flag = T
  } else if(!hasArg(NAs)){
    cli::cli_text('Error: {.var NAs} must be supplied')
    cli::cli_bullets(c("x" = 'You supplied nothing!'))
    flag = T
  } else if(weather2::w2_check_type_logical(NAs, "NAs")){
    flag = T
  } else {
    if(anyNA(data) & NAs == F){
      cli::cli_text('Error: {.var {data_name}} must not contain NA.')
      cli::cli_bullets(c("x" = 'You supplied some NAs!'))
      flag = T
    }
    if(!anyNA(data) & NAs == T){
      cli::cli_text('Error: {.var {data_name}} must contain NA.')
      cli::cli_bullets(c("x" = 'You supplied no NAs!'))
      flag = T
    }
  }
  return(flag)
}
