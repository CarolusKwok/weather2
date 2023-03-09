#' System tool: Check if an object is a dataframe
#'
#' @param data data
#' @param data_name name of the data
#'
#' @return
#' @export
#'
#' @examples
w2_check_type_dataframe = function(data, data_name){
  flag = F
  if(!hasArg(data_name)){
    cli::cli_text("Error: {.var data_name} must be provided.")
    cli::cli_bullets(c("x" = "You supplied nothing!"))
    flag = T
  } else if(!hasArg(data)){
    cli::cli_text("Error: {.var {data_name}} must be provided.")
    cli::cli_bullets(c("x" = "You supplied nothing!"))
    flag = T
  } else if(!is.data.frame(data)){
    cli::cli_text("Error: {.var {data_name}} must be a dataframe.")
    cli::cli_bullets(c("x" = "You supplied {.var {class(data)}}"))
    flag = T
  }
  return(flag)
}
