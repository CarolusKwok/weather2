#' System tool: Check if a column exists in a dataframe
#'
#' @param data data
#' @param data_name name of the data
#' @param value value
#' @param value_name name of the value
#'
#' @return
#' @export
#'
#' @examples
w2_check_col_exist = function(data, data_name, value, value_name){
  flag = F
  if(!hasArg(value)){
    cli::cli_text("Error: {.var value} must be provided.")
    cli::cli_bullets(c("x" = "You supplied nothing!"))
    flag = T
  } else if(!hasArg(value_name)){
    cli::cli_text("Error: {.var value_name} must be provided.")
    cli::cli_bullets(c("x" = "You supplied nothing!"))
    flag = T
  } else if(!hasArg(data)){
    cli::cli_text("Error: {.var data} must be provided.")
    cli::cli_bullets(c("x" = "You supplied nothing!"))
    flag = T
  } else if(!hasArg(data_name)){
    cli::cli_text("Error: {.var data_name} must be provided.")
    cli::cli_bullets(c("x" = "You supplied nothing!"))
    flag = T
  } else {
    check = tryCatch(dplyr::select(data, {{value}}), error = function(e){"ERROR"})
    if(!is.data.frame(check)){
      cli::cli_text("Error: {.var {value_name}} must be a column in {.var {data_name}}.")
      cli::cli_bullets(c("x" = "The column does not exist!"))
      flag = T
    }
  }
  return(flag)
}
