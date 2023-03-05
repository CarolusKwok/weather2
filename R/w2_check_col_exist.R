#' System tool: Check if an object is a column name under data
#'
#' @param data Data
#' @param value Value of the check item
#' @param value_name Value's name as a string
#'
#' @return
#' @export
#'
#' @examples w2_check_col_exist(data, info, "info")
w2_check_col_exist = function(data, value, value_name){
  flag = F
  if(!weather2::w2_col_exist(data, {{value}})){
    cli::cli_text("Error: {.var {value_name}} must be exist within data.")
    cli::cli_bullets(c("x" = 'The column name is not found in the dataframe.'))
    flag = T
  }
  return(flag)
}
