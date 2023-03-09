#' System tool: Check if a list are all unique items
#'
#' @param data data
#' @param data_name name of the data
#'
#' @return
#' @export
#'
#' @examples
w2_check_list_unique = function(data, data_name){
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
    length_data = length(data)
    length_unique = length(unique(data))
    if(length_data != length_unique){
      cli::cli_text('Error: All items in {.var {data_name}} must not unique.')
      cli::cli_bullets(c("x" = 'You supplied some non-unique items!'))
      flag = T
    }
  }
  return(flag)
}
