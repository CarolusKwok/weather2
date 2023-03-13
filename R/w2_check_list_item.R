#' System tool: Check if an item is within a list
#'
#' @param item item
#' @param item_name name of the item
#' @param list list
#'
#' @return
#' @export
#'
#' @examples
w2_check_list_item = function(item, item_name, list){
  flag = F
  if(!hasArg(item)){
    cli::cli_text('Error: {.var item} must be supplied.')
    cli::cli_bullets(c("x" = 'You supplied nothing!'))
    flag = T
  } else if(!hasArg(item_name)){
    cli::cli_text('Error: {.var item_name} must be supplied.')
    cli::cli_bullets(c("x" = 'You supplied nothing!'))
    flag = T
  } else if(!hasArg(list)){
    cli::cli_text('Error: {.var list} must be supplied')
    cli::cli_bullets(c("x" = 'You supplied nothing!'))
    flag = T
  } else {
    if(!(item %in% list)){
      cli::cli_text('Error: {.var {item_name}} must be a certain value')
      cli::cli_bullets(c("x" = 'You supplied the wrong value!'))
      cli::cli_bullets(c("i" = 'It should be one of the following --- {list}!'))
      flag = T
    }
  }
  return(flag)
}
