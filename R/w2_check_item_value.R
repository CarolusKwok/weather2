#' System tool: Check if a numeric value is equal to expected value
#'
#' @param item item
#' @param item_name name of item
#' @param expect expected value
#' @param type type
#'
#' @return
#' @export
#'
#' @examples
w2_check_item_value = function(item, item_name, expect, type = "="){
  flag = F
  if(!hasArg(item)){
    cli::cli_text('Error: {.var item} must be supplied.')
    cli::cli_bullets(c("x" = 'You supplied nothing!'))
    flag = T
  } else if(!hasArg(item_name)){
    cli::cli_text('Error: {.var item_name} must be supplied.')
    cli::cli_bullets(c("x" = 'You supplied nothing!'))
    flag = T
  } else if(!hasArg(expect)){
    cli::cli_text('Error: {.var expect} must be supplied')
    cli::cli_bullets(c("x" = 'You supplied nothing!'))
    flag = T
  } else if(weather2::w2_check_type_numeric(value = item, value_name = "item")){
    flag = T
  } else if(weather2::w2_check_type_numeric(value = expect, value_name = "expect")){
    flag = T
  } else if(weather2::w2_check_list_item(item = type, item_name = "type", list = c("=", "<", ">", "<=", ">="))){
    flag = T
  } else {
    if(!(item == expect) & type == "="){
      cli::cli_text('Error: {.var {item_name}} should be equal to an expected value.')
      cli::cli_bullets(c("x" = 'The expected value is {expect}.'))
      flag = T
    }
    if(!(item < expect) & type == "<"){
      cli::cli_text('Error: {.var {item_name}} should be smaller than an expected value.')
      cli::cli_bullets(c("x" = 'The expected value is {expect}.'))
      flag = T
    }
    if(!(item > expect) & type == ">"){
      cli::cli_text('Error: {.var {item_name}} should be larger than an expected value.')
      cli::cli_bullets(c("x" = 'The expected value is {expect}.'))
      flag = T
    }
    if(!(item <= expect) & type == "<="){
      cli::cli_text('Error: {.var {item_name}} should be smaller than or equal to an expected value.')
      cli::cli_bullets(c("x" = 'The expected value is {expect}.'))
      flag = T
    }
    if(!(item >= expect) & type == ">="){
      cli::cli_text('Error: {.var {item_name}} should be larger than or equal to an expected value.')
      cli::cli_bullets(c("x" = 'The expected value is {expect}.'))
      flag = T
    }
  }
  return(flag)
}
