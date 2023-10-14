#' System tool: check if a parameter in the function has an argument
#'
#' Note: Does not work with a default value
#'
#'
#' @param value value
#' @param value_name value_name
#'
#' @noRd
#'
#' @examples sys_hp_hasArg(1, "obj")
sys_hp_hasArg = function(value, value_name){
  if(!hasArg(value_name)){
    cli::cli_text('Error: {.var value_name} must be supplied.')
    cli::cli_bullets(c("x" = 'You supplied nothing!'))
    return(T)
  }
  if(!hasArg(value)){
    cli::cli_text('Error: {.var {value_name}} must be supplied.')
    cli::cli_bullets(c("x" = 'You supplied nothing!'))
    return(T)
  }
  return(F)
}

#' System tool: Returns a string value from symbols
#'
#' @param x value
#'
#' @noRd
#'
#' @examples sys_hp_sym2chr(hiiiii)
sys_hp_sym2chr = function(x){
  if(weather2:::sys_hp_hasArg({{x}}, "x")){return()}
  return(rlang::as_name(rlang::quo({{x}})))
}





#' System tools: Review the data, and pass it on.
#'
#' @param x
#'
#' @noRd
#'
#' @examples sys_hp_review(x)
sys_hp_review = function(x){
  print(x)
  return(x)
}
