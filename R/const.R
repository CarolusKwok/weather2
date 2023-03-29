#' Calling a scientific constant
#'
#' @param name Name of the constant
#' @param value Value of the constant
#' @param unit Unit of the constant
#' @param silent Should a message be returned when calling the constant? Default as `FALSE`.
#'
#' @keywords internal
#'
#' @examples const("standard acceleration of gravity", 9.80665, "m s^-2")
const = function(name,
                 value,
                 unit,
                 silent = F){
  #Check ####
  if(weather2::sys_ckc_character(value = name, value_name = "name")){return(invisible())}
  if(weather2::sys_ckc_numeric(value = value, value_name = "value")){return(invisible())}
  if(weather2::sys_ckc_character(value = unit, value_name = "unit")){return(invisible())}
  if(weather2::sys_ckc_logical(value = silent, value_name = "silent")){return(invisible())}

  #Work
  if(!silent){
    cli::cli_alert_info("Constant called:")
    cli::cli_bullets(text = c(" " = "Name : {.var {name}}",
                              " " = "Value: {.var {value}}",
                              " " = "Unit : {.var {unit}}"))
  }
  return(value)
}




