#' System tool: Checks the input of all w2_check_type_xxx function
#'
#' @param value
#' @param value_name
#'
#' @return
#' @export
#'
#' @examples
w2_check_type_help = function(value, value_name){
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

#' System tool: Check if an object is a character
#'
#' @param value value
#' @param value_name name of the value
#'
#' @return
#' @export
#'
#' @examples
w2_check_type_character = function(value, value_name){
  if(weather2::w2_check_type_help(value = value, value_name = value_name)){return(T)}
  if(!is.character(value)){
    cli::cli_text('Error: {.var {value_name}} must be a character')
    cli::cli_bullets(c("x" = 'You supplied a {.var {class(value)}}!'))
    return(T)
  }
  return(F)
}

#' System tool: Check if an object is a dataframe
#'
#' @param value value
#' @param value_name name of the value
#'
#' @return
#' @export
#'
#' @examples
w2_check_type_dataframe = function(value, value_name){
  if(weather2::w2_check_type_help(value = value, value_name = value_name)){return(T)}
  if(!is.data.frame(value)){
    cli::cli_text("Error: {.var {value_name}} must be a dataframe.")
    cli::cli_bullets(c("x" = "You supplied {.var {class(value)}}"))
    return(T)
  }
  return(F)
}

#' System tool: Check if an object is an integer
#'
#' @param value value
#' @param value_name name of the value
#'
#' @return
#' @export
#'
#' @examples
w2_check_type_integer = function(value, value_name){
  if(weather2::w2_check_type_help(value = value, value_name = value_name)){return(T)}
  if(!is.integer(value)){
    cli::cli_text('Error: {.var {value_name}} must be an integer')
    cli::cli_bullets(c("x" = 'You supplied a {.var {class(value)}}!'))
    return(T)
  }
  return(F)
}

#' System tool: Check if an object is a logical
#'
#' @param value value
#' @param value_name name of the value
#'
#' @return
#' @export
#'
#' @examples
w2_check_type_logical = function(value, value_name){
  if(weather2::w2_check_type_help(value = value, value_name = value_name)){return(T)}
  if(!is.logical(value)){
    cli::cli_text('Error: {.var {value_name}} must be a logical')
    cli::cli_bullets(c("x" = 'You supplied a {.var {class(value)}}!'))
    return(T)
  }
  return(F)
}

#' System tool: Check if an object is a numeric
#'
#' @param value value
#' @param value_name name of the value
#'
#' @return
#' @export
#'
#' @examples
w2_check_type_numeric = function(value, value_name){
  if(weather2::w2_check_type_help(value = value, value_name = value_name)){return(T)}
  if(!is.numeric(value)){
    cli::cli_text('Error: {.var {value_name}} must be a numeric')
    cli::cli_bullets(c("x" = 'You supplied a {.var {class(value)}}!'))
    return(T)
  }
  return(F)
}


#' System tool: Check if an object is a POSIXct
#'
#' @param value value
#' @param value_name name of the value
#'
#' @return
#' @export
#'
#' @examples
w2_check_type_POSIXct = function(value, value_name){
  if(weather2::w2_check_type_help(value = value, value_name = value_name)){return(T)}
  if(!lubridate::is.POSIXct(value)){
    cli::cli_text('Error: {.var {value_name}} must be a POSIXct')
    cli::cli_bullets(c("x" = 'You supplied a {.var {class(value)}}!'))
    return(T)
  }
  return(F)
}
