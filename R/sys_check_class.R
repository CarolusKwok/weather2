#' System tool: Checks the class of an object
#'
#' @param value Value of the object
#' @param value_name Name of the object, in character
#' @param class Expected class of an object, in character and can be a vector.
#' @param all Should all the values be tested in a list/ vector? Default as True
#' @param silent Should a text message be shown if the class of an object is incorrect? Default as False.
#'
#' @return
#' @export
#'
#' @examples sys.ck.class(1, "name", "numeric")
sys.ck.class = function(value, value_name, class, all = T, silent = F){
  #Checks ####
  if(weather2:::sys.ck.help_hasArg(value = value_name, value_name = "value_name")){return(T)}
  if(weather2:::sys.ck.help_hasArg(value = value, value_name = {{value_name}})){return(T)}
  if(weather2:::sys.ck.help_hasArg(value = class, value_name = "class")){return(T)}
  if(!is.character(class)){
    cli::cli_text('Error: {.var class} must be a {.var character}')
    cli::cli_bullets(c("x" = 'You supplied {.var {class(class)}}!'))
    return(T)
  }
  if(!is.logical(silent)){
    cli::cli_text('Error: Items in {.var silent} must be a {.var logical}')
    cli::cli_bullets(c("x" = 'You supplied the wrong thing!'))
    return(T)
  }
  if(!is.logical(all)){
    cli::cli_text('Error: Items in {.var all} must be a {.var logical}')
    cli::cli_bullets(c("x" = 'You supplied the wrong thing!'))
    return(T)
  }
  #Actually checking the values class#####
  if(all){
    class_value = lapply(X = value, FUN = inherits, class)
    if(sum(unlist(class_value)) != length(class_value)){
      if(!silent){
        cli::cli_text('Error: Items in {.var {value_name}} must be a {.var {class}}')
        cli::cli_bullets(c("x" = 'You supplied the wrong thing!'))
      }
      return(T)
    }
  } else {
    class_value = inherits(x = value, what = class)
    if(!class_value){
      if(!silent){
        cli::cli_text('Error: Items in {.var {value_name}} must be a {.var {class}}')
        cli::cli_bullets(c("x" = 'You supplied the wrong thing!'))
      }
      return(T)
    }
  }
  return(F)
}

#' System tool: Checks an object is a character
#'
#' A wrapper around `sys.ck.class`
#'
#' @param value Value of the object
#' @param value_name Name of the object, in character
#' @param class Expected class of an object, in character and can be a vector.
#' @param all Should all the values be tested in a list/ vector? Default as True
#' @param silent Should a text message be shown if the class of an object is incorrect? Default as False.
#'
#' @return
#' @export
#'
#' @examples sys.ck.class_character("text", "obj")
sys.ck.class_character = function(value, value_name, all = T, silent = F){
  weather2::sys.ck.class(value = value,
                         value_name = value_name,
                         class = "character",
                         all = all,
                         silent = silent)
}


#' System tool: Checks an object is a data.frame
#'
#' A wrapper around `sys.ck.class`
#'
#' @param value Value of the object
#' @param value_name Name of the object, in character
#' @param class Expected class of an object, in character and can be a vector.
#' @param all Should all the values be tested in a list/ vector? Default as True
#' @param silent Should a text message be shown if the class of an object is incorrect? Default as False.
#'
#' @return
#' @export
#'
#' @examples sys.ck.class_data.frame(data.frame(), "obj")
sys.ck.class_data.frame = function(value, value_name, all = T, silent = F){
  weather2::sys.ck.class(value = value,
                         value_name = value_name,
                         class = "data.frame",
                         all = all,
                         silent = silent)
}


#' System tool: Checks an object is an integer
#'
#' A wrapper around `sys.ck.class`
#'
#' @param value Value of the object
#' @param value_name Name of the object, in character
#' @param class Expected class of an object, in character and can be a vector.
#' @param all Should all the values be tested in a list/ vector? Default as True
#' @param silent Should a text message be shown if the class of an object is incorrect? Default as False.
#'
#' @return
#' @export
#'
#' @examples sys.ck.class_integer(1L, "obj")
sys.ck.class_integer = function(value, value_name, all = T, silent = F){
  weather2::sys.ck.class(value = value,
                         value_name = value_name,
                         class = "integer",
                         all = all,
                         silent = silent)
}

#' System tool: Checks an object is a logical
#'
#' A wrapper around `sys.ck.class`
#'
#' @param value Value of the object
#' @param value_name Name of the object, in character
#' @param class Expected class of an object, in character and can be a vector.
#' @param all Should all the values be tested in a list/ vector? Default as True
#' @param silent Should a text message be shown if the class of an object is incorrect? Default as False.
#'
#' @return
#' @export
#'
#' @examples sys.ck.class_logical(TRUE, "obj")
sys.ck.class_logical = function(value, value_name, all = T, silent = F){
  weather2::sys.ck.class(value = value,
                         value_name = value_name,
                         class = "logical",
                         all = all,
                         silent = silent)
}


#' System tool: Checks an object is a numeric
#'
#' A wrapper around `sys.ck.class`
#'
#' @param value Value of the object
#' @param value_name Name of the object, in character
#' @param class Expected class of an object, in character and can be a vector.
#' @param all Should all the values be tested in a list/ vector? Default as True
#' @param silent Should a text message be shown if the class of an object is incorrect? Default as False.
#'
#' @return
#' @export
#'
#' @examples sys.ck.class_numeric(1, "obj")
sys.ck.class_numeric = function(value, value_name, all = T, silent = F){
  weather2::sys.ck.class(value = value,
                         value_name = value_name,
                         class = "numeric",
                         all = all,
                         silent = silent)
}


#' System tool: Checks an object is a POSIXct
#'
#' A wrapper around `sys.ck.class`
#'
#' @param value Value of the object
#' @param value_name Name of the object, in character
#' @param class Expected class of an object, in character and can be a vector.
#' @param all Should all the values be tested in a list/ vector? Default as True
#' @param silent Should a text message be shown if the class of an object is incorrect? Default as False.
#'
#' @return
#' @export
#'
#' @examples sys.ck.class_POSIXct(ISOdatetime(2023, 01, 01, 00, 00, 00), "obj")
sys.ck.class_POSIXct = function(value, value_name, all = T, silent = F){
  weather2::sys.ck.class(value = value,
                         value_name = value_name,
                         class = "POSIXct",
                         all = all,
                         silent = silent)
}
