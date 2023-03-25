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
#' @examples sys_ckc(1, "name", "numeric")
sys_ckc = function(value, value_name, class, all = T, silent = F){
  #Checks ####
  if(weather2:::sys_hp_hasArg(value = value_name, value_name = "value_name")){return(T)}
  if(weather2:::sys_hp_hasArg(value = value, value_name = {{value_name}})){return(T)}
  if(weather2:::sys_hp_hasArg(value = class, value_name = "class")){return(T)}
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
    if(!("list" %in% class(value))){
      value = list(a = value)
    }
    class_value = lapply(X = value, FUN = inherits, class)
    if(sum(unlist(class_value)) != length(class_value)){
      if(!silent){
        classes = lapply(X = value, FUN = base::class)
        cli::cli_text('Error: Items in {.var {value_name}} must be a {.var {class}}')
        cli::cli_bullets(c("x" = 'You supplied {.var {unique(classes)}}!'))
      }
      return(T)
    }
  } else {
    class_value = inherits(x = value, what = class)
    if(!class_value){
      if(!silent){
        cli::cli_text('Error: Items in {.var {value_name}} must be a {.var {class}}')
        cli::cli_bullets(c("x" = 'You supplied {.var {class(value)}}'))
      }
      return(T)
    }
  }
  return(F)
}

#' System tool: Checks an object is a numeric or an integer
#'
#' A wrapper around `sys_ckc`
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
#' @examples sys_ckc_numint(list(10, 20, 30L, 40L), "obj") #Return `FALSE`
sys_ckc_numint = function(value, value_name, all = T, silent = F){
  weather2::sys_ckc(value = value,
                         value_name = value_name,
                         class = c("numeric", "integer"),
                         all = all,
                         silent = silent)
}


#' System tool: Checks an object is a character
#'
#' A wrapper around `sys_ckc`
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
#' @examples sys_ckc_character("text", "obj")
sys_ckc_character = function(value, value_name, all = T, silent = F){
  weather2::sys_ckc(value = value,
                         value_name = value_name,
                         class = "character",
                         all = all,
                         silent = silent)
}


#' System tool: Checks an object is a data.frame
#'
#' A wrapper around `sys_ckc`
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
#' @examples sys_ckc_dataframe(data.frame(), "obj")
sys_ckc_dataframe = function(value, value_name, all = T, silent = F){
  weather2::sys_ckc(value = value,
                         value_name = value_name,
                         class = "data.frame",
                         all = all,
                         silent = silent)
}


#' System tool: Checks an object is an integer
#'
#' A wrapper around `sys_ckc`
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
#' @examples sys_ckc_integer(1L, "obj")
sys_ckc_integer = function(value, value_name, all = T, silent = F){
  weather2::sys_ckc(value = value,
                         value_name = value_name,
                         class = "integer",
                         all = all,
                         silent = silent)
}

#' System tool: Checks an object is a logical
#'
#' A wrapper around `sys_ckc`
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
#' @examples sys_ckc_logical(TRUE, "obj")
sys_ckc_logical = function(value, value_name, all = T, silent = F){
  weather2::sys_ckc(value = value,
                         value_name = value_name,
                         class = "logical",
                         all = all,
                         silent = silent)
}


#' System tool: Checks an object is a numeric
#'
#' A wrapper around `sys_ckc`
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
#' @examples sys_ckc_numeric(1, "obj")
sys_ckc_numeric = function(value, value_name, all = T, silent = F){
  weather2::sys_ckc(value = value,
                         value_name = value_name,
                         class = "numeric",
                         all = all,
                         silent = silent)
}


#' System tool: Checks an object is a POSIXct
#'
#' A wrapper around `sys_ckc`
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
#' @examples sys_ckc_POSIXct(ISOdatetime(2023, 01, 01, 00, 00, 00), "obj")
sys_ckc_POSIXct = function(value, value_name, all = T, silent = F){
  weather2::sys_ckc(value = value,
                         value_name = value_name,
                         class = "POSIXct",
                         all = all,
                         silent = silent)
}


