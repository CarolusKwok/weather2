#' System tool: Check if a list/vector has a certain length
#'
#' @param list The list
#' @param list_name The name of the list
#' @param expected Expected length of the list. Must be an integer
#' @param mode Mode of comparison. Default as `==`
#' * "==" (equal to)
#' * "!=" (not equal to)
#' * ">" (larger than)
#' * "<" (smaller than)
#' * ">=" (larger or equal to)
#' * "<=" (smaller or equal to)
#' @param silent Should a text message be shown if incorrect? Default as False.
#'
#' @return
#' @export
#'
#' @examples sys.ck.list_length(c(1,2,3), "obj", 2L) #Return `TRUE`
sys.ck.list_length = function(list, list_name, expected, mode = "==", silent = F){
  #check ####
  if(weather2:::sys.ck.help_hasArg(list_name, "list_name")){return(T)}
  if(weather2:::sys.ck.help_hasArg(list, {{list_name}})){return(T)}
  if(weather2:::sys.ck.help_hasArg(expected, "expected")){return(T)}
  if(weather2::sys.ck.class_integer(value = expected, value_name = "expected")){return(T)}
  if(length(expected) != 1){
    cli::cli_text('Error: {.var expected} must be 1 value only.')
    cli::cli_bullets(c("x" = 'You supplied nothing!'))
    return(T)
  }
  if(length(mode) != 1){
    cli::cli_text('Error: {.var mode} must be 1 value only.')
    cli::cli_bullets(c("x" = 'You supplied nothing!'))
    return(T)
  }
  if(!(mode %in% c("==", "!=", ">", "<", ">=", "<="))){
    cli::cli_text('Error: {.var mode} must be of expected value.')
    cli::cli_bullets(c("x" = 'You should supply "==", "!=", ">", "<", ">=" and "<="!'))
    return(T)
  }
  if(weather2::sys.ck.class_logical(silent, "silent")){return(T)}
  #actually checking ####
  length_list = length(list)
  if(mode == "==" & !(length_list == expected)){
    if(!silent){
      cli::cli_text('Error: length of {.var {list_name}} must be equal to {expected}.')
      cli::cli_bullets(c("x" = 'You supplied {length_list}!'))
    }
    return(T)
  }
  if(mode == "!=" & !(length_list != expected)){
    if(!silent){
      cli::cli_text('Error: length of {.var {list_name}} must be not equal to {expected}.')
      cli::cli_bullets(c("x" = 'You supplied {length_list}!'))
    }
    return(T)
  }
  if(mode == ">"  & !(length_list > expected)){
    if(!silent){
      cli::cli_text('Error: length of {.var {list_name}} must be larger than {expected}.')
      cli::cli_bullets(c("x" = 'You supplied {length_list}!'))
    }
    return(T)
  }
  if(mode == "<"  & !(length_list < expected)){
    if(!silent){
      cli::cli_text('Error: length of {.var {list_name}} must be smaller than {expected}.')
      cli::cli_bullets(c("x" = 'You supplied {length_list}!'))
    }
    return(T)
  }
  if(mode == ">=" & !(length_list >= expected)){
    if(!silent){
      cli::cli_text('Error: length of {.var {list_name}} must be larger or equal to {expected}.')
      cli::cli_bullets(c("x" = 'You supplied {length_list}!'))
    }
    return(T)
  }
  if(mode == "<=" & !(length_list <= expected)){
    if(!silent){
      cli::cli_text('Error: length of {.var {list_name}} must be smaller or equal to {expected}.')
      cli::cli_bullets(c("x" = 'You supplied {length_list}!'))
    }
    return(T)
  }
  return(F)
}

#' System tool: Check if items in a list are in/ not in some expected value
#'
#' @param list The list
#' @param list_name The name of the list
#' @param expected Expected values in the list
#' @param mode Mode of comparision. Default as `in`
#' * "in" (within a list of expected value)
#' * "out" (__not__ within a list )
#' @param silent Should a text message be shown if incorrect? Default as False.
#'
#' @return
#' @export
#'
#' @examples sys.ck.list_item.in(c(1, 2, 3, 4), "obj", c("1", "2", "3"), mode = "out") #Returns `TRUE`
sys.ck.list_item.in = function(list, list_name, expected, mode = "in", silent = F){
  #Check ####
  if(weather2:::sys.ck.help_hasArg(list_name, "list_name")){return(T)}
  if(weather2:::sys.ck.help_hasArg(list, {{list_name}})){return(T)}
  if(weather2:::sys.ck.help_hasArg(expected, "expected")){return(T)}
  if(weather2::sys.ck.list_length(mode, "mode", expected = 1L)){return(T)}
  if(!(mode %in% c("in", "out"))){
    cli::cli_text('Error: {.var mode} must be {.var in} or {.var out}.')
    cli::cli_bullets(c("x" = 'You supplied {.var {mode}}!'))
    return(T)
  }
  if(weather2::sys.ck.class_logical(value = silent, value_name = "silent")){return(T)}

  #START WORKING
  list_in = list %in% expected
  if((mode == "in") & (sum(list_in) != length(list_in))){
    if(!silent){
      cli::cli_text('Error: {.var {list_name}} must be of expected value.')
      cli::cli_bullets(c("x" = 'You should supply {expected}!'))
    }
    return(T)
  }
  if((mode == "out") & (sum(list_in) != 0)){
    if(!silent){
      cli::cli_text('Error: {.var {list_name}} must not be of expected value.')
      cli::cli_bullets(c("x" = 'You should not supply {expected}!'))
    }
    return(T)
  }
  return(F)
}

#' System tool: Check if a list can be turned into a numeric
#'
#' @param list The list
#' @param list_name The name of the list
#' @param silent Should a text message be shown if incorrect? Default as False.
#'
#' @return
#' @export
#'
#' @examples sys.ck.list_numericable(list = c(1, 1, "a"), list_name = "obj") #Returns `TRUE`
sys.ck.list_numericable = function(list, list_name, silent = F){
  #Check ####
  if(weather2:::sys.ck.help_hasArg(list_name, "list_name")){return(T)}
  if(weather2:::sys.ck.help_hasArg(list, {{list_name}})){return(T)}
  if(weather2::sys.ck.class_logical(silent, "silent")){return(T)}
  #Actually Working ####
  list = list[!is.na(list)]
  list = suppressWarnings(as.numeric(list))
  if(anyNA(list)){
    if(!silent){
      cli::cli_text('Error: {.var {list_name}} must be numeric-able.')
      cli::cli_bullets(c("x" = 'You supplied some values that can not be numerics!'))
    }
    return(T)
  }
  return(F)
}

#' System tool: Check if a list includes/ excludes NA
#'
#' @param list The list
#' @param list_name The name of the list
#' @param mode Mode of comparision. Default as `exclude`
#' * "exclude" (expected to have no NAs in the list)
#' * "include" (expected to have NAs in the list)
#' @param silent Should a text message be shown if incorrect? Default as False.
#'
#' @return
#' @export
#'
#' @examples sys.ck.list_has.na(c(1, 2, 3), list_name = "obj") #Returns `TRUE`
sys.ck.list_has.na = function(list, list_name, mode = "exclude", silent = F){
  #Check ####
  if(weather2:::sys.ck.help_hasArg(list_name, "list_name")){return(T)}
  if(weather2:::sys.ck.help_hasArg(list, {{list_name}})){return(T)}
  if(weather2::sys.ck.list_length(mode, "mode", 1L)){return(T)}
  if(weather2::sys.ck.list_item.in(mode, "mode", expected = c("include", "exclude"))){return(T)}
  if(weather2::sys.ck.class_logical(silent, "silent")){return(T)}

  #Actually Working ####
  any_na = anyNA(list)
  if(mode == "exclude" & any_na){
    if(!silent){
      cli::cli_text('Error: {.var {list_name}} must exclude NA.')
      cli::cli_bullets(c("x" = 'You supplied some NA in {.var {list_name}}!'))
    }
    return(T)
  }
  if(mode == "include" & !any_na){
    if(!silent){
      cli::cli_text('Error: {.var {list_name}} must include NA.')
      cli::cli_bullets(x("x" = 'You supplied no NA in {.var {list_naeme}}'))
    }
    return(T)
  }
  return(F)
}

#' System tool: Check if items in list are unique or common
#'
#' @param list The list
#' @param list_name The name of the list
#' @param mode Mode of comparison. Default as `unique`
#' * "all unique" (expecting all items are unique items in the list)
#' * "common" (expected some items are not unique items in the list)
#' @param silent Should a text message be shown if incorrect? Default as False.
#'
#' @return
#' @export
#'
#' @examples sys.ck.list_item.unique(list = c(1,2,3), "obj", mode = "common") #Returns `TRUE`
sys.ck.list_item.unique = function(list, list_name, mode = "all unique", silent = F){
  #Check ####
  if(weather2:::sys.ck.help_hasArg(list_name, "list_name")){return(T)}
  if(weather2:::sys.ck.help_hasArg(list, {{list_name}})){return(T)}
  if(weather2::sys.ck.list_length(mode, "mode", 1L)){return(T)}
  if(weather2::sys.ck.list_item.in(mode, "mode", expected = c("all unique", "common"))){return(T)}
  if(weather2::sys.ck.class_logical(silent, "silent")){return(T)}

  #Work ####
  unique_list = unique(list)
  if((mode == "all unique") & (length(list) != length(unique_list))){
    if(!silent){
      cli::cli_text('Error: items in {.var {list_name}} must have all unique items.')
      cli::cli_bullets(c("x" = 'You supplied {.var {list_name}} with some duplicate items!'))
    }
    return(T)
  }
  if((mode == "common") & (length(list) == length(unique_list))){
    if(!silent){
      cli::cli_text('Error: items in {.var {list_name}} must have some common items.')
      cli::cli_bullets(c("x" = 'You supplied {.var {list_name}} with all unique items!'))
    }
    return(T)
  }
  return(F)
}
