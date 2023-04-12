#' System tool: Check if a colname is present/ absent in a dataframe
#'
#' @param value The value. Accpets `symbols` and `character`. Only accepts 1.
#' @param value_name The name of value
#' @param data The dataframe
#' @param data_name The name of dataframe
#' @param mode Mode of comparison. Default as `present`
#' * "present" (`value` should be a column within `data`)
#' * "absent" (`value` should __not__ be a column within `data`)
#' @param silent Should a text message be shown if incorrect? Default as `False`.
#'
#' @return
#' A logical value. `TRUE` if an error occured. `FALSE` if no error occured. A text message is provided if `silent` is `FALSE`.
#' @export
#'
#' @examples sys_ckd_colexist(value = y, value_name = "x", data = tibble::tibble(y = 0), data_name = "hi", mode = "absent") #Returns `TRUE`
sys_ckd_colexist = function(value, value_name, data, data_name, mode = "present", silent = F){
  #Check ####
  if(weather2:::sys_hp_hasArg(value_name, value_name = "value_name")){return(T)}
  if(weather2:::sys_hp_hasArg(data_name, value_name = "data_name")){return(T)}
  if(weather2:::sys_hp_hasArg(value, value_name = {{value_name}})){return(T)}
  if(weather2:::sys_hp_hasArg(data, value_name = {{data_name}})){return(T)}
  if(weather2::sys_ckl_length(list = mode, list_name = "present", expected = 1L)){return(T)}
  if(weather2::sys_ckl_ItemIn(list = mode, list_name = "mode", expected = c("present", "absent"))){return(T)}
  if(weather2::sys_ckc_logical(value = silent, value_name = "silent")){return(T)}

  #Work ####
  value = weather2:::sys_hp_sym2chr({{value}})
  colnames = colnames(data)
  if(mode == "present" & !(value %in% colnames)){
    if(!silent){
      cli::cli_text('Error: column {.var {value_name}} must be present in dataframe {.var {data_name}}')
      cli::cli_bullets(c("x" = 'You supplied {.var {value}}.',
                         "x" = 'Your column is not found in {.var {data_name}}!'))
    }
    return(T)
  }
  if(mode == "absent" & (value %in% colnames)){
    if(!silent){
      cli::cli_text('Error: column {.var {value_name}} must not be present in dataframe {.var {data_name}}')
      cli::cli_bullets(c("x" = 'You supplied {.var {value}}.',
                         "x" = 'Your column is found in {.var {data_name}}!'))
    }
    return(T)
  }
  return(F)
}






#' System tool: Check if grouping is present/ absent in a dataframe
#'
#' @param data The dataframe
#' @param data_name The name of dataframe
#' @param mode Mode of comparison. Default as `present`
#' * "present" (`data` should be grouped)
#' * "absent" (`data` should __not__ be grouped)
#' @param silent Should a text message be shown if incorrect? Default as `False`.
#'
#' @return
#' A logical value. `TRUE` if an error occured. `FALSE` if no error occured. A text message is provided if `silent` is `FALSE`.
#' @export
#'
#'
#' @examples sys_ckd_grouped(data(iris))
sys_ckd_grouped = function(data, data_name, mode = "absent", silent = F){
  #Check ####
  if(weather2:::sys_hp_hasArg(data_name, value_name = "data_name")){return(T)}
  if(weather2:::sys_hp_hasArg(data, value_name = {{data_name}})){return(T)}
  if(weather2::sys_ckl_length(list = mode, list_name = "present", expected = 1L)){return(T)}
  if(weather2::sys_ckl_ItemIn(list = mode, list_name = "mode", expected = c("present", "absent"))){return(T)}
  if(weather2::sys_ckc_logical(value = silent, value_name = "silent")){return(T)}

  #Work!
  grouped = dplyr::is_grouped_df(data)
  if(mode == "absent" & grouped){
    if(!silent){
      vars = dplyr::group_vars(x = data)
      cli::cli_text('Error: dataframe {.var {data_name}} must not be grouped!')
      cli::cli_bullets(c("x" = 'You supplied a grouped dataframe.',
                         "x" = 'Your dataframe is grouped by {.var {vars}}!'))
    }
    return(T)
  }
  if(mode == "present" & !grouped){
    if(!silent){
      cli::cli_text('Error: dataframe {.var {data_name}} must be grouped!')
      cli::cli_bullets(c("x" = 'You supplied a non-grouped dataframe!'))
    }
    return(T)
  }
  return(F)
}
