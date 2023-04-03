#' System tools: Get a non-clashing column name within a dataframe, by adding a numeric suffix ("_x")
#'
#' @param value Column name you are expecting to use. Can be a symbol or a character.
#' @param data The dataframe you are using
#'
#' @return A character
#' @export
#'
#' @examples sys_tld_GetColname(x, data = tibble::tibble(x = 0)) #Returns "x_1"
sys_tld_GetColname = function(value, data){
  #Check ####
  if(weather2:::sys_hp_hasArg(value, value_name = "value")){return(T)}
  if(weather2:::sys_hp_hasArg(data, value_name = "data")){return(T)}
  #Work ####
  value = weather2:::sys_hp_sym2chr({{value}})
  colnames_data = colnames(data)
  name_trial = value
  n = 0
  while(name_trial %in% colnames_data){
    n = n + 1
    name_trial = paste0(value, "_", n)
  }
  return(name_trial)
}


#' System tools: Return data by `name_as` and `overwrite`, with keyword `"*del*"`
#'
#' The special keyword `"*del*"` allows user to not add specific columns into the returned dataframe, even if the column is calculated.
#'
#' @param data The dataframe
#' @param name_as Names of the newly added columns into the dataframe. The keyword `"*del*"` is used here.
#' @param value Values of the new columns in a list.
#' __NOTE__
#' * The list must have the same length as `name_as`
#' * The items in the list must have the same length as the rows numbers of `data`
#' @param overwrite Let the new column names to overwrite the original dataframe columns? Default as `FALSE`.
#'
#' @return
#' @export
#'
#' @examples
sys_tld_FormatReturn = function(data, name_as, value, overwrite = F){
  if(weather2:::sys_hp_hasArg(data, value_name = "data")){return(invisible())}
  if(weather2:::sys_hp_hasArg(name_as, value_name = "name_as")){return(invisible())}
  if(weather2:::sys_hp_hasArg(value, value_name = "value")){return(invisible())}

  if(weather2::sys_ckc_dataframe(data, value_name = "data")){return(data)}
  if(weather2::sys_ckc_character(name_as, value_name = "name_as")){return(data)}
  if(weather2::sys_ckc_list(value, value_name = "value")){return(data)}
  if(weather2::sys_ckc_logical(overwrite, value_name = "overwrite")){return(data)}

  if(weather2::sys_ckl_length(value, list_name = "value", expected = length(name_as), mode = "==")){return(data)}

  nrows = nrow(data)
  for(i in value){
    if(weather2::sys_ckl_length(i, "A list in value", expected = nrows, mode = "==")){return(data)}
  }

  #Start working ####
  if(!overwrite){
    for(i in 1:length(name_as)){
      name_sel = name_as[i]
      if(name_sel != "*del*"){
        name_as[i] = weather2::sys_tld_GetColname({{name_sel}}, data = data)
      }
    }
  }

  for(i in 1:length(name_as)){
    name_sel = name_as[i]
    if(name_sel != "*del*"){
      value_sel = value[[i]]
      data = data %>% dplyr::mutate("{name_sel}" := value_sel)
    }
  }

  return(data)
}




#' System tools: Return groups of data, grouped by shifting values and row numbers
#'
#' In ascending order of row number, a sudden shift in value of the selected column will add the group number by 1.
#' The group number will always start at 1.
#'
#' @param data The dataframe itself
#' @param col The selected column of values in the dataframe
#' @param name_as Name of the new smoothed column. Default as `"grp"`. Accepts the keyword `"*del*"`
#' @param overwrite Let the new column names to overwrite the original dataframe columns? Default as `FALSE`.
#'
#' @return
#' @export
#'
#' @examples tibble::tibble(val = c(1,1,1,1,-1,-1, 1,1)) %>% sys_tld_GrpByAltVal(val)
sys_tld_GrpByAltVal = function(data, col, name_as = "grp", overwrite = F){
  #Check ####
  if(weather2::sys_ckc_dataframe(data, value_name = "data")){return(data)}
  if(weather2::sys_ckd_colexist({{col}}, value_name = "col", data = data, data_name = "data")){return(data)}
  if(weather2::sys_ckc_character(name_as, "name_as")){return(data)}
  if(weather2::sys_ckc_logical(overwrite, "overwrite")){return(data)}

  #Calculate the difference between lagged values ####
  data0 = dplyr::select(.data = data,
                        val = {{col}}) %>%
    dplyr::mutate(pval = dplyr::lag(val),
                  diff = (val != pval),
                  rows = 1:dplyr::n())
  data0$diff[1] = TRUE

  #Find the rows with diff ####
  rows = dplyr::filter(data0, diff)$rows

  #Extract the rows from data, and find the length ####
  magic = function(x, rows){return(length(rows[rows <= x]))}
  list = as.list(data0$rows) %>%
    lapply(FUN = magic, rows = rows) %>%
    unlist()

  #Return the data ####
  data = weather2::sys_tld_FormatReturn(data,
                                        name_as,
                                        value = list(list),
                                        overwrite = overwrite)
  return(data)
}

#' System tools: Create a column of labels, pasted on character value of multiple columns
#'
#' The columns of creating the label column will be passed from the `...` argument. Note that the `...` argument is not checked. Please make sure the columns are indeed present.
#'
#' @param data The dataframe itself
#' @param ... The column names to form the labels
#' @param name_as Name of the new label. Default as `"grp"`. Accepts the keyword `"*del*"`
#' @param overwrite Let the new column names to overwrite the original dataframe columns? Default as `FALSE`.
#'
#' @return
#' @export
#'
#' @examples tibble::tibble(a = c(1,2,3), b = c("a", "b", "c")) %>% sys_tld_MultiColumnLabel(a, b)
sys_tld_MultiColumnLabel = function(data, ..., name_as = "label", overwrite = F){
  #Check ####
  if(weather2::sys_ckc_dataframe(data, "data")){return(data)}
  #mc = match.call(expand.dots = FALSE)$...
  # for(i in mc){
  #   if(weather2::sys_ckd_colexist({{i}}, value_name = "...", data = data, data_name = "data")){return(data)}
  # }
  if(weather2::sys_ckc_character(name_as, "name_as")){return(data)}
  if(weather2::sys_ckl_length(name_as, "name_as", expected = 1L, mode = "==")){return(data)}
  if(weather2::sys_ckc_logical(overwrite, "overwrite")){return(data)}

  #Magic ####
  data0 = dplyr::select(data, ...) %>%
    dplyr::mutate_all(as.character)
  data_label = tibble::tibble(lab = "", new = "", .rows = nrow(data0))

  if(ncol(data0) >= 1){
    for(i in 1:ncol(data0)){
      data_label = dplyr::mutate(data_label,
                                 new = as.character(unlist(data0[,i])),
                                 lab = paste0(lab, new))
    }
  } else {
    data_label = dplyr::mutate(data_label,
                               lab = "No column supplied")
  }

  #Return the data ####
  data = weather2::sys_tld_FormatReturn(data,
                                        name_as = name_as,
                                        value = list(data_label$lab),
                                        overwrite = overwrite)
  return(data)
}
