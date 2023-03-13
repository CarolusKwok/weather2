#' Calculate by smoothing the column, by moving average
#'
#' @param data The Data frame
#' @param based A column within dataframe. This column must be "numeric-able", must not contain NAs, and must be unique
#' @param value A column within data
#' @param type Accepts 1 string of "left", "center", or "right"
#' @param weight Weighting or the size of the window as an integer
#' @param name_as Name of the new smoothed column. Default as predict_xxx, where xxx is the column name of the based.
#' @param NAs Removes NA in the smoothed column by treating missing data
#'
#' @return
#' @export
#'
#' @examples calc_smooth_ma(data, x, y, weight = 7)
calc_smooth_ma = function(data, based, value, type = "center", weight = 3, name_as = "", NAs = T){
  #Check ####
  if(weather2::w2_check_type_dataframe(data = data, data_name = "data")){return(data)}
  if(weather2::w2_check_col_exist(data = data, data_name = "data", value = {{based}}, value_name = "based")){return(data)}
  if(weather2::w2_check_col_exist(data = data, data_name = "data", value = {{value}}, value_name = "value")){return(data)}
  if(weather2::w2_check_type_character(value = type, value_name = "type")){return(data)}
  if(weather2::w2_check_list_item(item = type, item_name = "type", list = c("center", "top", "bottom"))){return(data)}
  if(weather2::w2_check_type_numeric(value = weight, value_name = "weight")){return(data)}
  if(weather2::w2_check_type_logical(value = NAs, value_name = "NAs")){return(data)}

  data0 = dplyr::select(data, x = {{based}})$x
  if(weather2::w2_check_list_na(data = data0, data_name = "based", NAs = F)){return(data)}
  if(weather2::w2_check_list_numericable(data = data0, data_name = "based")){return(data)}
  if(weather2::w2_check_list_unique(data = data0, data_name = "based")){return(data)}

  data0 = dplyr::select(data, x = {{value}})$x
  if(weather2::w2_check_list_na(data = data0, data_name = "value", NAs = T)){return(data)}
  if(weather2::w2_check_list_numericable(data = data0, data_name = "value")){return(data)}

  rows = nrow(data)
  if(length(weight) == 1){
    weight = as.integer(weight)
    if(weather2::w2_check_type_integer(value = weight, value_name = "weight")){return(data)}
    if(weather2::w2_check_item_value(item = weight, item_name = "weight", expect = rows, type = "<=")){return(data)}
  } else if(length(weight) > 1){
    len = length(weight)
    sum = sum(weight, na.rm = T)
    if(weather2::w2_check_item_value(item = len, item_name = "weight", expect = rows, type = "<=")){return(data)}
    if(weather2::w2_check_item_value(item = sum, item_name = "sum of weight", expect = 1, type = "=")){return(data)}
  }

  #arrange the data by based, grab based as x and value as y, mutate to have a list of na ####
  data = dplyr::arrange(data, {{based}})
  data0 = dplyr::select(data, x = {{based}}, y = {{value}})
  #calculate weight and get the sides ####
  if(length(weight) == 1){weight = rep(1/weight, weight)}
  weight = weight[length(weight):1]

  if(type == "center"){
    sides = 2
  } else if (type == "top") {
    sides = 1
    weight = weight[length(weight):1]
    data0 = dplyr::arrange(data0, dplyr::desc(x))
  } else if (type == "bottom"){
    sides = 1
  }
  #start calculating ####
  prediction = as.numeric(stats::filter(x = data0$y, filter = weight, method = "convolution", sides = sides))
  data0 = dplyr::mutate(data0, predict = prediction)
  if(NAs == F){
    #Add NA rows above and below data0, and add row information
    data0 = dplyr::mutate(data0, type = "ORIGINAL") %>%
      dplyr::bind_rows(tibble::tibble(x = NA, y = NA, predict = NA, type = "ADDED", .rows = length(weight)),
                       .,
                       tibble::tibble(x = NA, y = NA, predict = NA, type = "ADDED", .rows = length(weight))) %>%
      dplyr::mutate(row = 1:dplyr::n())

    #Reverse the weight back to normal
    #Calculate index information for smoothening
    weight = weight[length(weight):1]
    if(type == "center"){
      if((length(weight) %% 2) == 1){
        data0 = dplyr::mutate(data0,
                              start = row - floor(length(weight)/2),
                              end   = row + floor(length(weight)/2),
                              run   = (type == "ORIGINAL" & is.na(predict)))
      }
      if((length(weight) %% 2) == 0){
        data0 = dplyr::mutate(data0,
                              start = row - (length(weight)/2 - 1),
                              end   = row + (length(weight)/2),
                              run   = (type == "ORIGINAL" & is.na(predict)))
      }
    }
    if(type == "top" | type == "bottom"){
      data0 = dplyr::mutate(data0,
                            start = row - length(weight) + 1,
                            end   = row,
                            run   = (type == "ORIGINAL" & is.na(predict)))
    }

    #Calculate!
    list_run = dplyr::filter(data0, run == T)$row
    for(i in list_run){
      reana = data0[data0$start[i]:data0$end[i], "y"] %>%
        dplyr::mutate(w = weight,
                      s = y*w)
      sum   = sum(reana$s, na.rm = T)
      data0$predict[i] = sum
    }
    data0 = dplyr::filter(data0, type != "ADDED")
  }
  data0 = dplyr::arrange(data0, x)
  #return the data ####
  if(name_as == ""){name_as = paste0("predict_", colnames(dplyr::select(data, {{value}})))}
  expected_colname = weather2::w2_get_colname(data, name = name_as)
  data = dplyr::mutate(data, "{expected_colname}" := data0$predict)
  return(data)
}
