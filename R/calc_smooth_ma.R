#' Calculate by smoothing the column, by moving average
#'
#' @param data The Data frame
#' @param based A column within dataframe. This column must be "numeric-able", must not contain NAs, and must be unique
#' @param value A column within data
#' @param type Accepts 1 string of "left", "center", or "right"
#' @param weight Weighting or the size of the window
#' @param name_as Name of the new smoothed column. Default as predict_xxx, where xxx is the column name of the based.
#' @param NAs Removes NA in the smoothed column by treating missing data
#'
#' @return
#' @export
#'
#' @examples
calc_smooth_ma = function(data, based, value, type = "center", weight = 3, name_as = "", NAs = T){
  #Check ####
  if(weather2::w2_check_type_dataframe(data = data, data_name = "data")){return(invisible())}
  if(weather2::w2_check_col_exist(data = data, data_name = "data", value = {{based}}, value_name = "based")){return(invisible())}
  if(weather2::w2_check_col_exist(data = data, data_name = "data", value = {{value}}, value_name = "value")){return(invisible())}
  if(weather2::w2_check_type_numeric(value = weight, value_name = "weight")){return(invisible())}

  data0 = dplyr::select(data, x = {{based}})$x
  if(weather2::w2_check_list_na(data = data0, data_name = "based", NAs = F)){return(invisible())}
  if(weather2::w2_check_list_numericable(data = data0, data_name = "based")){return(invisible())}
  if(weather2::w2_check_list_unique(data = data0, data_name = "based")){return(invisible())}

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
  if(NAs == T){
    prediction = as.numeric(stats::filter(x = data0$y, filter = weight, method = "convolution", sides = sides))
    data0 = dplyr::mutate(data0, predict = prediction)
  }
  if(NAs == F){
    data0 = dplyr::bind_rows(tibble::tibble(x = NA, y = NA, predict = NA, .rows = length(weight)),
                             data0) %>%
      dplyr::bind_rows(tibble::tibble(x = NA, y = NA, predict = NA, .rows = length(weight)))
    prediction = as.numeric(stats::filter(x = data0$y, filter = weight, method = "convolution", sides = sides))

    weight = weight[length(weight):1]
    data0 = dplyr::mutate(data0,
                          predict = prediction,
                          row  = 1:dplyr::n())
    if(type == "center"){
      if((length(weight) %% 2) == 1){
        data0 = dplyr::mutate(data0,
                              start = row - floor(length(weight)/2),
                              end   = row + floor(length(weight)/2),
                              run = !is.na(x) & is.na(predict))
      }
      if((length(weight) %% 2) == 0){
        data0 = dplyr::mutate(data0,
                              start = row - (length(weight)/2 - 1),
                              end = row + (length(weight)/2),
                              run = !is.na(x) & is.na(predict))
      }
    }
    if(type == "top" | type == "bottom"){
      data0 = dplyr::mutate(data0,
                            start = row - length(weight) + 1,
                            end = row,
                            run = !is.na(x) & is.na(predict))
    }
    list_run = dplyr::filter(data0, run == T)$row
    for(i in list_run){
      start = data0$start[i]
      end   = data0$end[i]
      reana = data0[start:end, "y"] %>%
        dplyr::mutate(w = weight,
                      s = y*w)
      sum   = sum(reana$s, na.rm = T)
      data0$predict[i] = sum
    }
    data0 = dplyr::filter(data0,
                          !is.na(x) & !is.na(y) & !is.na(predict))
  }
  data0 = dplyr::arrange(data0, x)
  #return the data ####
  if(name_as == ""){name_as = paste0("predict_", colnames(dplyr::select(data, {{value}})))}
  expected_colname = weather2::w2_get_colname(data, name = name_as)
  data = dplyr::mutate(data, "{expected_colname}" := data0$predict)
  return(data)
}
