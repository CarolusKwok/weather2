#' Calculate by smoothing the column, by moving average
#'
#' @param data The Data frame
#' @param based A column within dataframe. This column must be "numeric-able", must not contain NAs, and must be unique
#' @param value A column within data
#' @param side Accepts 1 string of "left", "center", or "right"
#' @param weight weighting or the size of the window
#'
#' @return
#' @export
#'
#' @examples calc_smooth_ma(data, x, y)
calc_smooth_ma = function(data, based, value, side = "center", weight = 3){
  #Check ####
  if(weather2::w2_check_type_dataframe(data = data, data_name = "data")){return(invisible())}
  if(weather2::w2_check_col_exist(data = data, data_name = "data", value = {{based}}, value_name = "based")){return(invisible())}
  if(weather2::w2_check_col_exist(data = data, data_name = "data", value = {{value}}, value_name = "value")){return(invisible())}
  if(weather2::w2_check_type_numeric(value = weight, value_name = "weight")){return(invisible())}

  data0 = dplyr::select(data, x = {{based}})$x
  if(weather2::w2_check_list_na(data = data0, data_name = "based")){return(invisible())}
  if(weather2::w2_check_list_numericable(data = data0, data_name = "based")){return(invisible())}
  if(weather2::w2_check_list_unique(data = data0, data_name = "based")){return(invisible())}

  #arrange the data by based, grab based as x and value as y, mutate to have a list of na ####
  data = dplyr::arrange(data, {{based}})
  data0 = dplyr::select(data, x = {{based}}, y = {{value}}) %>%
    dplyr::mutate(predict = NA_real_)

  #calculate n and weight ####
  if(length(weight) == 1){
    n = weight
    weight = rep(1/n, n)
  }
  if(length(weight) != 1){
    n = length(weight)
  }
  #start calculating ####
  if(side == "left"){
    for(i in 1:nrow(data0)){
      start = i
      end = i+n-1
      ls = tibble::tibble(value  = data0$y[start:end],
                          weight = weight,
                          new    = value * weight)
      sum = sum(ls$new, na.rm = T)
      data0$predict[i] = sum
    }
  }
  if(side == "right"){
    data0 = dplyr::arrange(data0, dplyr::desc(x))
    for(i in 1:nrow(data0)){
      start = i
      end = i+n-1
      ls = tibble::tibble(value  = data0$y[start:end],
                          weight = weight,
                          new    = value * weight)
      sum = sum(ls$new, na.rm = T)
      data0$predict[i] = sum
    }
    data0 = dplyr::arrange(data0, x)
  }
  if(side == "center"){
    left = floor((n-1)/2)
    right= floor(n/2)
    for(i in 1:nrow(data0)){
      start = ifelse(i-left <=0, 1, i-left)
      end = i+right
      ls = data0$y[start: end]
      if(length(ls) != n){
        ls = c(rep(NA_real_, n - length(ls)), ls)
      }
      ls = tibble::tibble(value  = ls,
                          weight = weight,
                          new    = value * weight)
      sum = sum(ls$new, na.rm = T)
      data0$predict[i] = sum
    }
  }

  #return the data ####
  name_value = colnames(dplyr::select(data, {{value}}))
  expected_colname = weather2::w2_get_colname(data, name = paste0("predict_", name_value))
  data = dplyr::mutate(data, "{expected_colname}" := data0$predict)
  return(data)
}
