#' Calculate by smoothing and filling the columns, with linear models
#'
#' @param data The Data frame
#' @param based A column within dataframe. This column must be "numeric-able", must not contain NAs, and must be unique
#' @param value A column within data
#' @param trailing Remove trailing NAs within the value if T
#'
#' @return
#' @export
#'
#' @examples calc_smooth_lm(data, x, y, trailing = F)
calc_smooth_lm = function(data, based, value, trailing = T){
  #Check ####
  if(weather2::w2_check_type_dataframe(data = data, data_name = "data")){return(invisible())}
  if(weather2::w2_check_col_exist(data = data, data_name = "data", value = {{based}}, value_name = "based")){return(invisible())}
  if(weather2::w2_check_col_exist(data = data, data_name = "data", value = {{value}}, value_name = "value")){return(invisible())}
  if(weather2::w2_check_type_logical(value = trailing, value_name = "trailing")){return(invisible())}

  data0 = dplyr::select(data, x = {{based}})$x
  if(weather2::w2_check_list_na(data = data0, data_name = "based")){return(invisible())}
  if(weather2::w2_check_list_numericable(data = data0, data_name = "based")){return(invisible())}
  if(weather2::w2_check_list_unique(data = data0, data_name = "based")){return(invisible())}

  #Get text name ####
  name_based = colnames(dplyr::select(data, {{based}}))
  name_value = colnames(dplyr::select(data, {{value}}))

  #arrange data by x so data makes sense
  #select part of the data, convert based into x and value into y for easier processing.
  data = dplyr::arrange(data, {{based}})
  data0 = dplyr::select(data, x = {{based}}, y = {{value}})

  #mutate to give row number and list nas
  data0 = dplyr::mutate(data0 ,
                        row = 1:dplyr::n(),
                        nas = is.na(y))

  #filter the nas, get the row number, and get the rows 1 above and 1 behind it, and give them a group
  ls_grp = dplyr::filter(data0, nas == T)$row
  ls_grp = unique(c(ls_grp, (ls_grp+1), (ls_grp-1)))
  df_grp = dplyr::filter(data0,
                         row %in% ls_grp) %>%
    dplyr::bind_rows(.,.) %>%
    dplyr::arrange(x) %>%
    dplyr::filter(nas == F) %>%
    dplyr::slice(2:(dplyr::n()-1)) %>%
    dplyr::mutate(groups = ceiling((1:dplyr::n())/2))

  #left_join data0 to have groups, fill the gaps between groups, add a predict column with all nas (numeric)
  data0 = dplyr::left_join(x = data0, y = dplyr::select(.data = df_grp, x, groups), by = "x") %>%
    tidyr::fill(groups, .direction = "downup") %>%
    dplyr::mutate(predict = NA_real_)

  #create lm and the prediction. Add the prediction to data0
  for(i in 1:max(df_grp$groups)){
    lm = lm(formula = y~x, data = dplyr::filter(df_grp, groups == i), singular.ok = T)
    prediction = predict(object = lm, newdata = data0)
    data0 = dplyr::mutate(data0,
                          predict_temp = prediction,
                          predict = ifelse(groups == i, predict_temp, predict))
  }
  data0 = dplyr::select(data0, -predict_temp, -groups)

  #if there is a known value, replace the predict with the known value
  #find distinct, n replace the predict with NA if its a list of nas at the top n bottom
  data0 = dplyr::mutate(data0,
                        predict = ifelse(nas == F, y, predict)) %>%
    dplyr::distinct()
  if(trailing){
    na_first = data0$nas[1]
    na_last  = data0$nas[nrow(data0)]
    n = 1
    while(data0$nas[n] == T){
      data0$predict[n] = NA_real_
      n = n +1
    }
    n = nrow(data0)
    while(data0$nas[n] == T){
      data0$predict[n] = NA_real_
      n = n - 1
    }
  }
  #return the smoothed data! first guess the appropriate column name
  expected_colname = weather2::w2_get_colname(data, paste0("predict_", name_value))
  data = dplyr::mutate(data, "{expected_colname}" := data0$predict)

  return(data)
}
