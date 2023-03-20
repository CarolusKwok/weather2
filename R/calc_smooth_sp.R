#' Calculate by smoothing the column, by smoothing spline
#'
#' @param data The Data frame
#' @param based A column within dataframe. This column must be "numeric-able", must not contain NAs, and must be unique
#' @param value A column within data
#' @param df Degree of freedom. Must be between 1 and the number of rows in data (rows with NA values excluded)
#' @param name_as Name of the new smoothed column. Default as predict_xxx, where xxx is the column name of the based.
#'
#' @return
#' @export
#'
#' @examples calc_smooth_sp(data, x, y, df = 0.7*nrow(data), name_as = "")
calc_smooth_sp = function(data, based, value, df, name_as = ""){
  #Check ####
  if(weather2::sys.ck.class_data.frame(value = data, value_name = "data")){return(data)}
  if(weather2::sys.ck.data_col.exist(value = {{based}}, value_name = "based", data = data, data_name = "data")){return(data)}
  if(weather2::sys.ck.data_col.exist(value = {{value}}, value_name = "value", data = data, data_name = "data")){return(data)}
  if(weather2::sys.ck.class_numeric(value = df, value_name = "df")){return(data)}

  data0 = dplyr::select(data, x = {{based}})$x
  if(weather2::sys.ck.list_has.na(list = data0, list_name = "based")){return(data)}
  if(weather2::sys.ck.list_numericable(list = data0, list_name = "based")){return(data)}
  if(weather2::sys.ck.list_item.unique(list = data0, list_name = "based")){return(data)}

  data0 = dplyr::select(data, x = {{value}})$x
  if(weather2::sys.ck.list_has.na(list = data0, list_name = "value")){return(data)}
  if(weather2::sys.ck.list_numericable(list = data0, list_name = "value")){return(data)}

  data0 = dplyr::select(data, x = {{based}}, y = {{value}}) %>% tidyr::drop_na()
  if(weather2::sys.ck.list_numeric.value(list = df, list_name = "df", expected = nrow(data0), mode = "<=")){return(data)}
  if(weather2::sys.ck.list_numeric.value(list = df, list_name = "df", expected = 1, mode = ">")){return(data)}

  #Format the data and duplicate to data0 and data1 ####
  data = dplyr::arrange(data, {{based}})
  data0= dplyr::select(data,
                       x = {{based}},
                       y = {{value}})

  #Make the spline formula ####
  data1= tidyr::drop_na(data0)
  sp = stats::smooth.spline(x = data1$x, y = data1$y, df = df)

  #Return the smooth.spline to data0 ####
  prediction = stats::predict(object = sp, x = data0$x)$y
  data0 = dplyr::mutate(data0,
                        predict = prediction)

  #Return data0 to data ####
  if(name_as == ""){name_as = paste0("predict_", colnames(dplyr::select(data, {{value}})))}
  expected_colname = weather2::sys.tl.data_get.colname(value = name_as, data = data)
  data = dplyr::mutate(data, "{expected_colname}" := data0$predict)
  return(data)
}
