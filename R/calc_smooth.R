#' Calculate by smoothing and filling the columns, with linear models
#'
#' @param data The Data frame
#' @param based A column within dataframe. This column must be "numeric-able", must not contain NAs, and must be unique
#' @param value A column within data. This column must contain NAs
#' @param trailing Remove trailing NAs within the value if T
#' @param name_as Name of the new smoothed column. Default as `NULL`, which returns value of `based` with the prefix of `"slm_"`. Accepts the keyword `"*del*"`
#' @param overwrite Let the new column names to overwrite the original dataframe columns? Default as `FALSE`.
#'
#' @return
#' @export
#'
#' @examples calc_smooth_lm(data, x, y, trailing = F)
calc_smooth_lm = function(data, based, value, trailing = T, name_as = NULL, overwrite = F){
  #Check ####
  if(weather2::sys_ckf_CalcSmooth(data = data, based = {{based}}, value = {{value}})){return(data)}
  if(weather2::sys_ckc_logical(value = trailing, value_name = "trailing")){return(data)}
  if(!is.null(name_as)){
    if(weather2::sys_ckc_character(value = name_as, value_name = "name_as")){return(data)}
  }
  if(weather2::sys_ckc_logical(overwrite, "overwrite")){return(data)}

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
    dplyr::mutate(groups = as.factor(ceiling((1:dplyr::n())/2)))

  #left_join data0 to have groups, fill the gaps between groups, add a predict column with all nas (numeric)
  data0 = dplyr::left_join(x = data0, y = dplyr::select(.data = df_grp, x, groups), by = "x") %>%
    tidyr::fill(groups, .direction = "downup")

  #create lm and the prediction. Add the prediction to data0
  #find distinct, n replace the predict with NA if its a list of nas at the top n bottom
  if(length(unique(df_grp$groups)) == 1){
    lm = lm(formula = y~x, data = df_grp, singular.ok = T)
    prediction = suppressWarnings(predict(object = lm, newdata = data0))
  } else {
    lm = lm(formula = y~x * groups, data = df_grp, singular.ok = T)
    prediction = suppressWarnings(predict(object = lm, newdata = data0))
  }

  data0 = dplyr::mutate(data0,
                        predict = ifelse(nas == F, y, prediction)) %>%
    dplyr::select(-groups) %>%
    dplyr::distinct()
  #if there is a known value, replace the predict with the known value
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
  if(is.null(name_as)){name_as = paste0("slm_", name_value)}
  data = weather2::sys_tld_FormatReturn(data,
                                        name_as,
                                        list(data0$predict),
                                        overwrite = overwrite)
  return(data)
}




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
  if(weather2::sys_ckf_CalcSmooth(data = data, based = {{based}}, value = {{value}})){return(data)}
  if(weather2::sys_ckl_ItemIn(list = type, list_name = "type", expected = c("center", "top", "bottom"))){return(data)}
  if(weather2::sys_ckc_numeric(value = weight, value_name = "weight")){return(data)}
  if(weather2::sys_ckc_logical(value = NAs, value_name = "NAs")){return(data)}

  rows = nrow(data)
  if(length(weight) == 1){
    weight = as.integer(weight)
    if(weather2::sys_ckc_integer(value = weight, value_name = "weight")){return(data)}
    if(weather2::sys_ckl_NumericValue(list = weight, list_name = "weight", expected = rows, mode = "<=")){return(data)}
  } else if(length(weight) > 1){
    len = length(weight)
    sum = sum(weight, na.rm = T)
    if(weather2::sys_ckl_NumericValue(list = len, list_name = "weight", expected = rows, mode = "<=")){return(data)}
    if(weather2::sys_ckl_NumericValue(list = sum, list_name = "sum of weight", expected = 1, mode = "==")){return(data)}
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
  expected_colname = weather2::sys_tld_GetColname(value = name_as, data = data)
  data = dplyr::mutate(data, "{expected_colname}" := data0$predict)
  return(data)
}




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
  if(weather2::sys_ckf_CalcSmooth(data = data, based = {{based}}, value = {{value}})){return(data)}
  if(weather2::sys_ckc_numeric(value = df, value_name = "df")){return(data)}

  data0 = dplyr::select(data, x = {{based}}, y = {{value}}) %>% tidyr::drop_na()
  if(weather2::sys_ckl_NumericValue(list = df, list_name = "df", expected = nrow(data0), mode = "<=")){return(data)}
  if(weather2::sys_ckl_NumericValue(list = df, list_name = "df", expected = 1, mode = ">")){return(data)}

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
  expected_colname = weather2::sys_tld_GetColname(value = name_as, data = data)
  data = dplyr::mutate(data, "{expected_colname}" := data0$predict)
  return(data)
}
