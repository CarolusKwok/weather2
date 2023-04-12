#' Calculate by smoothing and filling the columns, with linear models
#'
#' Data smoothing is an important process in data analysis to fill up missing data, or to remove extreme values.
#' This function fills up missing data with linear model, predicted by the head and tail of the gap.
#'
#' @param data The dataframe itself, storing all observations. The dataframe must not be grouped.
#' @param based The column name of how the data should be arranged, in ascending order. This column must not contain NAs, must be unique, and must be "numericable".
#' @param value The column name of what values should be smoothed. This column must contain NAs.
#' @param max_fill The maximum size of a NA-gap. Default as `NULL`, which ignores this argument. Must be numeric.
#' @param trailing Remove all NAs at the head of prediction column? Default as `TRUE`.
#' @param name_as Names of the 1 new columns, i.e. the newly smoothed data. Default as `NULL`, i.e. the column name of `value` with a prefix of `"slm_"`. Keyword `"*del*"` is supported.
#' @param overwrite Let the new column names to overwrite the original dataframe columns? Default as `FALSE`.
#'
#' @return The same dataframe as `data`, with 1 additional column with column name defined in `name_as`.
#' @export
#'
#' @examples calc_smooth_lm(data, x, y, trailing = F)
calc_smooth_lm = function(data, based, value , max_fill = NULL, trailing = T, name_as = NULL, overwrite = F){
  #Check ####
  if(weather2::sys_ckf_CalcSmooth(data = data, based = {{based}}, value = {{value}})){return()}
  if(weather2::sys_ckc_logical(value = trailing, value_name = "trailing")){return()}
  if(!is.null(max_fill)){if(weather2::sys_ckc_numeric(value = max_fill, value_name = "max_fill")){return()}}

  if(is.null(name_as)){name_as = paste0("slm_", weather2:::sys_hp_sym2chr({{value}}))}
  if(weather2::sys_ckf_NameAsReturn(name_as = name_as,
                                    overwrite = overwrite,
                                    expected = 1L)){return()}

  #Arrange data so its in ascending based order ####
  data = dplyr::arrange(data, {{based}})
  #Find the data used, get NA values ####
  data0 = dplyr::select(data,
                        x = {{based}},
                        y = {{value}}) %>%
    dplyr::mutate(row = 1:dplyr::n(),
                  NAs = is.na(y))

  #Find the group ####
  ls_grp = dplyr::filter(data0, NAs)$row
  ls_grp = sort(unique(c(ls_grp, (ls_grp+1), (ls_grp-1))))
  df_grp = dplyr::filter(data0,
                         row %in% ls_grp) %>%
    dplyr::bind_rows(.,.) %>%
    dplyr::arrange(x) %>%
    dplyr::filter(!NAs) %>%
    dplyr::slice(2:(dplyr::n()-1)) %>%
    dplyr::mutate(groups = as.factor(ceiling((1:dplyr::n())/2)))

  #Left join data ####
  df_grp1_sel = dplyr::group_by(df_grp, x) %>%
    dplyr::reframe(grp1 = as.factor(min(as.numeric(groups), na.rm = T)))
  df_grp2_sel = dplyr::group_by(df_grp, x) %>%
    dplyr::reframe(grp2 = as.factor(max(as.numeric(groups), na.rm = T)))
  df_grp_sel = dplyr::rename(df_grp1_sel, grp = grp1)

  data0 = dplyr::left_join(x = data0, y = dplyr::select(.data = df_grp_sel, x, grp), by = "x") %>%
    dplyr::left_join(y = dplyr::select(.data = df_grp1_sel, x, grp1), by = "x") %>%
    dplyr::left_join(y = dplyr::select(.data = df_grp2_sel, x, grp2), by = "x") %>%
    tidyr::fill(grp, .direction = "updown") %>%
    tidyr::fill(grp1, .direction = "down") %>%
    tidyr::fill(grp2, .direction = "up")

  #Form the model ####
  df_grp = dplyr::rename(df_grp, grp = groups)
  max_grp = max(as.numeric(df_grp$grp), na.rm = T)
  if(max_grp == 1){
    lm = lm(formula = y~x, data = df_grp, singular.ok = T)
    prediction = suppressWarnings(predict(object = lm, newdata = data0))
  } else {
    lm = lm(formula = y~x * grp, data = df_grp, singular.ok = T)
    prediction = suppressWarnings(predict(object = lm, newdata = data0))
  }
  data0 = dplyr::mutate(data0,
                        predict = ifelse(!NAs, y, prediction))

  #Replace with NA value if trailing == T ####
  if(trailing){
    data0 = dplyr::mutate(data0,
                          predict = ifelse(is.na(grp1) & NAs, NA, predict),
                          predict = ifelse(is.na(grp2) & NAs, NA, predict))
  }
  #Replace with NA value if > group size ####
  if(!is.null(max_fill)){
    df_grp_size = dplyr::group_by(.data = df_grp, grp) %>%
      dplyr::reframe(size = max(x) - min(x))
    ## Find faulty groups ####
    ls_grpX = dplyr::filter(df_grp_size,
                            size > max_fill)$grp
    ## Replace! ####
    data0 = dplyr::mutate(data0,
                          predict = ifelse(grp %in% ls_grpX, NA, predict))
  }
  #Replace with known value ####
  data0 = dplyr::mutate(data0, predict = ifelse(NAs, predict, y))

  #Return the data ####
  data = weather2::sys_tld_FormatReturn(data,
                                        name_as = name_as,
                                        value = list(data0$predict),
                                        overwrite = overwrite)
  return(data)
}

#' Calculate by smoothing the column, by moving average
#'
#' Data smoothing is an important process in data analysis to fill up missing data, or to remove extreme values.
#' This function removes extreme values by calculating the (weighted) average within a moving window. The moving window is controlled by `type`, which has 3 choices representing the following scenarios (assuming your calculating the `n` row with a window size `s`) -
#' *`"left"`: The window is at the left of the arranged table, i.e. the smoothed value will be the weighted average of `n` to `n+s`
#' *`"center"`: The window is at the center of the arranged table, i.e. the smoothed value will be the weighted average of `n-(s/2)` to `n+(s/2)`
#' *`"right"`: The window is at the right of the arranged table, i.e. the smoothed value will be the weighted average of `n-s` to `n`
#'
#' @param data The dataframe itself, storing all observations. The dataframe must not be grouped.
#' @param based The column name of how the data should be arranged, in ascending order. This column must not contain NAs, must be unique, and must be "numericable".
#' @param value The column name of what values should be smoothed. This column must contain NAs.
#' @param type Type of smoothening performed by the window position, relative to the observation. Accepts 1 string of `"left"`, `"center"`, or `"right"`.
#' @param weight Weighting of the window as a numerical vector or the size of the window as an numerical
#' @param NAs Removes NA in the smoothed column by treating missing data.
#' @param name_as Names of the 1 new columns, i.e. the newly smoothed data. Default as `NULL`, i.e. the column name of `value` with a prefix of `"sma_"`. Keyword `"*del*"` is supported.
#' @param overwrite Let the new column names to overwrite the original dataframe columns? Default as `FALSE`.
#'
#' @return The same dataframe as `data`, with 1 additional column with column name defined in `name_as`.
#' @export
#'
#' @examples calc_smooth_ma(data, x, y, weight = 7)
calc_smooth_ma = function(data, based, value, type = "center", weight = 3, NAs = T, name_as = NULL, overwrite = F){
  #Check ####
  if(weather2::sys_ckf_CalcSmooth(data = data, based = {{based}}, value = {{value}}, check_na = F)){return()}
  if(weather2::sys_ckl_ItemIn(list = type, list_name = "type", expected = c("center", "top", "bottom"))){return()}
  if(weather2::sys_ckc_numeric(value = weight, value_name = "weight")){return()}
  if(weather2::sys_ckc_logical(value = NAs, value_name = "NAs")){return()}

  if(is.null(name_as)){name_as = paste0("sma_", weather2:::sys_hp_sym2chr({{value}}))}
  if(weather2::sys_ckf_NameAsReturn(name_as = name_as, overwrite = overwrite, expected = 1L)){return()}

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
  data = weather2::sys_tld_FormatReturn(data,
                                        name_as = name_as,
                                        value = list(data0$predict),
                                        overwrite = overwrite)
  return(data)
}




#' Calculate by smoothing the column, by smoothing spline
#'
#' Data smoothing is an important process in data analysis to fill up missing data, or to remove extreme values.
#' This function fills up missing data using cubic splines, a special function defined piecewise by polynomials. Splines keeps the general shape of the model, but allows for smoothed curves within the data. The smoothness of the data can be controlled by `df`, or how many control points is present within the smooth.
#'
#' @param data The dataframe itself, storing all observations. The dataframe must not be grouped.
#' @param based The column name of how the data should be arranged, in ascending order. This column must not contain NAs, must be unique, and must be "numericable".
#' @param value The column name of what values should be smoothed. This column must contain NAs.
#' @param df Degree of freedom for smoothing. Must be between 1 and the number of rows in data (rows with NA values excluded)
#' @param name_as Names of the 1 new columns, i.e. the newly smoothed data. Default as `NULL`, i.e. the column name of `value` with a prefix of `"ssp_"`. Keyword `"*del*"` is supported.
#' @param overwrite Let the new column names to overwrite the original dataframe columns? Default as `FALSE`.
#'
#' @return The same dataframe as `data`, with 1 additional column with column name defined in `name_as`.
#' @export
#'
#' @examples calc_smooth_sp(data, x, y, df = 0.7*nrow(data))
calc_smooth_sp = function(data, based, value, df, name_as = NULL, overwrite = F){
  #Check ####
  if(weather2::sys_ckf_CalcSmooth(data = data, based = {{based}}, value = {{value}})){return()}
  if(weather2::sys_ckc_numeric(value = df, value_name = "df")){return()}

  if(is.null(name_as)){name_as = paste0("ssp_", weather2:::sys_hp_sym2chr({{value}}))}
  if(weather2::sys_ckf_NameAsReturn(name_as = name_as, overwrite = overwrite, expected = 1L)){return()}

  data0 = dplyr::select(data, x = {{based}}, y = {{value}}) %>% tidyr::drop_na()
  if(weather2::sys_ckl_NumericValue(list = df, list_name = "df", expected = nrow(data0), mode = "<=")){return()}
  if(weather2::sys_ckl_NumericValue(list = df, list_name = "df", expected = 1, mode = ">")){return()}

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
  data = weather2::sys_tld_FormatReturn(data = data,
                                        name_as = name_as,
                                        value = list(data0$predict),
                                        overwrite = overwrite)
  return(data)
}
