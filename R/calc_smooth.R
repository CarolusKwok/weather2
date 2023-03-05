#' Calculate smoothing and filling data
#'
#' @param data Atmospheric sounding data
#' @param based Parameter used for smoothing, often based on height, but can also be based on pressure or other parameters. Must ensure there is no NA values.
#' @param value Value to be smoothed
#' @param type Type of smoothing. Only accepts "moving", "linear", and "spline".
#' @param name_as Column name of the smoothed data.
#' @param fill Data to be filled based on 'based'.
#' @param weight Weight for smoothing. Only used when type is "moving".
#' @param df Degree of freedom. Only used when type is "spline".
#'
#' @return
#' @export
#'
#' @examples calc(data, hght, uwnd, type = "moving")
calc_smooth = function(data, based, value, name_as, type,
                       weight = rep(1, 5), fill = seq(1, 50000, 1), df = 1){
  # Prep work####
  ## Get data from weather object####
  if(class(data)[1] == "weather"){
    data1 = data$data
    unit1 = data$unit
  } else {
    data1 = data
  }
  # Provide name_as if missing ####
  if(missing(name_as)){
    name_as = paste0(colnames(dplyr::select(data1, {{value}})), "_s")
  }
  # covert 'based' and 'value' into text ####
  colname_based = colnames(dplyr::select(data1, {{based}}))
  colname_value = colnames(dplyr::select(data1, {{value}}))

  # Copy the data set, select used columns and rename ####
  data2 = dplyr::select(data1, {{based}}, {{value}}) %>%
    dplyr::rename(based = {{based}}, value = {{value}})

  # calculate moving average ####
  if(type == "moving"){
     window = length(weight)
     data2 = dplyr::mutate(data2, predict = as.numeric(stats::filter(value, weight/window)))

     for(i in 1:nrow(data2)){
       if(is.na(data2$predict[i])){
         start = ceiling(i - window/2)
         end   = floor(i + window/2)

         if(start < 1){start = 1}
         if(end > nrow(data2)){end = nrow(data2)}

         data_temp = dplyr::select(data2, value)[start:end,]
         data_temp = dplyr::summarise(data_temp, mean(value, na.rm = T))[[1]]
         data2$predict[i] = data_temp
       }
     }
     data2 = dplyr::select(data2, predict)

     data_final = dplyr::mutate(data1, "{name_as}" := data2$predict)
  }
  # calculate linear smoothening ####
  if(type == "linear"){
    #Set data to be NA-less, Calculate groups for each set and the corrected data_gp_cor
    data2 = tidyr::drop_na(data2, based, value)

    data_gp = dplyr::bind_rows(data2, data2) %>%
      dplyr::arrange(based) %>%
      dplyr::slice(2:(dplyr::n()-1)) %>%
      dplyr::mutate(group = as.factor(floor((seq(1, dplyr::n(), 1) + 1)/2)))

    data_gp_cor = dplyr::mutate(data_gp,
                               n = ((seq(1, dplyr::n(), 1)+1) %% 2) * 0.0001,
                               type = "Original") %>%
      dplyr::mutate(based = based - n) %>%
      dplyr::select(-n)

    #Set the predict table
    temp = dplyr::select(data1, {{based}})
    colnames(temp) = "based"

    fill = fill[fill >= min(data_gp_cor$based) & fill <= max(data_gp_cor$based)]
    fill = tibble::tibble(based = fill) %>%
      dplyr::filter(!(based %in% data_gp$based)) %>%
      dplyr::filter(!(based %in% temp$based))
    colnames(fill) = colname_based

    data1 = dplyr::bind_rows(data1, fill)

    data_predict = dplyr::select(data1, {{based}}) %>%
      dplyr::rename(based = {{based}}) %>%
      dplyr::mutate(value = NA_real_,
                    group = as.factor(NA_real_),
                    type = "Predict",
                    order = seq(1, dplyr::n(), 1)) %>%
      dplyr::bind_rows(data_gp_cor) %>%
      dplyr::arrange(type) %>%
      dplyr::arrange(based) %>%
      tidyr::fill(group) %>%
      dplyr::filter(type == "Predict") %>%
      dplyr::arrange(order)


    #Create formula for prediction
    formula = lm(formula = value ~ based * group, data = data_gp)

    data_predict = dplyr::mutate(data_predict,
                                 value = predict(formula, data_predict)) %>%
      dplyr::relocate(based, value, group, type)

    #Join data_predict with data1
    data_final = dplyr::mutate(data1, "{name_as}" := data_predict$value) %>%
      dplyr::arrange({{based}})
  }
  # calculate spline smoothening ####
  if(type == "spline"){
    #Set data to be NA-less and create formula
    data2 = tidyr::drop_na(data2)
    df = nrow(data2) * df
    formula = smooth.spline(x = data2$based, y = data2$value, df = df)

    #Set fill to exclude min max and any value of data2$based
    fill = fill[fill >= min(data2$based) & fill <= max(data2$based)]
    fill = tibble::tibble(based = fill) %>%
      dplyr::filter(!(based %in% data2$based))
    colnames(fill) = colname_based

    data1 = dplyr::bind_rows(data1, fill)

    data2 = dplyr::select(data1, {{colname_based}}) %>%
      dplyr::rename(based = {{colname_based}}) %>%
      dplyr::mutate(based = ifelse(is.na(based), -999999999999, based))

    data1 = dplyr::mutate(data1, "{name_as}" := ifelse(data2$based == -999999999999, NA, stats::predict(formula, data2$based)$y)) %>%
      dplyr::arrange({{based}})

    #Join data_predict with data_org
    data_final = data1
  }
  # Return the data ####
  if(class(data) == "weather"){
    data$data = data_final
    data_final = data
  }
  return(data_final)
}
# calc_smooth(data, hght, uwnd, type = "linear") %>%
#   calc_smooth(hght, vwnd, type = "linear") %>%
#   draw_hodo(uwnd_s, vwnd_s, pres, hght)
