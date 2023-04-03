#' Draws the adiabatic line of Skew-T log-P thermodynamic diagram
#'
#' @param data The dataframe itself
#' @param pres Column name of pressure
#' @param temp Column name of temperature
#' @param plim Limit of the pressure scale. Default as `NULL`, which will automatically detect the maximum and minimum pressure provided in the dataframe.
#' @param dry_adi Values of the dry adiabatic line in Celsius, starting at the bottom of the chart.
#' @param wet_adi Values of the wet adiabatic line in Celsius, starting at the bottom of the chart.
#' @param isohume Values of the isohumes in Celsius, starting at the bottom of the chart.
#' @param size Size of the lines. Default as `0.3`.
#' @param color Color of the lines, ordered as dry adiabatic line, wet adiabatic line, and isohume. Default as `c()`
#'
#' @return
#' @export
#'
#' @examples
plot_skewt_adi = function(data, pres, temp, plim = NULL,
                          dry_adi = NULL, wet_adi = NULL, isohume = NULL,
                          size = 0.3, color = c("red", "purple", "blue")){
  #Check the data ####
  if(weather2::sys_ckc_dataframe(data, "data")){return(data)}
  if(weather2::sys_ckd_colexist({{pres}}, "pres", data = data, data_name = "data")){return(data)}
  if(weather2::sys_ckd_colexist({{temp}}, "pres", data = data, data_name = "data")){return(data)}
  if(!is.null(dry_adi)){
    if(!is.na(dry_adi)){
      if(weather2::sys_ckc_numint(dry_adi, value_name = "dry_adi")){return(data)}
    }
  }
  if(!is.null(wet_adi)){
    if(!is.na(wet_adi)){
      if(weather2::sys_ckc_numint(wet_adi, value_name = "wet_adi")){return(data)}
    }
  }
  if(!is.null(isohume)){
    if(!is.na(isohume)){
      if(weather2::sys_ckc_numint(isohume, value_name = "isohume")){return(data)}
    }
  }
  if(weather2::sys_ckl_length(size, "size", 1L, mode = "==")){return(data)}
  if(weather2::sys_ckl_length(color, "color", 3L, mode = "==")){return(data)}
  if(weather2::sys_ckc_numint(size, value_name = "size")){return(data)}
  if(weather2::sys_ckc_character(color, value_name = "color")){return(data)}

  #Calculate the pressure limits ####
  if(is.null(plim)){
    pres_value = dplyr::select(data, pres = {{pres}})$pres
    plim = c(max(pres_value, na.rm = T), min(pres_value, na.rm = T))
  }

  #Set the list to return to the ggplot
  list = list()

  #Calculate the dry adi lines ####
  mode = ifelse(is.null(dry_adi), "NULL",
                ifelse(  is.na(dry_adi),   "NA", "RUN"))

  if(mode == "NULL" | mode == "RUN"){
    if(is.null(dry_adi)){
      temp_value = dplyr::select(data, temp = {{temp}})$temp
      dry_adi = seq(min(temp_value, na.rm = T) - min(temp_value, na.rm = T)%%10 - 10, max(temp_value, na.rm = T) + max(temp_value, na.rm = T)%%10 + 10, 10)
    }
    data_dry = tibble::tibble(temp1 = dry_adi,
                              pres1 = plim[1]) %>%
      tidyr::expand_grid(tibble::tibble(pres2 = seq(max(plim, na.rm = T), min(plim, na.rm = T), -1))) %>%
      weather2::calc_adi_dry(pres1 = pres1, temp1 = temp1, pres2 = pres2)

    plot = weather2::geom_skewt_path(data = data_dry,
                                     mapping = ggplot2::aes(x = temp2, y = pres2, group = temp1),
                                     color = color[1], size = size, linetype = "dashed")
    list = append(list, plot)
  }

  #Calculate the wet adi lines ####
  mode = ifelse(is.null(wet_adi), "NULL",
                ifelse(  is.na(wet_adi),   "NA", "RUN"))

  if(mode == "NULL" | mode == "RUN"){
    if(is.null(wet_adi)){
      temp_value = dplyr::select(data, temp = {{temp}})$temp
      wet_adi = seq(min(temp_value, na.rm = T) - min(temp_value, na.rm = T)%%10 - 10, max(temp_value, na.rm = T) + max(temp_value, na.rm = T)%%10 + 10, 10)
    }
    acc = (max(plim, na.rm = T) - min(plim, na.rm = T))/1000
    data_wet = tibble::tibble(temp1 = wet_adi,
                              pres1 = plim[1]) %>%
      tidyr::expand_grid(tibble::tibble(pres2 = seq(max(plim, na.rm = T), min(plim, na.rm = T), -1))) %>%
      weather2::calc_adi_wet(pres1 = pres1, temp1 = temp1, pres2 = pres2, acc = acc)

    plot = weather2::geom_skewt_path(data = data_wet,
                                     mapping = ggplot2::aes(x = temp2, y = pres2, group = temp1),
                                     color = color[2], size = size, linetype = "dashed")
    list = append(list, plot)
  }

  #Calculate the isohume lines ####
  mode = ifelse(is.null(isohume), "NULL",
                ifelse(  is.na(isohume),   "NA", "RUN"))
  if(mode == "NULL" | mode == "RUN"){
    if(is.null(isohume)){
      temp_value = dplyr::select(data, temp = {{temp}})$temp
      isohume = seq(min(temp_value, na.rm = T) - min(temp_value, na.rm = T)%%10 - 10, max(temp_value, na.rm = T) + max(temp_value, na.rm = T)%%10 + 10, 10)
    }
    data_hum = tibble::tibble(temp1 = isohume,
                              pres1 = plim[1]) %>%
      tidyr::expand_grid(tibble::tibble(pres2 = seq(max(plim, na.rm = T), min(plim, na.rm = T), -1))) %>%
      weather2::calc_isohume(pres1 = pres1, temp1 = temp1, pres2 = pres2)


    plot = weather2::geom_skewt_path(data = data_hum,
                                     mapping = ggplot2::aes(x = temp2, y = pres2, group = temp1),
                                     color = color[3], size = size, linetype = "dashed")
    list = append(list, plot)
  }
  #Return the list ####
  return(list)
}
