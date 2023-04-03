#' Draws the base plot of Skew-T log-P thermodynamic diagram
#'
#' @param data Data frame itself
#' @param pres Column name of pressure
#' @param skew Skewness of the Skew-T
#' @param plim Limit of the pressure scale. Default as `NULL`, which will automatically detect the maximum and minimum pressure provided in the dataframe.
#' @param pline Values of the Isobars. Default as `NULL`, which will plot the isobars at 1000, 900 ... , 200, 100, 50, 25, and 10.
#' @param tline Values of the Isotherms. Default as `NULL`, which will plot the isotherms automatically the range of the provided temperature profile, with some to spare.
#' @param ... Any other parameters to be past to `ggplot2::aes()`
#'
#' @return
#' @export
#'
#' @examples
plot_skewt = function(data, pres, temp, skew = 45, plim = NULL, pline = NULL, tlim = NULL, tline = NULL, ...){
  #Check ####
  if(weather2:::sys_hp_hasArg(value = data, value_name = "data")){return(data)}
  if(weather2:::sys_hp_hasArg(value = pres, value_name = "pres")){return(data)}
  if(weather2:::sys_hp_hasArg(value = temp, value_name = "temp")){return(data)}

  if(weather2::sys_ckc_dataframe(value = data, value_name = "data")){return(data)}
  if(weather2::sys_ckd_colexist(value = {{pres}}, value_name = "pres", data = data, data_name = "data")){return(data)}
  if(weather2::sys_ckd_colexist(value = {{temp}}, value_name = "temp", data = data, data_name = "data")){return(data)}
  if(weather2::sys_ckc_numint(value = skew, value_name = "skew")){return(data)}
  if(weather2::sys_ckl_length(list = skew, list_name = "skew", expected = 1L, mode = "==")){return(data)}
  if(weather2::sys_ckl_NumericValue(skew, list_name = "skew", expected = 89, mode = "<=")){return(data)}
  if(weather2::sys_ckl_NumericValue(skew, list_name = "skew", expected = 1, mode = ">=")){return(data)}

  #Start plotting ####
  ##Base plot ####
  plot = ggplot2::ggplot(data, ggplot2::aes(y = {{pres}}, x = {{temp}}, skew = {{skew}}, ...))+
    ggplot2::theme_void()+
    ggplot2::theme(panel.background = ggplot2::element_rect(color = "#000000"),
                   axis.ticks = ggplot2::element_line(colour = "#000000",
                                                      size = 0.25,
                                                      lineend = "square"),
                   axis.ticks.length = ggplot2::unit(3,
                                                     units = "pt"),
                   axis.text.y = ggplot2::element_text(size = 10,
                                                       hjust = 0.5,
                                                       vjust = 0.5),
                   axis.text.x = ggplot2::element_text(size = 10,
                                                       hjust = 0.5,
                                                       vjust = 0.5),
                   axis.title.x = ggplot2::element_text(size = 10),
                   axis.title.y = ggplot2::element_text(size = 10,
                                                        angle = 90),
                   plot.margin = ggplot2::margin(5,5,5,5))+
    ggplot2::labs(x = "Temperature",
                  y = "Pressure")

  ##Defining the limits of the pressure scale & add pressure lines ####
  if(is.null(plim)){
    pres_values = dplyr::select(data, y = {{pres}})$y
    plim = c(max(pres_values), min(pres_values))
  }
  if(is.null(pline)){pline = c(seq(1000, 100, -100), 50, 25, 10)}
  data_plim = tibble::tibble(pres = plim) %>%
    weather2:::calc_skewty(pres,
                           name_as = "y")

  data_pres = tibble::tibble(label = pline) %>%
    weather2:::calc_skewty(y = label,
                           name_as = "y")

  plot = plot +
    ggplot2::scale_y_continuous(limits = data_plim$y,
                                breaks = data_pres$y,
                                labels = data_pres$label,
                                expand = c(0, 0),
                                oob = scales::squish_infinite)+
    ggplot2::geom_abline(data = data_pres,
                         ggplot2::aes(intercept = y, slope = 0), color = "#AAAAAA", size = 0.25)

  ##Adding temperature lines ####
  if(is.null(tlim)){
    temp_values = dplyr::select(data, x = {{temp}})$x
    tlim = c(min(temp_values, na.rm = T), max(temp_values, na.rm = T))
  }
  if(is.null(tline)){
    tline_min = min(tlim, na.rm = T) - (min(tlim, na.rm = T)%%10)
    tline_max = max(tlim, na.rm = T) + (max(tlim, na.rm = T)%%10)
    tline = seq(tline_min-30, tline_max+30, 10)
  }

  data_tlim = tibble::tibble(temp = tlim,
                             pres = plim[1],
                             angle = skew) %>%
    weather2:::calc_skewtx(x = temp, y = pres, angle = angle, name_as = "x")

  data_temp = tibble::tibble(label = tline,
                             y1 = plim[1],
                             y2 = plim[2],
                             angle = skew) %>%
    weather2:::calc_skewt(x = label, y = y1, angle = angle, name_as = c("x1", "y1"), overwrite = T) %>%
    weather2:::calc_skewt(x = label, y = y2, angle = angle, name_as = c("x2", "y2"), overwrite = T) %>%
    dplyr::mutate(slope = (y2 - y1) / (x2 - x1),
                  y = y1 - (slope * x1))

  plot = plot +
    ggplot2::scale_x_continuous(limits = data_tlim$x,
                                breaks = data_temp$x1,
                                labels = data_temp$label,
                                expand = c(0.2, 0),
                                oob = scales::squish_infinite)+
    ggplot2::geom_abline(data = data_temp,
                         ggplot2::aes(intercept = y, slope = slope), color = "#AAAAAA", size = 0.25)
  return(plot)
}

