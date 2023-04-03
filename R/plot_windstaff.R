#' Draws the base plot of a wind staff
#'
#' @param data Data frame itself
#' @param pres Column name of the pressure column
#' @param drct Column name of the direction column
#' @param sped Column name of the speed column
#' @param plim Limit of the pressure scale. Default as `NULL`, which will automatically detect the maximum and minimum pressure provided in the dataframe.
#' @param pline Values of the Isobars. Default as `NULL`, which will plot the isobars at 1000, 900 ... , 200, 100, 50, 25, and 10.
#' @param dlim Limit of the direction scale. Default as `NULL`, which will automatically assume the minimum and maximum degrees of wind direction to be 0 and 360.
#' @param dline Values of the direction lines. Default as `NULL`, which will be split the `dlim` by 45 degrees each.
#' @param ...
#'
#' @return
#' @export
#'
#' @examples
plot_windstaff = function(data, pres, drct, sped,
                          plim = NULL, pline = NULL, dlim = NULL, dline = NULL, ...){
  #Check ####
  if(weather2:::sys_hp_hasArg(value = data, value_name = "data")){return(data)}
  if(weather2:::sys_hp_hasArg(value = pres, value_name = "pres")){return(data)}
  if(weather2:::sys_hp_hasArg(value = drct, value_name = "drct")){return(data)}
  if(weather2:::sys_hp_hasArg(value = sped, value_name = "sped")){return(data)}

  if(weather2::sys_ckc_dataframe(value = data, value_name = "data")){return(data)}
  if(weather2::sys_ckd_colexist(value = {{pres}}, value_name = "pres", data = data, data_name = "data")){return(data)}
  if(weather2::sys_ckd_colexist(value = {{drct}}, value_name = "drct", data = data, data_name = "data")){return(data)}
  if(weather2::sys_ckd_colexist(value = {{sped}}, value_name = "sped", data = data, data_name = "data")){return(data)}

  #Graph the base plot ####
  plot = ggplot2::ggplot(data, ggplot2::aes(y = {{pres}}, x = {{drct}}, color = {{sped}}, ...))+
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
                   axis.title.x = ggplot2::element_text(size = 10,
                                                        face = "bold"),
                   axis.title.y = ggplot2::element_text(size = 10,
                                                        angle = 90,
                                                        face = "bold"),
                   plot.margin = ggplot2::margin(5,5,5,5))+
    ggplot2::labs(x = "Temperature",
                  y = "Pressure")+
    ggplot2::scale_color_gradient(low = "#0000FF", high = "#FF0000")
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
  ##Adding Direction lines ####
  if(is.null(dlim)){
    drct_values = seq(0, 360, 45)
    dlim = c(min(drct_values, na.rm = T), max(drct_values, na.rm = T))
  }
  if(is.null(dline)){
    dline_min = min(dlim, na.rm = T) - (min(dlim, na.rm = T)%%45)
    dline_max = max(dlim, na.rm = T) + (max(dlim, na.rm = T)%%45)
    dline = seq(dline_min, dline_max, 45)
  }

  data_dlim = tibble::tibble(drct = dlim) %>%
    weather2:::calc_emax(x = drct, name_as = "x")

  data_drct = tibble::tibble(label = dline) %>%
    weather2:::calc_emax(x = label, name_as = "x")

  plot = plot +
    ggplot2::scale_x_continuous(limits = data_dlim$x,
                                breaks = data_drct$x,
                                labels = data_drct$label,
                                expand = c(0, 0),
                                oob = scales::squish_infinite)+
    ggplot2::geom_vline(data = data_drct,
                        ggplot2::aes(xintercept = x), color = "#AAAAAA", size = 0.25)

  #Return plot ####
  return(plot)
}
