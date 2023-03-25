#' Draws a blank Skew-T plot
#'
#' @param data
#' @param pres
#' @param temp
#' @param angle
#' @param size
#' @param pres_lim
#' @param temp_lim
#' @param dry_adi
#' @param wet_adi
#' @param isohume
#'
#' @return
#' @export
#'
#' @examples
draw_td_skewt = function(data,
                         pres_col,
                         params,
                         groups  = NULL,
                         pres    = c(seq(1000, 100, -100), 50, 25, 10),
                         temp    = seq(-120, 50, 10),
                         dry_adi = seq(-100, 100, 10),
                         wet_adi = seq(-100, 100, 10),
                         isohume = seq(-100, 100, 10),
                         angle = 45,
                         pres_lim = NULL,
                         temp_lim = c(-80, 50),
                         silent = F){
  #Preset functions ####
  adi_process1 = function(adi, pres_lim){
    data = tibble::tibble(temp1 = adi + 273.15,
                          pres1 = pres_lim[1]) %>%
      tidyr::expand_grid(tibble::tibble(pres2 = seq(min(pres_lim),
                                                    max(pres_lim),
                                                    1)))
    return(data)
  }
  adi_process2 = function(data, angle){
    data = data %>%
      dplyr::mutate(temp1 = temp1 - 273.15,
                    temp2 = temp2 - 273.15) %>%
      weather2::calc_skewt(x = "temp2", y = "pres2", angle = angle)
    return(data)
  }

  #Data processing
  data = weather2::calc_skewt(data = data, x = params, y = pres_col, angle = angle)
  if(!silent){
    cli::cli_alert_info("{.var draw_td_skewt}: Transformation complete.")
    cli::cli_bullets(text = c(" " = "Here are the columns you may use:"))
    cli::cli_text("{colnames(data)}")
  }

  #Finding limits ####
  if(is.null(pres_lim)){pres_lim = c(max(pres), min(pres))}
  if(is.null(temp_lim)){temp_lim = c(min(temp), max(temp))}
  data_lim_pres = tibble::tibble(y = pres_lim,
                                 x = 0) %>%
    weather2::calc_skewt(x = "x", y = "y", angle = angle, mode = "y")

  data_lim_temp = tibble::tibble(x = temp_lim,
                                 y = max(pres_lim)) %>%
    weather2::calc_skewt(x = "x", y = "y", angle = angle, mode = "x")

  #START PLOTTING ####
  ##Temperature and Pressure lines ####
  data_pres = tibble::tibble(pres = pres,
                             x1 = min(temp),
                             x2 = max(temp)) %>%
    weather2::calc_skewt(x = c("x1", "x2"), y = "pres", angle = angle) %>%
    dplyr::mutate(x1_stx = min(x1_stx),
                  x2_stx = max(x2_stx))
  data_temp = tibble::tibble(temp = temp,
                             y1 = pres_lim[2],
                             y2 = pres_lim[1]) %>%
    weather2::calc_skewt(x = "temp", y = c("y1", "y2"), angle = angle)

  data_label_pres = dplyr::select(data_pres, y = x1_sty, label = pres)

  data_label_temp = dplyr::select(data_temp, x = y2_stx, label = temp)

  data_box = tibble::tibble(ts = c(temp_lim[1], temp_lim[2]),
                            ps = c(pres_lim[1], pres_lim[1]),
                            pe = c(pres_lim[2], pres_lim[2])) %>%
    weather2::calc_skewt(x = "ts", y = c("ps", "pe"), angle = angle)

  ## Adi Lines ####
  data_dry = adi_process1(adi = dry_adi, pres_lim = pres_lim) %>%
    weather2::calc_adi_dry(pres1 = pres1, pres2 = pres2, temp1 = temp1, overwrite = T) %>%
    adi_process2(angle = angle)

  data_wet = adi_process1(adi = wet_adi, pres_lim = pres_lim) %>%
    weather2::calc_adi_wet(pres1 = pres1, temp1 = temp1, pres2 = pres2) %>%
    adi_process2(angle = angle)

  data_hum = adi_process1(adi = isohume, pres_lim = pres_lim) %>%
    weather2::calc_isohume(pres1 = pres1, temp1 = temp1, pres2 = pres2, overwrite = T) %>%
    adi_process2(angle = angle)


  #THE PLOT ITSELF ####
  plot = ggplot2::ggplot(data)+
    ggplot2::geom_segment(data = data_pres, ggplot2::aes(y = x1_sty,
                                                         yend = x2_sty,
                                                         x = x1_stx,
                                                         xend = x2_stx),
                          color = "#AAAAAA", size = 0.25)+
    ggplot2::geom_segment(data = data_temp, ggplot2::aes(y = y1_sty,
                                                         yend = y2_sty,
                                                         x = y1_stx,
                                                         xend = y2_stx),
                          color = "#AAAAAA", size = 0.25)+
    ggplot2::geom_segment(data = data_box, ggplot2::aes(x = ps_stx,
                                                        y = ps_sty,
                                                        xend = ps_stx,
                                                        yend = pe_sty))+
    ggplot2::geom_path(data = data_dry, ggplot2::aes(x = temp2_stx,
                                                     y = temp2_sty,
                                                     group = temp1),
                       size = 0.25, linetype = "dashed", color = "firebrick2")+
    ggplot2::geom_path(data = data_wet, ggplot2::aes(x = temp2_stx,
                                                     y = temp2_sty,
                                                     group = temp1),
                       size = 0.25, linetype = "dashed", color = "darkviolet")+
    ggplot2::geom_path(data = data_hum, ggplot2::aes(x = temp2_stx,
                                                     y = temp2_sty,
                                                     group = temp1),
                       size = 0.25, linetype = "dashed", color = "dodgerblue4")+
    ggplot2::scale_x_continuous(breaks = data_label_temp$x,
                                labels = data_label_temp$label,
                                expand = c(0, 0),
                                limits = data_lim_temp$x_stx,
                                oob = scales::squish_infinite)+
    ggplot2::scale_y_continuous(breaks = data_label_pres$y,
                                labels = data_label_pres$label,
                                expand = c(0, 0),
                                limits = data_lim_pres$y_sty,
                                oob = scales::squish_infinite)+
    ggplot2::theme_void()+
    ggplot2::theme(axis.ticks = ggplot2::element_line(colour = "#000000",
                                                      size = 0.25,
                                                      lineend = "square"),
                   axis.ticks.length = ggplot2::unit(3,
                                                     units = "pt"),
                   axis.text.y = ggplot2::element_text(size = 10),
                   axis.text.x = ggplot2::element_text(size = 10,
                                                       angle = 90,
                                                       hjust = 0.5,
                                                       vjust = 0.5),
                   axis.title.x = ggplot2::element_text(),
                   axis.title.y = ggplot2::element_text(angle = 90),
                   plot.margin = ggplot2::margin(5,5,5,5))+
    ggplot2::labs(x = "Temperature",
                  y = "Pressure")
  return(plot)
}
