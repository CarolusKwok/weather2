#' Draws a blank Skew-T plot
#'
#'
#' THIS IS SCRAPPED DO NOT USE
#'
#'
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
                         colors,
                         groups,
                         pres    = c(seq(1000, 100, -100), 50, 25, 10),
                         temp    = seq(-120, 50, 10),
                         dry_adi = seq(-100, 100, 10),
                         wet_adi = seq(-100, 100, 10),
                         isohume = seq(-100, 100, 10),
                         angle = 45,
                         pres_lim = NULL,
                         temp_lim = NULL){
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

  #Finding limits ####
  if(is.null(pres_lim)){
    data_pres_lim = data %>%
      dplyr::select(x = pres_col)
    pres_lim = c(max(data_pres_lim$x), min(data_pres_lim$x))
    rm(data_pres_lim)
  }
  if(is.null(temp_lim)){
    data_temp_lim = data %>%
      dplyr::select(x = params)
    min_data_temp_lim = lapply(X = data_temp_lim, min, na.rm = T) %>%
      unlist() %>%
      min(na.rm = T)
    max_data_temp_lim = lapply(X = data_temp_lim, max, na.rm = T) %>%
      unlist() %>%
      max(na.rm = T)
    temp_lim = c((min_data_temp_lim - 5), (max_data_temp_lim + 5))

    rm(data_temp_lim, min_data_temp_lim, max_data_temp_lim)
  }
  data_lim_pres = tibble::tibble(y = pres_lim, x = 0) %>%
    weather2::calc_skewt(x = "x", y = "y", angle = angle, mode = "y")

  data_lim_temp = tibble::tibble(x = temp_lim, y = max(pres_lim)) %>%
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

  ##Adi Lines ####
  data_dry = adi_process1(adi = dry_adi, pres_lim = pres_lim) %>%
    weather2::calc_adi_dry(pres1 = pres1, pres2 = pres2, temp1 = temp1, overwrite = T) %>%
    adi_process2(angle = angle)

  data_wet = adi_process1(adi = wet_adi, pres_lim = pres_lim) %>%
    weather2::calc_adi_wet(pres1 = pres1, temp1 = temp1, pres2 = pres2) %>%
    adi_process2(angle = angle)

  data_hum = adi_process1(adi = isohume, pres_lim = pres_lim) %>%
    weather2::calc_isohume(pres1 = pres1, temp1 = temp1, pres2 = pres2, overwrite = T) %>%
    adi_process2(angle = angle)



  #Data processing ####
  print("Fuck")
  data_sel_y = tibble::tibble(.rows = nrow(data))
  data_sel_x = tibble::tibble(.rows = nrow(data))
  data_sel_g = tibble::tibble(.rows = nrow(data))

  if(hasArg(pres_col)){
    data_sel_y = dplyr::select(data,
                               y = pres_col)
  }
  if(hasArg(params)){
    data_sel_x = dplyr::select(data,
                               x = params)
  }
  if(hasArg(groups)){
    data_sel_g = dplyr::select(data,
                               g = groups)
  }

  data_sel = dplyr::bind_cols(data_sel_y,
                              data_sel_x,
                              data_sel_g) %>%
    weather2::calc_skewt(y = "y", x = paste0("x", 1:length(params)), angle = angle) %>%
    dplyr::mutate(label = "")

  print("YOLO")
  if(!hasArg(groups)){
    data_sel = data_sel
  } else if(length(groups) == 1){
    data_sel = dplyr::mutate(data_sel,
                             label = g)
  } else if (length(groups) > 1) {
    for(i in 1:length(groups)){
      data_sel = dplyr::mutate(data_sel,
                               label = paste(label, !!rlang::sym(paste0("g", i))))
    }
  }

  print(data_sel)
  #Base plot itself ####
  plot = ggplot2::ggplot(data_sel)+
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

  #Additional lines ####
  for(i in 1:length(params)){
    plot = plot +
      ggplot2::geom_path(ggplot2::aes(x = !!rlang::sym(paste0("x", i, "_stx")),
                                      y = !!rlang::sym(paste0("x", i, "_sty"))),
                         color = colors[[i]])
  }

  #Facet wrap for groups ####
  if(hasArg(groups)){
    plot = plot +
      ggplot2::facet_wrap(ggplot2::vars(label))
  }

  #Return the plot ####
  return(plot)
}
