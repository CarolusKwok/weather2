#' Plot atmospheric sounding data
#'
#' @param data Dataframe or Weather object of atmospheric sounding data
#' @param pres column name of atmospheric pressure, must be provided
#' @param temp column name of air temperature, must be provided
#' @param dwpt column name of dew point
#' @param frpt column name of frost point
#' @param drct column name of wind direction
#' @param sped column name of wind speed
#' @param hght column name for geopotential height
#' @param frpt column name of height
#' @param thta column name of potential temperature
#' @param thte column name of equivalent potential temperature
#' @param thtv column name of virtual potential temperature
#' @param log log status of the y-axis. Only accept logic values (T/F). T for log-10 scale. F for linear scale.
#' @param ybreak Breaks for the y-axis.
#' @param minor_ybreak Minor breaks for the y-axis.
#' @param xbreak Breaks for the x-axis.
#' @param minor_xbreak Minor breaks for the x-axis.
#' @param sspan Size of the secondary x-axis.
#' @param sbreak Breaks for the secondary x-axis.
#'
#' @return
#' @export
#'
#' @examples
draw_asnd = function(data, pres, temp, dwpt, frpt,
                     drct, sped, hght, thta, thte, thtv,
                     log = T,
                     ybreak = seq(100, 1000, 100), minor_ybreak = seq(100, 1000, 50),
                     xbreak = seq(-100, +60, 10), minor_xbreak = seq(-100, 60, 5),
                     sspan = 0.2, sbreak = seq(0, 360, 90)){
  ##### Check input #####
  if(missing(data)){
    cli::cli_text("Error: {.var data} must be filled")
    cli::cli_bullets(c("x" = "You haven't supply anything"))
    return(invisible())
  }
  if(!missing(data) & !(is.data.frame(data)) & (class(data) != "weather")){
    cli::cli_text("Error: {.var data} must be a Dataframe or a Weather object")
    cli::cli_bullets(c("x" = "You supplied it with {class(data)}"))
    return(invisible())
  }
  #Create data1 ####
  if(class(data) == "weather"){data1 = data$data} else{data1 = data}

  #Continue to check
  if(missing(pres)){
    cli::cli_text("Error: {.var pres} must be filled")
    cli::cli_bullets(c("x" = "You haven't supply anything"))
    return(invisible())
  }
  if(!missing(pres)){
    check = tryCatch(dplyr::select(data1, {{pres}}), error = function(e){NA})
    if(is.na(check[1,1])){
      cli::cli_text("Error: {.var pres} is not within the dataframe")
      cli::cli_bullets(c("x" = "{.var data} consist of {colnames(data1)}"))
      return(invisible())
    }
    check = dplyr::select(data1, {{pres}})
    check = check[[1]]
    if(class(check)[[1]] != "numeric"){
      cli::cli_text("Error: {.var pres} must be numeric")
      cli::cli_bullets(c("x" = "You supplied it with {class(check)}"))
      return(invisible())
    }
  }
  if(log != T & log != F){
    cli::cli_text("Error: {.var log} must be logical (T/F)")
    cli::cli_bullets(c("x" = "You supplied it with {class(log)}"))
    return(invisible())
  }
  if(min(ybreak) <= 0){
    cli::cli_text("Error: All number within list {.var ybreak} must be larger than 0.")
    cli::cli_bullets(c("x" = "You supplied it with numbers smaller than 0."))
    return(invisible())
  }
  if(sspan > 1 | sspan <= 0){
    cli::cli_text("Error: {.var sspan} must be within 0 & 1.")
    cli::cli_bullets(c("x" = "You supplied it with number {sspan}."))
    return(invisible())
  }
  if(!missing(hght) | !missing(drct) | !missing(sped) | !missing(thta) | !missing(thte) | !missing(thtv)){
    if((missing(drct) & !missing(sped)) | (!missing(drct) & missing(sped))){
      cli::cli_text("Error: {.var drct} or {.var sped} must be supplied")
      cli::cli_bullets(c("x" = "You only supplied one of them, and missed the other one"))
      return(invisible())
    }
    check = 0
    if(!missing(hght)){check = check + 1}
    if(!missing(drct) & !missing(sped)){check = check + 1}
    if(!missing(thta) | !missing(thte) | !missing(thtv)){check = check + 1}
    if(check > 1){
      cli::cli_text("Error: You can only choose one set of variables for secondary axis.")
      cli::cli_bullets(c("x" = "You supplied with more than one set, choose of the below",
                         "*" = "{.var hght}",
                         "*" = "{.var drct} and {.var sped}",
                         "*" = "{.var thta} or {.var thte} or {.var thtv}"))
      return(invisible())
    }
  }




  ##### Plotting the sounding #####
  plot_asnd = ggplot2::ggplot(data = data1, ggplot2::aes(x = 1, y = {{pres}}))+
    ggplot2::coord_cartesian(ylim = c(max({{ybreak}}), min({{ybreak}})),
                             xlim = c(min({{xbreak}}), max({{xbreak}})),
                             expand = F)+
    ggplot2::labs(y = "Pressure", x = "Temperature")+
    ggplot2::theme_bw()

  if(log == T){
     plot_asnd = plot_asnd +
       ggplot2::scale_y_continuous(breaks = {{ybreak}}, trans = "log10")+
       ggplot2::geom_hline(yintercept = {{minor_ybreak}}, color = "#EBEBEB")
   } else if(log == F){
     plot_asnd = plot_asnd +
       ggplot2::scale_y_continuous(breaks = {{ybreak}}, minor_breaks = {{minor_ybreak}})
   }
  if(!missing(temp)){
    plot_asnd = plot_asnd +
      ggplot2::geom_path(ggplot2::aes(x = {{temp}}, y = {{pres}}), color = "#FF0000", size = 0.5)+
      ggplot2::geom_point(ggplot2::aes(x = {{temp}}, y = {{pres}}), color = "#FF0000", size = 1)+
      ggplot2::scale_x_continuous(breaks = {{xbreak}}, name = "Temperature")
  }
  if(!missing(dwpt)){
    plot_asnd = plot_asnd +
      ggplot2::geom_path(ggplot2::aes(x = {{dwpt}}, y = {{pres}}), color = "#0000FF", size = 0.5)+
      ggplot2::geom_point(ggplot2::aes(x = {{dwpt}}, y = {{pres}}), color = "#0000FF", size = 1)+
      ggplot2::scale_x_continuous(breaks = {{xbreak}}, name = "Temperature")
  }
  if(!missing(frpt)){
    plot_asnd = plot_asnd +
      ggplot2::geom_path(ggplot2::aes(x = {{frpt}}, y = {{pres}}), color = "#AAAAAA", size = 0.5)+
      ggplot2::geom_point(ggplot2::aes(x = {{frpt}}, y = {{pres}}), color = "#AAAAAA", size = 0.75)+
      ggplot2::scale_x_continuous(breaks = {{xbreak}}, name = "Temperature")
  }

  ##### Prepwork using the second x axis #####
  if((!missing(drct) & !missing(sped)) | !missing(hght) | !missing(thta) | !missing(thte)  | !missing(thtv)){
    x_range = (max(xbreak) - min(xbreak)) * sspan
  }

  ##### Plotting data using the second x axis #####
  if(!missing(drct) & !missing(sped)){
    plot_asnd = plot_asnd +
      ggplot2::geom_point(ggplot2::aes(x = (({{drct}} / 360 - 1) * x_range) + max(xbreak),
                              y = {{pres}}, color = {{sped}}), size = 0.75) +
      ggplot2::geom_path(ggplot2::aes(x = (({{drct}} / 360 - 1) * x_range) + max(xbreak),
                             y = {{pres}}, color = {{sped}}), size = 1)+
      ggplot2::scale_x_continuous(breaks = {{xbreak}},
                                  sec.axis = ggplot2::sec_axis(trans = ~ ((((. - max(xbreak)) / x_range) + 1) * 360),
                                                               breaks = sbreak, name = "Wind Direction (°)"), name = "Temperature")+
      ggplot2::scale_color_distiller(palette = "Spectral", name = "Wind\nSpeed") +
      ggplot2::theme(axis.text.x.top = ggplot2::element_text(angle = 90, hjust = 0.5, vjust = 0.5))
  }
  if(!missing(hght)){
    data2 = dplyr::filter(data1, {{pres}} >= min(ybreak) & {{pres}} <= max(ybreak))
    x_min = dplyr::summarise(data2, min({{hght}}))$`min(hght)`[1]
    x_max = dplyr::summarise(data2, max({{hght}}))$`max(hght)`[1]
    x_len = x_max - x_min

    plot_asnd = plot_asnd +
      ggplot2::geom_point(ggplot2::aes(x = ((({{hght}} - x_min) / x_len - 1) * x_range) + max({{xbreak}}),
                                       y = {{pres}}), color = "#000000", size = 1)+
      ggplot2::geom_path(ggplot2::aes(x = ((({{hght}} - x_min) / x_len - 1) * x_range) + max({{xbreak}}),
                                      y = {{pres}}), color = "#000000", size = 0.5)+
      ggplot2::scale_x_continuous(breaks = {{xbreak}}, name = "Temperature",
                                  sec.axis = ggplot2::sec_axis(trans = ~((((. - max({{xbreak}})) / x_range) + 1) * x_len) + x_min,
                                                               breaks = sbreak, name = "Geopotential Height"))+
      ggplot2::theme(axis.text.x.top = ggplot2::element_text(angle = 90, hjust = 0.5, vjust = 0.5))
  }
  if(!missing(thta) | !missing(thte) | !missing(thtv)){
    data2 = dplyr::filter(data1, {{pres}} >= min(ybreak) & {{pres}} <= max(ybreak))

    ##### Find the smallest and large theta value#####
    if(!missing(thta)){
      thta_min = dplyr::summarise(data2, min({{thta}}))$`min(thta)`[1]
      thta_max = dplyr::summarise(data2, max({{thta}}))$`max(thta)`[1]
    } else{
      thta_min = NA_real_
      thta_max = NA_real_}
    if(!missing(thte)){
      thte_min = dplyr::summarise(data2, min({{thte}}))$`min(thte)`[1]
      thte_max = dplyr::summarise(data2, max({{thte}}))$`max(thte)`[1]
    } else{
      thte_min = NA_real_
      thte_max = NA_real_}
    if(!missing(thtv)){
      thtv_min = dplyr::summarise(data2, min({{thtv}}))$`min(thtv)`[1]
      thtv_max = dplyr::summarise(data2, max({{thtv}}))$`max(thtv)`[1]
    } else{
      thtv_min = NA_real_
      thtv_max = NA_real_}


    x_max = max(thta_max, thte_max, thtv_max, na.rm = T)
    x_min = min(thta_min, thte_min, thtv_min, na.rm = T)
    x_len = x_max - x_min

    ##### Plot thetas #####
    if(!missing(thta)){
      plot_asnd = plot_asnd +
        ggplot2::geom_point(ggplot2::aes(x = ((({{thta}} - x_min) / x_len - 1) * x_range) + max({{xbreak}}),
                                         y = {{pres}}), color = "#009F6B", size = 1)+
        ggplot2::geom_path(ggplot2::aes(x = ((({{thta}} - x_min) / x_len - 1) * x_range) + max({{xbreak}}),
                                        y = {{pres}}), color = "#009F6B", size = 0.5)
    }
    if(!missing(thte)){
      plot_asnd = plot_asnd +
        ggplot2::geom_point(ggplot2::aes(x = ((({{thte}} - x_min) / x_len - 1) * x_range) + max({{xbreak}}),
                                         y = {{pres}}), color = "#fe8d09", size = 1)+
        ggplot2::geom_path(ggplot2::aes(x = ((({{thte}} - x_min) / x_len - 1) * x_range) + max({{xbreak}}),
                                        y = {{pres}}), color = "#fe8d09", size = 0.5)
    }
    if(!missing(thtv)){
      plot_asnd = plot_asnd +
        ggplot2::geom_point(ggplot2::aes(x = ((({{thtv}} - x_min) / x_len - 1) * x_range) + max({{xbreak}}),
                                         y = {{pres}}), color = "#A020F0", size = 1)+
        ggplot2::geom_path(ggplot2::aes(x = ((({{thtv}} - x_min) / x_len - 1) * x_range) + max({{xbreak}}),
                                        y = {{pres}}), color = "#A020F0", size = 0.5)
    }

    plot_asnd = plot_asnd +
      ggplot2::scale_x_continuous(breaks = {{xbreak}}, name = "Temperature",
                                  sec.axis = ggplot2::sec_axis(trans = ~((((. - max({{xbreak}})) / x_range) + 1) * x_len) + x_min,
                                                               breaks = sbreak, name = "Potential Temperature"))+
      ggplot2::theme(axis.text.x.top = ggplot2::element_text(angle = 90, hjust = 0.5, vjust = 0.5))
  }

  return(plot_asnd)
}
