#' Plot rose graph
#'
#' @param data data frame containing data
#' @param drct column name of direction
#' @param value column name of value
#' @param sum column name of sum (optional)
#' @param breaks Interval between each break. Default as 45.
#' @param minor_breaks Interval between each minor break. Default as half of "breaks".
#'
#' @return
#' @export
#'
#' @examples draw_rose(data, drct, sped, time_int, breaks = 30, minor_breaks = 10)
draw_rose = function(data, drct, value, sum, breaks = 45, minor_breaks){
  #Give minor break intervals if not provided####
  if(missing(minor_breaks)){minor_breaks = breaks / 2}
  #extract if data is a Weather object
  if(class(data) == "weather"){data1 = data$data}
    else{data1 = data}

  #Start computing
  if(missing(sum)){
    colname_drct = dplyr::select(data1, {{drct}}) %>% colnames()
    colname_value = dplyr::select(data1, {{value}}) %>% colnames()

    data2 = dplyr::select(data1, {{drct}}, {{value}}) %>%
      dplyr::rename(drct = {{drct}},
                    value = {{value}}) %>%
      dplyr::mutate(drct = as.numeric(drct),
                    value = as.numeric(value)) %>%
      dplyr::group_by(drct, value) %>%
      tidyr::drop_na() %>%
      dplyr::summarise(count = dplyr::n())

    plot_rose = data2 %>%
      ggplot2::ggplot(ggplot2::aes(x = drct, y = count, fill = value))+
      ggplot2::geom_col()+
      ggplot2::theme_bw()+
      ggplot2::coord_polar()+
      ggplot2::scale_x_continuous(limits = c(0, 360),
                                  breaks = seq(0, 360-breaks, breaks),
                                  minor_breaks = seq(0, 360, minor_breaks))+
      ggplot2::labs(x = colname_drct,
                    y = "count",
                    fill = colname_value)
  }

  if(!missing(sum)){
    colname_drct = dplyr::select(data1, {{drct}}) %>% colnames()
    colname_value = dplyr::select(data1, {{value}}) %>% colnames()
    colname_sum = dplyr::select(data1, {{sum}}) %>% colnames()

    data2 = dplyr::select(data1, {{drct}}, {{value}}, {{sum}}) %>%
      dplyr::rename(drct = {{drct}},
                    value = {{value}},
                    sum = {{sum}}) %>%
      dplyr::mutate(drct = as.numeric(drct),
                    value = as.numeric(value),
                    sum = as.numeric(sum)) %>%
      dplyr::group_by(drct, value) %>%
      tidyr::drop_na() %>%
      dplyr::summarise(sum = base::sum(sum, na.rm = T))

    plot_rose = data2 %>%
      ggplot2::ggplot(ggplot2::aes(x = drct, y = sum, fill = value))+
      ggplot2::geom_col()+
      ggplot2::theme_bw()+
      ggplot2::coord_polar()+
      ggplot2::scale_x_continuous(limits = c(0, 360),
                                  breaks = seq(0, 360-breaks, breaks),
                                  minor_breaks = seq(0, 360, minor_breaks))+
      ggplot2::labs(x = colname_drct,
                    y = colname_sum,
                    fill = colname_value)
  }
  return(plot_rose)
}
