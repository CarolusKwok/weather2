#' Draws the base plot of SkewT
#'
#' @param data Data frame itself
#' @param pres Column name of pressure
#' @param skew Skewness of the SkewT
#' @param ... Any other parameters to be past to `ggplot2::aes()`
#'
#' @return
#' @export
#'
#' @examples
plot_skewt = function(data, pres, skew = 45, ...){
  plot = ggplot2::ggplot(data, ggplot2::aes(y = {{pres}}, skew = {{skew}}))+
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
