#' Compute coordinates for tropical cyclone wind rose
#'
#' The method for calculating the storm rose via `weather2::geom_tc_rose`
#'
#' @param mapping Set of aesthetic mappings created by aes(). If specified and inherit.aes = TRUE (the default), it is combined with the default mapping at the top level of the plot. You must supply mapping if there is no plot mapping.
#' @param data The data to be displayed in this layer. There are three options:
#' * If NULL, the default, the data is inherited from the plot data as specified in the call to ggplot().
#' * A data.frame, or other object, will override the plot data. All objects will be fortified to produce a data frame. See fortify() for which variables will be created.
#' * A function will be called with a single argument, the plot data. The return value must be a data.frame, and will be used as the layer data. A function can be created from a formula (e.g. ~ head(.x, 10)).
#' @param geom The geometric object to use to display the data, either as a ggproto Geom subclass or as a string naming the geom stripped of the geom_ prefix (e.g. "point" rather than "geom_point")
#' @param position Position adjustment, either as a string naming the adjustment (e.g. "jitter" to use position_jitter), or the result of a call to a position adjustment function. Use the latter if you need to change the settings of the adjustment.
#' @param na.rm If FALSE, the default, missing values are removed with a warning. If TRUE, missing values are silently removed.
#' @param show.legend logical. Should this layer be included in the legends? NA, the default, includes if any aesthetics are mapped. FALSE never includes, and TRUE always includes. It can also be a named logical vector to finely select the aesthetics to display.
#' @param inherit.aes If FALSE, overrides the default aesthetics, rather than combining with them. This is most useful for helper functions that define both data and aesthetics and shouldn't inherit behaviour from the default plot specification, e.g. borders().
#' @param ... Other arguments passed on to layer(). These are often aesthetics, used to set an aesthetic to a fixed value, like colour = "red" or size = 3. They may also be parameters to the paired geom/stat.
#'
#' @return
#' @export
#'
#' @examples
stat_tc_rose = function(mapping = NULL, data = NULL, geom = "polygon",
                        position = "identity", na.rm = FALSE, show.legend = NA,
                        inherit.aes = TRUE, ...) {
  ggplot2::layer(stat = weather2::StatTcRose, data = data, mapping = mapping, geom = geom,
                 position = position, show.legend = show.legend, inherit.aes = inherit.aes,
                 params = list(na.rm = na.rm, ...))
}


#' @rdname ggplot2-ggproto
#' @format NULL
#' @usage NULL
#' @export
StatTcRose = ggplot2::ggproto("StatTcRose", ggplot2::Stat,
                              required_aes = c("x", "y","rne", "rse", "rnw", "rsw"),
                              compute_group = function(data, scales){
                                grid = tibble::tibble(x = data$x[1],
                                                      y = data$y[1],
                                                      rne = data$rne[1],
                                                      rse = data$rse[1],
                                                      rsw = data$rsw[1],
                                                      rnw = data$rnw[1]) %>%
                                  tidyr::pivot_longer(cols = c("rne", "rse", "rsw", "rnw")) %>%
                                  tidyr::expand_grid(tibble::tibble(deg = seq(0, 360, 11.25))) %>%
                                  dplyr::filter((name == "rne" & 000 <= deg & deg <= 090) |
                                                (name == "rse" & 090 <= deg & deg <= 180) |
                                                (name == "rsw" & 180 <= deg & deg <= 270) |
                                                (name == "rnw" & 270 <= deg & deg <= 360)) %>%
                                  weather2::calc_geo_dest(lon1 = x, lat1 = y, brn1 = deg, dis = value,
                                                          name_as = c("x", "y", "brn2", "error"), overwrite = T) %>%
                                  dplyr::select(x, y)
                                return(grid)})


#' Plot tropical cyclone wind rose
#'
#' Plots the wind rose based on the NE, SE, SW, NW radius of wind strength, defined by `rne`, `rse`, `rsw`, `rnw`. Units as meter.
#'
#' @param mapping Set of aesthetic mappings created by aes(). If specified and inherit.aes = TRUE (the default), it is combined with the default mapping at the top level of the plot. You must supply mapping if there is no plot mapping.
#' @param data The data to be displayed in this layer. There are three options:
#' * If NULL, the default, the data is inherited from the plot data as specified in the call to ggplot().
#' * A data.frame, or other object, will override the plot data. All objects will be fortified to produce a data frame. See fortify() for which variables will be created.
#' * A function will be called with a single argument, the plot data. The return value must be a data.frame, and will be used as the layer data. A function can be created from a formula (e.g. ~ head(.x, 10)).
#' @param position Position adjustment, either as a string naming the adjustment (e.g. "jitter" to use position_jitter), or the result of a call to a position adjustment function. Use the latter if you need to change the settings of the adjustment.
#' @param na.rm If FALSE, the default, missing values are removed with a warning. If TRUE, missing values are silently removed.
#' @param show.legend logical. Should this layer be included in the legends? NA, the default, includes if any aesthetics are mapped. FALSE never includes, and TRUE always includes. It can also be a named logical vector to finely select the aesthetics to display.
#' @param inherit.aes If FALSE, overrides the default aesthetics, rather than combining with them. This is most useful for helper functions that define both data and aesthetics and shouldn't inherit behaviour from the default plot specification, e.g. borders().
#' @param ... Other arguments passed on to layer(). These are often aesthetics, used to set an aesthetic to a fixed value, like colour = "red" or size = 3. They may also be parameters to the paired geom/stat.
#'
#' @return
#' @export
#'
#' @examples
geom_tc_rose = function(mapping = NULL, data = NULL,
                        position = "identity", na.rm = FALSE,
                        show.legend = NA, inherit.aes = TRUE, ...){
  ggplot2::layer(stat = weather2::StatTcRose, geom = weather2::GeomTcRose,
                 data = data, mapping = mapping, position = position,
                 show.legend = show.legend, inherit.aes = inherit.aes,
                 params = list(na.rm = na.rm, ...))
}

#' @rdname ggplot2-ggproto
#' @format NULL
#' @usage NULL
#' @export
GeomTcRose <- ggplot2::ggproto("GeomTcRose", ggplot2::GeomPolygon,
                               default_aes = ggplot2::aes(colour = "black",
                               fill = NA, linetype = 1,
                               size = 0.5, alpha = 0.5))
