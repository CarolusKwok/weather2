#' Compute point coordinates for Skew-T plot points
#'
#' The method for calculating the points via `weather2::geom_skewt_point`
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
stat_skewt_point = function(mapping = NULL, data = NULL, geom = "point",
                            position = "identity", na.rm = FALSE, show.legend = NA,
                            inherit.aes = TRUE, ...) {
  ggplot2::layer(stat = weather2::StatSkewtPoint, data = data, mapping = mapping, geom = geom,
                 position = position, show.legend = show.legend, inherit.aes = inherit.aes,
                 params = list(na.rm = na.rm, ...))
}


#' @rdname ggplot2-ggproto
#' @format NULL
#' @usage NULL
#' @export
StatSkewtPoint = ggplot2::ggproto("StatSkewtPoint", ggplot2::Stat,
                                    required_aes = c("x", "y","skew"),
                                    compute_group = function(data, scales){
                                      grid = tibble::tibble(x = data$x,
                                                            y = data$y,
                                                            a = data$skew) %>%
                                        weather2::calc_skewt(x = x,
                                                             y = y,
                                                             angle = a,
                                                             name_as = c("x", "y"),
                                                             overwrite = T) %>%
                                        dplyr::select(x, y)
                                      return(grid)})



#' Plot points on a Skew-t plot
#'
#' Plots points based on the `weather2::calc_skewt` function, on a Skew-T plot
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
geom_skewt_point = function(mapping = NULL, data = NULL,
                            position = "identity", na.rm = FALSE,
                            show.legend = NA, inherit.aes = TRUE, ...){
  ggplot2::layer(stat = weather2::StatSkewtPoint, geom = ggplot2::GeomPoint,
                 data = data, mapping = mapping, position = position,
                 show.legend = show.legend, inherit.aes = inherit.aes,
                 params = list(na.rm = na.rm, ...))
}
