#' Plot sounding hodograph
#'
#' __NOTE: This is to-be-depreciated soon! Use the to-be new function `plot_hodo` instead__
#'
#'
#' @param data Dataframe or Weather object of atmospheric sounding data
#' @param uwnd column name of u-wind vector
#' @param vwnd column name of v-wind vector
#' @param pres column name of pressure
#' @param hght column name of height
#' @param color parameter for the hodograph line color. "hght" for hodograph line to be based on height. "pres" for hodograph line to be based on pressure.
#' @param label_breaks position of label breaks. must provide "hght" or "pres"
#' @param label_text switch for texted label. Only accepts T/F values
#' @param seed Seed for the label text to avoid overlapping
#' @param breaks breaks for the x and y-axis
#' @param center parameter for centering the graph at 0. Accpets "both", "uwnd", "vwnd", and both "uwnd" and "vwnd" at the same time.
#' @param equal switch for equal scaling between x-axis and y-axis.
#'
#' @return A `ggplot2` based hodoplot
#' @export
#'
#' @examples draw_hodo(data, uwnd, vwnd, pres, hght, color = "pres", seed = 1, label_breaks = c("hght", seq(0, 20000, 10)), label_text = T)
draw_hodo = function(data, uwnd, vwnd, pres, hght, color = "hght",
                     label_breaks, label_text = F, seed = NA,
                     breaks = seq(-100, 100, 10), center = "both", equal = T){
  #Convert Weather object in data into dataframe ####
  if(class(data) == "weather"){
    data = data$data
  }
  ##### plotting correction ####
  min_uwnd = dplyr::summarise(data, a = min({{uwnd}}, na.rm = T))$a[1]
  max_uwnd = dplyr::summarise(data, a = max({{uwnd}}, na.rm = T))$a[1]
  min_vwnd = dplyr::summarise(data, a = min({{vwnd}}, na.rm = T))$a[1]
  max_vwnd = dplyr::summarise(data, a = max({{vwnd}}, na.rm = T))$a[1]

  ##### plot plot plot #####
  plot_hodo = ggplot2::ggplot(data, ggplot2::aes(x = {{uwnd}}, y = {{vwnd}}))+
    ggplot2::theme_bw()+
    ggplot2::scale_x_continuous(breaks = breaks, name = "U-Wind")+
    ggplot2::scale_y_continuous(breaks = breaks, name = "V-Wind")+
    ggplot2::geom_hline(yintercept = 0, linetype = "dashed")+
    ggplot2::geom_vline(xintercept = 0, linetype = "dashed")

  if((!missing(pres) & missing(hght)) | (!missing(pres) & !missing(hght) & color == "pres")){
    plot_hodo = plot_hodo +
      ggplot2::geom_path(size = 0.75, ggplot2::aes(color = {{pres}}))+
      ggplot2::scale_color_gradientn(colours = c("#FF0000", "#00BB00", "#0000FF"), name = "Pressure")
  }
  if((missing(pres) & !missing(hght)) | (!missing(pres) & !missing(hght) & color == "hght")){
    plot_hodo = plot_hodo +
      ggplot2::geom_path(size = 0.75, ggplot2::aes(color = {{hght}}))+
      ggplot2::scale_color_gradientn(colours = c("#FF0000", "#00BB00", "#0000FF"), name = "Height")
  }
  ##### Add labels #####
  if(!missing(label_breaks)){
    if("pres" %in% label_breaks){
      filter_list = dplyr::select(data, {{pres}})[[1]] %in% label_breaks
      labels = dplyr::mutate(data, label = filter_list) %>% dplyr::filter(label == T)
      if(color == "hght"){
        plot_hodo = plot_hodo +
          ggplot2::geom_point(data = labels, ggplot2::aes(color = {{hght}}))
      }
      if(color == "pres"){
        plot_hodo = plot_hodo +
          ggplot2::geom_point(data = labels, ggplot2::aes(color = {{pres}}))
      }
      if(label_text){
        plot_hodo = plot_hodo +
          ggrepel::geom_text_repel(data = labels, inherit.aes = F, force = 20, force_pull = 20, seed = {{seed}},
                                   ggplot2::aes(x = {{uwnd}}, y = {{vwnd}}, label = {{pres}}))
      }
    }

    if("hght" %in% label_breaks){
      filter_list = dplyr::select(data, {{hght}})[[1]] %in% label_breaks
      labels = dplyr::mutate(data, label = filter_list) %>% dplyr::filter(label == T)
      if(color == "hght"){
        plot_hodo = plot_hodo + ggplot2::geom_point(data = labels, ggplot2::aes(color = {{hght}}))
      }
      if(color == "pres"){
        plot_hodo = plot_hodo + ggplot2::geom_point(data = labels, ggplot2::aes(color = {{pres}}))
      }
      if(label_text){
        plot_hodo = plot_hodo +
          ggrepel::geom_text_repel(data = labels, inherit.aes = F, force = 20, force_pull = 20, seed = {{seed}},
                                   ggplot2::aes(x = {{uwnd}}, y = {{vwnd}}, label = {{hght}}))
      }
    }
  }

  ##### center the plots and check equal coord#####
  if(center == "both"){
    mag = max(abs(min_uwnd), abs(max_uwnd), abs(min_vwnd), abs(max_vwnd))
    if(equal == T){
      plot_hodo = plot_hodo + ggplot2::coord_equal(xlim = c(-mag, +mag), ylim = c(-mag, +mag))
    } else {
      plot_hodo = plot_hodo + ggplot2::coord_cartesian(xlim = c(-mag, +mag), ylim = c(-mag, +mag))}
  }

  if(center == "uwnd"){
    mag = max(abs(min_uwnd), abs(max_uwnd))
    if(equal == T){
      plot_hodo = plot_hodo + ggplot2::coord_equal(xlim = c(-mag, +mag))
    } else {
      plot_hodo = plot_hodo + ggplot2::coord_cartesian(xlim = c(-mag, +mag))}
  }

  if(center == "vwnd"){
    mag = max(abs(min_vwnd), abs(max_vwnd))
    if(equal == T){
      plot_hodo = plot_hodo + ggplot2::coord_equal(ylim = c(-mag, +mag))
    } else {
      plot_hodo = plot_hodo + ggplot2::coord_cartesian(ylim = c(-mag, +mag))}
  }
  if("uwnd" %in% center & "vwnd" %in% center){
    umag = max(abs(min_uwnd), abs(max_uwnd))
    vmag = max(abs(min_vwnd), abs(max_vwnd))
    if(equal == T){
      plot_hodo = plot_hodo + ggplot2::coord_equal(xlim = c(-umag, +umag), ylim = c(-vmag, +vmag))
    } else {
      plot_hodo = plot_hodo + ggplot2::coord_cartesian(xlim = c(-umag, +umag), ylim = c(-vmag, +vmag))}
  }

  return(plot_hodo)
}
