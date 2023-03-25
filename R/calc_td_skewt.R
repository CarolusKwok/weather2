#' Calculate Skew-T plot coordinates
#'
#' @param data
#' @param x
#' @param y
#' @param mode
#' @param angle
#'
#' @return
#' @export
#'
#' @examples
calc_skewt = function(data, x, y, mode = NULL, angle = 45){
  #Check
  if(weather2::sys_ckc_dataframe(value = data, value_name = "data")){return(data)}
  if(weather2::sys_ckc_character(value = x, value_name = "x")){return(data)}
  if(weather2::sys_ckc_character(value = y, value_name = "y")){return(data)}
  for(i in x){
    if(weather2::sys_ckd_colexist(value = {{i}}, value_name = "x", data = data, data_name = "data")){return(data)}
  }
  for(i in y){
    if(weather2::sys_ckd_colexist(value = {{i}}, value_name = "y", data = data, data_name = "data")){return(data)}
    data0 = dplyr::select(data, x = {{i}})$x
    if(weather2::sys_ckl_NumericValue(list = data0, list_name = i, expected = 0, mode = ">")){return(data)}
  }

  if(!is.null(mode)){
    if(weather2::sys_ckl_length(list = mode, list_name = "mode", expected = 1L, mode = "<=")){return(data)}
    if(weather2::sys_ckl_ItemIn(list = mode, list_name = "mode", expected = c("x", "y"), mode = "in")){return(data)}
  }
  if(weather2::sys_ckc_numeric(angle, "angle")){return(data)}

  #Calculate
  if(is.null(mode)){
    if(length(x) >= length(y)){mode = "x"}
    if(length(x) < length(y)){mode = "y"}
  }
  if(mode == "x"){
    for(name in x){
      name_x = paste0(name, "_stx")
      name_y = paste0(name, "_sty")
      name_x = weather2::sys_tld_GetColname({{name_x}}, data)
      name_y = weather2::sys_tld_GetColname({{name_y}}, data)

      data_temp = dplyr::select(data,
                                x = {{name}},
                                y = {{y}}) %>%
        weather2:::calc_skewtx(x = x, y = y, name_as = "XXX", angle = angle) %>%
        weather2:::calc_skewty(y = y, name_as = "YYY")

      data = dplyr::mutate(data,
                           "{name_x}" := data_temp$XXX,
                           "{name_y}" := data_temp$YYY)
    }
  }
  if(mode == "y"){
    for(name in y){
      name_x = paste0(name, "_stx")
      name_y = paste0(name, "_sty")
      name_x = weather2::sys_tld_GetColname({{name_x}}, data)
      name_y = weather2::sys_tld_GetColname({{name_y}}, data)

      data_temp = dplyr::select(data,
                                x = {{x}},
                                y = {{name}}) %>%
        weather2:::calc_skewtx(x = x, y = y, name_as = "XXX", angle = angle) %>%
        weather2:::calc_skewty(y = y, name_as = "YYY")

      data = dplyr::mutate(data,
                           "{name_x}" := data_temp$XXX,
                           "{name_y}" := data_temp$YYY)
    }
  }
  return(data)
}



#' Calculate Skew-T plot x-coordinate
#'
#' @param x
#' @param y
#' @param angle
#' @param data
#' @param name_as
#'
#' @keywords internal
#'
#' @examples
calc_skewtx = function(data, x, y, name_as = NULL, angle = 45){
  if(is.null(name_as)){
    name_as = paste0(weather2:::sys_hp_sym2chr({{y}}), "_stx")
    name_as = weather2::sys_tld_GetColname({{name_as}}, data)
  }

  angle = angle * pi / 180

  data1 = dplyr::select(data,
                        x = {{x}},
                        y = {{y}}) %>%
    weather2:::calc_skewty(y, name_as = "YYY") %>%
    dplyr::mutate(angle = angle,
                  XXX = tan(angle) * YYY + x * 0.05)

  data = dplyr::mutate(data,
                       "{name_as}" := data1$XXX)
  #x = tan(angle) * weather2::calc_skewty(y) + x * 0.5
  return(data)
}

#' Calculate Skew-T plot y-coordinate
#'
#' @param y
#' @param data
#' @param name_as
#'
#' @keywords internal
#'
#' @examples
calc_skewty = function(data, y, name_as = NULL){
  if(is.null(name_as)){
    name_as = paste0(weather2:::sys_hp_sym2chr({{y}}), "_sty")
    name_as = weather2::sys_tld_GetColname({{name_as}}, data)
  }

  data1 = dplyr::select(data, y = {{y}}) %>%
    dplyr::mutate(YYY = -log10(y) + 4)

  data = dplyr::mutate(data, "{name_as}" := data1$YYY)
  return(data)
}
