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
calc_skewt = function(data, x, y, angle, name_as = NULL, overwrite = F){
  #Check ####
  if(weather2::sys_ckc_dataframe(data, "data")){return(data)}
  if(weather2::sys_ckd_colexist(value = {{x}}, value_name = "x", data = data, data_name = "data")){return(data)}
  if(weather2::sys_ckd_colexist(value = {{y}}, value_name = "y", data = data, data_name = "data")){return(data)}
  if(weather2::sys_ckd_colexist(value = {{angle}}, value_name = "angle", data = data, data_name = "data")){return(data)}
  if(!is.null(name_as)){
    if(weather2::sys_ckl_length(name_as, "name_as", expected = 2L, mode = "==")){return(data)}
    if(weather2::sys_ckc_character(name_as, "name_as")){return(data)}
  }
  if(weather2::sys_ckc_logical(overwrite, value_name = "overwrite")){return(data)}

  #Calculate ####
  data1 = dplyr::select(data,
                        x = {{x}},
                        y = {{y}},
                        a = {{angle}}) %>%
    weather2:::calc_skewtx(x = x, y = y, angle = a, name_as = "x2") %>%
    weather2:::calc_skewty(y, name_as = "y2")

  #Return the data ####
  if(is.null(name_as)){
    name_as = weather2:::sys_hp_sym2chr({{x}})
    name_as = paste0(name_as, c("_stx", "_sty"))
  }
  name_x = name_as[1]
  name_y = name_as[2]
  if(!overwrite){
    name_x = weather2::sys_tld_GetColname({{name_x}}, data)
    name_y = weather2::sys_tld_GetColname({{name_y}}, data)
  }

  data = dplyr::mutate(data,
                       "{name_x}" := data1$x2,
                       "{name_y}" := data1$y2)
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
#'
#'
calc_skewtx = function(data, x, y, angle, name_as = NULL){
  if(is.null(name_as)){
    name_as = paste0(weather2:::sys_hp_sym2chr({{y}}), "_stx")
    name_as = weather2::sys_tld_GetColname({{name_as}}, data)
  }
  data1 = dplyr::select(data,
                        x = {{x}},
                        y = {{y}},
                        a = {{angle}}) %>%
    weather2:::calc_skewty(y, name_as = "YYY") %>%
    dplyr::mutate(a = a * pi / 180,
                  XXX = (tan(a) * YYY + x * 0.05 - 1))

  data = dplyr::mutate(data,
                       "{name_as}" := data1$XXX)
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
