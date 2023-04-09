#' Calculate Emagram plot coordinates
#'
#' Emagrams are one of the 5 meteorological thermo-diagram, which in essence is a log-P linear-T diagram.
#' This function calculates the emagram coordinates in a linear graph.
#'
#' @param data The dataframe iteself.
#' @param x The column name of x-coordinate to be plotted onto the emagram. Usually it's some form of "temperature" data.
#' @param y The column name of y-coordinate to be plotted onto the emagram. Usually it's some form of "pressure" data.
#' @param name_as Names of the 2 new columns, i.e. the x- and y-coordinate of the emagram. Default as `NULL`, i.e. the column name of `x` with a suffix of `"_emx"` and `"_emy"`. Keyword `"*del*"` is supported.
#' @param overwrite Let the new column names to overwrite the original dataframe columns? Default as `FALSE`.
#'
#' @return The same dataframe as `data`, with 2 additional column with column name defined in `name_as`.
#' @export
#'
#' @examples calc_ema(data, x = temp, y = pres)
calc_ema = function(data, x, y, name_as = NULL, overwrite = F){
  #Check ####
  if(weather2::sys_ckc_dataframe(data, "data")){return(data)}
  if(weather2::sys_ckd_colexist(value = {{x}}, value_name = "x", data = data, data_name = "data")){return()}
  if(weather2::sys_ckd_colexist(value = {{y}}, value_name = "y", data = data, data_name = "data")){return()}

  if(is.null(name_as)){
    name_as = weather2:::sys_hp_sym2chr({{x}})
    name_as = paste0(name_as, c("_emx", "_emy"))
  }
  if(weather2::sys_ckf_NameAsReturn(name_as = name_as,
                                    overwrite = overwrite,
                                    expected = 2L)){return()}

  #Calculate ####
  data1 = dplyr::select(data,
                        x = {{x}},
                        y = {{y}}) %>%
    weather2:::calc_emax(x = x, name_as = "x2") %>%
    weather2:::calc_emay(y = y, name_as = "y2")

  #Return the data ####
  data = weather2::sys_tld_FormatReturn(data = data,
                                        name_as = name_as,
                                        value = list(data1$x2, data1$y2),
                                        overwrite = overwrite)
  return(data)
}

#' Calculate Emagram plot x-coordinate
#'
#' Emagrams are one of the 5 meteorological thermo-diagram, which in essence is a log-P linear-T diagram.
#' This function calculates the emagram __X__ coordinates in a linear graph.
#' __Warning: Internal function. Inputs are not checked before executed. Use with care.__
#' __Warning: This function has over_write function set as `TRUE`__
#'
#' @param data The dataframe iteself.
#' @param x The column name of x-coordinate to be plotted onto the emagram. Usually it's some form of "temperature" data.
#' @param name_as Names of the 1 new columns, i.e. the x-coordinate of the emagram. Default as `NULL`, i.e. the column name of `x` with a suffix of `"_emx"`. Keyword `"*del*"` is supported.
#'
#' @keywords internal
#' @return The same dataframe as `data`, with 1 additional column with column name defined in `name_as`.
#'
#' @examples calc_emax(data, temp)
calc_emax = function(data, x, name_as = NULL){
  if(is.null(name_as)){
    name_as = paste0(weather2:::sys_hp_sym2chr({{x}}), "_emx")
    name_as = weather2::sys_tld_GetColname({{name_as}}, data)
  }
  data1 = dplyr::select(data,
                        x = {{x}}) %>%
    dplyr::mutate(XXX = x * 0.05)
  data = weather2::sys_tld_FormatReturn(data = data,
                                        name_as = name_as,
                                        value = list(data1$XXX),
                                        overwrite = T)
  return(data)
}

#' Calculate Emagram plot y-coordinate
#'
#' Emagrams are one of the 5 meteorological thermo-diagram, which in essence is a log-P linear-T diagram.
#' This function calculates the emagram __Y__ coordinates in a linear graph.
#' __Warning: Internal function. Inputs are not checked before executed. Use with care.__
#' __Warning: This function has over_write function set as `TRUE`__
#'
#' @param data The dataframe iteself.
#' @param y The column name of y-coordinate to be plotted onto the emagram. Usually it's some form of "pressure" data.
#' @param name_as Names of the 1 new columns, i.e. the y-coordinate of the emagram. Default as `NULL`, i.e. the column name of `x` with a suffix of `"_emx"`. Keyword `"*del*"` is supported.
#'
#' @keywords internal
#' @return The same dataframe as `data`, with 1 additional column with column name defined in `name_as`.
#'
#' @examples calc_emax(data, pres)
calc_emay = function(data, y, name_as = NULL){
  if(is.null(name_as)){
    name_as = paste0(weather2:::sys_hp_sym2chr({{y}}), "_emy")
    name_as = weather2::sys_tld_GetColname({{name_as}}, data)
  }
  data1 = dplyr::select(data, y = {{y}}) %>%
    dplyr::mutate(YYY = -log10(y) + 4)
  data = weather2::sys_tld_FormatReturn(data = data,
                                        name_as = name_as,
                                        value = list(data1$YYY),
                                        overwrite = T)
  return(data)
}
