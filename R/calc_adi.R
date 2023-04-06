#' Calculate the final temperature by dry adiabatic lapse rate
#'
#' The dry adiabatic lapse rate refers to the rate of __dry__ air parcel temperature decrease as pressure decreases. Typically, the dry adiabatic lapse rate is ~-9.8°C/km.
#' This function calculates the final atmospheric temperature, via the difference of 2 pressure levels(`pres1`, `pres2`).
#'
#' @param data The dataframe itself. Each row of data will be considered as an observation for each calculation
#' @param pres1 The column name of the initial pressure level. Unit in `hPa`.
#' @param temp1 The column name of temperature at the initial pressure level. Unit in `degC`.
#' @param pres2 The column name of the final pressure level. Unit in `hPa`.
#' @param name_as Names of the 1 new columns, i.e. temperature at the final pressure level. Default as `NULL`, i.e. the column name of `temp1` with a suffix of `"2"`. Keyword `"*del*"` is supported.
#' @param overwrite Let the new column names to overwrite the original dataframe columns? Default as `FALSE`.
#'
#' @return The same dataframe as `data`, with 1 additional column with column name defined in `name_as`.
#' @export
#'
#' @examples calc_adi_dry(data, p1, t1, p2, name_as = "t2", overwrite = T)
calc_adi_dry = function(data, pres1, temp1, pres2, name_as = NULL, overwrite = F){
  #Check ####
  if(weather2::sys_ckc_dataframe(data, "data")){return()}
  if(weather2::sys_ckd_colexist({{pres1}}, value_name = "pres1", data = data, data_name = "data")){return()}
  if(weather2::sys_ckd_colexist({{temp1}}, value_name = "temp1", data = data, data_name = "data")){return()}
  if(weather2::sys_ckd_colexist({{pres2}}, value_name = "pres2", data = data, data_name = "data")){return()}

  if(is.null(name_as)){name_as = paste0(weather2:::sys_hp_sym2chr({{temp1}}), "2")}
  if(weather2::sys_ckf_NameAsReturn(name_as = name_as, overwrite = overwrite, expected = 1L)){return()}

  #Work ####
  data1 = dplyr::select(data,
                        p1 = {{pres1}},
                        t1 = {{temp1}},
                        p2 = {{pres2}}) %>%
    dplyr::mutate(t1 = t1 + 273.15,
                  t2 = (p2/p1) ^ 0.28571 * t1,
                  t1 = t1 - 273.15,
                  t2 = t2 - 273.15)

  #return the data! ####
  data = weather2::sys_tld_FormatReturn(data = data,
                                        name_as = name_as,
                                        value = list(data1$t2),
                                        overwrite = overwrite)
  return(data)
}


#' Calculate the final temperature by wet adiabatic lapse rate
#'
#' The dry adiabatic lapse rate refers to the rate of __wet__ air parcel temperature decrease as pressure decreases. Typically, the dry adiabatic lapse rate is ~-5°C/km, though varies greatly as saturation pressure changes.
#' This function calculates the final atmospheric temperature, via the difference of 2 pressure levels(`pres1`, `pres2`).
#'
#' @param data The dataframe itself. Each row of data will be considered as an observation for each calculation
#' @param pres1 The column name of the initial pressure level. Unit in `hPa`.
#' @param temp1 The column name of temperature at the initial pressure level. Unit in `degC`.
#' @param pres2 The column name of the final pressure level. Unit in `hPa`.
#' @param acc Accuracy of the calculation, in the units of `hPa`. The smaller the value, the more iterations will be performed to increase the accurarcy of the final calculation.
#' @param name_as Names of the 1 new columns, i.e. temperature at the final pressure level. Default as `NULL`, i.e. the column name of `temp1` with a suffix of `"2"`. Keyword `"*del*"` is supported.
#' @param overwrite Let the new column names to overwrite the original dataframe columns? Default as `FALSE`.
#'
#' @return The same dataframe as `data`, with 1 additional column with column name defined in `name_as`.
#' @export
#'
#' @examples calc_adi_dry(data, p1, t1, p2, name_as = "t2", overwrite = T)
calc_adi_wet = function(data, pres1, temp1, pres2, acc = 1, name_as = NULL, overwrite = F){
  #Check ####
  if(weather2::sys_ckc_dataframe(data, "data")){return()}
  if(weather2::sys_ckd_colexist({{pres1}}, value_name = "pres1", data = data, data_name = "data")){return()}
  if(weather2::sys_ckd_colexist({{temp1}}, value_name = "temp1", data = data, data_name = "data")){return()}
  if(weather2::sys_ckd_colexist({{pres2}}, value_name = "pres2", data = data, data_name = "data")){return()}
  if(weather2::sys_ckc_numeric(acc, "acc")){return()}

  if(is.null(name_as)){name_as = paste0(weather2:::sys_hp_sym2chr({{temp1}}), "2")}
  if(weather2::sys_ckf_NameAsReturn(name_as = name_as, overwrite = overwrite, expected = 1L)){return()}

  #Select the used data & Prep for the calculation####
  data0 = dplyr::select(data,
                        p  = {{pres1}},
                        t  = {{temp1}},
                        p2 = {{pres2}}) %>%
    dplyr::mutate(t = t + 273.15,
                  cp = p,
                  ct = t,
                  sign = sign(p2 - cp),
                  uinc = abs(acc),
                  sinc = abs(cp - p2),
                  finc = ifelse(uinc >= sinc, sinc, uinc),
                  run = (finc != 0))
  #Set the while loop and run  ####
  RUN = as.logical(sum(data0$run, na.rm = T))
  while(RUN){
    data0 = dplyr::mutate(data0,
                          es = 0.6113 * exp(5423 * (1/273.15 - 1/ct)),
                          rs = (0.622 * es)/(cp - es),
                          del = (0.28571 * ct + 2488.4 * rs) / (cp * (1 + (1.35E7 * rs / (ct * ct)))),
                          cp = ifelse(run, cp + (sign * finc), cp),
                          ct = ifelse(run, ct + (sign * del * finc), ct),
                          sinc = abs(cp - p2),
                          finc = ifelse(uinc >= sinc, sinc, uinc),
                          run = (finc != 0))
    RUN = as.logical(sum(data0$run, na.rm = T))
  }
  #Return the data ####
  data0 = dplyr::mutate(data0, ct = ct - 273.15)
  data = weather2::sys_tld_FormatReturn(data = data,
                                        name_as = name_as,
                                        value = list(data0$ct),
                                        overwrite = overwrite)
  return(data)
}

#' Calculate the final temperature by isohume/ equal saturated mixing ratio
#'
#' Air parcels, at different temperature or pressures, can have different amount of saturated moisture content. Isohumes are lines connecting different locations with equal relative humidity, or in other words, same saturated mixing ratio.
#'
#' This function calculates the final atmospheric temperature, via the difference of 2 pressure levels(`pres1`, `pres2`), and assumes that the air parcel has the same mixing ratio.
#'
#' @param data The dataframe itself. Each row of data will be considered as an observation for each calculation.
#' @param pres1 The column name of the initial pressure level. Unit in `hPa`.
#' @param temp1 The column name of temperature at the initial pressure level. Unit in `degC`.
#' @param pres2 The column name of the final pressure level. Unit in `hPa`.
#' @param name_as Names of the 1 new columns, i.e. temperature at the final pressure level. Default as `NULL`, i.e. the column name of `temp1` with a suffix of `"2"`. Keyword `"*del*"` is supported.
#' @param overwrite Let the new column names to overwrite the original dataframe columns? Default as `FALSE`.
#'
#' @return The same dataframe as `data`, with 1 additional column with column name defined in `name_as`.
#' @export
#'
#' @examples calc_adi_dry(data, p1, t1, p2, name_as = "t2", overwrite = T)
calc_isohume = function(data, pres1, temp1, pres2, name_as = NULL, overwrite = F){
  #Check ####
  if(weather2::sys_ckc_dataframe(data, "data")){return()}
  if(weather2::sys_ckd_colexist({{pres1}}, value_name = "pres1", data = data, data_name = "data")){return()}
  if(weather2::sys_ckd_colexist({{temp1}}, value_name = "temp1", data = data, data_name = "data")){return()}
  if(weather2::sys_ckd_colexist({{pres2}}, value_name = "pres2", data = data, data_name = "data")){return()}

  if(is.null(name_as)){name_as = paste0(weather2:::sys_hp_sym2chr({{temp1}}), "2")}
  if(weather2::sys_ckf_NameAsReturn(name_as = name_as, overwrite = overwrite, expected = 1L)){return()}

  #Work ####
  data1 = dplyr::select(data,
                        p1 = {{pres1}},
                        t1 = {{temp1}},
                        p2 = {{pres2}}) %>%
    dplyr::mutate(t1 = t1 + 273.15,
                  es1= 6.113 * exp(17.2694 * (t1 - 273.15) / (t1 - 35.86)),
                  rs = (622 * es1) / (p1 - es1),
                  es2= (rs * p2) / (622 + rs),
                  t2 = (- (log(es2/ 6.113) / 17.2694) * 35.86 + 273.15) / (1 - (log(es2/ 6.113) / 17.2694)),
                  t1 = t1 - 273.15,
                  t2 = t2 - 273.15)

  #return the data! ####
  data = weather2::sys_tld_FormatReturn(data = data,
                                        name_as = name_as,
                                        value = list(data1$t2),
                                        overwrite = overwrite)
  return(data)
}
