#' Calculate the __estimated__ final temperature and pressure of Lifting Condensation Level
#'
#' Lifting Condensation Level (lcl) refers to the pressure level where relative humidity of an air parcel will reach 100% with respect to liquid water when it's cooled by dry adiabatic lifting, i.e. only due to buoyancy of the air parcel.
#'
#' Understanding lcl is crucial in meteorology, as this shows where clouds/ fog may start to form. This formula calculates the lcl of an air parcel, given its pressure (`pres`), temperature (`temp`) and dew point (`dwpt`) are known
#'
#' @param data The dataframe itself. Each row of data will be considered as an observation for each calculation.
#' @param pres The column name of the air parcel pressure level. Unit in `hPa`.
#' @param temp The column name of the air parcel temperature. Unit in `degC`.
#' @param dwpt The column name of the air parcel dewpoint. Unit in `degC`.
#' @param name_as Names of the 3 new columns, in the order of "pressure of the LCL", "temperature of the LCL(estimated by temperature)", and "temperature of the LCL(estimated by dewpoint)". Default as `NULL`, i.e. the column names of `pres`, `temp`, and `dwpt`, with a prefix of `lcl`. Keyword `"*del*"` is supported.
#' @param overwrite Let the new column names to overwrite the original dataframe columns? Default as `FALSE`.
#'
#' @return The same dataframe as `data`, with 3 additional column with column name defined in `name_as`.
#' @export
#'
#' @examples calc_lcl_est(tibble::tibble(pres = 1000, temp = 20, dwpt = 10), pres, temp, dwpt) #Should return 861 for pressure
calc_lcl_est = function(data, pres, temp, dwpt, name_as = NULL, overwrite = F){
  #Check ####
  if(weather2:::sys_hp_hasArg(data, 'data')){return()}
  if(weather2:::sys_hp_hasArg(pres, 'pres')){return()}
  if(weather2:::sys_hp_hasArg(temp, 'temp')){return()}
  if(weather2:::sys_hp_hasArg(dwpt, 'dwpt')){return()}

  if(weather2::sys_ckc_dataframe(data, 'data')){return()}
  if(weather2::sys_ckd_colexist({{pres}}, "pres", data = data, data_name = "data")){return()}
  if(weather2::sys_ckd_colexist({{temp}}, "temp", data = data, data_name = "data")){return()}
  if(weather2::sys_ckd_colexist({{dwpt}}, "dwpt", data = data, data_name = "data")){return()}

  if(is.null(name_as)){name_as = c(paste0("lcl", weather2:::sys_hp_sym2chr({{pres}})),
                                   paste0("lcl", weather2:::sys_hp_sym2chr({{temp}})),
                                   paste0("lcl", weather2:::sys_hp_sym2chr({{dwpt}})))}
  if(weather2::sys_ckf_NameAsReturn(name_as = name_as, overwrite = overwrite, expected = 3L)){return()}

  #The calculation itself ####
  #Using equation 4.16b
  data0 = dplyr::select(data,
                        p = {{pres}},
                        t = {{temp}},
                        d = {{dwpt}}) %>%
    dplyr::mutate(t = t + 273.15,
                  d = d + 273.15,
                  lclp = p * (1 - 1.225*((t - d) / t))^3.5,
                  t = t - 273.15,
                  d = d - 273.15) %>%
    weather2::calc_adi_dry(p, t, lclp, name_as = "lclt_t", overwrite = T) %>%
    weather2::calc_isohume(p, d, lclp, name_as = "lclt_d", overwrite = T)

  #Return the data ####
  data = weather2::sys_tld_FormatReturn(data = data,
                                        name_as = name_as,
                                        value = list(data0$lclp,
                                                     data0$lclt_t,
                                                     data0$lclt_d),
                                        overwrite = overwrite)
  return(data)
}

#' Calculate the final temperature and pressure of Lifting Condensation Level, with high accuracy
#'
#' Lifting Condensation Level (lcl) refers to the pressure level where relative humidity of an air parcel will reach 100% with respect to liquid water when it's cooled by dry adiabatic lifting, i.e. only due to buoyancy of the air parcel.
#'
#' Understanding lcl is crucial in meteorology, as this shows where clouds/ fog may start to form. This formula calculates the lcl of an air parcel, given its pressure (`pres`), temperature (`temp`) and dew point (`dwpt`) are known. Unlike `calc_lcl_est`, this function provides high/ controllable accuracy with `acc`. In general, a `acc` of `10L` or above will provide the exact lcl pressure and temperature values.
#'
#'
#' @param data The dataframe itself. Each row of data will be considered as an observation for each calculation.
#' @param pres The column name of the air parcel pressure level. Unit in `hPa`.
#' @param temp The column name of the air parcel temperature. Unit in `degC`.
#' @param dwpt The column name of the air parcel dewpoint. Unit in `degC`.
#' @param acc The accuracy of the calculation. Unit in `decimal places`, as an integer. Default as `5L`.
#' @param name_as Names of the 3 new columns, in the order of "pressure of the LCL", "temperature of the LCL(estimated by temperature)", and "temperature of the LCL(estimated by dewpoint)". Default as `NULL`, i.e. the column names of `pres`, `temp`, and `dwpt`, with a prefix of `lcl`. Keyword `"*del*"` is supported.
#' @param overwrite Let the new column names to overwrite the original dataframe columns? Default as `FALSE`.
#'
#' @return The same dataframe as `data`, with 3 additional column with column name defined in `name_as`.
#' @export
#'
#' @examples calc_lcl(tibble::tibble(pres = 1000, temp = 20, dwpt = 10), pres, temp, dwpt)
calc_lcl = function(data, pres, temp, dwpt, acc = 5L, name_as = NULL, overwrite = F){
  #Check function ####
  if(weather2:::sys_hp_hasArg(data, 'data')){return()}
  if(weather2:::sys_hp_hasArg(pres, 'pres')){return()}
  if(weather2:::sys_hp_hasArg(temp, 'temp')){return()}
  if(weather2:::sys_hp_hasArg(dwpt, 'dwpt')){return()}

  if(weather2::sys_ckc_dataframe(data, 'data')){return()}
  if(weather2::sys_ckd_colexist({{pres}}, "pres", data = data, data_name = "data")){return()}
  if(weather2::sys_ckd_colexist({{temp}}, "temp", data = data, data_name = "data")){return()}
  if(weather2::sys_ckd_colexist({{dwpt}}, "dwpt", data = data, data_name = "data")){return()}
  if(weather2::sys_ckc_integer(value = acc, value_name = "acc")){return()}

  if(is.null(name_as)){name_as = c(paste0("lcl", weather2:::sys_hp_sym2chr({{pres}})),
                                   paste0("lcl", weather2:::sys_hp_sym2chr({{temp}})),
                                   paste0("lcl", weather2:::sys_hp_sym2chr({{dwpt}})))}
  if(weather2::sys_ckf_NameAsReturn(name_as = name_as, overwrite = overwrite, expected = 3L)){return()}

  #Preset function ####
  calc_lcl_intern = function(data, acc, range){
    data_range = tibble::tibble(shift = seq(+range, -range, -acc))
    data = tidyr::expand_grid(data, data_range) %>%
      dplyr::mutate(shift = ifelse(t == d, 0,
                            ifelse(lp + shift <= 0, 0, shift))) %>%
      dplyr::distinct() %>%
      dplyr::mutate(cp = lp + shift) %>%
      weather2::calc_adi_dry(p, t, cp, name_as = "ct", overwrite = T) %>%
      weather2::calc_isohume(p, d, cp, name_as = "cd", overwrite = T) %>%
      dplyr::mutate(cdiff = ct - cd,
                    csign = sign(cdiff)) %>%
      dplyr::group_by(rows, csign) %>%
      dplyr::mutate(filter = 0,
                    filter = ifelse(csign == -1, max(cdiff, na.rm = T), filter),
                    filter = ifelse(csign == +1, min(cdiff, na.rm = T), filter)) %>%
      dplyr::filter(filter == cdiff | filter == 0 | is.na(filter)) %>%
      dplyr::group_by(rows) %>%
      dplyr::summarise(p = p,
                       t = t,
                       d = d,
                       lp = mean(cp, na.rm = T)) %>%
      dplyr::ungroup() %>%
      dplyr::distinct()
    return(data)
  }


  #calculate the sequence of acc ####
  if(acc >= 0){
    acc_seq = 1/(10^(0:(acc)))
  } else {
    acc_seq = 10^(-acc)
  }
  acc = acc_seq[1]
  #preparing the hard work ####
  data0 = dplyr::select(data,
                        p = {{pres}},
                        t = {{temp}},
                        d = {{dwpt}}) %>%
    dplyr::mutate(rows = 1:dplyr::n()) %>%
    dplyr::relocate(rows) %>%
    weather2::calc_lcl_est(p, t, d, name_as = c("lp", "del", "del"), overwrite = T) %>%
    dplyr::select(-del) %>%
    dplyr::mutate(lp = lp - (lp%%acc))

  defaultW = options("dplyr.summarise.inform")
  options(dplyr.summarise.inform = FALSE)

  #Iterating!
  i = 1
  acc = acc_seq[i]
  cli::cli_progress_bar(format = "{cli::pb_spin} Calculating LCL. Iteration {i} {.strong ({acc} hPa)}", auto_terminate = F)

  for(acc in acc_seq){
    cli::cli_progress_update(force = T)
    if(acc > 0.1){
      range = acc * 50
    } else {
      range = acc * 15
    }
    data0 = calc_lcl_intern(data = data0, acc = acc, range = range)
    i = i + 1
  }
  data0 = weather2::calc_adi_dry(data0, pres1 = p, temp1 = t, pres2 = lp, name_as = "ltt", overwrite = T) %>%
    weather2::calc_isohume(pres1 = p, temp1 = d, pres2 = lp, name_as = "ltd", overwrite = T) %>%
    dplyr::mutate(lp = ifelse(is.nan(lp), NA_real_, lp),
                  ltt = ifelse(is.nan(ltt), NA_real_, ltt),
                  ltd = ifelse(is.nan(ltd), NA_real_, ltd))


  #Return the data
  data = sys_tld_FormatReturn(data = data,
                              name_as = name_as,
                              value = list(data0$lp,
                                           data0$ltt,
                                           data0$ltd),
                              overwrite = overwrite)

  return(data)
}
