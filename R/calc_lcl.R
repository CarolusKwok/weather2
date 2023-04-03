#' Calculate the final temperature and pressure of Lifting Condensation Level, with high accurary
#'
#' @param data The dataframe itself. Each row of data will be considered as an observation for each calculation
#' @param pres The column name of the initial pressure of the calculation. In any units.
#' @param temp The column name of the initial temperature of the calculation. Unit in `degC`.
#' @param dwpt The column name of the initial dewpoint of the calculation. Unit in `degC`.
#' @param name_as Names of the 3 new columns, in the order of "pressure of the LCL", "temperature of the LCL(estimated by temperature)", and "temperature of the LCL(estimated by dewpoint)". Default as `c("lclp", "lclt_t", "lclt_d")`. Keyword `"*del*"` is supported.
#' @param overwrite Let the new column names to overwrite the original dataframe columns? Default as `FALSE`.
#'
#' @return
#' @export
#'
#' @examples
calc_lcl = function(data, pres, temp, dwpt, acc = 5, name_as = c("lclp", "lclt_t", "lclt_d"), overwrite = F){
  #Check function ####
  if(weather2:::sys_hp_hasArg(data, 'data')){return(data)}
  if(weather2:::sys_hp_hasArg(pres, 'pres')){return(data)}
  if(weather2:::sys_hp_hasArg(temp, 'temp')){return(data)}
  if(weather2:::sys_hp_hasArg(dwpt, 'dwpt')){return(data)}

  if(weather2::sys_ckc_dataframe(data, 'data')){return(data)}
  if(weather2::sys_ckd_colexist({{pres}}, "pres", data = data, data_name = "data")){return(data)}
  if(weather2::sys_ckd_colexist({{temp}}, "temp", data = data, data_name = "data")){return(data)}
  if(weather2::sys_ckd_colexist({{dwpt}}, "dwpt", data = data, data_name = "data")){return(data)}
  if(weather2::sys_ckl_length(name_as, list_name = "name_as", expected = 3L)){return(data)}
  if(weather2::sys_ckc_character(name_as, value_name = "name_as")){return(data)}
  if(weather2::sys_ckc_logical(overwrite, value_name = "overwrite")){return(data)}


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
