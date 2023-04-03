#' Calculate the __estimated__ final temperature and pressure of Lifting Condensation Level
#'
#' @param data The dataframe itself. Each row of data will be considered as an observation for each calculation
#' @param pres The column name of the initial pressure of the calculation. In any units.
#' @param temp The column name of the initial temperature of the calculation. Unit in `degC`.
#' @param dwpt The column name of the initial dewpoint of the calculation. Unit in `degC`.
#' @param name_as Names of the 3 new columns, in the order of "pressure of the LCL", "temperature of the LCL(estimated by temperature)", and "temperature of the LCL(estimated by dewpoint)". Default as `c("lclp", "lclt_t", "lclt_d")`.
#' @param overwrite Let the new column names to overwrite the original dataframe columns? Default as `FALSE`.
#'
#' @return
#' @export
#'
#' @examples calc_lcl_est(tibble::tibble(pres = 1000, temp = 20, dwpt = 10), pres, temp, dwpt) #Should return 861 for pressure
calc_lcl_est = function(data, pres, temp, dwpt, name_as = c("lclp", "lclt_t", "lclt_d"), overwrite = F){
  #Check ####
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

  #The calculation itself ####
  #Using equation 4.16b
  data0 = dplyr::select(data,
                        p = {{pres}},
                        t = {{temp}},
                        d = {{dwpt}}) %>%
    dplyr::mutate(t = t+273.15,
                  d = d+273.15,
                  pt_a = 1 - 1.225*((t - d) / t),
                  lclp = p * pt_a^3.5,
                  t = t-273.15,
                  d = d-273.15) %>%
    weather2::calc_adi_dry(p, t, lclp, name_as = "lclt_t", overwrite = T) %>%
    weather2::calc_isohume(p, d, lclp, name_as = "lclt_d", overwrite = T)

  #Return the data ####
  name_lclp  = name_as[1]
  name_lcltt = name_as[2]
  name_lcltd = name_as[3]

  if(!overwrite){
    name_lclp = weather2::sys_tld_GetColname({{name_lclp}}, data = data)
    name_lcltt = weather2::sys_tld_GetColname({{name_lcltt}}, data = data)
    name_lcltd = weather2::sys_tld_GetColname({{name_lcltd}}, data = data)
  }

  data = dplyr::mutate(data,
                       "{name_lclp}" := data0$lclp,
                       "{name_lcltt}":= data0$lclt_t,
                       "{name_lcltd}":= data0$lclt_d)

  return(data)
}
