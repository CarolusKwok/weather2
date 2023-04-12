#' Calculate the __estimated__ final temperature and pressure of Convective Condensation Level
#'
#' The Convective Condensation Level, or __CCL__, refers to "the height (or pressure) where an air parcel becomes saturated when heated from below and lifted adiabatically due to buoyancy.". Or, in other words, CCL is when the isohume of an air parcel intersects with temperature profile of a sounding.
#' Typically, CCL is only used for air parcels from the surface, as the pressure level of CCL must be less than the starting surface in order to cool adiabatically. However, for this calculator, CCL is calculated as the closest pressure level that the corresponding isohume intersects with the temperature profile, allowing for multi-level calculation of CCL.
#' Do note that this algorithum supports computate of multiple sounding at once. __However, multiple sounding must be grouped by additional columns.__ Feed the additional columns used in the `...` argument.
#'
#' @param data The dataframe itself. Each row of data will be considered as an observation for each calculation
#' @param pres The column name of the initial pressure of the calculation. In any units.
#' @param temp The column name of the initial temperature of the calculation. Unit in `degC`.
#' @param dwpt The column name of the initial dewpoint of the calculation. Unit in `degC`.
#' @param asnd The column name of the sounding to be used in the calculation. Default as `NULL`, which assume all observations uses the first sounding in `sounding`
#' @param sounding A list of soundings. The column name of initial pressure, temperature and dewpoint must be defined as provided in `pres`, `temp`, and `dwpt`.
#' @param name_as Names of the 3 new columns, i.e. "pressure of the CCL", "temperature of the CCL(estimated by smoothened temperature profile)", and "temperature of the CCL(estimated by isohume)". Default as `c("cclp", "cclt", "cclh")`.
#' @param overwrite Let the new column names to overwrite the original dataframe columns? Default as `FALSE`.
#'
#' @return
#' @export
#'
#' @examples
#' load_asnd(time = Sys.time()) %>%
#' calc_ccl_est(data = dplyr::slice(., 1:3), pres, temp, dwpt, sounding = .)
calc_ccl_est = function(data, pres, temp, dwpt, asnd = NULL, sounding, name_as = c("cclp", "cclt", "cclh"), overwrite = F){
  #Preset function  ####
  magic = function(data0){
    #Smooth the dataframe
    data1 = tibble::tibble(p = seq(max(data0$p, na.rm = T), min(data0$p, na.rm = T), -1),
                           t = NA) %>%
      dplyr::filter(!(p %in% data0$p)) %>%
      dplyr::bind_rows(dplyr::select(data0, p, t)) %>%
      dplyr::arrange(p) %>%
      weather2::calc_smooth_lm(based = p,
                               value = t,
                               trailing = F,
                               name_as = "st",
                               overwrite = T) %>%
      dplyr::select(sp = p, st) %>%
      dplyr::filter((sp %% 1) == 0)
    #Calculate the CCL
    data2 =  tidyr::expand_grid(data0, data1) %>%
      dplyr::mutate(sp = ifelse(t == d, p, sp),
                    st = ifelse(t == d, t, st),
                    t = NULL) %>%
      dplyr::distinct() %>%
      weather2::calc_isohume(pres1 = p, temp1 = d, pres2 = sp,
                             name_as = "isohume",
                             overwrite = T) %>%
      dplyr::mutate(diff = isohume - st,
                    sign = sign(diff),
                    diff = abs(diff)) %>%
      weather2::sys_tld_GrpByAltVal(sign, name_as = "grp", overwrite = T) %>%
      dplyr::group_by(p, grp) %>%
      dplyr::filter(diff == suppressWarnings(min(diff, na.rm = T)) | is.na(diff)) %>%
      dplyr::group_by(p) %>%
      dplyr::mutate(nrow = dplyr::n(),
                    rm = (nrow == 1 & sp != p)) %>%
      dplyr::filter(diff == suppressWarnings(min(diff, na.rm = T)) | is.na(diff)) %>%
      dplyr::mutate(sp = ifelse(rm, NA, sp),
                    st = ifelse(rm, NA, st),
                    isohume = ifelse(rm, NA, isohume)) %>%
      dplyr::ungroup() %>%
      dplyr::select(sp, st, isohume)

    return(data2)
  }

  #Check ####
  if(weather2:::sys_hp_hasArg(data, 'data')){return(data)}
  if(weather2:::sys_hp_hasArg(pres, 'pres')){return(data)}
  if(weather2:::sys_hp_hasArg(temp, 'temp')){return(data)}
  if(weather2:::sys_hp_hasArg(dwpt, 'dwpt')){return(data)}

  if(weather2::sys_ckc_dataframe(data, 'data')){return(data)}
  if(weather2::sys_ckd_colexist({{pres}}, "pres", data = data, data_name = "data")){return(data)}
  if(weather2::sys_ckd_colexist({{temp}}, "temp", data = data, data_name = "data")){return(data)}
  if(weather2::sys_ckd_colexist({{dwpt}}, "dwpt", data = data, data_name = "data")){return(data)}

  mc = match.call(expand.dots = FALSE)$...
  for(i in mc){
    if(weather2::sys_ckd_colexist({{i}}, value_name = "...", data = data, data_name = "data")){return(data)}
  }

  if(weather2::sys_ckl_length(name_as, list_name = "name_as", expected = 3L)){return(data)}
  if(weather2::sys_ckc_character(name_as, value_name = "name_as")){return(data)}
  if(weather2::sys_ckc_logical(overwrite, value_name = "overwrite")){return(data)}

  #Make the original data as data0 ####
  name_p = weather2::sys_tld_GetColname("p", data)
  name_t = weather2::sys_tld_GetColname("t", data)
  name_d = weather2::sys_tld_GetColname("d", data)
  name_label = weather2::sys_tld_GetColname("label", data)

  data = weather2::sys_tld_MultiColumnLabel(data = data, ...,
                                            name_as = {{name_label}}, overwrite = T) %>%
    dplyr::arrange({{name_label}})
  data0 = dplyr::select(data,
                        "{name_label}" := !!rlang::sym({{name_label}}),
                        "{name_p}" := {{pres}},
                        "{name_t}" := {{temp}},
                        "{name_d}" := {{dwpt}},
                        ...)


  data1 = tibble::tibble(sp = NA, st = NA, isohume = NA, .rows = 0)
  #List out the grouping & Do the magic
  list_grp = dplyr::select(data0, x = {{name_label}})$x %>% unique()

  for(i in list_grp){
    data_cal = dplyr::filter(data0,
                             !!rlang::sym({{name_label}}) == i) %>%
      magic()

    data1 = dplyr::bind_rows(data1, data_cal)
  }

  #Return the data ####
  data = weather2::sys_tld_FormatReturn(data,
                                        name_as = name_as,
                                        value = list(data1$sp,
                                                     data1$st,
                                                     data1$isohume),
                                        overwrite = overwrite) %>%
    dplyr::select(-{{name_label}})
  return(data)
}
