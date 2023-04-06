#' Calculate the final temperature of an air parcel.
#'
#' An air parcel can be dry-adiabetically and moist-adiabetically cooled/ heated, which is determined by the mixing ratio of the said parcel. This formula automatically selects the correct final temperature and dew point of the parcel, by `calc_lcl` first.
#'
#' @param data The dataframe itself. Each row of data will be considered as an observation for each calculation.
#' @param pres1 The column name of the initial pressure of the calculation. Unit in `hPa`.
#' @param temp1 The column name of the initial temperature of the calculation. Unit in `degC`.
#' @param dwpt1 The column name of the initial dewpoint of the calculation. Unit in `degC`.
#' @param pres2 The column name of the final pressure of the calculation. Unit in `hPa`.
#' @param acc Accuracy of the calculation if uses moist adiabetic rate, which will be fed into `calc_adi_wet`.
#' @param name_as Names of the 2 new columns, in the order of "temperature of the final calculation", and "dewpoint of the final calculation". Default as `NULL`, which will utilized the column name of `temp1` and `dwpt1`, with suffix `"2"`. Keyword `"*del*"` is supported.
#' @param overwrite Let the new column names to overwrite the original dataframe columns? Default as `FALSE`.
#'
#' @return
#' @export
#'
#' @examples weather2::load_asnd() %>% dplyr::slice(1) %>% dplyr::mutate(pres2 = 200) %>% weather2::calc_parcel(pres, temp, dwpt, pres2)
calc_parcel = function(data, pres1, temp1, dwpt1, pres2, acc = 1, name_as = NULL, overwrite = F){
  #Check ####
  if(weather2::sys_ckc_dataframe(data, "data")){return()}
  if(weather2::sys_ckd_colexist(value = {{pres1}}, value_name = "pres1", data = data, data_name = "data")){return()}
  if(weather2::sys_ckd_colexist(value = {{temp1}}, value_name = "temp1", data = data, data_name = "data")){return()}
  if(weather2::sys_ckd_colexist(value = {{dwpt1}}, value_name = "dwpt1", data = data, data_name = "data")){return()}
  if(weather2::sys_ckd_colexist(value = {{pres2}}, value_name = "pres2", data = data, data_name = "data")){return()}

  if(is.null(name_as)){
    name_as = c(weather2:::sys_hp_sym2chr({{temp1}}), weather2:::sys_hp_sym2chr({{dwpt1}})) %>%
      paste0("2")
  }
  if(weather2::sys_ckc_character(name_as, "name_as")){return()}
  if(weather2::sys_ckl_length(name_as, "name_as", 2L)){return()}
  if(weather2::sys_ckc_logical(overwrite, "overwrite")){return()}

  #Magic ####
  data0 = dplyr::select(data,
                        p = {{pres1}},
                        t = {{temp1}},
                        d = {{dwpt1}},
                        p2= {{pres2}}) %>%
    weather2::calc_lcl(p, t, d, acc = 5, name_as = c("lp", "ltt", "ltd")) %>%
    weather2::calc_adi_dry(pres1 = p, temp1 = t, pres2 = p2, name_as = "t2_dry") %>%
    weather2::calc_isohume(pres1 = p, temp1 = d, pres2 = p2, name_as = "d2_dry") %>%
    weather2::calc_adi_wet(pres1 = `lp`, temp1 = `ltt`, pres2 = `p2`, name_as = "t2_wet", acc = acc) %>%
    weather2::calc_adi_wet(pres1 = `lp`, temp1 = `ltd`, pres2 = `p2`, name_as = "d2_wet", acc = acc) %>%
    dplyr::mutate(mode = ifelse(t >= d,
                                ifelse(lp > p2, "wet", "dry"),
                                ifelse(lp > p2, "dry", "wet")),
                  t2 = ifelse(mode == "wet", t2_wet, t2_dry),
                  d2 = ifelse(mode == "wet", d2_wet, d2_dry))
  #Return the data ####
  data = weather2::sys_tld_FormatReturn(data,
                                        name_as = name_as,
                                        value = list(data0$t2,
                                                     data0$d2),
                                        overwrite = overwrite)
  return(data)
}
