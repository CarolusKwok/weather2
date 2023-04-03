#' Calculate the final temperature by dry adiabatic lapse rate
#'
#' @param data
#' @param pres1
#' @param temp1
#' @param pres2
#' @param temp2
#' @param name_as
#' @param overwrite
#'
#' @return
#' @export
#'
#' @examples
calc_adi_dry = function(data, pres1, temp1, pres2, name_as = "temp2", overwrite = F){
  #Check ####
  if(weather2::sys_ckc_dataframe(data, "data")){return(data)}
  if(weather2::sys_ckd_colexist({{pres1}}, value_name = "pres1", data = data, data_name = "data")){return(data)}
  if(weather2::sys_ckd_colexist({{temp1}}, value_name = "temp1", data = data, data_name = "data")){return(data)}
  if(weather2::sys_ckd_colexist({{pres2}}, value_name = "pres2", data = data, data_name = "data")){return(data)}
  if(weather2::sys_ckl_length(name_as, list_name = "name_as", expected = 1L, mode = "==")){return(data)}
  if(weather2::sys_ckc_logical(value = overwrite, value_name = "overwrite")){return(data)}

  #Work ####
  RdCp = 0.28571
  data1 = dplyr::select(data,
                        p1 = {{pres1}},
                        t1 = {{temp1}},
                        p2 = {{pres2}}) %>%
    dplyr::mutate(t1 = t1 + 273.15,
                  t2 = (p2/p1) ^ RdCp * t1,
                  t1 = t1 - 273.15,
                  t2 = t2 - 273.15)

  #return the data! ####
  if(!overwrite){name_as = weather2::sys_tld_GetColname(value = {{name_as}}, data = data)}
  data = dplyr::mutate(data, "{name_as}" := data1$t2)
  return(data)
}


#' Calculate the final temperature by wet adiabatic lapse rate
#'
#' @param data
#' @param pres1
#' @param temp1
#' @param pres2
#' @param name_as
#' @param overwrite
#'
#' @return
#' @export
#'
#' @examples
calc_adi_wet = function(data, pres1, temp1, pres2, acc = 1, name_as = "temp2", overwrite = F){
  #Check ####
  if(weather2::sys_ckc_dataframe(data, "data")){return(data)}
  if(weather2::sys_ckd_colexist({{pres1}}, value_name = "pres1", data = data, data_name = "data")){return(data)}
  if(weather2::sys_ckd_colexist({{temp1}}, value_name = "temp1", data = data, data_name = "data")){return(data)}
  if(weather2::sys_ckd_colexist({{pres2}}, value_name = "pres2", data = data, data_name = "data")){return(data)}

  #Work ####
  acc = acc / 10
  data1 = dplyr::select(data,
                        p1 = {{pres1}},
                        t1 = {{temp1}},
                        p2 = {{pres2}}) %>%
    dplyr::mutate(t1 = t1 + 273.15,
                  t2 = NA,
                  p1 = p1 / 10,
                  p2 = p2 / 10,
                  pc = p1,
                  tc = t1,
                  iter = abs(p1 - p2) / acc,
                  iterc = 0,
                  run  = is.na(t2) & !is.na(p2))
  run_pro = as.logical(sum(data1$run, na.rm = T))

  while(run_pro){
    data1 = dplyr::mutate(data1,
                          iterc= iterc + 1,
                          es   = ifelse(run, 0.6113 * exp(5423 * (1/273.15 - 1/tc)), NA),
                          rs   = ifelse(run, (0.622 * es)/(pc - es)                , NA),
                          del1 = ifelse(run, (0.28571 * tc + 2488.4 * rs)          , NA),
                          del2 = ifelse(run, pc * (1 + (1.35E7 * rs / (tc * tc)))  , NA),
                          del  = del1/del2,

                          sign = sign((p2 - p1)),

                          pc = ifelse(run, pc + (sign       * acc), pc),
                          tc = ifelse(run, tc + (sign * del * acc), tc),

                          run = ((run == T) & (iterc < iter)))
    run_pro = as.logical(sum(data1$run, na.rm = T))
  }
  data1 = dplyr::mutate(data1,
                        p2 = pc,
                        t2 = tc,
                        p1 = p1 * 10,
                        p2 = p2 * 10,
                        t1 = t1 - 273.15,
                        t2 = t2 - 273.15) %>%
    dplyr::select(p1, t1, p2, t2)
  #Return data ####
  if(!overwrite){name_as = weather2::sys_tld_GetColname(value = {{name_as}}, data = data)}
  data = dplyr::mutate(data, "{name_as}" := data1$t2)
  return(data)
}

#' Calculate the final temperature by saturated mixing ratio
#'
#' @param data
#' @param pres1
#' @param temp1
#' @param pres2
#' @param temp2
#' @param name_as
#' @param overwrite
#'
#' @return
#' @export
#'
#' @examples
calc_isohume = function(data, pres1, temp1, pres2, name_as = "temp2", overwrite = F){
  #Check ####
  if(weather2::sys_ckc_dataframe(data, "data")){return(data)}
  if(weather2::sys_ckd_colexist({{pres1}}, value_name = "pres1", data = data, data_name = "data")){return(data)}
  if(weather2::sys_ckd_colexist({{temp1}}, value_name = "temp1", data = data, data_name = "data")){return(data)}
  if(weather2::sys_ckd_colexist({{pres2}}, value_name = "pres2", data = data, data_name = "data")){return(data)}

  #Work ####
  data1 = dplyr::select(data,
                        p1 = {{pres1}},
                        t1 = {{temp1}},
                        p2 = {{pres2}})

  data1 = dplyr::mutate(data1,
                        t1 = t1 + 273.15,
                        es1= 6.113 * exp(17.2694 * (t1 - 273.15) / (t1 - 35.86)),
                        rs = (622 * es1) / (p1 - es1),
                        es2= (rs * p2) / (622 + rs),
                        a  = log(es2/ 6.113) / 17.2694,
                        t2 = (- a * 35.86 + 273.15) / (1 - a),
                        t1 = t1 - 273.15,
                        t2 = t2 - 273.15)

  #return the data! ####
  if(!overwrite){name_as = weather2::sys_tld_GetColname(value = {{name_as}}, data = data)}
  data = dplyr::mutate(data, "{name_as}" := data1$t2)
  return(data)
}
