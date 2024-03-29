#' Calculate location by distance and bearing
#'
#' Using the Vincenty direct solution of geodesics on the ellipsoid.
#' A R adaptation using https://www.movable-type.co.uk/scripts/latlong-vincenty.html code.
#'
#' @param data The dataframe itself.
#' @param lon1 The column name of the longitude. Unit in `deg`.
#' @param lat1 The column name of the latitude. Unit in `deg`.
#' @param brn1 The column name of the bearing. Unit in `deg`.
#' @param dis The column name of the distance. Unit in `m`.
#' @param acc Minimum accuracy of the estimation. Default as `1e-12`. Unit in `deg`.
#' @param a Major semi-axes of the ellipsoid, i.e. mean diameter of the Earth's Equator. __DO NOT CHANGE IF YOU DON'T KNOW WHAT YOU ARE DOING__
#' @param b Minor semi-axes of the ellipsoid, i.e. mean diameter of the Earth's Prime Meridian. __DO NOT CHANGE IF YOU DON'T KNOW WHAT YOU ARE DOING__
#' @param name_as The names of the new column name. Default as `NULL`, i.e. `c("lon2", "lat2", "brn2", "error")`. Keyword `"*del*"` is supported.
#' @param overwrite Let the new column names to overwrite the original dataframe columns? Default as `FALSE`.
#'
#' @return The same dataframe as `data`, with 4 additional column with column name defined in `name_as`.
#' @export
#'
#' @examples calc_geo_dest(data = data.frame(lon1 = 1:200, lat1 = 1:200, brn1 = 1:200, dis = seq(1100, 21000, 100)), lon1, lat1, brn1, dis)
calc_geo_dest = function(data, lon1, lat1, brn1, dis, acc = 1e-12, a = 6378137.0, b = 6356752.314245,
                         name_as = NULL, overwrite = F){
  #Check ####
  if(weather2::sys_ckc_dataframe(value = {{data}}, value_name = "data")){return()}
  if(weather2::sys_ckd_colexist(value = {{lon1}}, value_name = "lon1", data = data, data_name = "data")){return()}
  if(weather2::sys_ckd_colexist(value = {{lat1}}, value_name = "lat1", data = data, data_name = "data")){return()}
  if(weather2::sys_ckd_colexist(value = {{brn1}}, value_name = "brn1", data = data, data_name = "data")){return()}
  if(weather2::sys_ckd_colexist(value = {{dis}}, value_name = "dis", data = data, data_name = "data")){return()}

  if(weather2::sys_ckc_numeric(value = acc, value_name = "acc")){return()}
  if(weather2::sys_ckc_numeric(value = a, value_name = "a")){return()}
  if(weather2::sys_ckc_numeric(value = b, value_name = "b")){return()}

  if(is.null(name_as)){name_as = c("lon2", "lat2", "brn2", "error")}
  if(weather2::sys_ckf_NameAsReturn(name_as = name_as,
                                    overwrite = overwrite,
                                    expected = 4L)){return()}

  #Define Constants ####
  f = (a-b)/a
  #Calculation Part 1####
  data0 = dplyr::select(data,
                        Lon1 = {{lon1}},
                        Lat1 = {{lat1}},
                        Brn1 = {{brn1}},
                        Dis = {{dis}}) %>%
    dplyr::mutate(Lon1     = Lon1 * pi / 180,
                  Lat1     = Lat1 * pi / 180,
                  Brn1     = Brn1 * pi / 180,

                  sinBrn1  = sin(Brn1),
                  cosBrn1  = cos(Brn1),
                  tanU1    = (1 - f) * tan(Lat1),
                  cosU1    = 1 / sqrt((1 + tanU1^2)),
                  sinU1    = tanU1 * cosU1,
                  sig1     = atan2(tanU1, cosBrn1),
                  sinBrn   = cosU1 * sinBrn1,
                  cosSqBrn = 1 - sinBrn^2,
                  uSq      = cosSqBrn * (a^2 - b^2) / (b^2),
                  A        = 1 + uSq/16384*(4096 + uSq*(-768 + uSq*(320-175*uSq))),
                  B        = uSq/1024 * (256+uSq*(-128+uSq*(74-47*uSq))),
                  sig      = Dis / (b * A))
  #Looping ####
  max_error = 9999
  max_iter = 200
  loop = TRUE
  while(loop){
    data0 = dplyr::mutate(data0,
                          cos2sigm = cos(2*sig1 + sig),
                          sinsig = sin(sig),
                          cossig = cos(sig),
                          delsig = B * sinsig*(cos2sigm + B/4 * (cossig * (-1+ 2 * (cos2sigm^2))-B/6*cos2sigm*(-3+4*(sinsig^2))*(-3+4*(cos2sigm^2)))),
                          sigI = sig,
                          sig = Dis / (b * A) + delsig,
                          Error = abs(sig-sigI))
    max_iter = max_iter - 1
    max_error= max(data0$Error, na.rm = T)
    loop = (max_iter >= 0) & (max_error > acc)
  }
  #Calculation Part 2####
  data0 = dplyr::mutate(data0,
                        x    = sinU1 * sinsig - cosU1 * cossig * cosBrn1,
                        Lat2 = atan2(sinU1 * cossig + cosU1 * sinsig * cosBrn1,
                                     (1 - f) * sqrt(sinBrn * sinBrn + x * x)),
                        Lam  = atan2(sinsig * sinBrn1,
                                     cosU1 * cossig - sinU1 * sinsig * cosBrn1),
                        C    = f/16*cosSqBrn * (4 + f * (4 - 3 * cosSqBrn)),
                        L    = Lam - (1 - C) * f * sinBrn * (sig + C * sinsig * (cos2sigm + C * cossig * (-1 + 2*(cos2sigm^2)))),
                        Lon2 = Lon1 + L,
                        Brn2 = atan2(sinBrn, -x),

                        Lon2 = Lon2 * 180 / pi,
                        Lat2 = Lat2 * 180 / pi,
                        Brn2 = Brn2 * 180 / pi)
  #Return the results ####
  data = weather2::sys_tld_FormatReturn(data = data,
                                        name_as = name_as,
                                        value = list(data0$Lon2,
                                                     data0$Lat2,
                                                     data0$Brn2,
                                                     data0$Error),
                                        overwrite = overwrite)
  return(data)
}



#' Calculate distance and bearing by location
#'
#' Using the Vincenty indirect solution of geodesics on the ellipsoid.
#' A R adaptation using https://www.movable-type.co.uk/scripts/latlong-vincenty.html code.
#'
#' @param data The dataframe itself.
#' @param lon1 The column name of the longitude in location 1. Unit in `deg`.
#' @param lat1 The column name of the latitude in location 1. Unit in `deg`.
#' @param lon2 The column name of the longitude in location 2. Unit in `deg`.
#' @param lat2 The column name of the latitude in location 2. Unit in `deg`.
#' @param acc Minimum accuracy of the estimation. Default as `1e-12`.
#' @param a Major semi-axes of the ellipsoid, i.e. mean diameter of the Earth's Equator. __DO NOT CHANGE IF YOU DON'T KNOW WHAT YOU ARE DOING__
#' @param b Minor semi-axes of the ellipsoid, i.e. mean diameter of the Earth's Prime Meridian. __DO NOT CHANGE IF YOU DON'T KNOW WHAT YOU ARE DOING__
#' @param name_as The names of the new column name. Default as `NULL`, i.e. `c("brn1", "brn2", "dis", "error")`. Keyword `"*del*"` is supported.
#' @param overwrite Let the new column names to overwrite the original dataframe columns? Default as `FALSE`.
#'
#' @return The same dataframe as `data`, with 4 additional column with column name defined in `name_as`.
#' @export
#'
#' @examples calc_geo_dist(data = tibble::tibble(lon1 = 1:90, lat1 = 1:90, lon2 = 90:1, lat2 = 90:1), lon1, lat1, lon2, lat2)
calc_geo_dist = function(data, lon1, lat1, lon2, lat2, acc = 1e-12, a = 6378137.0, b = 6356752.314245,
                         name_as = NULL, overwrite = F){
  #Check ####
  if(weather2::sys_ckc_dataframe(value = {{data}}, value_name = "data")){return()}
  if(weather2::sys_ckd_colexist(value = {{lon1}}, value_name = "lon1", data = data, data_name = "data")){return()}
  if(weather2::sys_ckd_colexist(value = {{lat1}}, value_name = "lat1", data = data, data_name = "data")){return()}
  if(weather2::sys_ckd_colexist(value = {{lon2}}, value_name = "lon2", data = data, data_name = "data")){return()}
  if(weather2::sys_ckd_colexist(value = {{lat2}}, value_name = "lat2", data = data, data_name = "data")){return()}
  if(weather2::sys_ckc_numeric(value = acc, value_name = "acc")){return()}
  if(weather2::sys_ckc_numeric(value = a, value_name = "a")){return()}
  if(weather2::sys_ckc_numeric(value = b, value_name = "b")){return()}

  if(is.null(name_as)){name_as = c("brn1", "brn2", "dis", "error")}
  if(weather2::sys_ckf_NameAsReturn(name_as = name_as,
                                    overwrite = overwrite,
                                    expected = 4L)){return()}
  #Define Constants ####
  f = (a-b)/a

  #Calculation ####
  ##Calculation part 1 ####
  data0 = dplyr::select(data,
                        Lon1 = {{lon1}},
                        Lat1 = {{lat1}},
                        Lon2 = {{lon2}},
                        Lat2 = {{lat2}}) %>%
    dplyr::mutate(Lon1 = Lon1 * pi / 180,
                  Lat1 = Lat1 * pi / 180,
                  Lon2 = Lon2 * pi / 180,
                  Lat2 = Lat2 * pi / 180,

                  L     = Lon2 - Lon1,
                  tanU1 = (1-f) * tan(Lat1),
                  cosU1 = 1 / sqrt((1 + tanU1*tanU1)),
                  sinU1 = tanU1 * cosU1,
                  tanU2 = (1-f) * tan(Lat2),
                  cosU2 = 1 / sqrt((1 + tanU2*tanU2)),
                  sinU2 = tanU2 * cosU2,
                  Lam   = L)

  ## Looping ####
  max_error = 9999
  max_iter = 200
  loop = TRUE
  while(loop){
    data0 = dplyr::mutate(data0,
                          sinLam = sin(Lam),
                          cosLam = cos(Lam),
                          sinSqSig = (cosU2*sinLam) * (cosU2*sinLam) + (cosU1*sinU2-sinU1*cosU2*cosLam)**2,
                          sinSig = sqrt(sinSqSig),
                          cosSig = sinU1*sinU2 + cosU1*cosU2*cosLam,
                          Sig = atan2(sinSig, cosSig),
                          sinBrn = cosU1 * cosU2 * sinLam / sinSig,
                          cosSqBrn = 1 - sinBrn*sinBrn,
                          cos2Sigm = cosSig - (2*sinU1*sinU2/cosSqBrn),

                          cos2Sigm = ifelse(is.nan(cos2Sigm), 0, cos2Sigm),

                          C = f/16*cosSqBrn*(4+f*(4-3*cosSqBrn)),
                          LamI = Lam,
                          Lam = L + (1-C) * f * sinBrn * (Sig + C*sinSig*(cos2Sigm+C*cosSig*(-1+2*cos2Sigm*cos2Sigm))),
                          Error = abs(Lam - LamI))
    max_iter = max_iter - 1
    max_error= max(data0$Error, na.rm = T)
    loop = (max_iter >= 0) & (max_error > acc)
  }
  ##Calculation part 2 ####
  data0 = dplyr::mutate(data0,
                        uSq = cosSqBrn * (a*a - b*b) / (b*b),
                        A = 1 + uSq/16384*(4096+uSq*(-768+uSq*(320-175*uSq))),
                        B = uSq/1024 * (256+uSq*(-128+uSq*(74-47*uSq))),
                        DelSig = B*sinSig*(cos2Sigm+B/4*(cosSig*(-1+2*cos2Sigm*cos2Sigm)-B/6*cos2Sigm*(-3+4*sinSig*sinSig)*(-3+4*cos2Sigm*cos2Sigm))),
                        Dis = b*A*(Sig-DelSig),
                        Brn1 = atan2(cosU2*sinLam,  cosU1*sinU2-sinU1*cosU2*cosLam),
                        Brn2 = atan2(cosU1*sinLam, -sinU1*cosU2+cosU1*sinU2*cosLam),

                        Brn1 = Brn1 * 180 / pi,
                        Brn2 = Brn2 * 180 / pi)
  #Return the results ####
  data = weather2::sys_tld_FormatReturn(data = data,
                                        name_as = name_as,
                                        value = list(data0$Brn1,
                                                     data0$Brn2,
                                                     data0$Dis,
                                                     data0$Error),
                                        overwrite = overwrite)
  return(data)
}
