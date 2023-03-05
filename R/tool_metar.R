#' Tool: Decode METAR reports
#'
#' @param data Dataframe that contains METAR code
#' @param code The column name of the METAR code
#' @param CAVOK If decode is T, supress CAVOK and convert all CAVOK into vsby = 9999. Default as T.
#'
#' @return
#' @export
#'
#' @examples tool_metar(data, code)
tool_metar = function(data, code, CAVOK){
  #Create dictionary ####
  wx = weather2::tool_metar_dict(type = "weather")
  cd = weather2::tool_metar_dict(type = "cloud")

  #Create "find_blank", "find_slash" & "shift" function ####
  find_blank = function(data){
    data = dplyr::mutate(data,
                         shift = stringr::str_locate(metar, " ")[,1],
                         temp = substr(metar, 1, shift -1))
    return(data)
  }
  find_slash = function(data){
    data = dplyr::mutate(data,
                         shift = stringr::str_locate(metar, "/")[,1],
                         temp = substr(metar, 1, shift -1))
    return(data)
  }
  shift = function(data){
    data = dplyr::mutate(data,
                         metar = substr(metar, shift + 1, 99999))
    return(data)
  }

  #Create a temporary dataset ####
  metar = dplyr::select(data, {{code}})
  colnames(metar) = c("metar")
  metar = dplyr::mutate(metar,
                        metar = paste0(metar, " "),
                        load = NA,
                        temp = NA,
                        shift = NA)

  unit = tibble::tibble(.rows = 1)

  #Col 1: Find station ####
  metar = dplyr::mutate(metar, load = substr(metar, 1, 4))
  data = dplyr::mutate(data, station = metar$load)
  metar = dplyr::mutate(metar, metar = substr(metar, 6, 99999))

  #Col 2: Find time ####
  metar = dplyr::mutate(metar, load = substr(metar, 1, 7))
  data = dplyr::mutate(data, time_d = metar$load)
  metar = dplyr::mutate(metar, metar = substr(metar, 9, 99999))

  #Col 3: Find type ####
  metar = find_blank(metar) %>%
    dplyr::mutate(shift = ifelse(temp == "AUTO", shift, 0),
                  load = ifelse(shift != 0, temp, NA))
  data = dplyr::mutate(data, type = metar$load)
  metar = shift(metar)

  #Col 4: Find wind direction ####
  metar = dplyr::mutate(metar, load = substr(metar, 1, 3)) %>%
    dplyr::mutate(load = stringr::str_remove(load, "^0+"),
                  load = ifelse(load == "", "0", load),
                  temp = suppressWarnings(as.numeric(load)))
  if(!anyNA(metar$temp)){
    metar = dplyr::mutate(metar, load = temp)
  }
  data = dplyr::mutate(data, drct = metar$load)
  metar = dplyr::mutate(metar, metar = substr(metar, 4, 99999))

  #Col 5: Find wind speed ####
  metar = find_blank(metar) %>%
    dplyr::mutate(shift = ifelse(substr(temp, nchar(temp)-1, nchar(temp)) == "KT" |
                                 substr(temp, nchar(temp)-2, nchar(temp)) == "MPS",
                                 shift, 0),
                  load = ifelse(shift != 0, temp, NA))
  if(sum(substr(metar$load, nchar(metar$load)-1, nchar(metar$load)) == "KT") == nrow(metar)){
    unit = dplyr::mutate(unit, sped = "kt")
    metar = dplyr::mutate(metar, load = substr(load, 1, nchar(load) - 2)) %>%
      dplyr::mutate(load = stringr::str_remove(load, "^0+"),
                    load = ifelse(load == "", "0", load))
  }
  if(sum(substr(metar$load, nchar(metar$load)-2, nchar(metar$load)) == "MPS") == nrow(metar)){
    unit = dplyr::mutate(unit, sped = "mps")
    metar = dplyr::mutate(metar, load = substr(load, 1, nchar(load) - 3)) %>%
      dplyr::mutate(load = stringr::str_remove(load, "^0+"),
                    load = ifelse(load == "", "0", load))
  }
  metar = dplyr::mutate(metar, temp = stringr::str_locate(load, "G")[,1]) %>%
    dplyr::mutate(load_2 = ifelse(!is.na(temp), substr(load, temp+1,  99999), NA_real_),
                  load   = ifelse(!is.na(temp), substr(load,      1, temp-1), load))

  metar = dplyr::mutate(metar, temp = suppressWarnings(as.numeric(load)))
  if(!anyNA(metar$temp)){
    metar = dplyr::mutate(metar, load = as.numeric(temp))
  }

  metar = dplyr::mutate(metar, temp = suppressWarnings(as.numeric(load_2))) %>%
    dplyr::mutate(temp = ifelse(is.na(load_2) & is.na(temp), -1, temp))
  if(!anyNA(metar$temp)){
    metar = dplyr::mutate(metar,
                          load_2 = as.numeric(temp),
                          load_2 = ifelse(load_2 == -1, NA, load_2))
  }
  data = dplyr::mutate(data,
                       sped = metar$load,
                       gust = metar$load_2)
  metar = shift(metar)

  #Col 6: Find var wind drct ####
  metar = find_blank(metar) %>%
    dplyr::mutate(shift = ifelse(stringr::str_detect(temp, "V") & (nchar(temp) == 7), shift, 0),
                   load = ifelse(shift != 0, temp, NA))
  data = dplyr::mutate(data, drct_var = metar$load)
  metar = shift(metar)

  #Col 7: Find vsby ####
  metar = dplyr::mutate(metar, vsby_unit = stringr::str_detect(metar, "SM")) %>%
    dplyr::mutate(vsby_unit = ifelse(vsby_unit == T, "mile", "meter"))

  if(sum(metar$vsby_unit == "mile") == nrow(metar)){
    metar = dplyr::mutate(metar, shift = stringr::str_locate(metar, "SM")[,1] + 2) %>%
      dplyr::mutate(temp = substr(metar, 1, shift -1),
                    load = temp)
  } else {
    metar = find_blank(metar) %>%
      dplyr::mutate(load = temp)
  }

  if(CAVOK == T){
    metar = dplyr::mutate(metar, load = ifelse(load == "CAVOK", 9999, load))
  }

  metar = dplyr::mutate(metar, temp = suppressWarnings(as.numeric(load)))
  if(!anyNA(metar$temp)){
    metar = dplyr::mutate(metar, load = temp)
  }
  data = dplyr::mutate(data, vsby = metar$load)
  metar = shift(metar)

  #Col 8: Find vsby_spe ####
  metar = find_blank(metar) %>%
    dplyr::mutate(shift = ifelse(nchar(temp) == 5 & !
                                (substr(temp, 4, 5) %in% wx$code) &
                                !stringr::str_detect(temp, "/"), shift, 0),
                  load = ifelse(shift != 0, temp, NA))
  data = dplyr::mutate(data, vsby_spe = metar$load)
  metar = shift(metar)
  #Col 9-16: Find runway vsby ####
  ##Col 9: Runway 1 ####
  metar = find_blank(metar) %>%
    dplyr::mutate(shift = ifelse(nchar(temp) >= 10, shift , 0),
                  load = ifelse(shift != 0, temp, NA))
  data = dplyr::mutate(data, vsby_r1 = metar$load)
  metar = shift(metar)
  ##Col 10: Runway 2 ####
  metar = find_blank(metar) %>%
    dplyr::mutate(shift = ifelse(nchar(temp) >= 10, shift , 0),
                  load = ifelse(shift != 0, temp, NA))
  data = dplyr::mutate(data, vsby_r2 = metar$load)
  metar = shift(metar)
  ##Col 11: Runway 3 ####
  metar = find_blank(metar) %>%
    dplyr::mutate(shift = ifelse(nchar(temp) >= 10, shift , 0),
                  load = ifelse(shift != 0, temp, NA))
  data = dplyr::mutate(data, vsby_r3 = metar$load)
  metar = shift(metar)
  ##Col 12: Runway 4 ####
  metar = find_blank(metar) %>%
    dplyr::mutate(shift = ifelse(nchar(temp) >= 10, shift , 0),
                  load = ifelse(shift != 0, temp, NA))
  data = dplyr::mutate(data, vsby_r4 = metar$load)
  metar = shift(metar)
  ##Col 13: Runway 5 ####
  metar = find_blank(metar) %>%
    dplyr::mutate(shift = ifelse(nchar(temp) >= 10, shift , 0),
                  load = ifelse(shift != 0, temp, NA))
  data = dplyr::mutate(data, vsby_r5 = metar$load)
  metar = shift(metar)
  ##Col 14: Runway 6 ####
  metar = find_blank(metar) %>%
    dplyr::mutate(shift = ifelse(nchar(temp) >= 10, shift , 0),
                  load = ifelse(shift != 0, temp, NA))
  data = dplyr::mutate(data, vsby_r6 = metar$load)
  metar = shift(metar)
  ##Col 15: Runway 7 ####
  metar = find_blank(metar) %>%
    dplyr::mutate(shift = ifelse(nchar(temp) >= 10, shift , 0),
                  load = ifelse(shift != 0, temp, NA))
  data = dplyr::mutate(data, vsby_r7 = metar$load)
  metar = shift(metar)
  ##Col 16: Runway 8 ####
  metar = find_blank(metar) %>%
    dplyr::mutate(shift = ifelse(nchar(temp) >= 10, shift , 0),
                  load = ifelse(shift != 0, temp, NA))
  data = dplyr::mutate(data, vsby_r8 = metar$load)
  metar = shift(metar)


  #Col 17 - 19: Find weather phenomena ####
  ##Col 17: wxph_1 ####
  metar = find_blank(metar) %>%
    dplyr::mutate(shift = ifelse(substr(temp, nchar(temp)-1, nchar(temp)) %in% wx$code, shift, 0),
                  load = ifelse(shift != 0, temp, NA))
  data = dplyr::mutate(data, wxph_1 = metar$load)
  metar = shift(metar)
  ##Col 18: wxph_2 ####
  metar = find_blank(metar) %>%
    dplyr::mutate(shift = ifelse(substr(temp, nchar(temp)-1, nchar(temp)) %in% wx$code, shift, 0),
                  load = ifelse(shift != 0, temp, NA))
  data = dplyr::mutate(data, wxph_2 = metar$load)
  metar = shift(metar)
  ##Col 19: wxph_3 ####
  metar = find_blank(metar) %>%
    dplyr::mutate(shift = ifelse(substr(temp, nchar(temp)-1, nchar(temp)) %in% wx$code, shift, 0),
                  load = ifelse(shift != 0, temp, NA))
  data = dplyr::mutate(data, wxph_3 = metar$load)
  metar = shift(metar)
  #Col 20-23: Find cloud ####
  ##Col 20: Okta 1 ####
  metar = find_blank(metar) %>%
    dplyr::mutate(shift = ifelse(substr(temp, 1, 3) %in% cd$code, shift, 0),
                  load = ifelse(shift != 0, temp, NA))
  data = dplyr::mutate(data, okta_1 = metar$load)
  metar = shift(metar)
  ##Col 21: Okta 2 ####
  metar = find_blank(metar) %>%
    dplyr::mutate(shift = ifelse(substr(temp, 1, 3) %in% cd$code, shift, 0),
                  load = ifelse(shift != 0, temp, NA))
  data = dplyr::mutate(data, okta_2 = metar$load)
  metar = shift(metar)
  ##Col 22: Okta 3 ####
  metar = find_blank(metar) %>%
    dplyr::mutate(shift = ifelse(substr(temp, 1, 3) %in% cd$code, shift, 0),
                  load = ifelse(shift != 0, temp, NA))
  data = dplyr::mutate(data, okta_3 = metar$load)
  metar = shift(metar)
  ##Col 23: Okta 4 ####
  metar = find_blank(metar) %>%
    dplyr::mutate(shift = ifelse(substr(temp, 1, 3) %in% cd$code, shift, 0),
                  load = ifelse(shift != 0, temp, NA))
  data = dplyr::mutate(data, okta_4 = metar$load)
  metar = shift(metar)
  #Col 24: Find temp ####
  metar = find_slash(metar) %>%
    dplyr::mutate(load = ifelse(shift != 0, temp, NA)) %>%
    dplyr::mutate(load = as.numeric(stringr::str_replace(string = load, pattern = "M", replacement = "-")))
  data = dplyr::mutate(data, temp = metar$load)
  metar = shift(metar)
  #Col 25: Find dwpt ####
  metar = find_blank(metar) %>%
    dplyr::mutate(load = ifelse(shift != 0, temp, NA)) %>%
    dplyr::mutate(load = as.numeric(stringr::str_replace(string = load, pattern = "M", replacement = "-")))
  data = dplyr::mutate(data, dwpt = metar$load)
  metar = shift(metar)

  #Col 26: Find pres ####
  metar = find_blank(metar) %>%
    dplyr::mutate(shift = ifelse(substr(temp, 1, 1) == "Q" | substr(temp, 1, 1) == "A", shift, 0),
                  load = ifelse(shift != 0, temp, NA))
  if(sum(substr(metar$load, 1, 1) == "Q", na.rm = T) == nrow(metar)){
    unit = dplyr::mutate(unit, pres = "hPa")
    metar = dplyr::mutate(metar, load = as.numeric(stringr::str_replace(load, "Q", "")))
  }
  if(sum(substr(metar$load, 1, 1) == "A", na.rm = T) == nrow(metar)){
    unit = dplyr::mutate(unit, pres = "inHg")
    metar = dplyr::mutate(metar, load = as.numeric(stringr::str_replace(load, "A", "")))
  }

  data = dplyr::mutate(data, pres = metar$load)
  metar = shift(metar)
  #Col 27-28: Find additional weather information ####
  ##Col 27: wxph_4 ####
  metar = find_blank(metar) %>%
    dplyr::mutate(shift = ifelse(!(temp %in% c("BECMG", "NOSIG", "TEMPO", "RMK")), shift, 0),
                  load = ifelse(shift != 0, temp, NA))
  data = dplyr::mutate(data, wxph_4 = metar$load)
  metar = shift(metar)

  ##Col 28: wxph_5 ####
  metar = find_blank(metar) %>%
    dplyr::mutate(shift = ifelse(!(temp %in% c("BECMG", "NOSIG", "TEMPO", "RMK")), shift, 0),
                  load = ifelse(shift != 0, temp, NA))
  data = dplyr::mutate(data, wxph_5 = metar$load)
  metar = shift(metar)
  ##Col 29: wxph_6 ####
  metar = find_blank(metar) %>%
    dplyr::mutate(shift = ifelse(!(temp %in% c("BECMG", "NOSIG", "TEMPO", "RMK")), shift, 0),
                  load = ifelse(shift != 0, temp, NA))
  data = dplyr::mutate(data, wxph_6 = metar$load)
  metar = shift(metar)

  #Col 30: Additional information ####
  metar = dplyr::mutate(metar,
                        shift = nchar(metar),
                        load = substr(metar, 1, shift - 1))
  data = dplyr::mutate(data, rmk = metar$load)

  #Return #####
  data = data[, colSums(is.na(data)) != nrow(data)] %>%
    dplyr::relocate({{code}}, .after = dplyr::last_col()) %>%
    dplyr::relocate(pres, .after = time_d) %>%
    dplyr::relocate(temp, dwpt, .after = pres)
  return(list(data = data, unit = unit))
}
