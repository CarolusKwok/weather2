#' Tool: Provide dictionary of commonly used sounding index abbreviation/ codes
#'
#' @return
#' @export
#'
#' @examples tool_asnd_dict()
tool_asnd_dict = function(){
  find_table = tibble::tibble(
    metric = c("Showalter index",
               "Lifted index",
               "LIFT computed using virtual temperature",
               "SWEAT index",
               "K index",
               "Cross totals index",
               "Vertical totals index",
               "Totals totals index",
               "Convective Available Potential Energy",
               "CAPE using virtual temperature",
               "Convective Inhibition",
               "CINS using virtual temperature",
               "Bulk Richardson Number",
               "Bulk Richardson Number using CAPV",
               "Temp [K] of the Lifted Condensation Level",
               "Pres [hPa] of the Lifted Condensation Level",
               "Equivalent potential temp [K] of the LCL",
               "Mean mixed layer potential temperature",
               "Mean mixed layer mixing ratio",
               "1000 hPa to 500 hPa thickness",
               "Precipitable water [mm] for entire sounding",

               "Equilibrum Level",
               "Equilibrum Level using virtual temperature",
               "Level of Free Convection",
               "LFCT using virtual temperature"),
    abbr = c("show","lift",
             "lifv","swet",
             "kinx","ctot",
             "vtot","tott",
             "cape","capv",
             "cins","cinv",
             "brch","brcv",
             "lclt","lclp",
             "lcth","mlth",
             "mlmr","thtk",
             "pwat",

             "eqlv",
             "eqtv",
             "lfct",
             "ltcv"))

  return(find_table)
}
