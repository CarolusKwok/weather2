#' Read a list of `.gpx` file with the same format into a data frame
#'
#' @param file Pathway of the files
#' @param read_time Should the time nodes be read and export as POSIXct in UTC? Default as `TRUE`.
#'
#' @return
#' @export
#'
#' @examples read_gpx(choose.file(), read_time = T)
read_gpx = function(file, read_time = T){
  #Preset function ####
  read_gpx_internal = function(file, read_time){
    gpx_parsed = XML::htmlTreeParse(file = file, useInternalNodes = T)
    coords = XML::xpathSApply(doc = gpx_parsed, path = "//trkpt", fun = XML::xmlAttrs)
    ele = XML::xpathSApply(doc = gpx_parsed, path = "//trkpt/ele", fun = XML::xmlValue)
    if(read_time){
      time = XML::xpathSApply(doc = gpx_parsed, path = "//trkpt/time", fun = XML::xmlValue)
      data_read = tibble::tibble(time = lubridate::as_datetime(time),
                                 station = NA_character_,
                                 lat = as.numeric(coords["lat", ]),
                                 lon = as.numeric(coords["lon", ]),
                                 ele = as.numeric(ele))
    } else {
      data_read = tibble::tibble(time = as.POSIXct(NA_character_),
                                 station = NA_character_,
                                 lat = as.numeric(coords["lat", ]),
                                 lon = as.numeric(coords["lon", ]),
                                 ele = as.numeric(ele))
    }
    return(data_read)
  }

  #The work ####
  data = do.call(rbind,lapply(file, read_gpx_internal, read_time = read_time))
  return(data)
}
