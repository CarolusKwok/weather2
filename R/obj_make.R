#' weather2 object - create a weather2 object using data
#'
#' @param data data, in a dataframe, with columns "station", "time", "type", and "value"
#'
#' @return
#' @export
#'
#' @examples obj_make(dataframe)
obj_make = function(data){
  obj = list(data = data,
             detail = list(station = unique(data$station),
                           time = unique(data$time),
                           type = unique(data$type)))
  class(obj) = c("weather2", "list")
  return(obj)
}
