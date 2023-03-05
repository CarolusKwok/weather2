#' weather2 object - split a weather2 object by parameter
#'
#' @param obj a weather2 object
#' @param by parameter in question, accepts "station", "time", "type"
#'
#' @return
#' @export
#'
#' @examples obj_split(obj, by = "station")
obj_split = function(obj, by){
  split = obj$detail[[{{by}}]]
  list = list()

  if(by == "station"){
    for(i in split){
      list = append(list, list(dplyr::filter(obj$data, station == i)))
    }
  }
  if(by == "time"){
    for(i in split){
      list = append(list, list(dplyr::filter(obj$data, time == i)))
    }
  }
  if(by == "type"){
    for(i in split){
      list = append(list, list(dplyr::filter(obj$data, type == i)))
    }
  }

  names(list) = paste0("data_", split)

  obj = append(obj, list)
  class(obj) = c("weather2", "list")
  return(obj)
}
