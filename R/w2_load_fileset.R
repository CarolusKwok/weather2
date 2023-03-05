#' System tools: Download files from website
#'
#' @param data Data frame containing columns "URL", "DIR", "Info"
#' @param title Title of the downloaded data
#' @param subattempt Attempts to be made per download URL to be attempted.
#'
#' @return
#' @export
#'
#' @examples w2_load_fileset(data, title = "Macao tidal data", subattempt = 10, worker = 0)
w2_load_fileset = function(data, title, subattempt = 1, worker = 20){
  worker = as.integer(worker)
  if(worker <= 0){
    return(data)
  } else if(worker == 1){
    weather2::w2_load_fileset_seq(data = data, title = title, subattempt = subattempt)
  } else {
    weather2::w2_load_fileset_stm(data = data, title = title, subattempt = subattempt, worker = worker)
  }
}
