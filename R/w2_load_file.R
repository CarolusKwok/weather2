#' System tools: Download files from website
#'
#' @param data Data frame containing columns "URL", "DIR", "Info"
#' @param title Title of the downloaded data
#' @param list_fail List failed-to-download items
#' @param attempt Attempts to be made per download.
#' @param worker Numbers of sessions to be open
#' @param threshold Threshold of file size, when compared to the largest item to be installed in the list
#'
#' @return
#' @export
#'
#' @examples w2_load_file(data, title = "Global nc data", attempt = 10)
w2_load_file = function(data, title, list_fail = F, attempt = 10, worker = 0, threshold = 0.5){
  #Format download process information ####
  worker = as.integer(worker)
  if(worker <= 0){
    return(data)
  } else if(worker == 1){
    weather2::w2_load_file_seq(data = data, title = title, list_fail = list_fail, attempt = attempt)
  } else {
    weather2::w2_load_file_stm(data = data, title = title, attempt = attempt, worker = worker, list_fail = list_fail, threshold = threshold)
  }
}
