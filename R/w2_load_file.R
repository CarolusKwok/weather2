#' System tools: Download files from website
#'
#' @param data Data frame containing columns "URL", "DIR", "Info"
#' @param title Title of the downloaded data
#' @param list_fail List failed-to-download items
#' @param attempt Attempts to be made per download.
#' @param worker Numbers of sessions to be open
#' @param threshold Threshold of file size, when compared to the largest item to be installed in the list
#' @param check Check the all of the parameters before execution, default as T
#'
#' @return
#' @export
#'
#' @examples w2_load_file(data, title = "Global nc data", attempt = 10, worker = 10, list_fail = T, threshold = 0.5)

w2_load_file = function(data, title = "test", attempt = 5, worker = 0, list_fail = T, threshold = 0.5, check = T){
  #Format download process information and CHECK ####
  worker = as.integer(worker)
  attempt = as.integer(attempt)
  if(check != F){
    if(weather2::w2_check_load(data = data,
                               title = title,
                               attempt = attempt,
                               worker = worker,
                               list_fail = list_fail,
                               threshold = threshold)){return(invisible())}
  }

  #Start ####
  if(worker <= 0){
    return(data)
  } else if(worker == 1){
    weather2::w2_load_file_seq(data = data,
                               title = title,
                               list_fail = list_fail,
                               attempt = attempt,
                               threshold = threshold)
  } else {
    weather2::w2_load_file_stm(data = data,
                               title = title,
                               attempt = attempt,
                               worker = worker,
                               list_fail = list_fail,
                               threshold = threshold)
  }
}
