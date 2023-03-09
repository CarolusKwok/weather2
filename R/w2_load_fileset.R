#' System tools: Download files from website
#'
#' @param data Data frame containing columns "URL", "DIR", "Info", "Set"
#' @param attempt Attempts to be made per download URL to be attempted
#' @param worker Numbers of sessions to be open
#' @param list_fail List failed-to-download items
#' @param threshold Threshold of file size, when compared to the largest item to be installed in the list
#' @param title Title of the downloaded data
#' @param check Check the all of the parameters before execution, default as T
#'
#' @return
#' @export
#'
#' @examples w2_load_fileset(data, title = "Macao tidal data", attempt = 10, worker = 20, list_fail = T, threshold = 0.5)
w2_load_fileset = function(data, title = "test", attempt = 5, worker = 0, list_fail = T, threshold = 0.5, check = T){
  #Format download process information and CHECK ####
  worker = as.integer(worker)
  attempt = as.integer(attempt)
  if(check != F){
    if(weather2::w2_check_load_set(data = data,
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
    weather2::w2_load_fileset_seq(data = data,
                                  title = title,
                                  attempt = attempt,
                                  threshold = threshold,
                                  list_fail = list_fail)
  } else {
    weather2::w2_load_fileset_stm(data = data,
                                  title = title,
                                  attempt = attempt,
                                  worker = worker,
                                  threshold = threshold,
                                  list_fail = list_fail)
  }
}
