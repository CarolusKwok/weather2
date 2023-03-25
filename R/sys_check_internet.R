#' System tool: Check if internet is connected
#'
#' @param attempt Attempts of internet check to be made. Default as 5.
#' @param silent Silent successful message
#' @param websites Website to test
#'
#' @return
#' @export
#'
#' @examples sys_ck_internet()
sys_ck_internet = function(attempt = 5, silent = T, websites = c("https://google.com", "https://www.un.org/")){
  flag = F
  for(i in 1:attempt){
    for(w in websites){
      temp = suppressWarnings(tryCatch(readLines(w, warn = F), error = function(e){NA}))
      if(!anyNA(temp)){
        flag = T
        break}
    }
  }
  if(flag == T & !silent){
    cli::cli_bullets(c("v" = 'Internet is connected.'))
  }
  if(flag == F){
    cli::cli_text('Error: Internet must be connected.')
    cli::cli_bullets(c("x" = 'Please check the your internet connection'))
  }
  return(flag)
}
