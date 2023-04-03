#' A wrapper for `ggplot2::aes`
#'
#' @param ...
#'
#' @return
#' @export
#'
#' @examples aes(x = temp)
aes = function(...){
  stuff = ggplot2::aes(...)
  return(stuff)
}
