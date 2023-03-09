#' List a list of *.gif files within a directory (and it's subdirectory)
#'
#' @param dir a list of directories
#' @param subdirectory Include subdirectory or not
#'
#' @return
#' @export
#'
#' @examples list_file_gif("C:/Users/carol/Desktop")
list_file_gif = function(dir, subdirectory = T){
  list = list()
  for(i in dir){
    temp = list.files(path = dir, pattern = "*.gif", recursive = subdirectory, full.names = T)
    list = unique(c(list, temp))
  }
  list = unlist(list)
  return(list)
}
