#' List a list of *.csv files within a directory (and it's subdirectory)
#'
#' @param dir a list of directories
#' @param subdirectory Include subdirectory or not
#'
#' @return
#' @export
#'
#' @examples list_file_csv("C:/Users/carol/Desktop")
list_file_csv = function(dir, subdirectory = T){
  list = list()
  for(i in dir){
    temp = list.files(path = dir, pattern = "*.csv", recursive = subdirectory, full.names = T)
    list = unique(c(list, temp))
  }
  return(list)
}
