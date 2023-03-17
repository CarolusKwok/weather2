#' List a list of files with a certain pattern, within a directory (and it's subdirectory)
#'
#' @param dir a list of directories
#' @param pattern a list of patterns to look for. Uses regular expression
#' @param subdirectory Include subdirectory or not
#'
#' @return
#' @export
#'
#' @examples file_list("C:/Users/carol/Desktop", "*_hi.csv")
file_list = function(dir, pattern, subdirectory = T){
  list = c()
  for(d in dir){
    for(p in pattern){
      temp = list.files(path = d, pattern = p, recursive = subdirectory, full.names = T)
      list = c(list, temp)
    }
  }
  list = unique(list)
  return(list)
}

#' List a list of .csv files within a directory (and it's subdirectory)
#'
#' @param dir a list of directories
#' @param subdirectory Include subdirectory or not
#'
#' @return
#' @export
#'
#' @examples file_list_csv("C:/Users/carol/Desktop")
file_list_csv = function(dir, subdirectory = T){
  list = weather2::file_list(dir = dir, pattern = "*.csv", subdirectory = subdirectory)
  return(list)
}

#' List a list of *.gif files within a directory (and it's subdirectory)
#'
#' @param dir a list of directories
#' @param subdirectory Include subdirectory or not
#'
#' @return
#' @export
#'
#' @examples file_list_gif("C:/Users/carol/Desktop")
file_list_gif = function(dir, subdirectory = T){
  list = weather2::file_list(dir = dir, pattern = "*.gif", subdirectory = subdirectory)
  return(list)
}

#' List a list of .jpg (and other common extensions) files within a directory (and it's subdirectory)
#'
#' @param dir a list of directories
#' @param subdirectory Include subdirectory or not
#'
#' @return
#' @export
#'
#' @examples file_list_jpg("C:/Users/carol/Desktop")
file_list_jpg = function(dir, subdirectory = T){
  list = weather2::file_list(dir = dir, pattern = c("*.jpg", "*.JPG", "*.JPEG", "*.jpe"), subdirectory = subdirectory)
  return(list)
}

#' List a list of *.png files within a directory (and it's subdirectory)
#'
#' @param dir a list of directories
#' @param subdirectory Include subdirectory or not
#'
#' @return
#' @export
#'
#' @examples file_list_png("C:/Users/carol/Desktop")
file_list_png = function(dir, subdirectory = T){
  list = weather2::file_list(dir = dir, pattern = "*.png", subdirectory = subdirectory)
  return(list)
}
