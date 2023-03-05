#' Read a list of *.csv file with the same format into a data frame
#'
#' @param file_list a list of csv file directories
#'
#' @return
#' @export
#'
#' @examples read_file_csv(c("C:/Users/carol/Desktop/1.csv", "C:/Users/carol/Desktop/2.csv", "C:/Users/carol/Desktop/3.csv"))
read_file_csv = function(file_list){
  df = do.call(rbind,lapply(file_list, utils::read.csv))
  return(df)
}
