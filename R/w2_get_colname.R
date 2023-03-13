#' System tools: Get a non-clashing column name within a dataframe, by adding a numeric suffix ("_x")
#'
#' @param data Dataframe
#' @param name Column name you are expecting to use
#'
#' @return
#' @export
#'
#' @examples w2_get_colname(data, "predict")
w2_get_colname = function(data, name){
  #Check ####
  if(weather2::w2_check_type_dataframe(value = data, value_name = "data")){return(invisible())}
  if(weather2::w2_check_type_character(value = name, value_name = "name")){return(invisible())}

  #Process... pretty self-explanatory ####
  colnames_data = colnames(data)
  name_trial = name
  n = 0
  while(name_trial %in% colnames_data){
    n = n + 1
    name_trial = paste0(name, "_", n)
  }

  return(name_trial)
}
