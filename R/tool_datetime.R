#' Tool: To create a sequence of date time
#'
#' @param start Starting time, can be in the format of Date or POSIXct.
#' @param end Ending time, can be in the format of Date or POSIXct.
#' @param tz Time zone of start and end. Must be a string.
#' @param by Time interval of the sequence. Must be a string.
#' @param duration Duration between starting time and ending time. Must be a string. Overrides when both start and end is supplied.
#'
#' @return
#' @export
#'
#' @examples tool_datetime(end = Sys.time(), by = "2 hours", duration = "1 day")
tool_datetime = function(start, end, tz = Sys.timezone(), by, duration){
  #Check for input ####
  if(missing(start) & missing(end)){
    cli::cli_text("Error: {.var start} or/and {.var end} must be filled")
    cli::cli_bullets(c("x" = "You haven't supply anything"))
    return(invisible())
  }
  if(!missing(start)){
    class = class(start)[1]
    if(class == "character"){
      start = try(as.Date(start), silent = T)
    }
    if(!(class(start)[1] %in% c("POSIXct", "POSIXt", "Date"))){
      cli::cli_text("Error: {.var start} must be a Date or a POSIXct")
      cli::cli_bullets(c("x" = "You supplied it with {class}"))
      return(invisible())
    }
  }
  if(!missing(end)){
    class = class(end)[1]
    if(class == "character"){
      end = try(as.Date(end), silent = T)
    }
    if(!(class(end)[1] %in% c("POSIXct", "POSIXt", "Date"))){
      cli::cli_text("Error: {.var end} must be a Date or a POSIXct")
      cli::cli_bullets(c("x" = "You supplied it with {class}"))
      return(invisible())
    }
  }
  if((missing(start) | missing(end)) & missing(duration)){
    cli::cli_text("Error: {.var duration} must be supplied")
    cli::cli_bullets(c("x" = "When only {.var start} or only {.var end} is supplied, duration must be supplied",
                       " " = "You supplied nothing"))
    return(invisible())
  }
  if(missing(tz)){
    if(tz != Sys.timezone()){
      cli::cli_text("Error: {.var tz} must be filled")
      cli::cli_bullets(c("x" = "You haven't supply anything"))
      return(invisible())
    }
  }
  if(missing(by)){
    cli::cli_text("Error: {.var by} must be filled")
    cli::cli_bullets(c("x" = "You haven't supply anything"))
    return(invisible())
  }

  #Create start end POSITct ####
  if(!missing(start)){
    start = as.POSIXct(x = start, tz = tz) %>% lubridate::with_tz(tzone = tz)
  }
  if(!missing(end)){
    end = as.POSIXct(x = end, tz = tz) %>% lubridate::with_tz(tzone = tz)
  }
  #Create start end by duration if needed
  if(missing(start)){
      start = seq.POSIXt(from = end, by = paste0("-", duration), length.out = 2)[2]
  }
  if(missing(end)){
      end = seq.POSIXt(from = start, by = paste0("+", duration), length.out = 2)[2]
  }

  #Check if start > end
  if(start > end){
    temp = start
    start = end
    end = temp
    remove(temp)
  }

  #Create sequence of time
  list = seq.POSIXt(from = start, to = end, by = by)
  return(list)
}
