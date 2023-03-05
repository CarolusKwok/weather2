#' Tool: Provide dictionary of commonly used METAR abbreviation/ codes
#'
#' @param type Type of dictionary to provide. Only accepts "weather" or "cloud", and only 1 input.
#'
#' @return
#' @export
#'
#' @examples tool_metar_dict("weather")
tool_metar_dict = function(type = "weather"){
  #Check input ####
  if(length(type) != 1){
    cli::cli_text("Error: {.var type} has more than 1 input.")
    cli::cli_bullets(c("x" = "You supplied {length(type)} input."))
    return(invisible())
  }
  if(type != "weather" & type != "cloud"){
    cli::cli_text('Error: {.var type} must be "weather" or "cloud".')
    cli::cli_bullets(c("x" = 'You supplied "{type}" as input.'))
    return(invisible())
  }
  #Return suitable dictionary ####
  if(type == "weather"){
    metar = tibble::tibble(code = c("+", "-",
                                    "BC", "BL", "DR", "DL",
                                    "FZ", "MI", "PR", "SH",
                                    "TS", "VC",
                                    "BR", "DU", "DS", "DZ",
                                    "FC", "FG", "FU", "GR",
                                    "GS", "HZ", "PL", "PO",
                                    "RA", "SA", "SG", "SN",
                                    "SQ", "SS", "VA",
                                    "UP", "WS"),
                           mean = c("Heavy", "Light",
                                     "Patches", "Blowing", "Drifting", "Distant lightning",
                                    "Freezing", "Shallow",  "Partial", "Showers",
                                    "Thunderstorm", "in the Vicinity",
                                            "Mist",       "Dust",   "Duststorm",    "Drizzle",
                                    "Funnel cloud",        "Fog",       "Smoke",       "Hail",
                                      "Small hail",       "Haze", "Ice pellets", "Dust devil",
                                            "Rain",       "Sand", "Snow grains",       "Snow",
                                          "Squall",  "Sandstorm","Volcanic ash",
                                    "Unidentified precipitation", "Wind shear"))
  }
  if(type == "cloud"){
    metar = tibble::tibble(
      code = c("FEW", "SCT", "BKN", "OVC",
               "NSC", "NCD", "CLR"),
      mean = c("Few (1 to 2 oktas)",
               "Scattered (3 to 4 oktas)",
               "Broken (5 to 7 oktas)",
               "Overcast (8 oktas)",
               "Nil significant cloud",
               "Nil cloud detected",
               "No cloud under 12,000 ft")
    )
  }
  return(metar)
}
