
suppressPackageStartupMessages( {
  library(purrr)
  library(dplyr)
  library(readr)
  library(stringr)
})


long_lat_to_decimal <- function(str) {
  if (str %>% trimws %>% is.na) {
    return(0)
  }
  main <- substr(str, 1, 3) %>% trimws %>% as.integer
  mins <- substr(str, 4, 6) %>% trimws %>% as.integer
  dir <- substr(str, 7, 7) %>% trimws
  if (identical(dir, 'N') || identical(dir, 'E')) {
    fac <- 1
  } else {
    fac <- -1
  }
  (main + mins / 60) * fac
}


"weather.stations.txt" %>%
  file.path("misc", .) %>%
  read_lines(skip = 41) %>%
  discard(~ nchar(.) < 50) %>%
  discard(~ str_detect(., "ICAO  IATA  SYNOP")) %>%
  map(function(line) {
    data.frame(
      state = substr(line, 1, 2),
      ICAO = substr(line, 21, 24), # ICAO = 4-character international id
      longitude = substr(line, 39, 45) %>% long_lat_to_decimal,
      latitude = substr(line, 48, 54) %>% long_lat_to_decimal,
      station = substr(line, 4, 19) %>% trimws, # STATION = 16 character station long name
      elevation = substr(line, 56, 59) %>% trimws %>% as.integer, # ELEV = Station elevation (meters)
      METAR = identical('X', substr(line, 63, 63)), # M = METAR reporting station.   Also Z=obsolete? site
      country = substr(line, 82, 83)
    )
  }) %>%
  reduce(rbind) %>%
  saveRDS(file.path("rds", "weather_stations.rds"))
