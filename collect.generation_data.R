
suppressPackageStartupMessages( {
  library(purrr)
  library(dplyr)
  library(lubridate)
  library(readr)
  library(stringr)
  library(readxl)
  library(lubridate)
})


readRDS(file.path("rds", "operating_records.rds"))  %>%
  mutate(DATA_YEAR = as.integer(year(DATE))) ->
  df_operating

df_names_to_upper <- function(df) {
  df %>%
    names %>%
    toupper %>%
    str_replace_all("\\s+", "_") ->
    new_names

  df %>%
    `names<-`(new_names)
}

is_xlxs <- "\\.xlsx$"
gen_regexp <- "Generator.+(201\\d)"
generator_data <- function(path) {
  str_match(path, gen_regexp) %>%
    nth(2) %>%
    as.integer ->
    file_year

  if (str_detect(path, is_xlxs)) {
    skip_count = 1
    if (file_year == 2011){
      sheet_name = "operable"
    } else {
      sheet_name = "Operable"
    }
  } else {
    skip_count = 0
    sheet_name = "Exist"
  }

  suppressWarnings({
    read_excel(path, sheet = sheet_name, skip = skip_count)
  }) -> df

  df %>%
    df_names_to_upper %>%
    filter(STATE == "CA") %>%
    select(UTILITY_ID, UTILITY_NAME, PLANT_CODE, PLANT_NAME, ENERGY_SOURCE_1, OPERATING_YEAR, PLANNED_RETIREMENT_YEAR) %>%
    distinct(PLANT_CODE, .keep_all = TRUE) %>%
    mutate(
      UTILITY_NAME = as.factor(UTILITY_NAME),
      PLANT_CODE = as.integer(PLANT_CODE),
      PLANT_NAME = as.factor(PLANT_NAME),
      ENERGY_SOURCE = as.factor(ENERGY_SOURCE_1),
      OPERATING_YEAR = as.integer(OPERATING_YEAR),
      PLANNED_RETIREMENT_YEAR = as.integer(PLANNED_RETIREMENT_YEAR)
    ) %>%
    select(-ENERGY_SOURCE_1) %>%
    cbind(DATA_YEAR = file_year)
}

# Loads in data concerning generator types in plants
list.files("eia", recursive=TRUE, pattern=".xls") %>%
  str_subset(gen_regexp) %>%
  file.path("eia", .) %>%
  map(generator_data) %>%
  reduce(rbind) %>%
  select(-UTILITY_ID) ->
  plant_data_generators
# plant_data_generators %>% glimpse


str_detect_lgl <- function(str, pattern){
  res <- str_detect(str, pattern)
  if (is.na(res)) {
    return(FALSE)
  }
  res
}
fix_water_source <- function(water_vec) {
  water_vec %>%
    tolower %>%
    map_chr(function(src){
      if (is.na(src)) {
        return(NA)
      }
      if (str_detect_lgl(src,'^n/a$')) {
        return(NA)
      }
      if (str_detect_lgl(src,'^na$')) {
        return(NA)
      }
      if (str_detect_lgl(src,'unavailable')) {
        return(NA)
      }
      if (str_detect_lgl(src,'^none$')) {
        return(NA)
      }

      if (str_detect_lgl(src,'municipality')) {
        return('city')
      }
      if (str_detect_lgl(src,'city')) {
        return('city')
      }
      if (str_detect_lgl(src,'district')) {
        return('city')
      }
      if (str_detect_lgl(src,'reclaim')) {
        return('city')
      }
      if (str_detect_lgl(src,'waste')) {
        return('city')
      }

      if (str_detect_lgl(src,'well')) {
        return('well')
      }
      if (str_detect_lgl(src,'air')) {
        return('air')
      }

      src
    })
}



fix_zip <- function(df) {
  df_zip <- if ("ZIP5" %in% names(df)) {
    df %>%
    mutate(
      ZIP = ZIP5
      ) %>%
      select(-ZIP5)
  } else {
    df
  }
  df_zip
}

fix_long_lat <- function(df) {
  df_lon_lat <- if ("LATITUDE" %in% names(df)) {
    df
  } else {
    df %>%
      mutate(
        LATITUDE = NA,
        LONGITUDE = NA
      )
  }
  df_lon_lat
}

fix_grid_voltage <- function(df) {
  df_grid_voltage <- if ("GRID_VOLTAGE_(KV)" %in% names(df)) {
    df
  } else if ("GRIDVOLTAGE" %in% names(df)) {
    df %>%
      mutate(
        `GRID_VOLTAGE_(KV)` = GRIDVOLTAGE
      ) %>%
      select(-GRIDVOLTAGE)
  } else {
    # print("Missing --- ")
    # print(names(df))
    df %>%
      mutate(
        `GRID_VOLTAGE_(KV)` = NA
      )
  }
  df_grid_voltage
}

plant_regexp <- "Plant.+(201\\d)"
plant_regexp_wide <- "Plant"
plant_data <- function(path) {
  str_match(path, plant_regexp) %>%
    nth(2) %>%
    as.integer ->
    file_year

  if (is.na(file_year)){
    file_year = 2011
  }

  if (str_detect(path, is_xlxs)) {
    skip_count = 1
    if (file_year == 2011) {
      sheet_name = "plant2011"
    } else if (file_year == 2012) {
      sheet_name = "plant2012"
    } else {
      sheet_name = "Plant"
    }
  } else {
    skip_count = 0
    sheet_name = "PlantY2010"
  }

  suppressWarnings({
    read_excel(path, sheet = sheet_name, skip = skip_count) %>%
      df_names_to_upper
  }) -> df

  df %>%
    filter(STATE == "CA") %>%
    fix_zip %>%
    fix_long_lat %>%
    fix_grid_voltage %>%
    select(UTILITY_ID, PLANT_CODE, PLANT_NAME, STREET_ADDRESS, CITY, ZIP, NAME_OF_WATER_SOURCE, SECTOR_NAME, LATITUDE, LONGITUDE, `GRID_VOLTAGE_(KV)`) %>%
    mutate(
      UTILITY_ID = as.integer(UTILITY_ID),
      PLANT_CODE = as.integer(PLANT_CODE),
      PLANT_NAME = as.factor(PLANT_NAME),
      ZIP = as.integer(ZIP),
      NAME_OF_WATER_SOURCE = as.factor(fix_water_source(NAME_OF_WATER_SOURCE)),
      SECTOR_NAME = as.factor(SECTOR_NAME)
    ) %>%
    cbind(DATA_YEAR = as.integer(file_year))
}


# Loads in data concerning location in plants
list.files("eia", recursive=TRUE, pattern=".xls") %>%
  str_subset(plant_regexp_wide) %>%
  file.path("eia", .) %>%
  map(plant_data) %>%
  reduce(rbind) %>%
  select(-PLANT_NAME) ->
  plant_data_meta


# df_operating %>% glimpse
# plant_data_generators %>% glimpse
# plant_data_meta %>% glimpse

plant_data_meta %>%
  filter(!is.na(LONGITUDE)) %>%
  distinct(PLANT_CODE, .keep_all = TRUE) %>%
  select(PLANT_CODE, LONGITUDE, LATITUDE) ->
  plant_data_meta_locations

plant_data_meta %>%
  filter(!is.na(`GRID_VOLTAGE_(KV)`)) %>%
  distinct(PLANT_CODE, .keep_all = TRUE) %>%
  select(PLANT_CODE, `GRID_VOLTAGE_(KV)`) ->
  plant_data_meta_voltage

plant_data_meta %>%
  select(-LONGITUDE, -LATITUDE, -`GRID_VOLTAGE_(KV)`) %>%
  left_join(plant_data_meta_locations, by="PLANT_CODE") %>%
  left_join(plant_data_meta_voltage, by="PLANT_CODE") %>%
  mutate(
      `GRID_VOLTAGE_(KV)` = as.numeric(`GRID_VOLTAGE_(KV)`)
    ) ->
  plant_data_meta_full


df_operating %>%
  left_join(plant_data_generators, by=c("DATA_YEAR" = "DATA_YEAR", "PLANT_CODE" = "PLANT_CODE")) %>%
  left_join(plant_data_meta_full, by=c("DATA_YEAR" = "DATA_YEAR", "PLANT_CODE" = "PLANT_CODE")) %>%
  # hard coded location setting for some plants
  mutate( # Fix for the missing NRG Station A
    LONGITUDE = ifelse(STREET_ADDRESS == "1201-A Illinois Street", -122.383241, LONGITUDE),
    LATITUDE = ifelse(STREET_ADDRESS == "1201-A Illinois Street", 37.756230, LATITUDE)
  ) %>%
  mutate( # Fix for the missing Dynegy South
    LONGITUDE = ifelse(STREET_ADDRESS == "990 Bay Boulevard", -117.092287, LONGITUDE),
    LATITUDE = ifelse(STREET_ADDRESS == "990 Bay Boulevard", 32.611443, LATITUDE)
  ) ->
  generation_data

generation_data %>% saveRDS(file.path("rds", "generation_data.rds"))
