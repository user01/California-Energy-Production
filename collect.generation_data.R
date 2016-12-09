
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
  reduce(rbind) ->
  plant_data_generators

# plant_data_generators %>% glimpse




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

  df_zip <- if ("ZIP5" %in% names(df)) {
    df %>%
    mutate(
      ZIP = ZIP5
      ) %>%
      select(-ZIP5)
  } else {
    df
  }

  df_zip %>%
    filter(STATE == "CA") %>%
    select(UTILITY_ID, PLANT_CODE, PLANT_NAME, STREET_ADDRESS, CITY, ZIP, NAME_OF_WATER_SOURCE, SECTOR_NAME) %>%
    mutate(
      UTILITY_ID = as.integer(UTILITY_ID),
      PLANT_CODE = as.integer(PLANT_CODE),
      PLANT_NAME = as.factor(PLANT_NAME),
      ZIP = as.integer(ZIP),
      NAME_OF_WATER_SOURCE = as.factor(NAME_OF_WATER_SOURCE),
      SECTOR_NAME = as.factor(SECTOR_NAME)
    ) %>%
    cbind(DATA_YEAR = file_year)
}


# Loads in data concerning location in plants
list.files("eia", recursive=TRUE, pattern=".xls") %>%
  str_subset(plant_regexp_wide) %>%
  file.path("eia", .) %>%
  map(plant_data) %>%
  reduce(rbind) ->
  plant_data_meta

# plant_data_meta %>% glimpse



df_operating %>%
  inner_join(plant_data_generators, by=c("DATA_YEAR" = "DATA_YEAR", "ORISPL_CODE" = "PLANT_CODE")) %>%
  inner_join(plant_data_meta, by=c("DATA_YEAR" = "DATA_YEAR", "ORISPL_CODE" = "PLANT_CODE")) ->
  generation_data

# generation_data %>% glimpse
data_results %>% saveRDS(file.path("rds", "generation_data.rds"))
