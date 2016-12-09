

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
    select(UTILITY_NAME, PLANT_CODE, PLANT_NAME, ENERGY_SOURCE_1, OPERATING_YEAR, PLANNED_RETIREMENT_YEAR) %>%
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

plant_data_generators %>% glimpse

df_operating %>%
  inner_join(plant_data_generators, by=c("DATA_YEAR" = "DATA_YEAR", "ORISPL_CODE" = "PLANT_CODE")) ->
  res

res %>% glimpse
res %>%
  filter(!is.na(`CO2_MASS (tons)`)) %>%
  glimpse

dd %>%
  filter(PLANT_CODE == 228) %>%
  glimpse

data.frame(a = 1:5, x = 9:13, y = "a") %>%
  inner_join(data.frame(b = 0:9, xx = c(10:13,10:12,10:12), y=c("a","b")), by=c("x"="xx","y"))

data.frame(a = 1:5, x = 9:13, y = "a") -> temp_df

temp_df %>%
  `names<-`(toupper(names(temp_df)))

""
