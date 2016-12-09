
suppressPackageStartupMessages( {
  library(purrr)
  library(dplyr)
  library(lubridate)
  library(readr)
  library(stringr)
})


file_paths <- list.files("epa", recursive=TRUE, pattern=".zip")

read_epa_data <- function(path){
  path %>%
    read_csv(col_types = cols(
      STATE = col_character(),
      FACILITY_NAME = col_character(),
      ORISPL_CODE = col_integer(),
      UNITID = col_character(),
      OP_DATE = col_character(),
      OP_HOUR = col_integer(),
      OP_TIME = col_double(),
      `GLOAD (MW)` = col_integer(),
      `SO2_MASS (lbs)` = col_double(),
      `SO2_RATE (lbs/mmBtu)` = col_double(),
      `NOX_RATE (lbs/mmBtu)` = col_double(),
      `NOX_MASS (lbs)` = col_double(),
      `CO2_MASS (tons)` = col_double(),
      `CO2_RATE (tons/mmBtu)` = col_double(),
      `HEAT_INPUT (mmBtu)` = col_double(),
      FAC_ID = col_integer(),
      UNIT_ID = col_integer()
    )) %>%
    mutate(
        DATE = fast_strptime(paste(OP_DATE, OP_HOUR),
                             "%m-%d-%Y %H",
                             tz = "UTC",
                             lt = FALSE)
      ) %>%
    filter(STATE == "CA") %>%
    select(FACILITY_NAME,
      ORISPL_CODE,
      UNIT_ID,
      DATE,
      `GLOAD (MW)`,
      `SO2_MASS (lbs)`,
      `SO2_RATE (lbs/mmBtu)`,
      `NOX_RATE (lbs/mmBtu)`,
      `NOX_MASS (lbs)`,
      `CO2_MASS (tons)`,
      `CO2_RATE (tons/mmBtu)`
    )
}


file_paths %>%
  map_chr(~ file.path("epa", .)) %>%
  map(read_epa_data) %>%
  reduce(rbind) %>%
  mutate(
      FACILITY_NAME = as.factor(FACILITY_NAME),
      PLANT_CODE = ORISPL_CODE
    ) %>%
  select(-ORISPL_CODE) ->
  data_results
# data_results %>% glimpse

data_results %>% saveRDS(file.path("rds", "operating_records.rds"))
