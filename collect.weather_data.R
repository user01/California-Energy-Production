

suppressPackageStartupMessages( {
  library(purrr)
  library(dplyr)
  library(lubridate)
  library(readr)
  library(stringr)
  library(readxl)
  library(jsonlite)
  library(weatherData)
  library(geosphere)
})

generation_data <- readRDS(file.path("rds", "generation_data.rds"))
weather_stations <- readRDS(file.path("rds", "weather_stations.rds"))
# generation_data %>% glimpse
# weather_stations %>% glimpse


# Helper code to check which stations actually match
# ==============================================================================
# weather_stations %>%
#   filter(state == 'CA') %>%
#   get("ICAO", .) %>%
#   unlist %>%
#   unique %>%
#   as.character -> picked_stations
#
# picked_stations %>% length
#
# picked_stations %>%
#   map(function(id){
#       getWeatherForDate(id,
#                         "2014-04-01",
#                         end_date="2014-04-01",
#                         opt_detailed=T,
#                         opt_custom_columns=T,
#                         custom_columns=c(2,4,8,10,12)) -> x
#       list(id, x)
#     }) -> res
#
# res %>%
#   discard(~ is.null(.[[2]])) ->
#   res_good
#
# res_good %>%
#   map_chr(~ .[[1]]) ->
#   res_good_chr
#
# res_good_chr %>% length
#
# dput(res_good_chr)


# Not all CA stations have hourly data, but these are known to have data
c("KAAT", "KACV", "KAUN", "KAVX", "KBFL", "KBAB", "KL35",
  "KBIH", "KBLH", "KBAN", "KBUR", "KCMA", "KNFG", "KCZZ", "KCRQ",
  "KCIC", "KNID", "KCNO", "KO22", "KCCR", "KAJO", "KCEC", "KDAG",
  "KEDU", "KEDW", "K9L2", "KNJK", "KEMT", "KBLU", "KFAT",
  "KFUL", "KGOO", "KHAF", "KHJO", "KHHR", "KHWD", "KIPL",
  "KNRS", "KPOC", "KWJF", "KNLC", "KLHM", "KLVK", "KLPC", "KLGB",
  "KSLI", "KLAX", "KCQT", "KWHP", "KMAE", "KMMH", "KMYV", "KMHR",
  "KMCC", "KMCE", "KMER", "KNKX", "KMOD", "KNUQ", "KMHV", "KSIY",
  "KMRY", "KMHS", "KMWS", "KF70", "KAPC", "KEED", "KNZY", "KDVO",
  "KOAK", "KOKB", "KNXF", "KONT", "KOVE", "KOXR", "KPSP", "KPMD",
  "KGXA", "KPAO", "KPRB", "KO69", "KPVF", "KNTD", "KPTV", "KRBL",
  "KRDD", "KRAL", "KRIV", "KRNM", "KSAC", "KSMF", "KSNS", "KSBD",
  "KSQL", "KNUC", "KSAN", "KMYF", "KSDM", "KSEE", "KSFO", "KSJC",
  "KRHV", "KSBP", "KNSI", "KSDB", "KSNA", "KSBA", "KSMX",
  "KSMO", "KSTS", "KIZA", "KTVL", "KSCK", "KTRM", "KTOA", "KSUU",
  "KTRK", "KTSP", "KO86", "KNXP", "KUKI", "KVCB", "KVNY", "KVBG",
  "KVCV", "KVIS", "KWVI", "KO54") -> weather_stations_with_hourly

generation_data %>%
  distinct(PLANT_CODE, .keep_all = TRUE) %>%
  select(PLANT_CODE, LONGITUDE, LATITUDE) ->
  generator_positions
weather_stations %>%
  filter(ICAO %in% weather_stations_with_hourly) %>%
  select(longitude,latitude) ->
  weather_positions

distance_mat <- distm(generator_positions %>% select(-PLANT_CODE),
                      weather_positions,
                      fun = distCosine)
weather_stations %>%
  filter(ICAO %in% weather_stations_with_hourly) %>%
  slice(apply(distance_mat, 1, which.min)) %>%
  select(ICAO) %>%
  mutate(ICAO = as.character(ICAO)) %>%
  cbind(generator_positions %>% select(PLANT_CODE)) ->
  generators_to_weather_stations

generation_data %>%
  left_join(generators_to_weather_stations, on="PLANT_CODE") ->
  generator_data_located


generator_data_located %>%
  get("ICAO",.) %>%
  unique %>%
  map(function(id) {
    cur_path <- file.path("misc", paste0(id, ".csv"))
    if (file.exists(cur_path)) {
      return(list(id,
                  read_csv(cur_path, col_types = cols(
                    Date = col_datetime(format = ""),
                    Max_TemperatureF = col_integer(),
                    Mean_TemperatureF = col_integer(),
                    Min_TemperatureF = col_integer()
                  )
                )))
    }

    try({
      getSummarizedWeather(id,
                           "2010-01-01",
                           end_date="2010-12-31") -> weather2010
      getSummarizedWeather(id,
                           "2011-01-01",
                           end_date="2011-12-31") -> weather2011
      getSummarizedWeather(id,
                           "2012-01-01",
                           end_date="2012-12-31") -> weather2012
      getSummarizedWeather(id,
                           "2013-01-01",
                           end_date="2013-12-31") -> weather2013
      getSummarizedWeather(id,
                           "2014-01-01",
                           end_date="2014-12-31") -> weather2014
      getSummarizedWeather(id,
                           "2015-01-01",
                           end_date="2015-12-31") -> weather2015
      rbind(weather2010, weather2011, weather2012, weather2013, weather2014, weather2015)
    }) -> weather_results

    write_csv(weather_results, cur_path)
    list(id, weather_results)
  }) ->
  station_data

station_data %>%
  map(function(station_set){
    station_id <- station_set[[1]]
    station_set %>%
      nth(2) %>%
      cbind(data.frame(ICAO = station_id))
  }) %>%
  reduce(rbind) %>%
  mutate(
    Date = as_date(Date),
    ICAO = as.character(ICAO)
  ) ->
  station_data_all

# station_data_all %>%
#   glimpse
# generator_data_located %>% glimpse

generator_data_located %>%
  select(FACILITY_NAME, PLANT_CODE, DATE, OP_TIME, ENERGY_SOURCE, LONGITUDE, LATITUDE, `GRID_VOLTAGE_(KV)`, ICAO) %>%
  mutate(Date = as_date(DATE)) %>%
  group_by(Date, PLANT_CODE) %>%
  summarise(
    FACILITY_NAME = FACILITY_NAME %>% first,
    ENERGY_SOURCE = ENERGY_SOURCE %>% first,
    LONGITUDE = LONGITUDE %>% first,
    LATITUDE = LATITUDE %>% first,
    GRIDVOLTAGE = `GRID_VOLTAGE_(KV)` %>% first,
    ICAO = ICAO %>% first,
    OP_TIME = mean(OP_TIME)
    ) %>%
  left_join(station_data_all, by=c("ICAO" = "ICAO", "Date" = "Date")) %>%
  select(-ICAO) %>%
  mutate(
    DATE = Date,
    TEMP_F_MAX = Max_TemperatureF,
    TEMP_F_MIN = Min_TemperatureF,
    TEMP_F_MEAN = Mean_TemperatureF
  ) %>%
  ungroup() %>%
  select(DATE, FACILITY_NAME, PLANT_CODE, ENERGY_SOURCE, OP_TIME, GRIDVOLTAGE, LONGITUDE, LATITUDE, TEMP_F_MAX, TEMP_F_MIN, TEMP_F_MEAN) ->
  generator_data_daily

# generator_data_daily %>%
#   glimpse
#
#
# generator_data_daily %>%
#   distinct(PLANT_CODE) %>%
#   glimpse
#
# generator_data_daily %>%
#   distinct(DATE) %>%
#   glimpse

generator_data_daily %>% saveRDS(file.path("rds", "generator_data_daily.rds"))
