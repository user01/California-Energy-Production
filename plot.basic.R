
suppressPackageStartupMessages( {
  library(purrr)
  library(dplyr)
  library(lubridate)
  library(readr)
  library(stringr)
  library(streamgraph)
  library(viridis)
  library(ggplot2)
  library(maps)
  library(htmlwidgets)
})


switch_src <- function(src) {
  if (identical(src,"BIT")) {
    return("Coal")
  }
  if (identical(src,"NG")) {
    return("Natural Gas")
  }
  if (identical(src,"GEO")) {
    return("Geothermal")
  }
  if (identical(src,"WH")) {
    return(NA)
  }
  if (identical(src,"WAT")) {
    return("Hydro")
  }
  if (identical(src,"OG")) {
    return("Natural Gas")
  }
  if (identical(src,"WDS")) {
    return("Waste")
  }
  if (identical(src,"OTH")) {
    return(NA)
  }
  if (identical(src,"WND")) {
    return("Wind")
  }
  if (identical(src,"DFO")) {
    return("Oil")
  }
  if (identical(src,"MSW")) {
    return("Waste")
  }
  if (identical(src,"OBG")) {
    return("Waste")
  }
  if (identical(src,"SUN")) {
    return("Solar")
  }
  if (identical(src,"LFG")) {
    return("Waste")
  }
  if (identical(src,"PC ")) {
    return( "Oil")
  }
  if (identical(src,"NUC")) {
    return("Nuclear")
  }
  if (identical(src,"SUB")) {
    return("Coal")
  }
  if (identical(src,"AB ")) {
    return("Waste")
  }
  if (identical(src,"JF ")) {
    return("Oil")
  }
  if (identical(src,"BLQ")) {
    return("Waste")
  }
  if (identical(src,"PUR")) {
    return("Waste")
  }
  if (identical(src,"MWH")) {
    return(NA)
  }
  return(NA)
}
plant_data_generators <- read_rds(file.path("rds", "plant_data_generators.rds"))
plant_data_generators %>%
  distinct(PLANT_CODE, .keep_all = T) %>%
  select(PLANT_CODE, ENERGY_SOURCE) %>%
  mutate(
    ENERGY_SOURCE = map_chr(ENERGY_SOURCE, switch_src)
    ) ->
  plant_sources

plant_data_meta <- read_rds(file.path("rds", "plant_data_meta.rds"))
# plant_data_meta %>% glimpse


plant_data_meta %>%
  distinct(PLANT_CODE, .keep_all = T) %>%
  filter(PLANT_CODE != 60237) %>%
  filter(PLANT_CODE != 56295) %>%
  filter(PLANT_CODE != 57923) %>%
  left_join(plant_sources, on="PLANT_CODE") %>%
  filter(!is.na(ENERGY_SOURCE)) %>%
  mutate(ENERGY_SOURCE = as.factor(ENERGY_SOURCE)) ->
  plants

all_states <- map_data("state")
#plot all states with ggplot
states <- subset(all_states, region %in% c( "california" ) )
plot_state <- ggplot()
plot_state <- plot_state + geom_polygon( data=states, aes(x=long, y=lat, group = group),colour="white", fill="grey10" )
plot_state <- plot_state + geom_point( data=plants, aes(x=LONGITUDE, y=LATITUDE, size=`GRID_VOLTAGE_(KV)`, color=ENERGY_SOURCE)) + scale_size(name="Grid Voltage (KV)")
ggsave(file.path("plots", "state.plants.png"), plot_state)




generator_data_daily <- read_rds(file.path("rds", "generator_data_daily.rds"))

generator_data_daily %>%
  filter(ENERGY_SOURCE == "NG") %>%
  group_by(PLANT_CODE, DATE) %>%
  summarise(
    FACILITY_NAME = FACILITY_NAME %>% first,
    OP_TIME = mean(OP_TIME, na.rm = T),
    GRIDVOLTAGE = mean(GRIDVOLTAGE, na.rm = T)
  ) %>%
  mutate(
    date=as.Date(DATE, format="%m/%d/%y"),
    contribution = OP_TIME * GRIDVOLTAGE
    ) %>%
  ungroup() %>%
  streamgraph(key="FACILITY_NAME", value="contribution", "date") %>%
  # streamgraph(key="FACILITY_NAME", value="contribution", "date", offset="expand") %>%
  sg_axis_x(tick_interval=10, tick_units="month") %>%
  sg_legend(TRUE, "Ticker: ") -> plot_steamgraph

htmlwidgets::saveWidget(plot_steamgraph, "steamgraph_all.html", selfcontained = TRUE)


moving_avg <- function(x,n=5){
  stats::filter(x,rep(1/n,n), sides=1)
}

generator_data_daily %>%
  filter(ENERGY_SOURCE == "NG") %>%
  group_by(PLANT_CODE, DATE) %>%
  summarise(
    FACILITY_NAME = FACILITY_NAME %>% first,
    OP_TIME = mean(OP_TIME, na.rm = T),
    GRIDVOLTAGE = mean(GRIDVOLTAGE, na.rm = T)
  ) %>%
  mutate(
    date=as.Date(DATE, format="%m/%d/%y"),
    contribution = OP_TIME * GRIDVOLTAGE
    ) %>%
  ungroup() -> t

t %>%
  filter(PLANT_CODE %% 5 == 0 & PLANT_CODE %% 3 == 0 ) %>%
  streamgraph(key="FACILITY_NAME", value="contribution", "date") %>%
  sg_axis_x(tick_interval=10, tick_units="month") %>%
  sg_legend(TRUE, "Ticker: ") -> plot_steamgraph_sml

htmlwidgets::saveWidget(plot_steamgraph_sml, "steamgraph_sub.html", selfcontained = TRUE)
