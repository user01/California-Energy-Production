
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
plot_state <- p_state + geom_polygon( data=states, aes(x=long, y=lat, group = group),colour="white", fill="grey10" )
plot_state <- p_state + geom_point( data=plants, aes(x=LONGITUDE, y=LATITUDE, size=`GRID_VOLTAGE_(KV)`, color=ENERGY_SOURCE)) + scale_size(name="Grid Voltage (KV)")
ggsave(file.path("plots", "state.plants.png"), p_state)
# p_state



operating_records <- read_rds(file.path("rds", "operating_records.rds"))
operating_records %>% glimpse
operating_records %>%
  distinct(PLANT_CODE) %>%
  glimpse




#
# generation_data <- read_rds(file.path("rds", "generation_data.rds"))
# generation_data %>% glimpse
#
# generation_data %>%
#   distinct(PLANT_CODE, .keep_all = T) %>%
#   select(ENERGY_SOURCE) %>%
#   unlist() %>%
#   unique() %>%
#   glimpse
#
# generator_data_daily <- read_rds(file.path("rds", "generator_data_daily.rds"))
# generator_data_daily %>% glimpse
#
# generator_data_daily %>%
#   distinct(FACILITY_NAME, .keep_all = T) %>%
#   glimpse
#
# generator_data_daily %>%
#   get("ENERGY_SOURCE", .) %>%
#   as.character %>%
#   unique %>%
#   glimpse
#
#
#
# stocks_url <- "http://infographics.economist.com/2015/tech_stocks/data/stocks.csv"
# stocks <- read.csv(stocks_url, stringsAsFactors=FALSE)
#
# stock_colors <- viridis_pal()(100)
#
# stock_colors %>%
#   glimpse
#
# stocks %>%
#   glimpse
#
# stocks %>%
#   mutate(date=as.Date(quarter, format="%m/%d/%y")) %>%
#   streamgraph(key="ticker", value="nominal", offset="expand") %>%
#   sg_fill_manual(stock_colors) %>%
#   sg_axis_x(tick_interval=10, tick_units="year") %>%
#   sg_legend(TRUE, "Ticker: ") -> p
#
# htmlwidgets::saveWidget(p, "temp.html", selfcontained = TRUE)
