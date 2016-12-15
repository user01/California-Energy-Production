

suppressPackageStartupMessages( {
  library(purrr)
  library(dplyr)
  library(multidplyr)
  library(lubridate)
  library(readr)
  library(stringr)
  require(doParallel)
  library(randomForest)
  library(geosphere)
  library(ggplot2)
})

generator_data_daily <- read_rds(file.path("rds", "generator_data_daily.rds"))
# generator_data_daily %>% glimpse

# Source: http://stackoverflow.com/a/39894310/2601448
getSeason <- function(input.date){
  numeric.date <- 100*month(input.date)+day(input.date)
  ## input Seasons upper limits in the form MMDD in the "break =" option:
  cuts <- base::cut(numeric.date, breaks = c(0,319,0620,0921,1220,1231))
  # rename the resulting groups (could've been done within cut(...levels=) if "Winter" wasn't double
  levels(cuts) <- c("Winter","Spring","Summer","Fall","Winter")
  return(cuts)
}

moving_avg <- function(x,n=5){
  stats::filter(x,rep(1/n,n), sides=1)
}


# hidden markov chain
generator_data_daily %>%
  filter(ENERGY_SOURCE == "NG") %>%
  group_by(PLANT_CODE, DATE) %>%
  mutate(
    OP_TIME = ifelse(is.na(OP_TIME), 0 , OP_TIME),
    ACTIVE = OP_TIME > 0.5
  ) %>%
  ungroup() %>%
  mutate(
    WEEKDAY = as.factor(wday(DATE, label = TRUE)),
    WEEKEND = WEEKDAY == "Sat" | WEEKDAY == "Sun",
    SEASON = getSeason(DATE),
    ACTIVE = as.factor(ACTIVE)
  ) %>%
  na.omit() %>%
  arrange(DATE) ->
  generators_clean

# generators_clean %>%
#   glimpse

generators_clean %>%
  distinct(PLANT_CODE, .keep_all = T) ->
  plant_data

plant_data %>%
  get('PLANT_CODE', .) %>%
  map(function(plant_code){

    plant_data %>%
      filter(PLANT_CODE == plant_code) %>%
      select(LONGITUDE, LATITUDE) ->
      plant_location

    plant_data %>%
      filter(PLANT_CODE != plant_code) %>%
      select(LONGITUDE, LATITUDE) ->
      plant_other_location

    distm(plant_location,
          plant_other_location,
          fun = distCosine) %>%
      c() ->
      plant_other_distances


    plant_data %>%
      filter(PLANT_CODE != plant_code) %>%
      mutate(distance = plant_other_distances) %>%
      select(PLANT_CODE, distance) %>%
      head(3) ->
      plant_other_location_and_distances

    generators_clean %>%
      left_join(plant_other_location_and_distances, by="PLANT_CODE") %>%
      filter(!is.na(distance)) %>%
      group_by(DATE) %>%
      summarise(
        PLANT_CODE = plant_code,
        GRIDVOLTAGE_NEIGHBORS = mean(GRIDVOLTAGE),
        OP_TIME_NEIGHBORS = mean(OP_TIME)
      ) ->
      neighbor_data

    neighbor_data
  }) %>%
  reduce(rbind) ->
  neighbor_data

generators_clean %>%
  left_join(neighbor_data, by=c("PLANT_CODE", "DATE")) ->
  generators_neighbor

# generators_neighbor %>%
#   glimpse

# generators_neighbor %>%
#   filter(PLANT_CODE == 228) ->
#   costa

# Note, 60k / 222k are true. Uneven, but not outrageously so

if_else_ <- function(.data, .truth, .lhs, .rhs) {
  if (.truth) {
    .lhs(.data)
  } else {
    .rhs(.data)
  }
}

is_invalid <- function(.data) {
  # This function is adapted from Stack Overflow at:
  # http://stackoverflow.com/a/19655909/2601448
  if (is.function(.data)) {
    return(FALSE)
  }

  return(is.null(.data) ||
         length(.data) == 0 ||
         all(is.na(.data)) ||
         all(.data == ""))
}
combo_internal <- function(.set, .map_fn) {
  # For invalid choices (empty, or NaN, etc), return an empty list
  if (is_invalid(.set)) {
    return(list())
  }

  elements <- .set %>% (function(e) {
    list(e, 1:length(e))
  }) %>% transpose

  seq(1, 2^length(.set)) %>% map(function(mask) {
    elements %>% discard(function(pair) {
      # if the pair index is masked (0), drop it. keep if slot is 1
      pair %>% dplyr::nth(2) %>% -1 %>% (function(e) {
        bitwShiftL(1, e)
      }) %>%
      bitwAnd(mask) %>%
      `>`(0)
    }) %>% .map_fn(function(e) {
      dplyr::nth(e, 1)
    })
  }) %>% discard(function(e) {
    length(e) < 1  # ignore empties (probably not useful)
  })
}
combo_chr <- function(.set) {
  combo_internal(.set, purrr::map_chr)
}

factor_scores <- function(predictions, truths, possible_results) {
  possible_results %>%
    unique %>%
    map(function(fct) {
      list(truths, predictions) %>%
        transpose %>%
        map(unlist) %>%
        discard(~ .[1] != as.numeric(fct)) -> truths_and_predictions

      truths_and_predictions %>%
        length ->
        predictions_total

      truths_and_predictions %>%
        keep(~ .[1] == .[2]) %>%
        length ->
        predictions_correct

      predictions_total - predictions_correct ->
        predictions_wrong

      data.frame(predictions_total, predictions_correct, predictions_wrong) %>%
        `colnames<-`(paste(as.character(fct), c("total", "correct", "wrong"), sep = "_"))

    }) %>%
    reduce(cbind)
}

k_fold_prob <- function(data, current_formula, k = 10, resample = TRUE) {
  folds <- cut(seq(1, nrow(data)), breaks = k, labels = FALSE) %>% if_else_(resample,
    sample, identity)
  # NOTE: call to sample is random. For consistent behavior, use set.seed

  1:k %>%
    purrr::map(function(fold_number) {
      temp_train <- data %>% filter(folds != fold_number)
      temp_test <- data %>% filter(folds == fold_number)

      current_formula %>%
        randomForest(data = temp_train, importance=TRUE, ntree=64) %>%
        predict(temp_test) %>%
        factor_scores(temp_test$ACTIVE, unique(data$ACTIVE)) # TODO: remove this hard coded factor
    }) %>%
    purrr::reduce(rbind) %>%
    dmap(sum)
}

# res <- randomForest(ACTIVE ~ GRIDVOLTAGE + OP_TIME, data = costa, importance=TRUE, ntree=200)
# varImpPlot(res)


# costa %>%
#   # head(50) %>%
#   mutate(
#     OP_TIME_LAG = lag(OP_TIME, 1),
#     OP_TIME_MA = moving_avg(OP_TIME),
#     wday = as.factor(wday(DATE, label = TRUE)),
#     wend = wday == "Sat" | wday == "Sun",
#     season = getSeason(DATE)
#   ) %>%
#   na.omit() ->
#   costa_featured

# costa_featured %>%
#   glimpse


# barplot(prop.table(table(costa_featured$ACTIVE)))

generate_df_combos <- function(df) {
  df %>%
    select(-ACTIVE,-DATE,-LONGITUDE,-LATITUDE,-FACILITY_NAME,-OP_TIME,
           -ENERGY_SOURCE,-PLANT_CODE,-GRIDVOLTAGE,-TEMP_F_MIN,-TEMP_F_MAX) %>%
    names ->
    n
  if (length(n) > 10) {
    stop("Too many names")
  }
  n %>%
    combo_chr %>%
    discard(~ length(.) < 2 | length(.) > 5)
}

# costa_featured %>%
#   generate_df_combos
#
# set.seed(0451)
# k_fold_prob(costa_featured, ACTIVE ~ GRIDVOLTAGE + TEMP_F_MEAN)
# set.seed(0451)
# k_fold_prob(costa_featured, ACTIVE ~ GRIDVOLTAGE + TEMP_F_MEAN + wday + wend + season + OP_TIME_LAG)
# set.seed(0451)
# k_fold_prob(costa_featured, ACTIVE ~ GRIDVOLTAGE + TEMP_F_MEAN + wday + wend + season + OP_TIME_LAG + OP_TIME_MA)
# # set.seed(0451)
# # k_fold_prob(costa_featured, ACTIVE ~ GRIDVOLTAGE + TEMP_F_MEAN + wday + wend + season + OP_TIME_LAG + OP_TIME_MA + OP_TIME_MA2)



full_test <- function(df) {
  df %>%
    generate_df_combos %>%
    map(function(combo) {
      combo %>%
        paste(collapse = " + ") %>%
        c("ACTIVE", .) %>%
        paste(collapse = " ~ ") ->
        forml

      k_fold_prob(df, as.formula(forml), 5) %>%
        data.frame(Formula = forml, .)
    }) %>%
    reduce(rbind) %>%
    mutate(
      TPR = (TRUE_correct / TRUE_total),
      FPR = (TRUE_wrong / TRUE_total),
      TNR = (FALSE_correct / FALSE_total),
      FNR = (FALSE_wrong / FALSE_total),
      AccuracyINACTIVE = (FALSE_correct / FALSE_total),
      AccuracyACTIVE = (TRUE_correct / TRUE_total),
      BalancedErrorRate = ((FALSE_wrong / FALSE_total) +
      (TRUE_wrong / TRUE_total)) *
      (1 / 2)) %>%
    select(Formula, TPR, FPR, TNR, FNR, AccuracyINACTIVE, AccuracyACTIVE, BalancedErrorRate) %>%
    arrange(BalancedErrorRate) %>%
    head(3)
}


# costa_results %>%
#   arrange(BalancedErrorRate)

# generators_clean %>%
#   glimpse
#
# cluster <- create_cluster(detectCores() - 1)
# set_default_cluster(cluster)
#
# generators_clean %>%
#   distinct(PLANT_CODE) %>%
#   partition(PLANT_CODE) %>%
#   do(fit = fitdist(.$PLANT_CODE)))

registerDoParallel(cores = detectCores() - 1)

process_plant <- function(plant_code) {
  generators_neighbor %>%
    filter(PLANT_CODE == plant_code) %>%
    mutate(
      OP_TIME_LAG = lag(OP_TIME, 1),
      OP_TIME_MA = moving_avg(OP_TIME)
    ) %>%
    na.omit() ->
    df

  df %>%
    get("ACTIVE", .) %>%
    unlist %>%
    unique %>%
    length ->
    active_levels
  if (active_levels < 2) {
    return(data_frame(PLANT_CODE = plant_code,
                      FACILITY_NAME = df$FACILITY_NAME %>% first,
                      Formula = "ACTIVE ~ NO + CHANGES",
                      TPR = 0,
                      FPR = 0,
                      TNR = 0,
                      FNR = 0,
                      AccuracyINACTIVE = 0,
                      AccuracyACTIVE = 0,
                      BalancedErrorRate = 0))
  }
  full_test(df) %>%
    cbind(data_frame(PLANT_CODE = plant_code,
      FACILITY_NAME = df$FACILITY_NAME %>% first), .)
}
#
# system.time({
#
#   generators_neighbor %>%
#     distinct(PLANT_CODE) %>%
#     slice(1:10) %>%
#     unlist %>%
#     unname -> plant_codes
#
#   (foreach(i = 1:length(plant_codes) ) %dopar% {
#     plant_code <- plant_codes[[i]]
#     process_plant(plant_code)
#   }) %>%
#   reduce(rbind) -> res
#
# })

# generators_neighbor %>%
#   distinct(PLANT_CODE) %>%
#   glimpse
#
# generators_neighbor %>%
#   distinct(PLANT_CODE) %>%
#   slice(1:20) %>%
#   unlist %>%
#   unname -> plant_codes
#
# (foreach(i = 1:length(plant_codes) ) %dopar% {
#   plant_code <- plant_codes[[i]]
#   set.seed(i + 500)
#   try({
#     process_plant(plant_code)
#   })
#   }) -> res_1_20


run_set <- function(s) {
  generators_neighbor %>%
    distinct(PLANT_CODE) %>%
    slice(s) %>%
    unlist %>%
    unname -> plant_codes

  (foreach(i = 1:length(plant_codes) ) %dopar% {
    plant_code <- plant_codes[[i]]
    set.seed(i + 500)
    try({
      process_plant(plant_code)
    })
  }) -> res

  res
}

run_set(1:20) -> res_1_20
run_set(21:40) -> res_21_80
run_set(41:60) -> res_41_60
run_set(61:80) -> res_61_80
run_set(81:100) -> res_81_100
run_set(101:112) -> res_101_112


list(res_1_20, res_21_80, res_41_60, res_61_80, res_81_100, res_101_112) %>%
  reduce(append) %>%
  discard(~ typeof(.) != 'list') %>%
  reduce(rbind) %>%
  filter(TPR > 0.001) ->
  res_all


res_all %>% write_csv('temp.results.csv')

res_all %>%
  arrange(BalancedErrorRate) %>%
  distinct(PLANT_CODE, .keep_all=T) ->
  res_winners

res_winners %>% glimpse
res_winners %>%
  arrange(BalancedErrorRate) %>%
  select(FACILITY_NAME, Formula, AccuracyACTIVE, AccuracyINACTIVE, BalancedErrorRate) %>%
  head(5) %>%
  write_csv('temp.winners.csv')



""
