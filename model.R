

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

ggsave_local <- function(filename, plot) {
  ggsave_local(file.path("plots", filename), plot)
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

# Note, 60k / 222k are active. Uneven, but not outrageously so

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

# res_winners %>% glimpse
res_winners %>%
  arrange(BalancedErrorRate) %>%
  select(FACILITY_NAME, Formula, AccuracyACTIVE, AccuracyINACTIVE, BalancedErrorRate) %>%
  head(5) %>%
  write_csv('temp.winners.csv')

res_winners %>%
  select(AccuracyACTIVE, AccuracyINACTIVE, BalancedErrorRate) %>%
  summarise(
    AccuracyACTIVE = mean(AccuracyACTIVE),
    AccuracyINACTIVE = mean(AccuracyINACTIVE),
    BalancedErrorRate = mean(BalancedErrorRate)
  )

res_winners$PLANT_CODE %>% unlist -> valid_plants

valid_plants %>%
  map(function(plant_code){
    res_winners %>%
      filter(PLANT_CODE == plant_code) %>%
      get("Formula", .) %>%
      first %>%
      as.character %>%
      as.formula ->
      forml

    generators_neighbor %>%
      filter(PLANT_CODE == plant_code) %>%
      mutate(
        OP_TIME_LAG = lag(OP_TIME, 1),
        OP_TIME_MA = moving_avg(OP_TIME)
      ) %>%
      na.omit() ->
      raw_data
    fit <- randomForest(forml, data = raw_data, importance=TRUE, ntree=64)

    predict(fit, raw_data) -> forecasts

    raw_data %>%
      mutate(
        ACTIVE_FORECAST = forecasts,
        production = ifelse(ACTIVE_FORECAST == 'TRUE', GRIDVOLTAGE, 0),
        group="Forecast"
      ) %>%
      select(DATE, production, group)
  }) %>%
  reduce(rbind) %>%
  group_by(DATE) %>%
  summarise(
    production = sum(production)
  ) %>%
  select(DATE, production) %>%
  mutate(group="Forecast") %>%
  ungroup() ->
  dat_forecast


# dat_forecast %>%
#   arrange(DATE) %>%
#   tail(5) %>%
#   glimpse


# generators_clean %>% glimpse

generators_clean %>%
  filter(PLANT_CODE %in% valid_plants) %>%
  select(DATE, ACTIVE, GRIDVOLTAGE, OP_TIME) %>%
  mutate(production = ifelse(ACTIVE == 'TRUE', OP_TIME * GRIDVOLTAGE, 0)) %>%
  group_by(DATE) %>%
  summarise(
    # production_truth = GRIDVOLTAGE * OP_TIME
    production = sum(production)
    # production_truth = sum(ifelse(ACTIVE, GRIDVOLTAGE * OP_TIME, 0))
  ) %>%
  select(DATE, production) %>%
  mutate(group="Truth") %>%
  ungroup() ->
  dat

# dat %>% glimpse

dat_forecast %>%
  left_join(dat, by="DATE") %>%
  mutate(residual = (production.x - production.y)^2 ) ->
  dat_residuals

# dat_residuals %>% glimpse

# dat_residuals %>%
#   filter(residual < 1) %>%
#   glimpse

# 652/1457

# dat_residuals$residual %>% mean
# dat_residuals$residual %>% discard(~ . < 1) %>% glimpse

qplot(dat_residuals$residual, geom="histogram") + coord_equal(ratio = 200) -> plot_residuals_hist
ggsave_local('residuals.hist.png', plot_residuals_hist)


ggplot(data=dat_residuals, aes(x=DATE, y=residual)) +
    geom_point() +
    xlab("Time") + ylab("Residual Error") +
    coord_equal(ratio = 0.0002) ->
    p_residuals

ggsave_local('stack.residuals.png', p_residuals)

dat_full <- rbind(dat,dat_forecast)

# dat_full %>% glimpse

ggplot(data=dat_full %>% filter(DATE < "2014-01-01") , aes(x=DATE, y=production, group=group, color=group)) +
    geom_line(alpha=0.95) +
    # scale_alpha_manual(values = c(0.1, 0.1, 1, 1)) +
    # geom_point() +
    expand_limits(y=0) +
    xlab("Time") + ylab("Energy Produced (MW)") +
    ggtitle("Dispatch Stack") + coord_equal(ratio = 0.06) ->
    p_full_stack

# p_full_stack
ggsave_local('full.stack.png', p_full_stack)


ggplot(data=dat_full %>% filter(group == "Forecast"), aes(x=DATE, y=production, group=group, color=group)) +
    geom_line(alpha=0.95, color="red") +
    # scale_alpha_manual(values = c(0.1, 0.1, 1, 1)) +
    # geom_point() +
    expand_limits(y=0) +
    xlab("Time") + ylab("Energy Produced (MW)") +
    ggtitle("Dispatch Stack") + coord_equal(ratio = 0.1) ->
    p_full_forecast

ggplot(data=dat_full %>% filter(group == "Truth" & DATE < "2014-01-01"), aes(x=DATE, y=production, group=group)) +
    geom_line(alpha=0.95, color="blue") +
    # scale_alpha_manual(values = c(0.1, 0.1, 1, 1)) +
    # geom_point() +
    expand_limits(y=0) +
    xlab("Time") + ylab("Energy Produced (MW)") +
    ggtitle("Dispatch Stack") + coord_equal(ratio = 0.1) ->
    p_full_truth

ggsave_local('full.p_full_forecast.png', p_full_forecast)
ggsave_local('full.p_full_truth.png', p_full_truth)




ggplot(data=dat_full %>% filter(DATE > "2013-01-01" & DATE < "2013-09-01"), aes(x=DATE, y=production, group=group, color=group)) +
    geom_line(alpha=0.95) +
    xlab("Time") + ylab("Energy Produced (MW)") +
    coord_equal(ratio = 0.02) ->
    p_full_stack_tight

ggsave_local('full.p_full_stack_tight.png', p_full_stack_tight)




valid_plants %>%
  map(function(plant_code){
    res_winners %>%
      filter(PLANT_CODE == plant_code) %>%
      get("Formula", .) %>%
      first %>%
      as.character %>%
      as.formula ->
      forml

    generators_neighbor %>%
      filter(PLANT_CODE == plant_code) %>%
      mutate(
        OP_TIME_LAG = lag(OP_TIME, 1),
        OP_TIME_MA = moving_avg(OP_TIME)
      ) %>%
      na.omit() ->
      raw_data
    fit <- randomForest(forml, data = raw_data, importance=TRUE, ntree=64)
    list(
      plant_code,
      fit)
  }) -> rf_res





res_winners %>% arrange(-AccuracyACTIVE) -> temp
ggplot(data=temp, aes(x=FACILITY_NAME, y=AccuracyACTIVE)) +
    geom_bar(stat="identity", position=position_dodge(), fill="red") ->
    plot_rf_active
ggplot(data=temp, aes(x=FACILITY_NAME, y=AccuracyINACTIVE)) +
    geom_bar(stat="identity", position=position_dodge(), fill="blue") ->
    plot_rf_inactive

ggsave_local('full.plot_rf_active.png', plot_rf_active)
ggsave_local('full.plot_rf_inactive.png', plot_rf_inactive)
