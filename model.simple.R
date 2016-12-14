

suppressPackageStartupMessages( {
  library(purrr)
  library(dplyr)
  library(lubridate)
  library(readr)
  library(stringr)
  library(randomForest)
})

generator_data_daily <- read_rds(file.path("rds", "generator_data_daily.rds"))
generator_data_daily %>% glimpse

# hidden markov chain
generator_data_daily %>%
  filter(ENERGY_SOURCE == "NG") %>%
  group_by(PLANT_CODE, DATE) %>%
  mutate(ACTIVE = OP_TIME > 0.5) ->
  generators_clean

# generators_clean %>%
#   # select(DATE, PLANT_CODE, FACILITY_NAME, ACTIVE, GRIDVOLTAGE, TEMP_F_MAX, TEMP_F_MIN, TEMP_F_MEAN) %>%
#   glimpse

# generators_clean %>%
#   get("ACTIVE", .) %>%
#   unlist() %>%
#   sum(na.rm=T) %>%
#   glimpse

generators_clean %>%
  filter(PLANT_CODE == 228) %>%
  mutate(ACTIVE = as.factor(ACTIVE)) %>%
  ungroup() ->
  costa


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
        randomForest(data = temp_train, importance=TRUE, ntree=128) %>%
        predict(temp_test) %>%
        factor_scores(temp_test$ACTIVE, unique(data$ACTIVE)) # TODO: remove this hard coded factor
    }) %>%
    purrr::reduce(rbind) %>%
    dmap(sum)
}

# res <- randomForest(ACTIVE ~ GRIDVOLTAGE + OP_TIME, data = costa, importance=TRUE, ntree=200)
# varImpPlot(res)

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

costa %>%
  # head(50) %>%
  mutate(
    OP_TIME_LAG = lag(OP_TIME, 1),
    OP_TIME_MA = moving_avg(OP_TIME),
    wday = as.factor(wday(DATE, label = TRUE)),
    wend = wday == "Sat" | wday == "Sun",
    season = getSeason(DATE)
  ) %>%
  na.omit() ->
  costa_featured

costa_featured %>%
  glimpse


# barplot(prop.table(table(costa_featured$ACTIVE)))

generate_df_combos <- function(df) {
  df %>%
    select(-ACTIVE,-DATE,-LONGITUDE,-LATITUDE,-FACILITY_NAME,-OP_TIME,
           -ENERGY_SOURCE,-PLANT_CODE) %>%
    names ->
    n
  if (length(n) > 10) {
    stop("Too many names")
  }
  n %>%
    combo_chr
}

# costa_featured %>%
#   generate_df_combos

set.seed(0451)
k_fold_prob(costa_featured, ACTIVE ~ GRIDVOLTAGE + TEMP_F_MEAN)
set.seed(0451)
k_fold_prob(costa_featured, ACTIVE ~ GRIDVOLTAGE + TEMP_F_MEAN + wday + wend + season + OP_TIME_LAG)
set.seed(0451)
k_fold_prob(costa_featured, ACTIVE ~ GRIDVOLTAGE + TEMP_F_MEAN + wday + wend + season + OP_TIME_LAG + OP_TIME_MA)
# set.seed(0451)
# k_fold_prob(costa_featured, ACTIVE ~ GRIDVOLTAGE + TEMP_F_MEAN + wday + wend + season + OP_TIME_LAG + OP_TIME_MA + OP_TIME_MA2)


k_fold_prob(costa, ACTIVE ~ GRIDVOLTAGE + TEMP_F_MEAN + OP_TIME)
