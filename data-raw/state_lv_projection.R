library(tidyverse)
library(fable)

# Looks like we need more data
test_years <- 2
forecast_years <- 5

core <- fst::read_fst("data-raw/core.fst")
pop <-  fst::read_fst("data-raw/pop.fst")
pop_summary_proj <- pop %>% 
  group_by(year, agegrp, sex) %>% 
  summarise(pop = sum(pop), .groups = "drop") %>% 
  group_nest(agegrp, sex) %>% 
  mutate(data = map(data, function(.df) {
    model_lm <- lm(pop ~ year, data = .df)
    fcst_df <- tibble(year = max(.df$year) + 1:forecast_years)
    fcst_df$pop <- predict(model_lm, fcst_df)
    bind_rows(.df, fcst_df)
  })) %>% 
  unnest(cols = data)


# core %>% 
#   filter(year == 2017) %>% 
#   group_by(drg) %>% 
#   summarise(seps = sum(seps)) %>% 
#   arrange(-seps) %>% 
#   # mutate(i = row_number()) %>% 
#   # ggplot(aes(i, seps)) +
#   # geom_point()
#   mutate(
#     cumsum = cumsum(seps),
#     cumsum_pc = cumsum / sum(seps)
#   ) %>% 
#   view

drgs_selected <- core %>%
  filter(year == 2017) %>%
  group_by(drg) %>%
  summarise(seps = sum(seps)) %>% 
  
  arrange(-seps) %>% 
  left_join(drg_lkup) %>% 
  filter(!is.na(drgx)) %>% 
  
  filter(seps >= 30000) %>%
  pluck("drg")

activity_ls <- core %>% 
  filter(drg %in% drgs_selected) %>% 
  # left_join(pop_summary_proj) %>% 
  left_join(
    pop %>%
      group_by(year, agegrp, sex) %>%
      summarise(pop = sum(pop), .groups = "drop")
  ) %>%
  mutate(
    rate = seps / pop,
    group = paste0(drg, "_", sameday, "_", sex, "_", agegrp)
  ) %>% 
  split(.$group) %>% 
  keep(~nrow(.x) == length(unique(core$year)))

projections = list()

for (i in 1:length(activity_ls)) {
  
  message(paste(i, "/", length(activity_ls)))
  
  ts <- activity_ls[[i]] %>%
    as_tsibble(index = year, key = group) %>%
    mutate(max_rate = max(rate, na.rm = TRUE) * 2)
  
  max_rateA = ts$max_rate[1]
  
  last_test_year  = max(ts$year)
  train_year_last = last_test_year - test_years

  scaled_logit <-     function(x, lower=0, upper = 1){ log((x-lower)/(upper-x)) }
  inv_scaled_logit <- function(x, lower=0, upper = 1){ (upper-lower)*exp(x)/(1+exp(x)) + lower}
  my_scaled_logit <- fabletools::new_transformation(scaled_logit, inv_scaled_logit)
  
  ts2 <- ts %>%
    filter(year <= train_year_last) %>%
    model(Naive = NAIVE(rate),
          Mean  = MEAN(rate ~ window(3)),
          LM    = TSLM(rate ~ trend()),
          LM2   = TSLM( my_scaled_logit(rate, lower=0, upper = max_rateA) ~ trend()),
          ARIMA = ARIMA(my_scaled_logit(rate, lower=0, upper = max_rateA) ~ PDQ(0, 0, 0)),
          ETS   = ETS(  my_scaled_logit(rate, lower=0, upper = max_rateA)))
  
  # Forecasts
  ts2A <- ts2 %>%
    forecast(h = paste(test_years, " years")) %>%
    mutate(h = row_number())
  
  # Ensemble model
  # Identify best 3 models
  ensemble_models <- ts2A %>%
    left_join(ts2A %>% accuracy(ts, by = "h")) %>%
    data.frame %>%
    select(.model,year, MAPE) %>%
    filter(year == last_test_year) %>%
    filter(.model != "LM") %>%
    arrange(MAPE) %>%
    mutate(ensemble_models = c(rep(1,3),rep(0,2))) %>%
    filter(ensemble_models == 1) %>%
    .$.model
  
  ts_ensemble <- ts2A %>%
    filter(.model %in% ensemble_models)
  
  # Rename the rows (required for next step)
  names(ts_ensemble$rate) <- c(rep(1,test_years),
                               rep(2,test_years),
                               rep(3,test_years))
  
  # Create mixture distribution based on these models
  # Reference Hydnman https://github.com/robjhyndman/quantile_ensemble_talk/blob/master/demo.R
  ts_ensemble2 <- ts_ensemble %>%
    group_by(group) %>%
    summarise(
      rate = distributional::dist_mixture(rate[1],rate[2],rate[3], weights = c(0.34,0.33,0.33))) %>%
    mutate(.mean = mean(rate)) %>%
    transmute(.model = "ENSEMBLE", rate, .mean) %>%
    mutate(group = unique(ts_ensemble$group)) %>% 
    as_fable(key = c(group, .model))
  
  # Bind the ensemble to the other models
  ts3 <- bind_rows(ts2A, ts_ensemble2) %>%
    mutate(h = row_number())
  
  # Calculate model performance
  model_performance <- ts3 %>%
    accuracy(data = ts, measures = list(mape = MAPE))
  
  # Identify best model
  best_model <- model_performance %>%
    filter(mape == min(mape)) %>%
    .$.model
  
  # Re-estimate model
  # Base models stage 2
  ts2_stage2 <- ts %>%
    model(Naive = NAIVE(rate),
          Mean  = MEAN(rate ~ window(3)),
          LM    = TSLM(rate ~ trend()),
          LM2   = TSLM( my_scaled_logit(rate, lower=0, upper = max_rateA) ~ trend()),
          ARIMA = ARIMA(my_scaled_logit(rate, lower=0, upper = max_rateA) ~ PDQ(0, 0, 0)),
          ETS   = ETS(  my_scaled_logit(rate, lower=0, upper = max_rateA)))
  
  # Forecasts
  ts2A_stage2 <- ts2_stage2 %>%
    forecast(h = paste(forecast_years, " years")) %>%
    mutate(h = row_number())
  
  # Ensemble model Stage 2
  # Create tsible with these ensemble models
  ts_ensemble_stage2 <- ts2A_stage2 %>%
    filter(.model %in% ensemble_models)
  
  # Rename the rows (required for next step)
  names(ts_ensemble_stage2$rate) <- c(rep(1,forecast_years),
                                      rep(2,forecast_years),
                                      rep(3,forecast_years))
  
  # Create mixture distribution based on these models
  # Reference Hydnman https://github.com/robjhyndman/quantile_ensemble_talk/blob/master/demo.R
  ts_ensemble2_stage2 <- ts_ensemble_stage2 %>%
    group_by(group) %>%
    summarise(
      rate = distributional::dist_mixture(rate[1],rate[2],rate[3], weights = c(0.34,0.33,0.33))) %>%
    mutate(.mean = mean(rate)) %>%
    transmute(.model = "ENSEMBLE", rate, .mean) %>%
    mutate(group = unique(ts_ensemble$group)) %>% 
    as_fable(key = c(group, .model))
  
  # Bind the ensemble to the other models nd filter to best model plus ensemble
  ts3_stage2 <- bind_rows(ts2A_stage2, ts_ensemble2_stage2) %>%
    mutate(h = row_number()) %>%
    filter(.model %in% unique(c(best_model,"ENSEMBLE"))) %>%
    hilo(level = 75) %>%
    mutate(max_rate= max_rateA) %>%
    select(year, group, .model, .mean, "75%", max_rate) %>%
    separate(group, into = c("drg","dayonly","sex","agegrp"), sep = "_",remove = FALSE) %>%
    rename(rate = .mean) %>%
    # filter(year %in% c(2021,2026,2031,2036,2041)) %>%
    # ** BOOKMARKED
    left_join(pop_summary_proj) %>%
    mutate(seps = round(pop * rate,0))
  
  ts4_stage2 <- ts %>%
    mutate(.model = "Observed") %>%
    as_tsibble(index = year, key = c("group",".model")) %>%
    bind_rows(ts3_stage2)
  # %>%
  #   mutate(small_cell2 = ts$small_cell2[1])
  
  projections[[ts$group[1]]] <- list("data" = ts4_stage2,
                                     "perf" = model_performance %>% mutate(best_model = best_model))
  
}

saveRDS(projections, "data-raw/projections.rds")

activity_ls %>% length
projections %>% length

# https://www.aapc.com/codes/drg-codes-range/