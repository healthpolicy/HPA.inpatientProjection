library(tidyverse)
library(fable)
library(sparkline)

projected_rates <- readRDS("data-raw/projections.rds")
usethis::use_data(projected_rates, overwrite = TRUE)

drg_spk_df_raw <- projected_rates %>%
  map(function(.l) {
    .l_split <- as_tibble(.l$data) %>%
      mutate(obs = ifelse(.model == "Observed", "obs", "pred")) %>%
      split(.$obs)
    .pred <- .l_split$pred %>%
      filter(.model == unique(.l$perf$best_model))
    if (".model_alos" %in% names(.l$data)) {
      .pred <- .pred %>% 
        filter(.model_alos == unique(.l$perf_alos$best_model))
    }
    .pred %>%
      bind_rows(.l_split$obs) %>%
      select(group, year, seps, los, pop, rate) %>%
      arrange(year)
  }) %>%
  bind_rows() %>%
  separate(group, into = c("drg_grp", "dayonly", "sex", "age"), sep = "_", remove = FALSE) %>%
  group_by(drg_grp, year, dayonly) %>%
  summarise(across(c(seps, los), ~sum(., na.rm = TRUE)), .groups = "drop") %>%
  mutate(
    alos = los / seps,
    dayonly = ifelse(dayonly == "1", "sd", "ov")
  ) %>%
  select(-los) %>%
  pivot_wider(names_from = dayonly, values_from = c(seps, alos))

# https://github.com/htmlwidgets/sparkline/issues/14#issuecomment-386594939
spk_tool <- function(labels, type) {
  .fn <- type %>% switch(
    line = "function(sparkline, options, field){return %s[field.x];}",
    bar = "function(sparkline, options, field){return %s[field[0].offset];}"
  )
  htmlwidgets::JS(sprintf(.fn, jsonlite::toJSON(labels)))
}
generate_spk <- function(.df, .value_col, .type, .y_max, .digits) {
  spk_chr(
    .df[[.value_col]], type = .type,
    chartRangeMin = 0, chartRangeMax = .y_max,
    tooltipFormatter = spk_tool(
      labels = paste0(
        .df$year, ": ",
        prettyNum(round(.df[[.value_col]], digits = .digits), big.mark = ",")
      ),
      type = .type
    )
  )
}

drg_spk_df <- drg_spk_df_raw %>%
  group_nest(drg_grp) %>%
  mutate(spk = map(data, ~tibble(
    seps_ov = generate_spk(.x, "seps_ov", "line", .y_max = max(.x$seps_sd, .x$seps_ov), .digits = 0),
    seps_sd = generate_spk(.x, "seps_sd", "line", .y_max = max(.x$seps_sd, .x$seps_ov), .digits = 0),
    alos_ov = generate_spk(.x, "alos_ov", "bar", .y_max = max(.x$alos_ov), .digits = 1)
  ))) %>%
  select(-data) %>%
  unnest(cols = spk) %>%
  left_join(drg_grp_lkup) %>%
  select(drg_grp, drg_grpx, seps_sd, seps_ov, alos_ov)
# drg_spk_df %>%
#   DT::datatable(escape = FALSE) %>%
#   sparkline::spk_add_deps()
usethis::use_data(drg_spk_df, overwrite = TRUE)
