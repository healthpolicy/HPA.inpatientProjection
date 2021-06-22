# https://www.hcup-us.ahrq.gov/db/nation/nis/tools/stats/FileSpecifications_NIS_2017_Core.TXT
nis_core[, .(n = sum(DISCWT), los = sum(LOS * DISCWT)), by = .(YEAR)]
nis_core[, .(n = sum(DISCWT)), by = .(YEAR, DRG)] %>% 
  pivot_wider(names_from = YEAR, values_from = n)

plot_yq <- function(.df, .group_var, .value_var = "value") {
  .df %>% 
    mutate(yq = paste(YEAR, DQTR)) %>% 
    ggplot(aes(
      yq, !!rlang::sym(.value_var), 
      col = !!rlang::sym(.group_var),
      group = !!rlang::sym(.group_var)
    )) +
    geom_point() +
    geom_line()
}

summarise_category_and_plot <- function(.var) {
  summary <- nis_core[, .(value = sum(DISCWT)), by = c("YEAR", "DQTR", .var)] %>% 
    filter(DQTR %in% 1:4) %>% 
    drop_na()
  summary[[.var]] <- as.factor(summary[[.var]])
  plot_yq(summary, .var)
}

summarise_category_and_plot("age")
summarise_category_and_plot("FEMALE")
summarise_category_and_plot("HOSP_DIVISION")
summarise_category_and_plot("PL_NCHS")
summarise_category_and_plot("ZIPINC_QRTL")
summarise_category_and_plot("sameday")

nis_core[sameday == "overnight", 
         .(n = sum(DISCWT), los = sum(LOS * DISCWT)), 
         by = .(YEAR, DQTR)] %>% 
  filter(DQTR %in% 1:4) %>% 
  mutate(value = los / n, group = "Total") %>% 
  plot_yq("group") +
  ylim(0, 6)

# Note taking
# Data prior to 2014