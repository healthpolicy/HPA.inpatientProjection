library(tidyverse)
library(dtplyr)
library(data.table)
library(tidycensus)
library(rvest)

# 1. NIS data acquisition ----

get_hcup_spec <- function(.file, .year) {
  spec_url <- paste0("https://www.hcup-us.ahrq.gov/db/nation/nis/tools/stats/",
                     "FileSpecifications_NIS_", .year, "_", .file, ".TXT")
  spec_df_raw <- tibble::tibble(src = readr::read_lines(spec_url))
  spec_ls <- spec_df_raw %>% 
    mutate(chunk_no = cumsum(src == "")) %>% 
    filter(src != "") %>% 
    split(.$chunk_no) %>% 
    setNames(c("info", "fwf", "content")) %>% 
    map("src")
  message(paste(spec_ls$info[1], "- getting spec..."))
  
  col_number_nchar <- min(which(str_split(spec_ls$fwf[2], "")[[1]] == " "))
  spec_fwf <- tibble(
    col = spec_ls$fwf %>% substr(1, col_number_nchar),
    desc = spec_ls$fwf %>% substr(col_number_nchar + 1, nchar(.))
  ) %>% 
    filter(str_detect(col, "\\d")) %>% 
    mutate(
      desc = str_trim(desc),
      desc = str_replace_all(tolower(desc), "\\s", "_"),
      desc = case_when(
        str_detect(desc, "data_element_name") ~ "var",
        str_detect(desc, "data_element_type") ~ "type",
        str_detect(desc, "starting_column") ~ "start",
        str_detect(desc, "ending_column") ~ "end",
        str_detect(desc, "data_element_label") ~ "varx"
      )
    ) %>% 
    filter(!is.na(desc)) %>% 
    separate(col, into = c("start", "end"), sep = "\\-") %>% 
    mutate(across(c(start, end), as.numeric))
  
  spec_df <- tibble(src = spec_ls$content)
  for (.fwf in 1:nrow(spec_fwf)) {
    .fwf_info <- spec_fwf[.fwf, ]
    spec_df[[.fwf_info$desc]] <- substr(spec_df$src, .fwf_info$start, .fwf_info$end)
  }
  
  spec_df %>% 
    mutate(
      across(c(var, type), ~str_remove_all(., "\\s")),
      across(c(start, end), as.numeric),
      width = end - start + 1
    ) %>% 
    select(var, varx, type, start, end, width)
}

read_hcup_fwf <- function(.file, .year, .spec_df, .root_dir) {
  filepath <- paste0(.root_dir, "NIS_", .year, "\\NIS_", .year, "_", .file, ".ASC")
  data_read_in <- readr::read_fwf(
    filepath, fwf_widths(.spec_df$width, .spec_df$var)
    # , n_max = 100000
  )
  attr(data_read_in, "spec") <- .spec_df
  return(data_read_in)  
}

file_year_combn <- expand_grid(
  year = 2017:2007,
  file = c("Core", "Hospital")
)

.root_dir <- "S:\\Health Data\\USA\\HCUP\\"

for (i in 1:nrow(file_year_combn)) {
  .file <- file_year_combn$file[i]
  .year <- file_year_combn$year[i]
  .spec_df <- get_hcup_spec(.file, .year)
  raw_data <- read_hcup_fwf(.file, .year, .spec_df, .root_dir)
  if (.file == "Core") {
    .target_columns <- c(
      "DISCWT", 
      "KEY_NIS", "KEY",
      "NIS_STRATUM", "YEAR", "DQTR",
      "AGE", "FEMALE", "RACE", "HCUP_ED", "HOSP_DIVISION",
      "HOSP_NIS", "HOSPID",
      "DRG", "PL_NCHS", "ZIPINC_QRTL", "PAY1",        
      "LOS", "TOTCHG"
    )
    .target_columns <- .target_columns %>% intersect(names(raw_data))
    .read_in <- raw_data[, .target_columns]
  } else if (.file == "Hospital") {
    .read_in <- raw_data
  }
  fst::write_fst(.read_in, paste0("data-raw/working/", .file, "_", .year, ".fst"), compress = 100)
  # result_ls[[.file]][[.year]] <- .read_in
  rm(raw_data);rm(.read_in);gc()
}

drg2015_q1q3 <- read_fwf(
  "S:/Health Data/USA/HCUP/NIS_2015/NIS_2015Q1Q3_DX_PR_GRPS.ASC",
  fwf_cols(DRG = c(181, 183), KEY_NIS = c(594, 603)) # https://www.hcup-us.ahrq.gov/db/nation/nis/tools/stats/FileSpecifications_NIS_2015Q1Q3_DX_PR_GRPS.TXT
)
drg2015_q4 <- read_fwf(
  "S:/Health Data/USA/HCUP/NIS_2015/NIS_2015Q4_DX_PR_GRPS.ASC",
  fwf_cols(DRG = c(1, 3), KEY_NIS = c(366, 375)) # https://www.hcup-us.ahrq.gov/db/nation/nis/tools/stats/FileSpecifications_NIS_2015Q4_DX_PR_GRPS.TXT
)

core2015 <- fst::read_fst("data-raw/working/Core_2015.fst", as.data.table = TRUE)
drg2015 <- data.table(bind_rows(drg2015_q1q3, drg2015_q4), key = "KEY_NIS")
core2015[drg2015, on = "KEY_NIS"] %>% 
  fst::write_fst("data-raw/working/Core_2015.fst", compress = 100)

# 2. DRG lookup ----

drg_grp_lkup_raw <- read_html("https://www.aapc.com/codes/drg-codes-range") %>% 
  html_elements(".cpt-dark-grey") %>% 
  map(
    ~html_elements(.x, "div") %>% 
      html_text() %>% 
      tibble(value = .) %>% 
      mutate(name = c("drg_grp", "drg_range", "drg_grpx"))
  ) %>% 
  bind_rows(.id = "i") %>% 
  pivot_wider()

drg_lkup_raw <- map(1:29, function(i) {
  .html <- read_html(paste0("https://www.aapc.com/codes/drg-codes-range/", i, "/"))
  .chr <- .html %>% 
    html_elements(".list-code-range") %>% 
    html_text() %>% 
    str_remove_all("\t|\n") %>% 
    str_trim()
  tibble(src = .chr) %>% 
    mutate(
      drg = substr(src, 1, 3),
      drgx = substr(src, 4, nchar(src)),
      drgx = str_trim(drgx),
      drgx = paste(drg, drgx)
    ) %>% 
    select(-src)
}) %>% 
  bind_rows()

drg_lkup <- drg_grp_lkup_raw %>% 
  mutate(
    drg_grp = case_when(
      drg_range == "001-019" ~ "00",
      drg_range == "981-989" ~ "26",
      drg_range == "998-998" ~ "98",
      drg_range == "999-999" ~ "99",
      TRUE ~ drg_grp
    ),
    drg_grpx = paste(drg_grp, drg_grpx)
  ) %>% 
  split(.$i) %>%  
  map(function(x) {
    range_num <- as.numeric(str_split(x$drg_range, "\\-")[[1]])
    tibble(
      drg = formatC(range_num[1]:range_num[2], width = 3, flag = "0"),
      drg_grp = x$drg_grp,
      drg_grpx = x$drg_grpx
    )
  }) %>% 
  bind_rows() %>% 
  left_join(drg_lkup_raw)

drg_grp_lkup <- drg_lkup %>% 
  select(drg_grp, drg_grpx) %>% 
  unique()

usethis::use_data(drg_lkup, overwrite = TRUE)
usethis::use_data(drg_grp_lkup, overwrite = TRUE)


# 3. NIS data aggregation ----

core <- list.files("data-raw/working/", full.names = TRUE) %>% 
  .[str_detect(., "Core")] %>% 
  map(function(.filename) {
    .df <- fst::read_fst(.filename, as.data.table = TRUE
                  # , to = 1000
    )
    .df$agegrp <- case_when(
      .df$AGE %in% 0:4 ~ "0004",
      .df$AGE %in% 5:14 ~ "0514",
      .df$AGE %in% 15:44 ~ "1544",
      .df$AGE %in% 45:69 ~ "4569",
      .df$AGE %in% 70:84 ~ "7084",
      .df$AGE %in% 85:100 ~ "85+"
    )
    .df$sex <- case_when(
      .df$FEMALE == 1 ~ "2", 
      .df$FEMALE == 0 ~ "1"
    )
    .df$sameday <- case_when(
      .df$LOS %in% 0:1 ~ "1",
      .df$LOS > 1 ~ "2"
    )
    # Shifting years
    .df$year <- .df$YEAR + 2
    
    .df2 <- .df[!is.na(agegrp) & !is.na(sex) & !is.na(sameday),
                .(seps = sum(DISCWT), los = sum(DISCWT * LOS)),
                by = .(year, DRG, agegrp, sex, sameday)] %>% 
      janitor::clean_names() %>% 
      mutate(drg = formatC(drg, width = 3, flag = "0"))
    
    .df2 %>% 
      lazy_dt() %>% 
      left_join(drg_lkup, by = "drg") %>% 
      filter(!is.na(drg_grp)) %>% 
      group_by(year, drg_grp, agegrp, sex, sameday) %>%
      summarise(across(c(seps, los), sum)) %>% 
      ungroup() %>% 
      as_tibble()
  }) %>% 
  data.table::rbindlist(fill = TRUE)


# 4. Population data ----

div_pop_ls <- map(2009:2019 %>% setNames(., .), function(.year) {
  .var_list <- load_variables(.year, "acs5", cache = TRUE)
  .var_list2 <- .var_list %>% 
    filter(concept == "SEX BY AGE") %>% 
    filter(str_detect(label, "^.*?!!.*?!!.*?!!.*?$")) %>% 
    mutate(label2 = str_replace(label, "^.*?!!.*?!!(.*?!!.*?)$", "\\1"))
  res <- get_acs(geography = "division", 
                 variables = .var_list2$name %>% 
                   setNames(.var_list2$label2), 
                 year = .year)
  message(paste(.year, "done"))
  return(res)
})

div_pop <- div_pop_ls %>% 
  bind_rows(.id = "year") %>% 
  separate(variable, into = c("sex", "age"), sep = "!!")

pop <- div_pop %>% 
  mutate(
    agegrp = ifelse(age == "Under 5 years", "0", str_replace(age, "^(.*?)\\s.*$", "\\1")),
    agegrp = as.numeric(agegrp),
    agegrp = case_when(
      agegrp %in% 0:4 ~ "0004",
      agegrp %in% 5:14 ~ "0514",
      agegrp %in% 15:44 ~ "1544",
      agegrp %in% 45:69 ~ "4569",
      agegrp %in% 70:84 ~ "7084",
      agegrp %in% 85:100 ~ "85+"
    ),
    sex = case_when(
      sex %in% c("Male", "Male:") ~ "1",
      sex %in% c("Female", "Female:") ~ "2"
    )
  ) %>% 
  group_by(year, GEOID, NAME, agegrp, sex) %>% 
  summarise(pop = sum(estimate), .groups = "drop") %>% 
  mutate(year = as.numeric(year))

fst::write_fst(core, "data-raw/core.fst", compress = 100)
fst::write_fst(pop,  "data-raw/pop.fst",  compress = 100)



# To map geography (later) ------------------------------------------------

# https://www.irs.gov/statistics/soi-tax-stats-individual-income-tax-statistics-2017-zip-code-data-soi
# income <- vroom::vroom("data-raw/17zpallagi.csv") %>%
#   as.data.table()

# https://www.cdc.gov/nchs/data_access/urban_rural.htm#Data_Files_and_Documentation
# urban_rural <- haven::read_sas("data-raw/NCHSurbruralcodes2013.sas7bdat")

# https://wonder.cdc.gov/wonder/sci_data/codes/fips/type_txt/cntyxref.asp
# zip_fips_lkup <- map(1:10, ~read_fwf(
#   paste0("data-raw/zipcty/zipcty", .x),
#   fwf_cols(zip = c(1, 5), stateabb = c(24, 25), fips = c(26, 28))
# )) %>%
#   rbindlist() %>%
#   unique() %>%
#   filter(!is.na(zip)) %>%
#   mutate(fips = as.numeric(fips))

# zip_info <- inner_join(
#   
#   income[, .(n = sum(N1), adj_gross_inc = weighted.mean(A00100, N1)), 
#          by = .(stateabb = STATE, zip = zipcode)] %>% 
#     mutate(ZIPINC_QRTL = cut(
#       adj_gross_inc,
#       breaks = c(-Inf, 44000, 56000, 74000, Inf),
#       labels = 1:4
#     )) %>% 
#     mutate(ZIPINC_QRTL = as.character(ZIPINC_QRTL)),
#   
#   urban_rural %>% 
#     select(stateabb = ST_ABBREV, fips = ctyfips, ctypop, PL_NCHS = CODE2013) %>% 
#     left_join(zip_fips_lkup) %>% 
#     group_by(stateabb, zip) %>% 
#     summarise(PL_NCHS = round(weighted.mean(PL_NCHS, ctypop)), .groups = "drop")
# 
# ) %>% 
#   left_join(
#     census_div_lkup %>% 
#       select(stateabb, div)
#   ) %>% 
#   select(-adj_gross_inc, -stateabb)
# 
# nis_for_zip_mapping <- nis_core[, .(DISCWT, KEY_NIS, YEAR, HOSP_DIVISION, PL_NCHS, ZIPINC_QRTL)]


# Temporary showcase version ----------------------------------------------

usmap_shape <- USAboundaries::us_states() %>% 
  left_join(census_div_lkup, by = c("state_abbr" = "stateabb")) %>% 
  group_by(div, divx) %>% 
  summarise(geometry = st_union(geometry), .groups = "drop") %>% 
  filter(!is.na(div))

trend <- nis_core[, .(n = sum(DISCWT)), by = .(YEAR, DQTR, HOSP_DIVISION)] %>% 
  filter(DQTR > 0) %>% 
  arrange(HOSP_DIVISION, YEAR, DQTR) %>% 
  group_by(div = HOSP_DIVISION) %>% 
  mutate(year = row_number() + 2017) %>% 
  ungroup() %>% 
  select(year, div, n)

trend %>% 
  group_by(div = HOSP_DIVISION) %>% 
  mutate(YEAR = YEAR + 3) %>% 
  nest() %>% .$data %>%  .[[1]] -> x

x <- x %>% 
  arrange(YEAR, DQTR) %>% 
  mutate(i = row_number())
model <- lm(n ~ i, data = x)
x2 <- x %>% mutate(YEAR = YEAR + 3, pred = TRUE)
x2_pred <- bind_rows(x, x2) %>% 
  mutate(i = row_number()) %>% 
  filter(pred) %>% 
  predict(model, .)
x2 %>% 
  mutate(n = x2_pred) %>% 
  bind_rows(x) %>% 
  mutate(yq = paste(YEAR, DQTR)) %>% 
  ggplot(aes(yq, n)) + geom_point() + facet_wrap(~HOSP_DIVISION)

te <- nis_core[, .(n = sum(DISCWT)), by = .(YEAR, DQTR, HOSP_DIVISION)]
te %>% 
  mutate(yq = paste(YEAR, DQTR)) %>% 
  ggplot(aes(yq, n)) +
  geom_point() +
  facet_wrap(~HOSP_DIVISION)


# $sameday

# %>% 
#   leaflet() %>% 
#   addTiles() %>% 
#   addPolygons(fill = ~divx, label = ~divx)
  
  


.nis <- nis_for_zip_mapping[HOSP_DIVISION == "1"]
.zip <- zip_info[div == "1"]

.nis[YEAR == "2017"] %>% 
  group_by(PL_NCHS, ZIPINC_QRTL) %>% 
  summarise(sum(DISCWT))

nis_core %>% map_dbl(~mean(.x < 0))

# Pop proportion (+/-10%)
# Mathing inc/nchs
# Remnant go to inter division

# POR: div > zip
# POT: div > hosp
# CS: A-DRG? > DRG

# Census div / ZIP / inc_qtl / nchs

# https://www.hcup-us.ahrq.gov/db/nation/nis/nisdde.jsp
nis_core$ZIPINC_QRTL
nis_core$PL_NCHS
# https://www.cdc.gov/nchs/data_access/urban_rural.htm
# 1	"Central" counties of metro areas of >=1 million population
# 2	"Fringe" counties of metro areas of >=1 million population
# 3	Counties in metro areas of 250,000-999,999 population
# 4	Counties in metro areas of 50,000-249,999 population
# 5	Micropolitan counties
# 6	Not metropolitan or micropolitan counties
# .	Missing


nis_hosp$HOSP_DIVISION
# Division 1 (New England): Maine, New Hampshire, Vermont, Massachusetts, Rhode Island, Connecticut
# Division 2 (Mid-Atlantic): New York, Pennsylvania, New Jersey
# Division 3 (East North Central): Wisconsin, Michigan, Illinois, Indiana, Ohio
# Division 4 (West North Central): Missouri, North Dakota, South Dakota, Nebraska, Kansas, Minnesota, Iowa
# Division 5 (South Atlantic): Delaware, Maryland, District of Columbia, Virginia, West Virginia, North Carolina, South Carolina, Georgia, Florida
# Division 6 (East South Central) Kentucky, Tennessee, Mississippi, Alabama
# Division 7 (West South Central) Oklahoma, Texas, Arkansas, Louisiana
# Division 8 (Mountain) Idaho, Montana, Wyoming, Nevada, Utah, Colorado, Arizona, New Mexico
# Division 9 (Pacific) Alaska, Washington, Oregon, California, Hawaii
nis_hosp$NIS_STRATUM


-99
nis_core$age <- cut(nis_core$AGE, 
                    breaks = c(5 * 0:17, Inf), 
                    labels = paste0(
                      formatC(5 * 0:17, width = 2, flag = "0"),
                      c(formatC(5 * 1:17 - 1, width = 2, flag = "0"), "")
                    ), 
                    right = FALSE)






map(nis_core, ~mean(.x == -99))
glimpse(nis_core)
nis_core$AGE %>% summary


# Is HOSP_NIS consistent?

# Place of Residence
# Place of Treatment
# Clinical Services

nis_core[, .N, by = .(HOSP_NIS, YEAR)] %>% 
  spread(YEAR, N)
nis_core[, .N, by = .(PL_NCHS, YEAR)] %>% 
  spread(YEAR, N)
nis_core[, .N, by = .(ZIPINC_QRTL, YEAR)] %>% 
  spread(YEAR, N)

nis_hosp


# -------------------------------------------------------------------------

usethis::use_data(usmap_shape, overwrite = TRUE)
