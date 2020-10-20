library(tidyverse)
library(data.table)
library(datasets)
# library(tidycensus)


# From HCUP source to fst files -------------------------------------------

# Copied and modified from Jim's function (S:\Health Data\USA\HCUP_R)
read_in_NIS <- function (dirA = "S:\\Health Data\\USA\\HCUP\\",
                         YearA,
                         str_start = c(1, 3, 4, 10),
                         str_end   = c(2, 3, 9, 100)) {

  dirC = paste0(dirA,"NIS_",YearA)

  read_inA <- function(fileA = "_Core") {

    urlfileA = paste0("FileSpecifications_NIS_",YearA,fileA,".TXT")
    url = paste0("https://www.hcup-us.ahrq.gov/db/nation/nis/tools/stats/",urlfileA)
    tmpfile <- tempfile()

    download.file(url, tmpfile)
    widths <- (str_end - str_start) + 1

    spec1 = readr::read_fwf(tmpfile,
                            skip=8,
                            n_max=10,
                            fwf_widths(widths))
    widths2 <- (spec1$X3 - spec1$X1) + 2
    colnames2 = c("Database_name",
                  "Discharge_year",
                  "File_name",
                  "Data_element_number",
                  "Data_element_name",
                  "Start",
                  "End",
                  "Digits_after_decimal",
                  "Data_element_type",
                  "Data_element_label")
    n_ <- readr::read_fwf(tmpfile,
                          skip=1,
                          n_max=1,
                          fwf_widths(c(23,8))) %>%
      .$X2 %>%
      as.numeric()
    spec <- readr::read_fwf(tmpfile,
                            skip=20,
                            fwf_widths(widths2,colnames2))

    widths3 <- (spec$End - spec$Start) + 1

    file.remove(tmpfile)

    textfile <- paste0(dirC,"\\NIS_",YearA,fileA,".ASC")
    readr::read_fwf(textfile,
                    fwf_widths(widths3,trimws(as.character(spec$Data_element_name)))) %>%
      data.table::data.table() %>%
      mutate_at(vars(starts_with("I10_DX")),funs(trimws(.)))
  }

  list(
    core = read_inA(fileA = "_Core"),
    hosp = read_inA(fileA = "_Hospital")
  )
}

nis0_ls <- map(
  2015:2017 %>% setNames(., .), 
  ~read_in_NIS(YearA = .x)
)

nis2015_drg <- map2(
  list("NIS_2015Q1Q3_DX_PR_GRPS.ASC", "NIS_2015Q4_DX_PR_GRPS.ASC"),
  list(
    fwf_cols(DRG = c(181, 183), KEY_NIS = c(594, 603)),
    fwf_cols(DRG = c(1, 3), KEY_NIS = c(366, 375))
  ),
  ~readr::read_fwf(file.path("S:\\Health Data\\USA\\HCUP\\", "NIS_2015", .x), .y)
) %>% 
  data.table::rbindlist()

nis0_ls[["2015"]]$core <- nis0_ls[["2015"]]$core %>% 
  left_join(nis2015_drg) %>% 
  as.data.table()

nis_core <- map(nis0_ls, "core") %>% 
  map(
    select,
    DISCWT, KEY_NIS, NIS_STRATUM,
    YEAR, DQTR,
    AGE, FEMALE, RACE,
    HCUP_ED, HOSP_DIVISION, HOSP_NIS,
    DRG,
    PL_NCHS, ZIPINC_QRTL, PAY1,
    LOS, TOTCHG
  ) %>%
  data.table::rbindlist(fill = TRUE)

nis_hosp <- map(nis0_ls, "hosp") %>%
  data.table::rbindlist()

fst::write_fst(nis_core, "data-raw/nis_core.fst", compress = 100)
fst::write_fst(nis_hosp, "data-raw/nis_hosp.fst", compress = 100)


# Modifying fst files -----------------------------------------------------

nis_core <- fst::read_fst("data-raw/nis_core.fst", as.data.table = TRUE)[AGE != -99 & PL_NCHS != -99]
nis_hosp <- fst::read_fst("data-raw/nis_hosp.fst", as.data.table = TRUE)

census_div_lkup <- tibble(
  reg = as.numeric(state.region),
  regx = state.region,
  div = as.numeric(state.division),
  divx = state.division,
  statex = state.name,
  stateabb = state.abb
)

# https://www.irs.gov/statistics/soi-tax-stats-individual-income-tax-statistics-2017-zip-code-data-soi
income <- vroom::vroom("data-raw/17zpallagi.csv") %>% 
  as.data.table()

# https://www.cdc.gov/nchs/data_access/urban_rural.htm#Data_Files_and_Documentation
urban_rural <- haven::read_sas("data-raw/NCHSurbruralcodes2013.sas7bdat")

# https://wonder.cdc.gov/wonder/sci_data/codes/fips/type_txt/cntyxref.asp
zip_fips_lkup <- map(1:10, ~read_fwf(
  paste0("data-raw/zipcty/zipcty", .x),
  fwf_cols(zip = c(1, 5), stateabb = c(24, 25), fips = c(26, 28))
) %>% 
  unique()) %>% 
  bind_rows()


urban_rural %>% filter(ctyfips == 119)

zip_info <- inner_join(
  income_zip[, .(adj_gross_inc = weighted.mean(A00100, N1)), 
             by = .(stateabb = STATE, zip = zipcode)] %>% 
    mutate(ZIPINC_QRTL = cut(
      adj_gross_inc,
      breaks = c(-1, 44000, 56000, 74000, Inf),
      labels = 1:4
    )) %>% 
    mutate(ZIPINC_QRTL = as.character(ZIPINC_QRTL)),
  urban_rural_zip %>% 
    mutate(zip = formatC(ctyfips, width = 5, flag = "0")) %>% 
    select(zip, PL_NCHS = CODE2013)  
)


nis_core$PL_NCHS %>% class

nis_core

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

# usethis::use_data(DATASET, overwrite = TRUE)
