library(tidyverse)
library(data.table)


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
  2015:2017, 
  ~read_in_NIS(YearA = .x)
)

nis_core <- map(nis0_ls, "core") %>% 
  map(
    select, 
    DISCWT, KEY_NIS, NIS_STRATUM,
    YEAR, DQTR,
    AGE, FEMALE, RACE,
    HCUP_ED, HOSP_DIVISION, HOSP_NIS,
    starts_with("DRG"),
    PL_NCHS, ZIPINC_QRTL, PAY1,
    LOS, TOTCHG
  ) %>% 
  data.table::rbindlist(fill = TRUE)

nis_hosp <- map(nis0_ls, "hosp") %>% 
  data.table::rbindlist()

fst::write_fst(nis_core, "data-raw/nis_core.fst", compress = 100)
fst::write_fst(nis_hosp, "data-raw/nis_hosp.fst", compress = 100)


# Modifying fst files -----------------------------------------------------





# usethis::use_data(DATASET, overwrite = TRUE)
