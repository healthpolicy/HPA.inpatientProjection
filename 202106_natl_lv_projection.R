# US population by age/sex
# national - division - census block

library(tidycensus)
library(tidyverse)
# https://walker-data.com/tidycensus/articles/basic-usage.html

census_api_key("918149b60d174e68a9f6e575d224d410553deb93")

div_pop_ls <- map(2009:2017 %>% setNames(., .), function(.year) {
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


div_pop %>% 
  group_by(year) %>% 
  summarise(pop = sum(estimate))