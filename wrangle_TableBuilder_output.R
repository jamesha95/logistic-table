### Here we wrangle the raw TableBuilder output

library(tidyverse)
#devtools::install_github('wfmackey/absmapsdata')
library(absmapsdata) # Will's very helpful package

# We extracted data from TableBuilder as 'csv string value'

raw_by_sa4 <- read_csv("raw_data/ABS Stats conf unemp data.csv", 
                       # There's lots of lines of junk in TableBuilder output
                       skip = 10, 
                       col_names = c("dataset",
                                     "sa4",
                                     "age", 
                                     "sex",
                                     "atsi",
                                     "education",
                                     "lfs",
                                     "count"
                       ),
                       col_types = "ccfffffd") %>%
  filter(!(is.na(lfs))) %>%
  left_join(sa42016 %>% select(sa4_name_2016, state_name_2016, areasqkm_2016),
            by = c("sa4" = "sa4_name_2016")) %>%
  filter(!str_starts(sa4, "Migratory"), 
         !str_starts(sa4, "No usual")) %>%
  select(-dataset) %>%
  rename(state = state_name_2016) %>%
  select(state, everything())

pop_data <- read_csv('raw_data/population_by_sa4.csv', skip = 10, col_names = c("dataset", "sa4", "population"))

raw_by_sa4 <- raw_by_sa4 %>% left_join(select(pop_data, -dataset), by = "sa4") %>%
  mutate(density = population/areasqkm_2016)

write_csv(raw_by_sa4, "clean_data/unemployment_data_by_sa4.csv")






