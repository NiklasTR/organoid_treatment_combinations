library(tidyverse)
library(here)

ctg_data <- tibble(files = list.files(here("data/ctg_data/validation"), pattern = ".TXT"),
                   path = list.files(here("data/ctg_data/validation"), 
                                     pattern = ".TXT", 
                                     full.names = TRUE)) %>% 
  mutate(data = map(path, ~ read_delim(.x, 
                                       "\t", 
                                       escape_double = FALSE, 
                                       col_names = FALSE, 
                                       trim_ws = TRUE) %>%
                      magrittr::set_colnames(c('name', 'well', 'pcount')))) %>% 
  
  #### Formatting columns
  
  separate(files, c("date", "operator", "mithras", "experiment_id"), sep = "_", remove = FALSE) %>%
  mutate(experiment_id = substr(experiment_id, 1, nchar(experiment_id)-4)) %>%
  # I renamed one file that had irregular name patterns: **mv 181030_AF_M2_D105T01V007L13.TXT 181030_AF_M2_D105T01V006L13.TX**. 
  unnest(data) %>% 
  mutate(row = substr(well, 1,1),
         col = substr(well, 2,3),
         col_num = as.numeric(col)) %>% 
  
  #### Adding annotation column 
  left_join(., read_csv(here("anno/combi_validation_robot.csv")) %>%  
              rename(well = destination.well) %>%
              select(drug_pair_conc, well) %>%
              separate(well, c("row", "col"), sep = 1, remove = FALSE) %>%
              distinct() %>%
              separate(drug_pair_conc, c("drug_1", "drug_2", "conc_1", "conc_2"), 
                       remove = FALSE, sep = "_")) %>%
  
  #### Adding addtional features
  
  mutate(id = experiment_id) %>%
  mutate(row_num = match(row, LETTERS[1:26])) %>%
  mutate(line = substr(id, 1, 7),
         drug = paste0(drug_1, "_", drug_2)) %>% 
  mutate(plate_no = substr(id, 9, nchar(id)-3) %>% as.numeric()) 

ctg_data <- ctg_data %>%
  left_join(., ctg_data %>% 
              dplyr::select(line, plate_no) %>%
              distinct() %>%
              group_by(line) %>% 
              mutate(replicate = if_else(plate_no == min(plate_no), 1,2)))

ctg_data %>% write_csv(here("data/processed/ctg_data.csv"))