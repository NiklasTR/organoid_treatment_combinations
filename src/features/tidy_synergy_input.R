library(tidyverse)
library(here)

ctg_norm <- read_csv(here::here("data/processed/ctg_norm.csv"))

df <- ctg_norm
#not a universal function
syn_table <- df %>%
  #filter(drug != "DMSO_DMSO") %>%
  #filter(drug_1 != "DMSO") %>%
  #filter(drug_2 != "DMSO") %>%
  rename(rv = viability) %>%
  #setup the easy stuff
  dplyr::transmute(
    BlockID = paste(drug, sep = "_"),
    Response = rv*100,
    Replicate = replicate,
    DrugRow = drug_1,
    DrugCol = drug_2,
    #build matrix
    Row = ifelse(drug_1 != "DMSO", as.numeric(conc_1), 6),
    Col = ifelse(drug_2 != "DMSO", as.numeric(conc_2), 6),
    #define concentrations
    ConcRow = 10/(5^(as.numeric(conc_1)-1)),
    ConcCol = 10/(5^(as.numeric(conc_2)-1)),
    ConcRow = ifelse(drug_1 != "DMSO", ConcRow, 0),
    ConcCol = ifelse(drug_2 != "DMSO", ConcCol, 0),
    ConcUnit = "uM",
    line = line
  ) %>%  arrange(BlockID, Replicate, Row) 

#Next I add the DMSO controls
syn_table_p2 <- rbind(syn_table %>% filter(DrugCol != "DMSO"),
                      
                      syn_table %>% filter(DrugCol == "DMSO") %>%
                        select(-BlockID)  %>%
                        mutate(BlockID = map(DrugRow , ~ syn_table %>% 
                                               filter(DrugCol != "DMSO" & grepl(BlockID, pattern = .x)) %>% 
                                               distinct() %>%
                                               dplyr::select(BlockID))) %>% unnest() %>% distinct(),
                      
                      syn_table %>% filter(BlockID == "DMSO_DMSO") %>%
                        select(-BlockID)  %>%
                        mutate(BlockID = map(DrugRow , ~ syn_table %>% 
                                               filter(DrugCol != "DMSO" & DrugRow != "DMSO") %>% 
                                               distinct() %>%
                                               dplyr::select(BlockID))) %>% unnest() %>% distinct() 
)

syn_table <- syn_table_p2 %>% 
  group_by(BlockID, Replicate, DrugRow,
           DrugCol, Row, Col, ConcRow, ConcCol, ConcUnit, line) %>%
  summarise(Response = mean(Response)) %>% 
  # they changed the API 
  rename(block_id = BlockID,
         drug_row = DrugRow, 
         drug_col = DrugCol, 
         response = Response, 
         conc_r = ConcRow, 
         conc_c = ConcCol, 
         conc_r_unit = ConcUnit,
         replicate = Replicate) %>% 
  mutate(conc_c_unit = conc_r_unit) %>% 
  # fix wrong negative controls
  ungroup() %>%
  separate(block_id, c("ctrl_1", "ctrl_2"), remove = FALSE, sep = "_") %>% 
  #filter(ctrl_1 != drug_row & drug_col == "DMSO")
  mutate(Row_t = if_else(ctrl_1 != drug_row & drug_col == "DMSO",
                         Col,
                         Row),
         Col_t = if_else(ctrl_1 != drug_row & drug_col == "DMSO",
                         Row,
                         Col),
         conc_r_t = if_else(ctrl_1 != drug_row & drug_col == "DMSO",
                            conc_c,
                            conc_r),
         conc_c_t = if_else(ctrl_1 != drug_row & drug_col == "DMSO",
                            conc_r,
                            conc_c)) %>%
  dplyr::select(-ctrl_1, -ctrl_2, -Row, -Col, -conc_r, -conc_c) %>%
  dplyr::select(block_id,
                drug_row, 
                drug_col,
                Row = Row_t,
                Col = Col_t, 
                conc_r = conc_r_t,
                conc_c = conc_c_t,
                everything())

#save(syn_table, file = "syn_table_targeted_complete_valid.Rdata")

syn_table %>% write_csv(here::here("data/processed/synergy_input.csv"))

# save set of the syn_table
# syn_shape <- syn_table %>% 
#   nest(-line, -replicate) %>% 
#   mutate(reshaped = map(data, ~ ReshapeData(.x,
#                                             data.type = "viability",
#                                             impute = TRUE, 
#                                             noise = TRUE, 
#                                             correction = "all")))
