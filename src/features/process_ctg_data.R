library(tidyverse)
library(here)

ctg_data <- read_csv(here("data/processed/ctg_data.csv"))

ctg_loess <- ctg_data %>% dplyr::select(row_num, col_num, pcount, id) %>% 
  # na values
  drop_na() %>%
  #set the ctrl column 13 to NA
  #mutate(pcount = ifelse(col_num == 13, NA, pcount)) %>%
  split(.$id) %>% lapply(function(s){
    ## loess fit. family is 'symmetric' to be robust to outliers
    fit <- loess(pcount ~ row_num + col_num, data=s, family='symmetric')
    ## apply normalization
    tibble(norm_fac = fit$fitted) %>% cbind(s %>% drop_na(),.) %>% 
      mutate(pcount_norm = pcount - (norm_fac - median(norm_fac)))
  }) %>% bind_rows() %>% full_join(ctg_data)

ctg_loess %>% write_csv(here::here("data/processed/ctg_loess.csv"))

ctg_norm <- ctg_loess %>% 
  left_join(., ctg_loess %>% 
              filter(drug == "DMSO_DMSO") %>%
              group_by(id) %>% 
              summarise(dmso_solo_id = mean(pcount_norm))) %>%
  mutate(viability = pcount_norm/dmso_solo_id) %>% 
  #I crop the minimal viability to be zero
  mutate(viability_uncropped = viability) %>% 
  mutate(viability = if_else(viability < 0, 0, viability))


ctg_norm %>% write_csv(here::here("data/processed/ctg_norm.csv"))

