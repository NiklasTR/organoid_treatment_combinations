library(tidyverse)
library(synergyfinder)

## load data
syn_table <- read_csv(here::here("data/processed/synergy_input.csv"))

## define function
tidy_synergy = function(df, me){ lapply(as.list(me), function(m, f = df){
  #hacky way of ignoring cases in which the synergy scores could not be calculated
  return(tryCatch(f %>% 
                    ReshapeData(., data.type = "viability") %>%
                    #PlotDoseResponse() %>%
                    CalculateSynergy(method = m, correction = TRUE) %>%
                    tibble(BlockID = .$drug.pairs %>% simplify() %>% .[4],
                           method = .$method,
                           score_average = .$scores %>% .[[1]] %>% .[-1,-1] %>% simplify() %>% mean(),
                           score_sd = .$scores %>% .[[1]] %>% .[-1,-1] %>% simplify() %>% sd()) %>%
                    select(BlockID, method, score_average, score_sd) %>% distinct(), 
                  error=function(e) NULL))}
) %>% bind_rows()
  #PlotSynergy(type = "all")
}

## run calculation
## TODO check for version stability 
syn_scores <- syn_table %>% 
  #filter(block_id == "Everolimus_AZD5363") %>%
  group_by(line, replicate , block_id) %>% 
  do(tidy_synergy(df = ., me = c("ZIP", "HSA", "Bliss", "Loewe"))) 

syn_scores <- syn_scores %>%
  ungroup() %>%
  #complete the df for scores that could not be calculated
  mutate(method = as.factor(method),
         line = as.factor(line)) %>% 
  complete(block_id, method, replicate, line) %>%
  mutate(score_average = ifelse(is.nan(score_average), NA, score_average))

syn_scores %>% write_csv(here::here("data/processed/synergy_scores.csv"))
#save(syn_scores, file = "syn_scores_targeted_complete_valid.Rdata")
