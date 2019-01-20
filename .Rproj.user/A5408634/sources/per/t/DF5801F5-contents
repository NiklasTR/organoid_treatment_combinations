library(synergyfinder)

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