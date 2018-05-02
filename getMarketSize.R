getMarketSize_basic <- function(dfSim){
  marketSize <- dfSim %>%
    group_by(wm_yr_wk) %>%
    summarise(total_quantity = sum(quantity)) %>%
    extract2("total_quantity") %>%
    max() %>%
    multiply_by(1.1)
  
  dfSim <- dfSim %>%
    mutate(market_size = marketSize) %>%
    group_by(wm_yr_wk) %>%
    mutate(outside = market_size - sum(quantity)) %>%
    ungroup() %>%
    mutate(lnsr = log(quantity / outside)) %>%
    #     mutate(ppr = price / roll_count) %>%
    #     mutate(ppre = price / roll_equivalency) %>%
    mutate(rollback = ifelse(type == 'rollback', 1, 0))
  
  return(dfSim)
}

getMarketSize <- function(df, segmented = FALSE){
  if(segmented){
    listOffunctions <- split(df, df[segment_by])
    posAttributeDataWithSimilarity <- lapply(listOffunctions, function(x) getMarketSize_basic(x))
    posAttributeDataWithSimilarity %<>% do.call("rbind", .)
  }else{
    posAttributeDataWithSimilarity <- getMarketSize_basic(df)
  }
  return(posAttributeDataWithSimilarity)
}