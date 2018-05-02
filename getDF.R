getDF <- function(whichSegment){
  allSegments <- names(allFunc3[[whichSegment]])
  Beverage_UPCs <- attributeData %>% filter(medium_level_community == allSegments[whichSegment])
  Beverage_UPCs <- as.list(Beverage_UPCs$upc_nbr)
  Beverage_Output <- vector('list', length(Beverage_UPCs))
  print(allSegments[whichSegment])
  print(length(Beverage_UPCs))
  mnl_wos <- readRDS(paste0('FoodStorage_LineReview/', allSegments[whichSegment], '_mnl.rds'))
  
  #######
  # 
  # whichSegment <- 'Beverage Dispenser'                   ############# required input
  # deleteUPCs <- 7065200744                   ############# required input
  # addUPCs <- integer(0)          ############# optional input
  # added_price <- integer(0)      ############# optional input, required only if 'addUPCs' is provided
  # 
  # allStores <- store_upc_on_hand[as.character(deleteUPCs)] %>% unlist()
  # names(allStores) <- NULL
  # #allStores <- which(names(allFunc3) %in% as.character(allStores1))########################################################### store_nbr to stress upon
  # y <- which(names(allFunc3[[1]]) == whichSegment)                ########################################################### community to consider in the selected store
  # 
  # names(mnl_wos) <- names(allFunc3)
  # print(Sys.time())
  # system.time(
  #   AllstoreOutput <- mclapply(allStores %>% head(), function(x){
  #     store_df <- allFunc3[[x]][[y]] %>% 
  #       ungroup() %>% 
  #       select(-c(outside, lnsr)) %>% 
  #       mutate(add = NA)                                              ########################################################### getting data for the selected store and community
  #     deletedIndex <- which(store_df$upc_nbr == deleteUPCs)           ########################################################### identifying indices of the deleted UPCs
  #     
  #     predictedSales <- MNL_withoutSimilarity_Prediction_Multiple_Drop(df_all = store_df, 
  #                                                                      model = mnl_wos, 
  #                                                                      deleted = deletedIndex, 
  #                                                                      segmented = TRUE)
  #     return(predictedSales)
  #   }, mc.cores = numCores)
  # )
  # print(Sys.time())
  #####
  
  getpredict <<- function(whichStore, deleteUPCs){
    x <- which(names(allFunc3) == whichStore)
    store_df <- allFunc3[[x]][[y]] %>% 
      ungroup() %>% 
      select(-c(outside, lnsr)) %>% 
      mutate(add = NA)                                              ########################################################### getting data for the selected store and community
    deletedIndex <- which(store_df$upc_nbr == deleteUPCs)           ########################################################### identifying indices of the deleted UPCs
    
    if(nrow(store_df) > 0){
      predictedSales <- MNL_withoutSimilarity_Prediction_Multiple_Drop(df_all = store_df, 
                                                                       model = mnl_wos, 
                                                                       deleted = deletedIndex, 
                                                                       segmented = TRUE)
    } else {
      predictedSales <- data.frame(
        'store_nbr' = numeric(0),
        'upc_nbr' = numeric(0),
        'rollback' = numeric(0),
        'dollar' = numeric(0),
        'quantity' = numeric(0),
        'price' = numeric(0),
        'lprice' = numeric(0),
        'market_size' = numeric(0),
        'add' = logical(0),
        'adjustedPredictedQuantityPostDrop' = numeric(0),
        'walkoff' = character(0)
      )
    }
    return(predictedSales)
  }
  ##### 
  # 
  # AllstoreOutput <- vector('list', length = length(allStores))
  # for(i in allStores){
  #   AllstoreOutput[[i]] <- getpredict(i)
  # }
  # 
  # s <- Sys.time()
  # AllstoreOutput <- lapply(allStores, function(x) getpredict(x))
  # s-Sys.time()
  # 
  # 
  # Beverage_UPCs <- list(7065200744, 7065200745, 7065200767, 7065276707, 7065276708, 7065276709)
  ##### 
  s <- Sys.time()
  Beverage_Output <- lapply(Beverage_UPCs, function(z) {
    deleteUPCs <- z
    addUPCs <- integer(0)         
    added_price <- integer(0)     
    allStores <- store_upc_on_hand[as.character(deleteUPCs)] %>% unlist()
    names(allStores) <- NULL
    y <<- which(names(allFunc3[[1]]) == allSegments[whichSegment])                ########################################################### community to consider in the selected store
    output <- lapply(allStores, function(k) getpredict(k, deleteUPCs))
    return(output)
  })
  print(Sys.time() - s)
  
  # output <- lapply(Beverage_Output, function(x) x %<>% 
  #                    do.call('rbind', .) %>%
  #                    filter(!store_nbr %like% 'Error') %>%
  #                    mutate(deleted_upc = deleteUPCs)
  # )
  
  saveRDS(Beverage_Output, paste0('FoodStorage_LineReview/', allSegments[whichSegment], '_Output.rds'))
}
