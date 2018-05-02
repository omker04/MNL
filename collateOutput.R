source('setupPkgs.R')
attribute <- c("food_type", "configuration", "volume", "shape", 
               "price_indicator", "brand", "piece_count", "material", "medium_level_community")
attributeData <- readxl::read_excel('FoodStorage_LineReview/Food Storage Attributes_modonly.xlsx') %>% 
  set_colnames(., tolower(gsub(pattern = ' ', replacement = '_', x = colnames(.))))


collateOutput <- function(whichSegment){
  output <- readRDS(paste0('FoodStorage_LineReview/', whichSegment, '_Output.rds'))
  
  
  item <- lapply(output, function(x) {
    if(is.list(x) & !is.data.frame(x) & length(x) != 0){
      x %<>% 
        do.call('rbind', .) %>% 
        filter(!store_nbr %like% 'Error')
    }
    if(!is.null(nrow(x))){
      x$upc_nbr <- as.numeric(x$upc_nbr)
      todelete <- x %>%
        filter(is.na(adjustedPredictedQuantityPostDrop)) %>%
        select(upc_nbr) %>%
        unique() %>%
        as.numeric()
      print(todelete)
      nbr_stores <- uniqueN(x$store_nbr)
      print(nbr_stores)
      item1 <- x %>%
        filter(!dollar %like% 'Inf') %>%
        mutate_each(funs(as.numeric), dollar, quantity, price, adjustedPredictedQuantityPostDrop) %>% 
        group_by_(., .dots = lapply(c('upc_nbr', attribute), as.symbol)) %>%
        summarise(dollar = sum(dollar, na.rm = TRUE),
                  quantity = sum(quantity, na.rm = TRUE),
                  price = mean(price, na.rm = TRUE),
                  adjustedPredictedQuantityPostDrop = sum(adjustedPredictedQuantityPostDrop, na.rm = TRUE),
                  both_store_avbl = uniqueN(store_nbr)) %>%
        mutate(deleted_upc = todelete) %>% 
        mutate(deleted_upc_store_avbl = nbr_stores) %>% 
        mutate(diff = adjustedPredictedQuantityPostDrop - quantity) %>% 
        mutate(denominator = if_else(adjustedPredictedQuantityPostDrop == 0, quantity, 0)) %>% 
        ungroup() %>% 
        group_by(deleted_upc) %>% 
        mutate(transfer = 100 * diff / sum(denominator)) %>%
        mutate(demand_transfer = if_else(transfer == -100, transfer * NA, transfer * 1)) %>% 
        select_(.dots = lapply(c('deleted_upc', 'deleted_upc_store_avbl', 'upc_nbr', 'both_store_avbl',
                                 attribute, 'dollar', 'quantity', 'price', 'adjustedPredictedQuantityPostDrop', 'demand_transfer'), as.symbol)) %>%
        ungroup()
      return(item1)
    }
  }) %>% do.call('rbind', .)
}


allTogether <- rbind(
  Basic_Lunch_Bag,
  Beverage,
  Canisters,
  Carriers,
  Dry_Dispenser,
  DurableStorage,
  FoodKeepers,
  FreshWorks,
  Ice,
  LargeBowls,
  LunchBagAddons,
  LunchBagsBoxes,
  MatchingSemi,
  Misc,
  Pitchers,
  Premium,
  SemiDurable,
  WaterBottles
)
allTogether %<>% 
  filter(!is.nan(price)) %>% 
  filter(!is.infinite(demand_transfer)) %>% 
  inner_join(., attributeData %>% select(upc, signing_desc), by = c('deleted_upc' = 'upc')) %>% 
  inner_join(., attributeData %>% select(upc, signing_desc), by = c('upc_nbr' = 'upc'))

for(i in 1:nrow(allTogether)){
  if(!is.na(allTogether$demand_transfer[i]) & 
      allTogether$demand_transfer[i] <= -99){
    #print(paste('pre', allTogether$demand_transfer[i]))
    allTogether$demand_transfer[i] <- NA
    #print(paste('post', allTogether$demand_transfer[i]))
  }
}

allTogether %<>% 
  set_colnames(., c('Rollup.A', 'Rollup.A.Stores', 'Rollup.B', 'Rollup.AandB.Stores.Avbl', attribute, 
                    'Rollup.B.Dollars', 'Rollup.B.Units.Pre', 'Rollup.B.Avg.Price', 'Rollup.B.Predicted.Units.Post', 
                    'Demand.of.Rollup.A.Transferred.to.Rollup.B', 'Rollup.A.Description', 'Rollup.B.Description'))

write.csv(allTogether, 'FoodStorage_LineReview/FoodStorage_consolidated_output.csv', row.names = FALSE)
  
  
