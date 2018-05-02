###################################### Pre Process Data Prep #############################
source('setupPkgs.R')
clean_varname <- . %>% str_to_lower() %>% str_replace_all(" ", "_")
clean_dfname <- . %>% set_names(names(.) %>% clean_varname())

# allStoreData <- data_2yr %>% clean_dfname()
# allStoreData <- arrange(allStoreData, store_nbr, wm_yr_wk)
# allStoreData <- apply(allStoreData, 2, function(x) return(as.numeric(x))) %>% as.data.frame()

attributeData <- readxl::read_excel("FoodStorage/FS_ATTRIBUTES_FOR_DT_012817.xlsx") %>%
  clean_dfname() %>% 
  select(upc_, brand, price_indicator, durability, subcategory) %>% 
  set_colnames(., c('upc_nbr', 'brand', 'fineline', 'item_function', 'subcat')) %>% 
  filter(!is.na(upc_nbr)) 

#selectedStore <- 345
selectedStoreData <- allStoreData #%>% filter(store_nbr == selectedStore)
rm(allStoreData)
attribute <- colnames(attributeData)[-1]

source('dataPrep.R')
system.time(posAttributeDataFinal <- dataPrep(selectedStoreData, attributeData, attribute))


# getMAPE <- function(pred){
#   MeanAPE <- mean(abs(pred$quantity - pred$estimated_quantity)/pred$quantity)*100
#   MedianAPE <- median(abs(pred$quantity - pred$estimated_quantity)/pred$quantity)*100
#   nbr <- nrow(unique(pred[attribute]))
#   return(c(MeanAPE, MedianAPE, nbr))
# }

###################################### All SEGMENTED MODELS #############################

source('AttributeSimilarity.R')
source('errorDistribution.R')
posAttributeDataFinal %<>% split(., posAttributeDataFinal$store_nbr)
system.time(posAttributeDataWithSimilarity <- mclapply(posAttributeDataFinal, function(x) similarityCalculation(x, FALSE, TRUE), mc.cores = 4))
allFunc <- mclapply(posAttributeDataWithSimilarity, function(x) split(x, x$item_function), mc.cores = 8)
allFunc2 <- mclapply(allFunc, function(x) lapply(x, 
                                                 function(y){
                                                   z <- y %>% 
                                                     # group_by(upc_nbr) %>% 
                                                     arrange(upc_nbr, desc(wm_yr_wk)) %>% 
                                                     group_by(upc_nbr) %>% 
                                                     slice(1) %>% 
                                                     ungroup()
                                                   maxWeek <- max(z$wm_yr_wk)
                                                   z %<>% filter(wm_yr_wk == maxWeek)
                                                   return(z)
                                                  }),
                     mc.cores = 8)

allFunc3 <- mclapply(allFunc, function(x) lapply(x,
                                                 function(y) y %<>% 
                                                   select(-c(wm_yr_wk, type, rollback)) %>% 
                                                   group_by(upc_nbr, brand, subcat, fineline, item_function) %>% 
                                                   mutate_each(funs(mean), store_nbr, dollar, quantity, price, market_size) %>% 
                                                   mutate(outside = median(outside)) %>% 
                                                   mutate(lprice = log(price)) %>% 
                                                   mutate(lnsr = log(quantity/outside)) %>% 
                                                   unique()
                                                   #mutate(lnsr = log(quantity/outside))
                                                 ), mc.cores = 8)

testData <- read_rds('BathRoom Tissues/testData.rds')
selectedStoreData_test <- testData
system.time(posAttributeDataFinal_test <- dataPrep(selectedStoreData_test, attributeData, attribute))
posAttributeDataFinal_test %<>% split(., posAttributeDataFinal_test$store_nbr)
system.time(posAttributeDataWithSimilarity_test <- mclapply(posAttributeDataFinal_test, function(x) similarityCalculation(x, FALSE, TRUE), mc.cores = 8))
allFunc_test <- mclapply(posAttributeDataWithSimilarity_test, function(x) split(x, x$item_function), mc.cores = 8)
allFunc_test %<>% mclapply(., function(x) lapply(x, function(y) y %<>% 
                                                   select(-c(wm_yr_wk, type, rollback)) %>% 
                                                   group_by(upc_nbr, brand, subcat, fineline, item_function) %>% 
                                                   mutate_each(funs(mean), dollar, quantity) %>% 
                                                   mutate_each(funs(mean), store_nbr, price, market_size) %>% 
                                                   mutate(outside = median(outside)) %>% 
                                                   mutate(lprice = log(price), lnsr = log(quantity/outside)) %>% 
                                                   unique()
                                                   ), mc.cores = 8)

statusDF <- mclapply(1:length(allFunc), function(x) lapply(1:length(allFunc[[x]]), function(y){
  preDelete <- data.frame('upc_nbr' = allFunc[[x]][[y]]$upc_nbr, 'pre' = TRUE)
  postDelete <- data.frame('upc_nbr' = allFunc_test[[x]][[y]]$upc_nbr, 'post' = TRUE)
  statusDF <- full_join(attributeData %>% filter(item_function == names(allFunc[[x]])[y]), preDelete) %>% full_join(., postDelete)
  return(statusDF %>% unique())
}))
deleted <- mclapply(statusDF, function(x) lapply(x, function(y) which(y$pre == TRUE & is.na(y$post))))
added <- mclapply(statusDF, function(x) lapply(x, function(y) y$upc_nbr[which(y$post == TRUE & is.na(y$pre))]))


################## MNL withOUT similarity #############
  
  source('MNL_withoutSimilarity_Calculation.R')
  source('MNL_withoutSimilarity_Prediction.R')
  source('MNL_withoutSimilarity_Prediction_Drop.R')
  source('unifiedDeleteReco.R')  
  source('MNL_withoutSimilarity_Prediction_Multiple_Drop.R')

  system.time(mnl_wos <- mclapply(allFunc, function(y) lapply(y, function(x) MNL_withoutSimilarity_Calculation(x, TRUE)), mc.cores = 6))
  system.time(pred_wos <- mclapply(allFunc, function(y) lapply(y, function(x) MNL_withoutSimilarity_Prediction(x, mnl_wos, TRUE)), mc.cores = 8))
  system.time({mape_wos <- mclapply(pred_wos, function(y) lapply(y, function(x) getMAPE(x)), mc.cores = 8)
  plot_wos <- getMapeDensity(mape_wos, TRUE, TRUE)})
  system.time(pred_drop_wos <- mclapply(allFunc2, function(y) lapply(y, function(x) MNL_withoutSimilarity_Prediction_Drop(x, mnl_wos, attributeData, TRUE)), mc.cores = 8))
  system.time(deleteReco_wos <- getUnifiedDeleteReco(pred_drop_wos, TRUE, 8))
  system.time(validation_wos <- mclapply(allFunc_test, function(y) lapply(y, function(x) lapply(x, function(z) MNL_withoutSimilarity_Prediction_Multiple_Drop(z, mnl_wos, , TRUE))), mc.cores = 8))
  
  
  validation <- lapply(1:length(allFunc), function(x) lapply(1:length(allFunc[[x]]), function(y){
    df <- allFunc3[[x]][[y]] %>% select(-c(outside, lnsr)) %>% mutate(add = NA)
    whichAdded <- which(allFunc_test[[x]][[y]]$upc_nbr == added[[x]][[y]])
    whichDeleted <- which(df$upc_nbr %in% statusDF[[x]][[y]]$upc_nbr[deleted[[x]][[y]]])
    df <- full_join(df, allFunc_test[[x]][[y]][whichAdded,] %>% select(-c(outside, lnsr, market_size, quantity)) %>% mutate(add = TRUE))
    outputDF <- MNL_withoutSimilarity_Prediction_Multiple_Drop(df, mnl_wos, whichDeleted, TRUE)
    added1 <- which(outputDF$add == TRUE)
    outputDF$adjustedPredictedQuantityPostDrop[added1] <- outputDF$predictedQuantityPostDrop[added1] - outputDF$predictedQuantity[added1]
    original <- allFunc_test[[x]][[y]] %>% ungroup() %>% select(upc_nbr, quantity) %>% mutate('status' = 'observed')
    predicted <- outputDF %>% ungroup() %>% select(upc_nbr, adjustedPredictedQuantityPostDrop) %>% set_colnames(., c('upc_nbr', 'quantity')) %>% mutate('status' = 'predicted')
    ggDF <- rbind(original, predicted[complete.cases(predicted),])
    ggDF$upc_nbr <- as.factor(ggDF$upc_nbr)
    return(ggDF)
  }))
  
  pdf('BathRoom Tissues/validation-scatterplot.pdf')
  par(mfrow = c(1,4))
  for(x in 1:length(allFunc)){
    for(y in 1:length(allFunc[[x]])){
      d <- split(validation[[x]][[y]], validation[[x]][[y]]$status)
      d <- lapply(d, function(x) x <- x %>% select(-status))
      c <- c(15, 16, 24, 45)
      s <- c(307, 541, 549)
      plot(d$observed$quantity, d$predicted$quantity, main = paste('Str -', s[x], 'Comm -', c[y]), xlab = 'observed weekly qty', ylab = 'predicted weekly qty') + abline(0,1)
    }
  }
  dev.off()
  
  pdf('BathRoom Tissues/validation-scatterplot-comm15.pdf')
  par(mfrow = c(1,3))
  for(x in 1:length(allFunc)){
    y = 1
      d <- split(validation[[x]][[y]], validation[[x]][[y]]$status)
      d <- lapply(d, function(x) x <- x %>% select(-status))
      c <- c(15, 16, 24, 45)
      s <- c(307, 541, 549)
      plot(d$observed$quantity, d$predicted$quantity, main = paste('Store --', s[x]), xlab = 'observed weekly qty', ylab = 'predicted weekly qty') + abline(0,1)
  }
  dev.off()
  
  for(x in 1:length(allFunc)){
    for(y in 1:length(allFunc[[x]]))
      assign(x = paste('p', x, y, sep = '_'), value = ggplot(validation[[x]][[y]], aes(x=upc_nbr, y=quantity, fill=status)) +
               geom_bar(stat = 'identity', position = position_dodge()) + coord_flip() +
               theme(legend.position = 'bottom'))
  }
  
  grid.arrange(p_1_1, p_1_2,  p_1_3, p_1_4, p_2_1, p_2_2,  p_2_3, p_2_4, p_3_1, p_3_2,  p_3_3, p_3_4, ncol = 4)
  
  pdf('BathRoom Tissues/Bath-Tissue_Validation.pdf')
  grid.arrange(p_1_1 + ggtitle('Community - 15'), p_1_2 + ggtitle('Community - 16'),  p_1_3 + ggtitle('Community - 24'), p_1_4 + ggtitle('Community - 45'), ncol = 2, top = 'STORE -- 307')
  grid.arrange(p_2_1 + ggtitle('Community - 15'), p_2_2 + ggtitle('Community - 16'),  p_2_3 + ggtitle('Community - 24'), p_2_4 + ggtitle('Community - 45'), ncol = 2, top = 'STORE -- 541')
  grid.arrange(p_3_1 + ggtitle('Community - 15'), p_3_2 + ggtitle('Community - 16'),  p_3_3 + ggtitle('Community - 24'), p_3_4 + ggtitle('Community - 45'), ncol = 2, top = 'STORE -- 549')
  dev.off()
  
  
################## MNL with similarity #############

  source('MNL_withSimilarity_Calculation.R')
  source('MNL_withSimilarity_Prediction.R')
  source('MNL_withSimilarity_Prediction_Drop.R')
  
#### with CrossPrice ######
    system.time(mnl_ws <- mclapply(allFunc, function(y) lapply(y, function(x) MNL_withSimilarity_Calculation(x, TRUE)), mc.cores = 8))
    system.time(pred_ws <- mclapply(allFunc, function(y) lapply(y, function(x) MNL_withSimilarity_Prediction(x, mnl_ws, TRUE)), mc.cores = 8))
    mape_ws <- mclapply(pred_ws, function(y) lapply(y, function(x) getMAPE(x)), mc.cores = 8)
    plot_ws <- getMapeDensity(mape_ws, TRUE)
    system.time(pred_drop_ws <- mclapply(allFunc2, function(y) lapply(y, function(x) MNL_withSimilarity_Prediction_Drop(x, mnl_ws, attributeData, TRUE)), mc.cores = 8))
    rm(mnl_ws, pred_ws, mape_ws, plot_ws, pred_drop_ws)
    
#### with CrossPrice ######
    system.time(mnl_ws_cp <- mclapply(allFunc, function(y) lapply(y, function(x) MNL_withSimilarity_Calculation(x, TRUE, TRUE)), mc.cores = 8))
    system.time(pred_ws_cp <- mclapply(allFunc, function(y) lapply(y, function(x) MNL_withSimilarity_Prediction(x, mnl_ws_cp, TRUE, TRUE)), mc.cores = 8))
    mape_ws_cp <- mclapply(pred_ws_cp, function(y) lapply(y, function(x) getMAPE(x)), mc.cores = 8)
    #write_rds(getMapeDensity(mape_ws_cp, TRUE), 'mape_ws_cp.rds')
    plot_ws_cp <- getMapeDensity(mape_ws_cp, TRUE)
    system.time(pred_drop_ws_cp <- mclapply(allFunc2, function(y) lapply(y, function(x) MNL_withSimilarity_Prediction_Drop(x, mnl_ws_cp, attributeData, TRUE, TRUE)), mc.cores = 8))
  

################## Rooderkerk #############
  
  source('Rooderkerk_Calculation.R')
  source('Rooderkerk_Prediciton.R')
  source('Rooderkerk_Prediction_Drop.R')
    
#### without CrossPrice ######
    system.time(mnl_rdk <- mclapply(allFunc, function(y) lapply(y, function(x) Rooderkerk_Calculation(x, TRUE)), mc.cores = 8))
    system.time(pred_rdk <- mclapply(allFunc, function(y) lapply(y, function(x) Rooderkerk_Prediction(x, mnl_rdk, TRUE)), mc.cores = 8))
    mape_rdk <- mclapply(pred_rdk, function(y) lapply(y, function(x) getMAPE(x)), mc.cores = 8)
    plot_rdk <- getMapeDensity(mape_rdk, TRUE)
    system.time(pred_drop_rdk <- mclapply(allFunc2, function(y) lapply(y, function(x) Rooderkerk_Prediction_Drop(x, mnl_rdk, attributeData, TRUE)), mc.cores = 8))
    
#### with CrossPrice ######
    system.time(mnl_rdk_cp <- mclapply(allFunc, function(y) lapply(y, function(x) Rooderkerk_Calculation(x, TRUE, TRUE)), mc.cores = 8))
    system.time(pred_rdk_cp <- mclapply(allFunc, function(y) lapply(y, function(x) Rooderkerk_Prediction(x, mnl_rdk_cp, TRUE, TRUE)), mc.cores = 8))
    mape_rdk_cp <- mclapply(pred_rdk_cp, function(y) lapply(y, function(x) getMAPE(x)), mc.cores = 8)
    plot_rdk_cp <- getMapeDensity(mape_rdk_cp, TRUE)
    system.time(pred_drop_rdk_cp <- mclapply(allFunc2, function(y) lapply(y, function(x) Rooderkerk_Prediction_Drop(x, mnl_rdk_cp, attributeData, TRUE, TRUE)), mc.cores = 8))
    
    
    

    

###################################### All NON-SEGMENTED MODELS #############################
posAttributeDataWithSimilarity <- mclapply(posAttributeDataWithSimilarity, function(y) similarityCalculation(y, FALSE), mc.cores = 8)
posAttributeDataWithSimilarity2 <- mclapply(posAttributeDataWithSimilarity, function(y) y %>% 
  group_by(subcat, fineline, brand) %>% 
  arrange(desc(wm_yr_wk)) %>% 
  slice(1) %>% 
  ungroup(), mc.cores = 8)



################## MNL withOUT similarity #############
    
  source('MNL_withoutSimilarity_Calculation.R')
  source('MNL_withoutSimilarity_Prediction.R')
  source('MNL_withoutSimilarity_Prediction_Drop.R')
    
  mnl_wos_unseg <- mclapply(posAttributeDataWithSimilarity, function(y) MNL_withoutSimilarity_Calculation(y), mc.cores = 8)
  pred_wos_unseg <- mclapply(posAttributeDataWithSimilarity, function(y) MNL_withoutSimilarity_Prediction(y, mnl_wos_unseg), mc.cores = 8)
  mape_wos_unseg <- mclapply(pred_wos_unseg, getMAPE, mc.cores = 8)
  plot_wos_unseg <- getMapeDensity(mape_wos_unseg)
  pred_drop_wos_unseg <- mclapply(posAttributeDataWithSimilarity2, function(y) MNL_withoutSimilarity_Prediction_Drop(y, mnl_wos_unseg, attributeData), mc.cores = 8)
  

################## MNL with similarity #############
  
  source('MNL_withSimilarity_Calculation.R')
  source('MNL_withSimilarity_Prediction.R')
  source('MNL_withSimilarity_Prediction_Drop.R')
  
#### without CrossPrice ######
    mnl_ws_unseg <- mclapply(posAttributeDataWithSimilarity, function(y) MNL_withSimilarity_Calculation(y), mc.cores = 8)
    pred_ws_unseg <- mclapply(posAttributeDataWithSimilarity, function(y) MNL_withSimilarity_Prediction(y, mnl_ws_unseg), mc.cores = 8)
    mape_ws_unseg <- mclapply(pred_ws_unseg, getMAPE, mc.cores = 8)
    plot_ws_unseg <- getMapeDensity(mape_ws_unseg)
    pred_drop_ws_unseg <- mclapply(posAttributeDataWithSimilarity2, function(y) MNL_withSimilarity_Prediction_Drop(y, mnl_ws_unseg, attributeData), mc.cores = 8)
  
#### with CrossPrice ######
    mnl_ws_cp_unseg <- mclapply(posAttributeDataWithSimilarity, function(y) MNL_withSimilarity_Calculation(y, crossPrice = TRUE), mc.cores = 8)
    pred_ws_cp_unseg <- mclapply(posAttributeDataWithSimilarity, function(y) MNL_withSimilarity_Prediction(y, mnl_ws_cp_unseg, crossPrice = TRUE), mc.cores = 8)
    mape_ws_cp_unseg <- mclapply(pred_ws_cp_unseg, getMAPE, mc.cores = 8)
    plot_ws_cp_unseg <- getMapeDensity(mape_ws_cp_unseg)
    pred_drop_ws_cp_unseg <- mclapply(posAttributeDataWithSimilarity2, function(y) MNL_withSimilarity_Prediction_Drop(y, mnl_ws_cp_unseg, attributeData, crossPrice = TRUE), mc.cores = 8)

    

################## Rooderkerk #############
    
  source('Rooderkerk_Calculation.R')
  source('Rooderkerk_Prediciton.R')
  source('Rooderkerk_Prediction_Drop.R')
    
#### without CrossPrice ######
    mnl_rdk_unseg <- mclapply(posAttributeDataWithSimilarity, function(y) Rooderkerk_Calculation(y), mc.cores = 8)
    pred_rdk_unseg <- mclapply(posAttributeDataWithSimilarity, function(y) Rooderkerk_Prediction(y, mnl_rdk_unseg), mc.cores = 8)
    mape_rdk_unseg <- mclapply(pred_rdk_unseg, getMAPE, mc.cores = 8)
    plot_rdk_unseg <- getMapeDensity(mape_rdk_unseg)
    pred_drop_rdk_unseg <- mclapply(posAttributeDataWithSimilarity2, function(y) Rooderkerk_Prediction_Drop(y, mnl_rdk_unseg, attributeData), mc.cores = 8)
    
#### with CrossPrice ######
    mnl_rdk_cp_unseg <- mclapply(posAttributeDataWithSimilarity, function(y) Rooderkerk_Calculation(y, crossPrice = TRUE), mc.cores = 8)
    pred_rdk_cp_unseg <- mclapply(posAttributeDataWithSimilarity, function(y) Rooderkerk_Prediction(y, mnl_rdk_cp_unseg, crossPrice = TRUE), mc.cores = 8)
    mape_rdk_cp_unseg <- mclapply(pred_rdk_cp_unseg, getMAPE, mc.cores = 8)
    plot_rdk_cp_unseg <- getMapeDensity(mape_rdk_cp_unseg)
    pred_drop_rdk_cp_unseg <- mclapply(posAttributeDataWithSimilarity2, function(y) Rooderkerk_Prediction_Drop(y, mnl_rdk_cp_unseg, attributeData, crossPrice = TRUE), mc.cores = 8)
    

    
###################################### End Of Process #############################}