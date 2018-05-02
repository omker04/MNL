source('setupPkgs.R')

clean_varname <- . %>%                                          ########################################################### input-data clean_up function
  str_to_lower() %>% 
  str_replace_all("\\.", "_")

clean_dfname <- . %>%                                           ########################################################### input-data clean_up function
  set_names(names(.) %>% clean_varname())
numCores <- 4

attributeData     :: ##  Input Attribute File, 1st column should be upc_nbr
  selectedStoreData :: ##  Input POS info File
  
  ####### must be set as global variables  ###
  attribute <<-                                                   ########################################################### not considering the column with upc_nbr
  segment_by <<- 
  ####### global variable assignment ends 
  
  attribute %<>% tolower()
segment_by %<>% tolower()
selectedStoreData %<>% clean_dfname()
attributeData %<>% 
  clean_dfname() %>% 
  mutate_each_(funs(factor), attribute)                         ########################################################### typecasting each attribute variable to factor

source('dataPrep.R')                                            ########################################################### preparing data for further use
posAttributeDataFinal <- dataPrep(selectedStoreData, 
                                  attributeData, 
                                  attribute)

posAttributeDataFinal %<>% 
  split(., posAttributeDataFinal$store_nbr)                     ########################################################### splitting data bt store_nbr to get store_level models


###########################################################
############## UNSEGMENTED MNL MODEL ######################
###########################################################
source('getMarketSize.R')                                       ########################################################### adding market_size and outside_good columns
posAttributeDataWithSimilarity <- mclapply(posAttributeDataFinal, 
                                           function(x) 
                                             getMarketSize(x, segmented = FALSE), 
                                           mc.cores = numCores)

allFunc3 <- mclapply(posAttributeDataWithSimilarity,            ########################################################### getting average Weekly POS info for post-deletion estimation
                     function(y) y %<>% 
                       select(-c(wm_yr_wk, type, rollback)) %>% 
                       group_by_(.dots = lapply(c('upc_nbr', attribute), as.symbol)) %>% 
                       mutate_each(funs(mean), store_nbr, dollar, quantity, price, market_size) %>% 
                       mutate(outside = median(outside)) %>% 
                       mutate(lprice = log(price)) %>% 
                       mutate(lnsr = log(quantity/outside)) %>% 
                       unique(),
                     mc.cores = numCores)

source('MNL_withoutSimilarity_Calculation.R')                   ########################################################### generate the linear model for each store-segment
source('MNL_withoutSimilarity_Prediction_Multiple_Drop.R')      ########################################################### generate demand transference for each deletion

mnl_wos <- mclapply(posAttributeDataWithSimilarity, 
                    function(x) 
                      try(MNL_withoutSimilarity_Calculation(x, segmented = FALSE)), 
                    mc.cores = numCores)


attributeData %<>%  
  mutate_each_(funs(factor), attribute)                         ########################################################### typecasting each attribute variable to factor

whichStore       ############# required input
deleteUPCs       ############# required input
addUPCs          ############# optional input
added_price      ############# optional input, required only if 'addUPCs' is provided

x <- which(names(allFunc3) == whichStore)                       ########################################################### store_nbr to stress upon

store_df <- allFunc3[[x]] %>% 
  ungroup() %>% 
  select(-c(outside, lnsr)) %>% 
  mutate(add = NA)                                              ########################################################### getting data for the selected store and community

deletedIndex <- which(store_df$upc_nbr == deleteUPCs)           ########################################################### identifying indices of the deleted UPCs
add_df <- attributeData %>% 
  filter(upc_nbr %in% addUPCs) %>% 
  mutate(price = added_price, add = TRUE)                       ########################################################### creating a df with the added UPCs with their corresponding prices

new_assort_df <- full_join(store_df, add_df)                    ########################################################### outer-join to obtain attribute details of added UPCs
predictedSales <- MNL_withoutSimilarity_Prediction_Multiple_Drop(df_all = new_assort_df, 
                                                                 model = mnl_wos, 
                                                                 deleted = deletedIndex, 
                                                                 segmented = FALSE)
save.image()
