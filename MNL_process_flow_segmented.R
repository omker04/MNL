source('setupPkgs.R')

clean_varname <- . %>%                                          ########################################################### input-data clean_up function
  str_to_lower() %>% 
  str_replace_all(" ", "_")

clean_dfname <- . %>%                                           ########################################################### input-data clean_up function
  set_names(names(.) %>% clean_varname())

numCores <- 6

attributeData <- readxl::read_excel('Food Storage Attributes_modonly.xlsx')     ## ::  Input Attribute File, 1st column should be upc_nbr
selectedStoreData <- readRDS('testData_platform.rds') ## ::  Input POS info File

####### must be set as global variables  ###
attribute <<- c('Food Type', 'Configuration', 'Volume', 'Shape', 'Price Indicator', 'Brand', 'Piece Count', 'Material', 'Medium Level Community')                                                  ########################################################### not considering the column with upc_nbr
segment_by <<- 'Medium Level Community'
####### global variable assignment ends 

attribute %<>% 
  tolower() %>% 
  gsub(pattern = ' ', replacement = '_', .)

segment_by %<>% 
  tolower() %>% 
  gsub(pattern = ' ', replacement = '_', .)

selectedStoreData %<>% 
  clean_dfname()
attributeData %<>% 
  clean_dfname() %>% 
  mutate_each_(funs(factor), attribute)                         ########################################################### typecasting each attribute variable to factor
whichUPC <- which(colnames(attributeData) %like% 'upc')
colnames(attributeData)[whichUPC] <- 'upc_nbr'

source('dataPrep.R')                                            ########################################################### preparing data for further use
posAttributeDataFinal <- dataPrep(selectedStoreData, 
                                  attributeData, 
                                  attribute) 

posAttributeDataFinal %<>% 
  split(., posAttributeDataFinal$store_nbr)                     ########################################################### splitting data by store_nbr to get store_level models


###########################################################
############### SEGMENTED MNL MODEL #######################
###########################################################

source('getMarketSize.R')                                       ########################################################### adding market_size and outside_good columns
posAttributeDataWithSimilarity <- mclapply(posAttributeDataFinal, 
                                           function(x) 
                                             getMarketSize(x, segmented = TRUE), 
                                           mc.cores = numCores)
allFunc <- mclapply(posAttributeDataWithSimilarity, 
                    function(x) 
                      split(x, x[segment_by]),                  ########################################################### splitting each store data by the segment attribute
                    mc.cores = numCores)       

#on_hand_wks <- 52
allFunc3 <- mclapply(allFunc,                                   ########################################################### getting average Weekly POS info for post-deletion estimation
                     function(x) 
                       lapply(x, 
                              function(y) 
                                y %<>%
                                left_join(., store_upc_on_hand) %>%
                                select(-c(wm_yr_wk, type, rollback)) %>%
                                group_by_(.dots = lapply(c('upc_nbr', attribute), as.symbol)) %>%
                                mutate(dollar = sum(dollar)/on_hand_wks,
                                       quantity = sum(quantity)/on_hand_wks) %>% 
                                mutate_each(funs(mean), store_nbr, price, market_size) %>%
                                mutate(outside = median(outside)) %>%
                                mutate(lprice = log(price)) %>%
                                mutate(lnsr = log(quantity/outside)) %>%
                                #select(-on_hand_wks) %>% 
                                unique()
                       ), 
                     mc.cores = numCores)

source('MNL_withoutSimilarity_Calculation.R')                   ########################################################### generate the linear model for each store-segment
source('MNL_withoutSimilarity_Prediction_Multiple_Drop.R')      ########################################################### generate demand transference for each deletion

mnl_wos <- vector('list', length = length(allFunc))

mnl_wos <- mclapply(allFunc, 
                    function(x) 
                      lapply(x, 
                             function(y) 
                               try(
                                 MNL_withoutSimilarity_Calculation(y, segmented = TRUE)
                               )
                      ),
                    mc.cores = numCores
)








whichStore <-  98                    ############# required input
whichSegment <- 'Dawn Blue'                   ############# required input
deleteUPCs <- 3700011045                  ############# required input
addUPCs <- integer(0)          ############# optional input
added_price <- integer(0)      ############# optional input, required only if 'addUPCs' is provided

x <- which(names(allFunc3) == whichStore)                       ########################################################### store_nbr to stress upon
y <- which(names(allFunc3[[x]]) == whichSegment)                ########################################################### community to consider in the selected store

store_df <- allFunc3[[x]][[y]] %>%
  ungroup() %>%
  select(-c(outside, lnsr)) %>%
  mutate(add = NA)                                              ########################################################### getting data for the selected store and community

deletedIndex <- which(store_df$upc_nbr == deleteUPCs)           ########################################################### identifying indices of the deleted UPCs
add_df <- attributeData %>%
  select_(.dots = lapply(c('upc_nbr', attribute), as.symbol)) %>%
  filter(upc_nbr %in% addUPCs) %>%
  mutate(price = added_price, add = TRUE)                       ########################################################### creating a df with the added UPCs with their corresponding prices

new_assort_df <- full_join(store_df, add_df)                    ########################################################### outer-join to obtain attribute details of added UPCs
predictedSales <- MNL_withoutSimilarity_Prediction_Multiple_Drop(df_all = new_assort_df,
                                                                 model = mnl_wos,
                                                                 deleted = deletedIndex,
                                                                 segmented = TRUE)




