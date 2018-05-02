source('setupPkgs.R')

clean_varname <- . %>% str_to_lower() %>% str_replace_all(" ", "_")
clean_dfname <- . %>% set_names(names(.) %>% clean_varname())

attributeData <- read.csv("AttributeData.csv") %>%
  clean_dfname() %>%
  filter(!is.na(upc_nbr)) 

selectedStoreData <- allStoreData

source('dataPrep.R')
posAttributeDataFinal <- dataPrep(selectedStoreData, attributeData, attribute)


######## Data Partition Step - partitioned by store_nbr ########

posAttributeDataFinal %<>% split(., posAttributeDataFinal$store_nbr)

################################################################

#################################################
####### Modelling Starts ########################
#################################################

source('AttributeSimilarity.R')
source('errorDistribution.R')

# mclapply function is used to fork

system.time(posAttributeDataWithSimilarity <- mclapply(posAttributeDataFinal, function(x) similarityCalculation(x, TRUE), mc.cores = 20))
allFunc <- mclapply(posAttributeDataWithSimilarity, function(x) split(x, x$item_function), mc.cores = 20)
allFunc2 <- mclapply(allFunc, function(x) lapply(x, 
                                                 function(y) y %>% 
                                                   group_by(subcat, fineline, brand) %>% 
                                                   arrange(desc(wm_yr_wk)) %>% 
                                                   slice(1) %>% 
                                                   ungroup()),
                     mc.cores = 20)


source('MNL_withoutSimilarity_Calculation.R')
source('MNL_withoutSimilarity_Prediction.R')
source('MNL_withoutSimilarity_Prediction_Drop.R')

mnl_wos <- mclapply(allFunc, function(y) lapply(y, function(x) MNL_withoutSimilarity_Calculation(x, TRUE)), mc.cores = 20)
pred_wos <- mclapply(allFunc, function(y) lapply(y, function(x) MNL_withoutSimilarity_Prediction(x, mnl_wos, TRUE)), mc.cores = 20)
mape_wos <- mclapply(pred_wos, function(y) lapply(y, function(x) getMAPE(x)), mc.cores = 20)
plot_wos <- getMapeDensity(mape_wos, TRUE)
pred_drop_wos <- mclapply(allFunc2, function(y) lapply(y, function(x) MNL_withoutSimilarity_Prediction_Drop(x, mnl_wos, attributeData, TRUE)), mc.cores = 20)


