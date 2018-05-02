# unzip('TeraJDBC__indep_indep.14.10.00.44.zip', overwrite = TRUE)
# unzip('teradataR.zip')

source('setupPkgs.R')
source('getDataFromTeradata.R')
clean_varname <- . %>% str_to_lower() %>% str_replace_all(" ", "_")
clean_dfname <- . %>% set_names(names(.) %>% clean_varname())

allStoreData <- read_rds('data_3store_2yr.rds') %>% 
  #selectedStore_2yr_data %>% 
  clean_dfname()
allStoreData <- arrange(allStoreData, store_nbr, wm_yr_wk)
allStoreData <- apply(allStoreData, 2, function(x) return(as.numeric(x))) %>% as.data.frame()

attributeData <- read_excel("AttributeData.xlsx", na = "n/a") %>%
  clean_dfname() %>%
  filter(!is.na(upc_nbr)) 

# weatherData <- read_excel("weather_variables.xlsx", na = "n/a") %>% clean_dfname()
# mergedData <- merge(allStoreData, weatherData, by = 'wm_yr_wk')

selectedStore <- 3135
selectedStoreData <- allStoreData %>% filter(store_nbr == selectedStore)

recode_info <- tibble(report_code = c(0, 7), type = c("regular", "rollback"))
posAttributeData <- selectedStoreData %>% 
  filter(report_code %in% c(0, 7)) %>% 
  group_by(upc_nbr, wm_yr_wk, report_code) %>% 
  summarise(dollar = sum(wkly_sales),
            quantity = sum(wkly_qty),
            n_item = n_distinct(item_nbr)) %>% 
  ungroup()

posAttributeDataAllUPC <- posAttributeData %>% 
  left_join(recode_info, by = "report_code") %>%
  left_join(attributeData, by = c('upc_nbr' = 'upc_nbr')) %>% 
  filter(dollar > 0) %>% 
  filter(quantity > 0)

posAttributeData <- posAttributeData %>% 
  left_join(recode_info, by = "report_code") %>%
  inner_join(attributeData, by = c('upc_nbr' = 'upc_nbr')) %>% 
  filter(dollar > 0) %>% 
  filter(quantity > 0)

posAttributeDataFinal <- posAttributeData %>% 
  select_(., .dots = list('upc_nbr', 'wm_yr_wk', 'dollar', 'quantity',
         'type', 'subcat', 'item_function', 'fineline', 'brand')) %>% 
  group_by(wm_yr_wk, type, subcat, item_function, fineline, brand) %>% 
  summarise(
    dollar = sum(dollar),
    quantity = sum(quantity)
  ) %>%
  ungroup() %>%
  mutate(price = dollar / quantity) %>%
  mutate_each(funs(factor), type, subcat, item_function, fineline, brand) %>%
  mutate(lprice = log(price))

