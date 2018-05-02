# selectedStoreData :: the input data fetched from Teradata, column names in lower case
# attributeData :: the .csv input file, column names in lower case
# attribute :: the column names of the .csv file except the first column, in lower case.

dataPrep <- function(selectedStoreData, attributeData, attribute){
  clean_varname <- . %>% str_to_lower() %>% str_replace_all(" ", "_")
  clean_dfname <- . %>% set_names(names(.) %>% clean_varname())
  
  selectedStoreData %<>% clean_dfname()
  attributeData %<>% clean_dfname()
  attribute <- tolower(attribute)
  
  recode_info <- tibble(report_code = c(0, 7), type = c("regular", "rollback"))
  posAttributeData <- selectedStoreData %>% 
    filter(report_code %in% c(0, 7)) %>% 
    group_by(store_nbr, upc_nbr, wm_yr_wk, report_code) %>% 
    summarise(dollar = sum(wkly_sales),
              quantity = sum(wkly_qty),
              n_item = n_distinct(item_nbr)) %>% 
    ungroup()
  
  # posAttributeDataAllUPC <- posAttributeData %>% 
  #   left_join(recode_info, by = "report_code") %>%
  #   left_join(attributeData, by = c('upc_nbr' = 'upc_nbr')) %>% 
  #   filter(dollar > 0) %>% 
  #   filter(quantity > 0)
  
  posAttributeData %<>% 
    left_join(recode_info, by = "report_code") %>%
    inner_join(attributeData, by = c('upc_nbr' = 'upc_nbr')) %>% 
    filter(dollar >= 0) %>% 
    filter(quantity >= 0)
  
  posAttributeDataFinal <- posAttributeData %>% 
    select_(., .dots = lapply(c('store_nbr', 'upc_nbr', 'wm_yr_wk', 'dollar', 'quantity', 'type', attribute), as.symbol)) %>% 
    # group_by(store_nbr, wm_yr_wk, type, subcat, item_function, fineline, brand) %>% 
    group_by_(.dots = lapply(c('store_nbr', 'upc_nbr', 'wm_yr_wk', 'type', attribute), as.symbol)) %>% 
    summarise(
      dollar = sum(dollar),
      quantity = sum(quantity)
    ) %>%
    ungroup() %>%
    mutate(price = dollar / quantity) %>%
    #mutate_each_(funs(factor), c('type', attribute)) %>%
    #mutate_each_(funs(factor), type, subcat, item_function, fineline, brand) %>%
    mutate(lprice = log(price))
  
  return(posAttributeDataFinal)
}