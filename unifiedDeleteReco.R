getUnifiedDeleteReco <- function(pred_drop, segmented = FALSE, numCores = 4){
  getStoreWiseDF <- function(x){
    if(nrow(x[[1]]) != 0){
      preSale <- x[[1]] %>% 
        select(GUPC, wm_yr_wk, price, estimated_quantity_pre_drop) %>% 
        unique() %>% 
        mutate(estimated_total_sale_pre_drop = estimated_quantity_pre_drop * price) %>% 
        select(estimated_total_sale_pre_drop) %>% 
        sum()
    }else{
      preSale <- 0
    }
    x[[1]] %<>% 
      mutate(pre_delete_sales = preSale) %>% 
      mutate(estimated_sales_pre_drop = estimated_quantity_pre_drop * price) %>% 
      mutate(estimated_sales_post_drop = estimated_quantity_post_drop * price) %>% 
      unique()
    
    postSale <- x[[1]] %>% 
      group_by(GUPC_deleted) %>% 
      summarise(post_delete_sales = sum(estimated_sales_post_drop)) %>% 
      as.data.frame()
    
    preSale <- x[[1]] %>% 
      select(GUPC, dollar, quantity, price) %>% 
      unique()
    
    x[[1]] %<>% 
      inner_join(., postSale) %>% 
      select(store_nbr, GUPC,
             total_sales_before_drop, total_sales_after_drop,
             estimated_quantity_pre_drop, estimated_quantity_post_drop,
             pre_delete_sales, post_delete_sales,
             estimated_sales_pre_drop, estimated_sales_post_drop,
             GUPC_deleted) %>% 
      unique()
    
    x[[1]]$total_sales_after_drop %<>% as.character() %>% as.numeric()
    
    x[[1]] %<>% 
      select(store_nbr, GUPC_deleted, total_sales_before_drop, total_sales_after_drop, pre_delete_sales, post_delete_sales) %>% 
      mutate(change_quantity = total_sales_before_drop - total_sales_after_drop) %>% 
      mutate(change_sales = pre_delete_sales - post_delete_sales) %>% 
      inner_join(., preSale, by = c('GUPC_deleted'='GUPC')) %>% 
      unique() %>% as.data.frame()
    return(x[[1]])
  }
  
  if(segmented){
    preDeleteSale <- mclapply(pred_drop, function(y) lapply(y, function(x){getStoreWiseDF(x)}), mc.cores = numCores)
    storeLevelReco <- preDeleteSale %>% 
      mclapply(., function(y) do.call('rbind', y), mc.cores = 4) # %>% 
      # do.call('rbind', .)
    
  }else{
    preDeleteSale <- lapply(pred_drop, function(x) getStoreWiseDF(x))
    storeLevelReco <- preDeleteSale # %>% do.call('rbind', .)
  }
  
  getReco <- function(storeLevelReco){
    Q_reco <- storeLevelReco %>% 
      group_by(GUPC_deleted) %>% 
      summarise(category_change_quantity = sum(change_quantity),
                pre_drop_quantity = sum(quantity),
                avg_price = mean(price),
                pos_store_count = n()) %>% 
      mutate(avg_quantity_change_per_store = category_change_quantity / pos_store_count) %>% 
      arrange(category_change_quantity) %>% 
      mutate(qunatity_rank = rank(category_change_quantity))
    
    D_reco <- storeLevelReco %>% 
      group_by(GUPC_deleted) %>% 
      summarise(category_change_dollar = sum(change_sales),
                pre_drop_dollar = sum(dollar),
                avg_price = mean(price),
                pos_store_count = n()) %>% 
      mutate(avg_dollar_change_per_store = category_change_dollar / pos_store_count) %>% 
      arrange(category_change_dollar) %>% 
      mutate(dollar_rank = rank(category_change_dollar))
    
    QD_reco <- inner_join(Q_reco, D_reco) %>% 
      #mutate(quantity_rank = rank(category_change_quantity)) %>% 
      #mutate(dollar_rank = rank(category_change_dollar)) %>% 
      arrange(GUPC_deleted) %>% 
      as.data.frame()
    
    return(list('quantity_wise' = Q_reco, 'dollar_wise' = D_reco, 'quantity_dollar_comp' = QD_reco))
  }
  
  storeReco <- mclapply(storeLevelReco, function(y) getReco(y), mc.cores = numCores)
  nationalReco <- storeLevelReco %>% 
    do.call('rbind', .) %>% 
    getReco(.)
  
  return(list('storeReco' = storeReco, 'overallReco' = nationalReco))
}