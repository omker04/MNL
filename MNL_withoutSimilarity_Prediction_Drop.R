MNL_withoutSimilarity_Prediction_Drop <- function(df_all, model, attributeData, segmented = FALSE){
  attributeGUPC <- attributeData %>% 
    #select(-upc_nbr) %>% 
    mutate(GUPC = apply(., 1, function(x) paste(x, collapse = " || "))) %>% 
    unique()
  
  similarityVariable <- c('')
  attributeVariable <- c(' + item_function')
  
  if(segmented)
    attributeVariable <- c('')
  
  try({
    dummy  <- dummyVars(paste0(" ~ rollback + subcat + brand + lprice", attributeVariable, similarityVariable), data = df_all, fullRank = T)
    getDummy <- data.frame(predict(dummy, newdata = df_all))
    getDummy$market_size <- df_all$market_size
    
    whichStore <- which(names(model) == unique(df_all$store_nbr))
    model <- model[[whichStore]]
    selected_mnl <- model
    if(segmented)
      selected_mnl <- model[[which(names(model) == unique(df_all$item_function))]]
    
    predict_mnl <- function(df, mnl_model = selected_mnl){                ##### function to get unit_quantity from model predictions of mnl, basically from lnsr to unit_quantity
      dfa <- df 
      market_size <- dfa$market_size[1]
      qj_div_q0   <- mnl_model %>% predict(dfa) %>% exp()
      qss <- sum(qj_div_q0)
      q0 <- market_size / (1 + qss)
      qj <- qj_div_q0 * q0
      dfa %>% mutate(q = qj)
    }
    
    pred_no_drop_mnl <- getDummy %>% predict_mnl()
    gupc_id <- df_all %>% nrow() %>% seq_len()
    
    q_mnl <- gupc_id %>%
      map(~ getDummy %>% slice(-.x) %>% predict_mnl())
    
    sum_q <- function(dfl){
      sum_s <- dfl %>% map("q") %>% map_dbl(sum) %>% as.array()
    }
    
    select_max_q <- function(dfl){
      idx_max <- dfl %>% map("q") %>% map_dbl(sum) %>% which.max()
      dfl[[idx_max]]
    }
    
    a <- array()
    for (j in  1:nrow(getDummy)){
      a[j] <- paste("scenario - ",j)
    }
    
    model_results <- q_mnl
    
    each_scenario_sales_mnl <- unlist(model_results %>% sum_q())
    each_scenario_sales_mnl_2 <- cbind(each_scenario_sales_mnl,a) %>% as.data.frame()
    colnames(each_scenario_sales_mnl_2) <- c("quantity","L_O_O")
    each_scenario_sales_mnl_2$no_drop_quantity <- sum(pred_no_drop_mnl$q)
    
    df_all_with_GUPC <- merge(df_all, attributeGUPC)
    
    final_res_mnl <- data.frame(Q = each_scenario_sales_mnl_2$quantity,
                                no_drop_Q = each_scenario_sales_mnl_2$no_drop_quantity,
                                upc_nbr = df_all_with_GUPC$GUPC)
    
    each_scenario_quantity <- lapply(q_mnl, "[[", "q")
    
    complete_upcs_data_mnl <- gupc_id %>%
      map(~ df_all_with_GUPC %>% slice(-.x) %>%
            mutate(actual_sales_before_drop = sum(df_all$quantity)) %>%
            mutate(total_sales_before_drop = sum(pred_no_drop_mnl$q)) %>%
            mutate(total_sales_after_drop = final_res_mnl$Q[.x]) %>%
            mutate(estimated_quantity_post_drop = each_scenario_quantity[[.x]]) %>%
            mutate(estimated_quantity_pre_drop = pred_no_drop_mnl$q[-.x]) %>%
            mutate(GUPC_deleted = df_all_with_GUPC$GUPC[.x]))
    
    rbind_res_mnl <- do.call("rbind", complete_upcs_data_mnl)
    return(list(rbind_res_mnl, final_res_mnl))
  })
}
