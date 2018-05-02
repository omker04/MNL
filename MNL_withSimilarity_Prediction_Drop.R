source('calculateSimilarity_PostDeletion.R')

MNL_withSimilarity_Prediction_Drop <- function(df_all, model, attributeData, segmented = FALSE, crossPrice = FALSE){
  attributeGUPC <- attributeData %>% 
    select(-upc_nbr) %>% 
    mutate(GUPC = apply(., 1, function(x) paste(x, collapse = " || "))) %>% 
    unique()
  
  crossPriceVariables <- c('')
  attributeVariables <- c(' + item_function')
  similarityVariables <- c('')
  if(crossPrice){
    if(segmented){
      crossPriceVariables <- c(' + subcat_price_sim + brand_price_sim')
      similarityVariables <- c('')
    }else{
      crossPriceVariables <- c(' + subcat_price_sim + brand_price_sim + item_func_price_sim')
      similarityVariables <- c(' + item_function_sim')
    }
  }
  if(segmented)
    attributeVariables <- c('')
  
  try({
    dummy_mnl  <- dummyVars(paste0(" ~ rollback + subcat + brand", attributeVariables, " + lprice + subcat_sim + brand_sim", similarityVariables, crossPriceVariables), data = df_all, fullRank = T)
    getDummy <- data.frame(predict(dummy_mnl, newdata = df_all))
    getDummy$market_size <- df_all$market_size
    
    whichStore <- which(names(model) == unique(df_all$store_nbr))
    model <- model[[whichStore]]
    selected_mnl <- model
    if(segmented)
      selected_mnl <- model[[which(names(model) == unique(df_all$item_function))]]
    
    predict_mnl <- function(df, all_gupc, mnl_model = selected_mnl){                ##### function to get unit_quantity from model predictions of mnl, basically from lnsr to unit_quantity
      dfa <- df %>% additionalSimilarity(.,all_gupc)
      if(crossPrice)
        dfa <- df %>% additionalSimilarity_CrossPrice(.,all_gupc)
      market_size <- dfa$market_size[1]
      qj_div_q0   <- mnl_model %>% predict(dfa) %>% exp()
      qss <- sum(qj_div_q0)
      q0 <- market_size / (1 + qss)
      qj <- qj_div_q0 * q0
      #dfa %>% rename(p = price) %>% mutate(q = qj) %>% mutate(sales = p * q)
      dfa %>% mutate(q = qj)
    }
    
    predict_mnl_drop <- function(df, all_gupc, x, mnl_model = selected_mnl){                ##### function to get unit_quantity from model predictions of mnl, basically from lnsr to unit_quantity
      dfa <- df %>% additionalSimilarity_PostDrop(., all_gupc, x)
      if(crossPrice)
        dfa <- df %>% additionalSimilarity_CrossPrice_PostDrop(., all_gupc, x)
      market_size <- dfa$market_size[1]
      qj_div_q0   <- mnl_model %>% predict(dfa) %>% exp()
      qss <- sum(qj_div_q0)
      q0 <- market_size / (1 + qss)
      qj <- qj_div_q0 * q0
      #dfa %>% rename(p = price) %>% mutate(q = qj) %>% mutate(sales = p * q)
      dfa %>% mutate(q = qj)
    }
    
    pred_no_drop_ws <- getDummy %>% predict_mnl(., df_all)
    gupc_id <- df_all %>% nrow() %>% seq_len()
    
    q_mnl_ws <- gupc_id %>%
      map(~ getDummy %>% slice(-.x) %>% predict_mnl_drop(., df_all, .x))
    
    sum_q <- function(dfl){
      sum_s <- dfl %>% map("q") %>% map_dbl(sum) %>% as.array()
    }
    
    select_max_q <- function(dfl){
      idx_max <- dfl %>% map("q") %>% map_dbl(sum) %>% which.max()
      dfl[[idx_max]]
    }
    
    a<-array()
    for (j in  1:nrow(getDummy)){
      a[j] <- paste("scenario-",j)
    }
    
    model_results <- q_mnl_ws
    
    each_scenario_sales_ws <- unlist(model_results %>% sum_q())
    each_scenario_sales_ws_2 <- cbind(each_scenario_sales_ws,a) %>% as.data.frame()
    colnames(each_scenario_sales_ws_2) <- c("quantity","L_O_O")
    each_scenario_sales_ws_2$no_drop_quantity <- sum(pred_no_drop_ws$q)

    df_all_with_GUPC <- merge(df_all, attributeGUPC)
    
    final_res_ws <- data.frame(Q = each_scenario_sales_ws_2$quantity,
                                no_drop_Q = each_scenario_sales_ws_2$no_drop_quantity,
                                upc_nbr = df_all_with_GUPC$GUPC)
    
    each_scenario_sales_ws <- lapply(q_mnl_ws, "[[", "q")
    
    complete_upcs_data_mnl_ws <- gupc_id %>%
      map(~ df_all_with_GUPC %>% slice(-.x) %>%
            mutate(actual_sales_before_drop = sum(df_all$quantity)) %>%
            mutate(total_sales_before_drop = sum(pred_no_drop_ws$q)) %>%
            mutate(total_sales_after_drop = final_res_ws$Q[.x]) %>%
            mutate(estimated_quantity_post_drop = each_scenario_sales_ws[[.x]]) %>%
            mutate(estimated_quantity_pre_drop = pred_no_drop_ws$q[-.x]) %>%
            mutate(GUPC_deleted = df_all_with_GUPC$GUPC[.x]))
    
    rbind_res_mnl_ws <- do.call("rbind", complete_upcs_data_mnl_ws)
    return(list(rbind_res_mnl_ws, final_res_ws))
  })
}