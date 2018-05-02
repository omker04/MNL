Rooderkerk_Prediction_Drop <- function(df_all, model, attributeData, segmented = FALSE, crossPrice = FALSE){
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
  
  tryCatch({
    if(length(df_all[[1]])==0){error("")}
    
    whichStore <- which(names(model) == unique(df_all$store_nbr))
    model <- model[[whichStore]]
    selected_rdk <- model
    if(segmented)
      selected_rdk <- model[[which(names(model) == unique(df_all$item_function))]]
    
    dummy  <- dummyVars(paste0(" ~ rollback + subcat + brand", attributeVariables, " + lprice + subcat_sim + brand_sim", similarityVariables, crossPriceVariables), data = df_all, fullRank = T)
    
    # if(similarity == "similarity"){
    #   dmy_rdk  <- dummyVars(" ~ rollback + subcat + brand + price", data = df_all, fullRank = T)
    # }else{dmy_rdk  <- dummyVars(" ~ rollback + subcat + brand + lprice", data = df_all, fullRank = T)}
    
    getDummy_rdk <- data.frame(predict(dummy, newdata = df_all))
    
    predict_rdk  <- function(df, all_gupc, model = selected_rdk){                   ### Function to get unit_quantity from model prediction
      dfa <- df %>% additionalSimilarity(., all_gupc)
      if(crossPrice)
        dfa <- df %>% additionalSimilarity_CrossPrice(., all_gupc)
      q <- model %>% predict(dfa) %>% exp()
      dfa %>% mutate(q = q)
    }
    
    predict_rdk_drop <- function(df, all_gupc, x, model = selected_rdk){                   ### Function to get unit_quantity from model prediction
      dfa <- df %>% additionalSimilarity_PostDrop(., all_gupc, x)
      if(crossPrice)
        dfa <- df %>% additionalSimilarity_CrossPrice_PostDrop(., all_gupc, x)
      q <- model %>% predict(dfa) %>% exp()
      dfa %>% mutate(q = q)
    }
    
    pred_no_drop_rdk <- getDummy_rdk %>% predict_rdk(., df_all)
    
    gupc_id <- df_all %>% nrow() %>% seq_len()
    
    q_rdk <- gupc_id %>%
      map(~ getDummy_rdk %>% slice(-.x) %>% predict_rdk_drop(. ,df_all, .x))
    
    sum_q <- function(dfl){
      sum_s <- dfl %>% map("q") %>% map_dbl(sum) %>% as.array()
    }
    
    select_max_q <- function(dfl){
      idx_max <- dfl %>% map("q") %>% map_dbl(sum) %>% which.max()
      dfl[[idx_max]]
    }
    
    a <- array()
    for (j in  1:length(df_all$type)){
      a[j] <- paste("scenario-",j)
    }
    
    model_results <- q_rdk
    
    each_scenario_sales_rdk <- unlist(model_results %>% sum_q())
    each_scenario_sales_rdk_2 <- cbind(each_scenario_sales_rdk,a) %>% as.data.frame()
    colnames(each_scenario_sales_rdk_2) <- c("Q","L_O_O")
    each_scenario_sales_rdk_2$no_drop_quantity <- sum(pred_no_drop_rdk$q)
    
    df_all_with_GUPC <- merge(df_all, attributeGUPC)
    
    final_res_rdk <- data.frame(Q = each_scenario_sales_rdk_2$Q,
                                no_drop_Q = each_scenario_sales_rdk_2$no_drop_quantity,
                                upc_nbr = df_all_with_GUPC$GUPC)
    
    each_scenario_quantity_rdk <- lapply(q_rdk, "[[", "q")
    
    complete_upcs_data_rdk <- gupc_id %>%
      map(~ df_all_with_GUPC %>% slice(-.x) %>%
            mutate(actual_sales_before_drop = sum(df_all$quantity)) %>%
            mutate(total_sales_before_drop = sum(pred_no_drop_rdk$q)) %>%
            mutate(total_sales_after_drop = final_res_rdk$Q[.x]) %>%
            mutate(estimated_quantity_post_drop = each_scenario_quantity_rdk[[.x]]) %>%
            mutate(estimated_quantity_pre_drop = pred_no_drop_rdk$q[-.x]) %>%
            mutate(GUPC_deleted = df_all_with_GUPC$GUPC[.x]))
    
    rbind_res_rdk <-do.call("rbind", complete_upcs_data_rdk)
    rbind_res_rdk$total_sales_after_drop %<>% as.character %>% as.numeric(.)
    return(list(rbind_res_rdk, final_res_rdk))
  },
  error = function(cond){}
  )
}

