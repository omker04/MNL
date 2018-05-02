MNL_withoutSimilarity_Prediction_Basic <- function(df_all, model, segmented = FALSE){
  try({
    similarityVariable <- c('')
    attributeVariable <- c(' + item_function')
    if(segmented)
      attributeVariable <- c('')
    
    dummy_mnl  <- dummyVars(paste0(" ~ rollback + subcat + brand + lprice", attributeVariable, similarityVariable), data = df_all, fullRank = T)
    getDummy <- data.frame(predict(dummy_mnl, newdata = df_all))
    getDummy$market_size <- df_all$market_size
    
    whichStore <- which(names(model) == unique(df_all$store_nbr))
    model <- model[[whichStore]]
    selected_mnl <- model
    if(segmented)
      selected_mnl <- model[[which(names(model) == unique(df_all$item_function))]]
    
    predict_mnl_wos <- function(df, mnl_model = selected_mnl){ 
      dfa <- df
      market_size <- dfa$market_size[1]
      qj_div_q0   <- mnl_model %>% predict(dfa) %>% exp()
      qss <- sum(qj_div_q0)
      q0 <- market_size / (1 + qss)
      qj <- qj_div_q0 * q0
      dfa %>% mutate(q = qj)
    }
    
    pred_no_drop_mnl <- getDummy %>% predict_mnl_wos()
    
    df_all$estimated_quantity <- pred_no_drop_mnl$q
    return(df_all)
  })
}

MNL_withoutSimilarity_Prediction <- function(df_all, model, segmented = FALSE){
  df_all <- split(df_all, df_all$wm_yr_wk)
  df_predict <- lapply(df_all, function(x) MNL_withoutSimilarity_Prediction_Basic(df_all = x, model = model, segmented = segmented))
  df_predict <- do.call('rbind', df_predict)
  return(df_predict)
}