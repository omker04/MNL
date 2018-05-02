source('calculateSimilarity_PostDeletion.R')

MNL_withSimilarity_Prediction_Basic <- function(df_all, model, segmented = FALSE, crossPrice = FALSE){
  try({
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
    
    dummy_mnl  <- dummyVars(paste0(" ~ rollback + subcat + brand", attributeVariables, " + lprice + subcat_sim + brand_sim", similarityVariables, crossPriceVariables), data = df_all, fullRank = T)
    getDummy <- data.frame(predict(dummy_mnl, newdata = df_all))
    getDummy$market_size <- df_all$market_size
    
    whichStore <- which(names(model) == unique(df_all$store_nbr))
    model <- model[[whichStore]]
    selected_mnl <- model
    if(segmented)
      selected_mnl <- model[[which(names(model) == unique(df_all$item_function))]]
    
    predict_mnl <- function(df, all_gupc, mnl_model = selected_mnl){
      if(crossPrice){
        dfa <- df %>% additionalSimilarity_CrossPrice(., all_gupc)
      }else{
        dfa <- df %>% additionalSimilarity(., all_gupc)
      }
      
      market_size <- dfa$market_size[1]
      qj_div_q0   <- mnl_model %>% predict(dfa) %>% exp()
      qss <- sum(qj_div_q0)
      q0 <- market_size / (1 + qss)
      qj <- qj_div_q0 * q0
      dfa %>% mutate(q = qj)
    }
    
    pred_no_drop_mnl <- getDummy %>% predict_mnl(., df_all)
    
    df_all$estimated_quantity <- pred_no_drop_mnl$q
    return(df_all)
  })
}

MNL_withSimilarity_Prediction <- function(df_all, model, segmented = FALSE, crossPrice = FALSE){
  df_all <- split(df_all, df_all$wm_yr_wk)
  df_predict <- mclapply(df_all, function(x) MNL_withSimilarity_Prediction_Basic(df_all = x, model = model, segmented = segmented, crossPrice = crossPrice), mc.cores = 8)
  df_predict <- do.call('rbind', df_predict)
  return(df_predict)
}