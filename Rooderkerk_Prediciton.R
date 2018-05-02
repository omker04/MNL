source("calculateSimilarity_PostDeletion.R")

Rooderkerk_Prediction_Basic <- function(df_all, model, segmented = FALSE, crossPrice = FALSE){
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
    
    dummy  <- dummyVars(paste0(" ~ rollback + subcat + brand", attributeVariables, " + lprice + subcat_sim + brand_sim", similarityVariables, crossPriceVariables), data = df_all, fullRank = T)
    getDummy <- data.frame(predict(dummy, newdata = df_all))
    getDummy$market_size <- df_all$market_size
    
    whichStore <- which(names(model) == unique(df_all$store_nbr))
    model <- model[[whichStore]]
    selected_rdk <- model
    if(segmented)
      selected_rdk <- model[[which(names(model) == unique(df_all$item_function))]]
    
    predict_rdk <- function(df, all_gupc, rdk_model = selected_rdk){
      dfa <- df %>% additionalSimilarity(.,all_gupc)
      if(crossPrice)
        dfa <- df %>% additionalSimilarity_CrossPrice(.,all_gupc)
      market_size <- dfa$market_size[1]
      qj_div_q0   <- rdk_model %>% predict(dfa) %>% exp()
      dfa %>% mutate(q = qj_div_q0)
    }
    
    pred_no_drop <- getDummy %>% predict_rdk(., df_all)
    
    df_all$estimated_quantity <- pred_no_drop$q
    return(df_all)
  })
}

Rooderkerk_Prediction <- function(df_all, model, segmented = FALSE, crossPrice = FALSE){
  df_all <- split(df_all, df_all$wm_yr_wk)
  df_predict <- lapply(df_all, function(x) Rooderkerk_Prediction_Basic(df_all = x, model = model, segmented = segmented, crossPrice = crossPrice))
  df_predict <- do.call('rbind', df_predict)
  return(df_predict)
}