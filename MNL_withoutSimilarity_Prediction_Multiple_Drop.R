MNL_withoutSimilarity_Prediction_Multiple_Drop <- function(df_all, model, deleted, segmented = FALSE, attributes = attribute, segment = segment_by){
  df_all %<>% as.data.frame()
  
  attributeVariable <- attributes
  if(segmented)
    attributeVariable <- anti_join(data.frame('v1' = attributes), data.frame('v1' = segment))$v1 %>% as.character()
  
  if(!'rollback' %in% colnames(df_all)){
    df_all %<>% mutate(rollback = 0)
  }else{
    if(sum(is.na(df_all$rollback)) > 0)
      df_all$rollback <- 0
  }
  
  if(!('lprice' %in% colnames(df_all)) || sum(is.na(df_all$lprice)) > 0)
    df_all$lprice = log(df_all$price)
  if(sum(is.na(df_all$market_size)) > 0)
    df_all$market_size[is.na(df_all$market_size)] <- unique(df_all$market_size)[!is.na(unique(df_all$market_size))]
  if(sum(is.na(df_all$store_nbr)) > 0)
    df_all$store_nbr[is.na(df_all$store_nbr)] <- unique(df_all$store_nbr)[!is.na(unique(df_all$store_nbr))]
  
  for(i in which(colnames(df_all) %in% attributes)){
    if(!is.factor(df_all[i]))
      df_all[i] <- as.factor(df_all[i] %>% as_vector())
  }
  
  
  try({
    whichStore <- which(names(model) == unique(df_all$store_nbr))
    model <- model[[whichStore]]
    selected_mnl <- model
    if(segmented){
      selected_mnl <- model[[which(names(model) == unique(df_all[,segment]))]]
      #selected_mnl <- model[[unique(df_all[segment]) %>% as.numeric()]]
    }
    
    predict_mnl <- function(df, mnl_model = selected_mnl){                ##### function to get unit_quantity from model predictions of mnl, basically from lnsr to unit_quantity
      dfa <- df 
      market_size <- dfa$market_size[1]
      qj_div_q0   <- mnl_model %>% predict(dfa) %>% exp()
      # na_output <- which(is.na(qj_div_q0))
      # qj_div_q0[na_output] <- sum(mnl_model$coefficients * dfa[na_output,], na.rm = TRUE)
      qss <- sum(qj_div_q0, na.rm = TRUE)
      q0 <- market_size / (1 + qss)
      qj <- qj_div_q0 * q0
      dfa %>% mutate(q = qj) %>% mutate(og = q0)
    }
    
    dummy  <- dummyVars(paste0(" ~ rollback + lprice + ", paste(attributeVariable, collapse = ' + ')), data = df_all, fullRank = T)
    getDummy <- data.frame(predict(dummy, newdata = df_all))
    getDummy$market_size <- df_all$market_size
    
    pred_no_drop_mnl <- getDummy %>% predict_mnl()
    df_all$predictedOutsideGood <- pred_no_drop_mnl$og
    df_all$predictedQuantity <- pred_no_drop_mnl$q
    NoDropDF <- df_all
    
    if(length(deleted) == 0 || deleted == 0 || sum(deleted > nrow(df_all)) != 0){
      stop('"deleted" argument is missing or is out of range')
    }
    if(length(deleted) != 0){
      #print(sum(deleted > nrow(df_all)))
      df_all <- df_all[-deleted,]
      dummy  <- dummyVars(paste0(" ~ rollback + lprice + ", paste(attributeVariable, collapse = ' + ')), data = df_all, fullRank = T)
      getDummy <- data.frame(predict(dummy, newdata = df_all))
      getDummy$market_size <- df_all$market_size
      
      pred_no_drop_mnl <- getDummy %>% predict_mnl()
      df_all$predictedQuantityPostDrop <- pred_no_drop_mnl$q
      
      NoDropDF %<>% left_join(., df_all)
      whichAdded <- which(is.na(NoDropDF$quantity))
      NoDropDF$quantity[whichAdded] <- NoDropDF$predictedQuantity[whichAdded]
      NoDropDF %<>% 
        mutate(adjustedPredictedQuantityPostDrop = quantity * predictedQuantityPostDrop / predictedQuantity)
      
      NoDropDF$walkoff <- (100 * (sum(NoDropDF$quantity) - sum(NoDropDF$adjustedPredictedQuantityPostDrop, na.rm = TRUE))/sum(NoDropDF$quantity)) %>% round(., 4) %>% paste(., '%')
      NoDropDF$adjustedPredictedQuantityPostDrop[whichAdded] <- NoDropDF$adjustedPredictedQuantityPostDrop[whichAdded] - NoDropDF$quantity[whichAdded]
      NoDropDF$quantity[whichAdded] <- NA
    }
    
    return(NoDropDF %>% 
             select_(.dots = lapply(c('store_nbr', 'upc_nbr', attributes, 'rollback', 
                               'dollar', 'quantity', 'price', 'lprice', 'market_size', 'add', 
                               'adjustedPredictedQuantityPostDrop', 'walkoff'), 
                               as.symbol)))
  })
}