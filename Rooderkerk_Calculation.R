Rooderkerk_Calculation <- function(df_all, segmented = FALSE, crossPrice = FALSE){
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
  getDummy <- getDummy[which(!unlist(lapply(getDummy, function(x) 0 == var(if (is.factor(x)) as.integer(x) else x))))]
  
  VIF_new <- function(df){
    n_col <- ncol(df)
    R <- sapply(1 : n_col, function(x) summary(lm(as.formula(paste(paste(colnames(df)[x], " ~ "),  paste(colnames(df)[-c(x)], collapse = "+") )), df))$r.squared)
    return(1/(1-R^2))
  }
  VIF_df <- data.frame(Variables = colnames(getDummy), VIF = VIF_new(getDummy))
  
  rem_flag <- sapply(VIF_df$VIF, function(x) ifelse(x>10 || is.nan(x),"remove","keep"))
  var_to_remove <- VIF_df$Variables[(rem_flag == "remove")] %>% as.character()
  
  colname <- colnames(getDummy)[!colnames(getDummy) %in% var_to_remove]
  # allCols <- colnames(getDummy)
  # getDummy <- getDummy[,colname] %>% as.data.frame()
  # colnames(getDummy) <- colname
  # getDummy_lnsr <- data.frame(lnsr = df_all$lnsr, getDummy)
  
  if(length(colname) == 0){
    getDummy_quantity <- data.frame(quantity = df_all$quantity, getDummy)
    rooderkerk_diagnos <- paste('log(quantity) ~ ', paste(colnames(getDummy), collapse = '+'))
    var_to_remove <- colname
  }else{
    getDummy <- getDummy[,colname] %>% as.data.frame()
    colnames(getDummy) <- colname
    getDummy_quantity <- data.frame(quantity = df_all$quantity, getDummy)
    rooderkerk_diagnos <- paste("log(quantity) ~ ", paste(colname, collapse = "+"))
  }
  print(rooderkerk_diagnos)
  try({
    final_rooderkerk <- lm(rooderkerk_diagnos, getDummy_quantity)
    final_rooderkerk_summary <- summary(final_rooderkerk)
    print(final_rooderkerk_summary)
    if(length(colname) == 0){
      return(final_rooderkerk)
    }else{
      not_sig_var <- ifelse(coef(final_rooderkerk_summary)[,4] > 0.05, rownames(coef(final_rooderkerk_summary)), 0)
      if(sum(colname %in% not_sig_var[not_sig_var!=0]) == length(colname)){
        return(final_rooderkerk)
      }else{
        allVars <- c('(Intercept)', colname)
        rooderkerk_diagnos <- paste("log(quantity) ~ 0 +", paste(colnames(getDummy)[!colnames(getDummy) %in% not_sig_var], collapse = "+"))
        final_rooderkerk  <- lm(rooderkerk_diagnos, getDummy_quantity)
        
        print(unique(df_all$item_function))
        print(final_rooderkerk %>% summary())
        return(final_rooderkerk)
      }
    }
  })
}