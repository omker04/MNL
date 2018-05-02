MNL_withoutSimilarity_Calculation <- function(df_all, segmented = FALSE){
  
  attributeVariable <- attribute
  if(segmented)
    attributeVariable <- anti_join(data.frame('v1' = attribute), data.frame('v1' = segment_by))$v1 %>% as.character()
  
  
  dummy  <- dummyVars(paste0(" ~ rollback + lprice + ", paste(attributeVariable, collapse = ' + ')), data = df_all, fullRank = T)
  getDummy <- data.frame(predict(dummy, newdata = df_all))
  
  VIF_new <- function(df){
    n_col <- ncol(df)
    R <- sapply(1 : n_col, function(x) summary(lm(as.formula(paste(paste(colnames(df)[x], " ~ "),  paste(colnames(df)[-c(x)], collapse = "+") )), df))$r.squared)
    return(1/(1-R^2))
  }
  
  vif <- VIF_new(getDummy)
  var_to_remove <- sapply(vif, function(x) ifelse(x>10 || is.nan(x),1,0))
  var_to_remove <- colnames(getDummy)[(var_to_remove==1)]
  
  colname <- colnames(getDummy)[!colnames(getDummy) %in% var_to_remove]
  
  if(length(colname) == 0){
    getDummy_lnsr <- data.frame(lnsr = df_all$lnsr, getDummy)
    mnl_diagnos <- paste('lnsr ~ 0 +', paste(colnames(getDummy), collapse = '+'))
    var_to_remove <- colname
  }else{
    getDummy <- getDummy[,colname] %>% as.data.frame()
    colnames(getDummy) <- colname
    getDummy_lnsr <- data.frame(lnsr = df_all$lnsr, getDummy)
    mnl_diagnos <- paste("lnsr ~ 0 +", paste(colname, collapse = "+"))
  }
  print(mnl_diagnos)
  
  try({
    final_mnl <- lm(mnl_diagnos, getDummy_lnsr)
    final_mnl_summary <- summary(final_mnl)
    if(length(colname) == 0){
      return(final_mnl)
    }else{
      not_sig_var <- ifelse(coef(final_mnl_summary)[,4] > 0.05, rownames(coef(final_mnl_summary)), 0)
      if(length(not_sig_var[which(not_sig_var != 0)]) == length(colname) ){
        mnl_diagnos <- paste("lnsr ~ ", paste(colname, collapse = "+"))
      }else{
        mnl_diagnos <- paste("lnsr ~ 0 +", paste(colnames(getDummy)[!colnames(getDummy) %in% not_sig_var], collapse = "+"))
      }
      final_mnl <- lm(mnl_diagnos, getDummy_lnsr)
      print(summary(final_mnl))
      return(final_mnl)
    }
  })
}
