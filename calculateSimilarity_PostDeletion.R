source('calculateSimilarity.R')

additionalSimilarity <- function(df, all_gupc){
  df %>%
    mutate(brand_sim = calc_discrete_similarity(all_gupc$brand)) %>%
    mutate(item_function_sim = calc_discrete_similarity(all_gupc$item_function)) %>%
    mutate(subcat_sim = calc_discrete_similarity(all_gupc$subcat)) %>%
    mutate(fineline_sim = calc_discrete_similarity(all_gupc$fineline))
    #mutate(price_sim = calc_continuous_similarity(all_gupc$price))
}

additionalSimilarity_PostDrop <- function(df, all_gupc, x){
  df %>%
    mutate(brand_sim = calc_discrete_similarity(all_gupc$brand[-x])) %>%
    mutate(item_function_sim = calc_discrete_similarity(all_gupc$item_function[-x])) %>%
    mutate(subcat_sim = calc_discrete_similarity(all_gupc$subcat[-x])) %>%
    mutate(fineline_sim = calc_discrete_similarity(all_gupc$fineline[-x]))
    #mutate(price_sim = calc_continuous_similarity(all_gupc$price[-x]))
}

additionalSimilarity_CrossPrice <- function(df, all_gupc){
  df %>%
    mutate(brand_sim = calc_discrete_similarity(all_gupc$brand)) %>%
    mutate(item_function_sim = calc_discrete_similarity(all_gupc$item_function)) %>%
    mutate(subcat_sim = calc_discrete_similarity(all_gupc$subcat)) %>%
    mutate(fineline_sim = calc_discrete_similarity(all_gupc$fineline)) %>% 
    #mutate(price_sim = calc_continuous_similarity(all_gupc$price)) %>%
    mutate(brand_price_sim = calc_cross_impact_with_discrete_similarity(all_gupc$lprice, all_gupc$brand)) %>%
    mutate(subcat_price_sim = calc_cross_impact_with_discrete_similarity(all_gupc$lprice, all_gupc$subcat)) %>%
    mutate(item_func_price_sim = calc_cross_impact_with_discrete_similarity(all_gupc$lprice, all_gupc$item_function)) %>%
    mutate(fineline_price_sim = calc_cross_impact_with_discrete_similarity(all_gupc$lprice, all_gupc$fineline))
}

additionalSimilarity_CrossPrice_PostDrop <- function(df, all_gupc, x){
  df %>%
    mutate(brand_sim = calc_discrete_similarity(all_gupc$brand[-x])) %>%
    mutate(item_function_sim = calc_discrete_similarity(all_gupc$item_function[-x])) %>%
    mutate(subcat_sim = calc_discrete_similarity(all_gupc$subcat[-x])) %>%
    mutate(fineline_sim = calc_discrete_similarity(all_gupc$fineline[-x])) %>% 
    #mutate(price_sim = calc_continuous_similarity(all_gupc$price[-x])) %>%
    mutate(brand_price_sim = calc_cross_impact_with_discrete_similarity(all_gupc$lprice[-x], all_gupc$brand[-x])) %>%
    mutate(subcat_price_sim = calc_cross_impact_with_discrete_similarity(all_gupc$lprice[-x], all_gupc$subcat[-x])) %>%
    mutate(item_func_price_sim = calc_cross_impact_with_discrete_similarity(all_gupc$lprice[-x], all_gupc$item_function[-x])) %>%
    mutate(fineline_price_sim = calc_cross_impact_with_discrete_similarity(all_gupc$lprice[-x], all_gupc$fineline[-x]))
}

