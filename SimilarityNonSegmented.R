source("calculateSimilarity.R")

posAttributeDataWithSimilarity <- posAttributeDataFinal %>%
  group_by(wm_yr_wk) %>%
  mutate(brand_sim = calc_discrete_similarity(brand)) %>%
  mutate(subcat_sim = calc_discrete_similarity(subcat)) %>%
  mutate(item_function_sim = calc_discrete_similarity(item_function)) %>%
  mutate(fineline_sim = calc_discrete_similarity(fineline)) %>%
  mutate(brand_price_sim = calc_cross_impact_with_discrete_similarity(lprice, brand)) %>%
  mutate(subcat_price_sim = calc_cross_impact_with_discrete_similarity(lprice, subcat)) %>%
  mutate(item_func_price_sim = calc_cross_impact_with_discrete_similarity(lprice, item_function)) %>%
  mutate(fineline_price_sim = calc_cross_impact_with_discrete_similarity(lprice, fineline)) %>%
  #        mutate(price_cat_sim = calc_discrete_similarity(price_cat)) %>%
  #         mutate(packtype_sim = calc_discrete_similarity(pack_type)) %>%
  #         mutate(rollcount_sim = calc_continuous_similarity(roll_count)) %>%
  #         mutate(rollequiv_sim = calc_continuous_similarity(roll_equivalency)) %>%
  ungroup()

marketSize <- posAttributeDataWithSimilarity %>%
  group_by(wm_yr_wk) %>%
  summarise(total_quantity = sum(quantity)) %>%
  extract2("total_quantity") %>%
  max() %>%
  multiply_by(1.015)

posAttributeDataWithSimilarity <- posAttributeDataWithSimilarity %>%
  mutate(market_size = marketSize) %>%
  group_by(wm_yr_wk) %>%
  mutate(outside = market_size - sum(quantity)) %>%
  ungroup() %>%
  mutate(lnsr = log(quantity / outside)) %>%
  #     mutate(ppr = price / roll_count) %>%
  #     mutate(ppre = price / roll_equivalency) %>%
  mutate(rollback = ifelse(type == 'rollback', 1, 0))

