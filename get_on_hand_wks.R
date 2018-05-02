source('setupPkgs.R')
on_hand_data <- fread('FoodStorage_LineReview/on_hand_data.csv') %>% 
  as.data.frame() %>% 
  set_colnames(gsub(pattern = 'repl_sku_wkly_inv.', 
                    replacement = '', 
                    x = colnames(.)))

item_upc <- selectedStoreData %>% 
  select(item_nbr, upc_nbr) %>% 
  unique()

on_hand_data_edited <- on_hand_data %>% 
  mutate(wkly_on_hand = sun_on_hand_qty +
           mon_on_hand_qty +
           tue_on_hand_qty +
           wed_on_hand_qty +
           thu_on_hand_qty +
           fri_on_hand_qty +
           sat_on_hand_qty) %>% 
  mutate(wkly_on_hand_ind = if_else(wkly_on_hand > 0, 1, 0)) %>% 
  select(store_nbr, item_nbr, wm_yr_wk, wkly_on_hand_ind) %>% 
  left_join(item_upc, .) %>% 
  group_by(upc_nbr, store_nbr, wm_yr_wk) %>% 
  summarise(on_hand_ind = sum(wkly_on_hand_ind)) %>% 
  mutate(on_hand_ind = if_else(on_hand_ind > 0, 1, 0)) %>% 
  arrange(store_nbr, upc_nbr, wm_yr_wk) %>% 
  select(store_nbr, upc_nbr, everything())

store_upc_on_hand <- on_hand_data_edited %>% 
  group_by(store_nbr, upc_nbr) %>% 
  summarise(on_hand_wks = sum(on_hand_ind)) %>% 
  ungroup()
