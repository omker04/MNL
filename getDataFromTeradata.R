source('setupPkgs.R')
clean_varname <- . %>% str_to_lower() %>% str_replace_all(" ", "_")
clean_dfname <- . %>% set_names(names(.) %>% clean_varname())

allStores <- data.frame(fread('allStores.txt'))
allStores <- allStores[-1,] %>% as.integer()

data_2138stores_2yr <- read_rds('data_2138stores_2yr.rds')
leftStores <- allStores[-which(allStores %in% unique(data_2138stores_2yr$STORE_NBR))]

startWeek <- 11610
endWeek <- 11711
#storeNbr <- allStores[101:500]
#storeNbr <- 100
categoryNbr <- 5342
userID <- "smishr2"
password <- "hcm780M#4"
acctgDept <- 74

GetTeradataData = function(sqlcode) {
  query = paste(sqlcode)
  drv = JDBC("com.teradata.jdbc.TeraDriver", "TeraJDBC__indep_indep.14.10.00.44/terajdbc4.jar:TeraJDBC__indep_indep.14.10.00.44/tdgssconfig.jar")
  myconn = try({dbConnect(drv,"jdbc:teradata://WMB/database=us_wm_vm, TMODE=ANSI,LOGMECH=LDAP", userID, password)})
  output = dbGetQuery(myconn,query)
  dbDisconnect(myconn)
  return(output)
}

# query1 = paste0("select mds_fam_id, upc_nbr, item1_desc, acctg_dept_desc, dept_subcatg_desc, dept_category_desc, fineline_desc, cust_base_retail_amt, brand_name from ww_core_dim_vm.item_dim where current_ind = 'Y' and country_code = 'US' and dept_category_nbr = ",
#                categoryNbr,  " and acctg_dept_nbr = 11;")
# 
# query2 = paste0("select store_nbr, item_nbr, wm_yr_wk, report_code, sell_price, wkly_qty, wkly_sales from sku_dly_pos where wm_yr_wk > ",
#                 startWeek, " and item_nbr in (", paste(itemNbr, collapse = ', '), ") and store_nbr in (", paste(storeNbr, collapse = ', '),
#                 ") and report_code in (0,7);")

query = paste0("select s.*, i.upc_nbr from (select store_nbr, item_nbr, wm_yr_wk, report_code, sell_price, wkly_qty, wkly_sales from sku_dly_pos where wm_yr_wk > ",
               startWeek, " and wm_yr_wk < ", endWeek, " and store_nbr in (", paste(storeNbr, collapse = ', '),
               ") and report_code in (0,7)) s join (select mds_fam_id, upc_nbr, item1_desc, acctg_dept_desc, dept_subcatg_desc, dept_category_desc, fineline_desc, cust_base_retail_amt, brand_name from ww_core_dim_vm.item_dim where current_ind = 'Y' and country_code = 'US' and dept_category_nbr = ",
               categoryNbr,  " and acctg_dept_nbr = ", acctgDept, ") i on i.mds_fam_id = s.item_nbr;")

print(query)

df <- GetTeradataData(query)
write_rds(GetTeradataData(query), 'data_250Store_2yr_2.rds')

storeCluster <- read.csv('all_store_cluster_assn_2126s_201507e_201606.csv') %>% 
  filter(Sales_cluster %in% c('C', 'D'))

clusC <- storeCluster$store_nbr[which(storeCluster$Sales_cluster == 'C')]
clusD <- storeCluster$store_nbr[which(storeCluster$Sales_cluster == 'D')]

dataInHand <- read_rds('data_2138stores_2yr.rds')




for(i in 1:5){
  #storeNbr <- allStores[(100*(i-1) + 1):(100*i)]
  #storeNbr <- allStores[(100*(i-1) + 1):(100*i-50)]
  storeNbr <- (1000*(i-1) + 1):(1000*i)
  #print(storeNbr)
  query = paste0("select s.*, i.upc_nbr from (select store_nbr, item_nbr, wm_yr_wk, report_code, sell_price, wkly_qty, wkly_sales from sku_dly_pos where wm_yr_wk > ",
                 startWeek, " and wm_yr_wk < ", endWeek, " and store_nbr in (", paste(storeNbr, collapse = ', '),
                 ") and report_code in (0,7)) s join (select mds_fam_id, upc_nbr, item1_desc, acctg_dept_desc, dept_subcatg_desc, dept_category_desc, fineline_desc, cust_base_retail_amt, brand_name from ww_core_dim_vm.item_dim where current_ind = 'Y' and country_code = 'US' and dept_category_nbr = ",
                 categoryNbr,  " and acctg_dept_nbr = ", acctgDept, ") i on i.mds_fam_id = s.item_nbr;")
  data <- GetTeradataData(query)
  write_rds(data, paste0('FoodStorage_LineReview/Data_2_1000_Store_test',i,'.rds'))
  rm(data)
}



for(i in 17:20){
  storeNbr <- allStores[(100*(i-1) + 1):(100*i-50)]
  print(storeNbr)
  query = paste0("select s.*, i.upc_nbr from (select store_nbr, item_nbr, wm_yr_wk, report_code, sell_price, wkly_qty, wkly_sales from sku_dly_pos where wm_yr_wk > ",
                 startWeek, " and wm_yr_wk < ", endWeek, " and store_nbr in (", paste(storeNbr, collapse = ', '),
                 ") and report_code in (0,7)) s join (select mds_fam_id, upc_nbr, item1_desc, acctg_dept_desc, dept_subcatg_desc, dept_category_desc, fineline_desc, cust_base_retail_amt, brand_name from ww_core_dim_vm.item_dim where current_ind = 'Y' and country_code = 'US' and dept_category_nbr = ",
                 categoryNbr,  " and acctg_dept_nbr = ", acctgDept, ") i on i.mds_fam_id = s.item_nbr;")
  data <- GetTeradataData(query)
  write_rds(data, paste0('BathRoom Tissues/Data_50_Store_2yr_allStores',i,'_1.rds'))
  rm(data)
  storeNbr <- allStores[(100*(i-1) + 51):(100*i)]
  print(storeNbr)
  query = paste0("select s.*, i.upc_nbr from (select store_nbr, item_nbr, wm_yr_wk, report_code, sell_price, wkly_qty, wkly_sales from sku_dly_pos where wm_yr_wk > ",
                 startWeek, " and store_nbr in (", paste(storeNbr, collapse = ', '),
                 ") and report_code in (0,7)) s join (select mds_fam_id, upc_nbr, item1_desc, acctg_dept_desc, dept_subcatg_desc, dept_category_desc, fineline_desc, cust_base_retail_amt, brand_name from ww_core_dim_vm.item_dim where current_ind = 'Y' and country_code = 'US' and dept_category_nbr = ",
                 categoryNbr,  " and acctg_dept_nbr = ", acctgDept, ") i on i.mds_fam_id = s.item_nbr;")
  data <- GetTeradataData(query)
  write_rds(data, paste0('BathRoom Tissues/Data_50_Store_2yr_allStores',i,'_2.rds'))
  rm(data)
}
for(i in 21:30){
  storeNbr <- NP[(100*(i-1) + 1):(100*i)]
  print(storeNbr)
  query = paste0("select s.*, i.upc_nbr from (select store_nbr, item_nbr, wm_yr_wk, report_code, sell_price, wkly_qty, wkly_sales from sku_dly_pos where wm_yr_wk > ",
                 startWeek, " and wm_yr_wk < ", endWeek, " and store_nbr in (", paste(storeNbr, collapse = ', '),
                 ") and report_code in (0,7)) s join (select mds_fam_id, upc_nbr, item1_desc, acctg_dept_desc, dept_subcatg_desc, dept_category_desc, fineline_desc, cust_base_retail_amt, brand_name from ww_core_dim_vm.item_dim where current_ind = 'Y' and country_code = 'US' and dept_category_nbr = ",
                 categoryNbr,  " and acctg_dept_nbr = ", acctgDept, ") i on i.mds_fam_id = s.item_nbr;")
  data <- GetTeradataData(query)
  write_rds(data, paste0('BathRoom Tissues/Data_100_Store_2yr_',i+15,'.rds'))
  rm(data)
}

allStores_data <- data.frame()
for(i in 1:12)
  allStores_data %<>%  rbind(., read_rds(paste0('BathRoom Tissues/Data_100_Store_2yr_allStores',i,'.rds')) %>% clean_dfname())
