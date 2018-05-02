

delete_reco_C_wos <- getUnifiedDeleteReco(c(read_rds('pred_drop_wos_C_part1.rds'), read_rds('pred_drop_wos_C_part2.rds')), TRUE)
delete_reco_C_ws <- getUnifiedDeleteReco(c(read_rds('pred_drop_ws_C_part1.rds'), read_rds('pred_drop_ws_C_part2.rds')), TRUE)
delete_reco_D_wos <- getUnifiedDeleteReco(c(read_rds('pred_drop_wos_D_part1.rds'), read_rds('pred_drop_wos_D_part2.rds')), TRUE)
delete_reco_D_ws <- getUnifiedDeleteReco(c(read_rds('pred_drop_ws_D_part1.rds'), read_rds('pred_drop_ws_D_part2.rds')), TRUE)

C_ws <- delete_reco_C_ws[[3]] %>% select(GUPC_deleted, quantity_rank, dollar_rank)
colnames(C_ws)[-1] <- paste0('C_ws_', colnames(C_ws[-1]))
C_wos <- delete_reco_C_wos[[3]] %>% select(GUPC_deleted, quantity_rank, dollar_rank)
colnames(C_wos)[-1] <- paste0('C_wos_', colnames(C_wos[-1]))
D_ws <- delete_reco_D_ws[[3]] %>% select(GUPC_deleted, quantity_rank, dollar_rank)
colnames(D_ws)[-1] <- paste0('D_ws_', colnames(D_ws[-1]))
D_wos <- delete_reco_D_wos[[3]] %>% select(GUPC_deleted, quantity_rank, dollar_rank)
colnames(D_wos)[-1] <- paste0('D_wos_', colnames(D_wos[-1]))

rankMatrix <- C_ws %>% inner_join(., C_wos) %>% inner_join(., D_wos) %>% inner_join(., D_ws)
corMtx <- cor(rankMatrix[,-1])
View(corMtx)

CwsQ <- C_ws %>% arrange(C_ws_quantity_rank) %>% select(-C_ws_dollar_rank)
CwsD <- C_ws %>% arrange(C_ws_dollar_rank) %>% select(-C_ws_quantity_rank)
DwsQ <- D_ws %>% arrange(D_ws_quantity_rank) %>% select(-D_ws_dollar_rank)
DwsD <- D_ws %>% arrange(D_ws_dollar_rank) %>% select(-D_ws_quantity_rank)
CwosQ <- C_wos %>% arrange(C_wos_quantity_rank) %>% select(-C_wos_dollar_rank)
CwosD <- C_wos %>% arrange(C_wos_dollar_rank) %>% select(-C_wos_quantity_rank)
DwosQ <- D_wos %>% arrange(D_wos_quantity_rank) %>% select(-D_wos_dollar_rank)
DwosD <- D_wos %>% arrange(D_wos_dollar_rank) %>% select(-D_wos_quantity_rank)


rankMatrixTop10 <- CwsQ[1:10,] %>% full_join(., CwsD[1:10,]) %>% full_join(., DwsQ[1:10,]) %>% full_join(., DwsD[1:10,]) %>% 
  full_join(., CwosQ[1:10,]) %>% full_join(., CwosD[1:10,]) %>% full_join(., DwosQ[1:10,]) %>% full_join(., DwosD[1:10,])
corMtxTop10 <- cor(rankMatrixTop10[,-1], use = 'complete.obs')


write.xlsx(rbind(delete_reco_C_ws[[3]] %>% mutate(cluster = 'C'), delete_reco_D_ws[[3]] %>% mutate(cluster = 'D')), file = 'DeleteRecommendation.xlsx', sheetName = 'WithSimilarity', col.names = TRUE, row.names = FALSE)
write.xlsx(rbind(delete_reco_C_wos[[3]] %>% mutate(cluster = 'C'), delete_reco_D_wos[[3]] %>% mutate(cluster = 'D')), file = 'DeleteRecommendation.xlsx', sheetName = 'WithoutSimilarity', col.names = TRUE, row.names = FALSE, append = TRUE)
write.xlsx(corMtx, file = 'DeleteRecommendation.xlsx', sheetName = 'correlationMatrix', col.names = TRUE, row.names = FALSE, append = TRUE)
write.xlsx(corMtxTop10, file = 'DeleteRecommendation.xlsx', sheetName = 'correlationMatrixTop10', col.names = TRUE, row.names = FALSE, append = TRUE)
write.xlsx(rankMatrixTop10, file = 'DeleteRecommendation.xlsx', sheetName = 'rankMatrixTop10', col.names = TRUE, row.names = FALSE, append = TRUE)


pred_drop <- c(read_rds('pred_drop_wos_C_part1.rds'), read_rds('pred_drop_wos_C_part2.rds'))
