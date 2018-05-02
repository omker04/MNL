# source('setup_packages.R')
library(dplyr)
library(magrittr)
library(tidyr)
library(tibble)
options(tibble.width = Inf)

library(stringr)
library(lubridate)
library(ggplot2)
library(purrr)

library(readr)
library(readxl)

source('support.r')

# attribute data
df <- read_excel("Copy of Copy of Paper Towel Attributes.xlsx", na = "n/a") %>%
    clean_dfname() %>%
    filter(!is.na(upc)) %>%
    select(-`10_digit`)
write_rds(df, "attribute.rds")


# POS data
library(RODBC)
source("support_sql.r")
#wmb <- odbcConnect("WMB", uid = Sys.getenv("WMID"), pwd = Sys.getenv("WMPW"))

# library(RJDBC)
# library(teradataR)
# drv=JDBC("com.teradata.jdbc.TeraDriver","/Users/smishr2/Downloads/TeraJDBC__indep_indep.14.10.00.44/terajdbc4.jar:/Users/smishr2/Downloads/TeraJDBC__indep_indep.14.10.00.44/tdgssconfig.jar")
# conn = dbConnect(drv,"jdbc:teradata://WMZ/database=wm_ad_hoc,TMODE=ANSI,LOGMECH=LDAP","smishr2","hcm780M#8")
# dbGetQuery(conn,"select * from dbc.dbcinfo")

#library(RODBC)
#library(teradataR)
#tdConnect(dsn = "WMB", uid = "smishr2", pwd = .rs.askForPassword("Password?"), database = "US_WM_VM", dType =c("odbc","jdbc"))


# query <- paste0(
    # "SELECT ",
    # "POS.ITEM_NBR, ITEM.UPC_NBR, POS.WM_YR_WK, POS.REPORT_CODE, POS.WKLY_SALES, POS.WKLY_QTY, POS.STORE_NBR ",
    # "FROM ",
    # "(SELECT ITEM_NBR, WM_YR_WK, REPORT_CODE, WKLY_SALES, WKLY_QTY, STORE_NBR FROM US_WM_VM.SKU_DLY_POS WHERE WM_YR_WK BETWEEN 11425 AND 11629 AND STORE_NBR = 100) POS ",
    # "INNER JOIN",
    # "(SELECT ITEM_NBR, UPC_NBR FROM US_WM_VM.ITEM WHERE UPC_NBR IN", paren(df$upc), ") ITEM ",
    # "ON ",
    # "POS.ITEM_NBR = ITEM.ITEM_NBR ",
    # ";")
# query
#
# pos <- dbGetQuery(conn,query)
# #system.time(pos <- sqlQuery(wmb, query))

#time_window_strt<-c(11529,11533,11537,11542,11546,11551,11603,11608,11612)
#k=8 # value of K has to be changed from 1 to 9 to change the time-window and the code has to be run in order


file_pos <- "paper_towels_sales_dump.csv"
pos <- read.csv(file_pos, header= TRUE, as.is = TRUE)
# pos %>%
#     clean_dfname() %>% filter(wm_yr_wk > time_window_strt[k][1]) %>%
#     write_rds("pos.rds")


pos %>%
    clean_dfname()  %>%
    write_rds("pos.rds")

#odbcClose(wmb)


