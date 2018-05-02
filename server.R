shinyServer(function(input, output, session) {
  #####
  # createAlert(session, 'warning', title = 'Warning!! You must have Teradata Read Access before trying to fetch data.', 
  #             style = 'danger', dismiss = TRUE)
  # 
  # data <- reactiveValues(selectedStore_2yr_data = NULL, attributeData = NULL, posAttributeDataFinal = NULL)
  # 
  # 
  # observeEvent(input$getData,{
  #   data$selectedStore_2yr_data <- NULL
  #   closeAlert(session, alertId = 'Alert1')
  #   startWeek <- input$startWeek
  #   storeNbr <- input$storeNbr
  #   #storeNbr <- allStores[1:input$storeNbr %>% as.numeric()]
  #   categoryNbr <- input$categoryNbr
  #   userID <- input$userID
  #   password <- input$password
  #   acctgDept <- input$acctgNbr
  #   
  #   withProgress(message = 'Data Fetching In Progress', value = 0.3, detail = 'Trying to connect to Teradata',{
  #     startTime <- Sys.time()
  #     GetTeradataData = function(sqlcode) {
  #       query = paste(sqlcode)
  #       drv = JDBC("com.teradata.jdbc.TeraDriver", "TeraJDBC__indep_indep.14.10.00.44/terajdbc4.jar:TeraJDBC__indep_indep.14.10.00.44/tdgssconfig.jar")
  #       myconn = dbConnect(drv,"jdbc:teradata://WMB/database=us_wm_vm, TMODE=ANSI,LOGMECH=LDAP", userID, password)
  #       incProgress(amount = 0.4, detail = 'Connection Established. Querrying Database')
  #       output = dbGetQuery(myconn,query)
  #       dbDisconnect(myconn)
  #       return(output)
  #     }
  #     query = paste0("select s.*, i.upc_nbr from (select store_nbr, item_nbr, wm_yr_wk, report_code, sell_price, wkly_qty, wkly_sales from sku_dly_pos where wm_yr_wk > ",
  #                    startWeek, " and store_nbr in (", paste(storeNbr, collapse = ', '),
  #                    ") and report_code in (0,7)) s join (select mds_fam_id, upc_nbr, item1_desc, acctg_dept_desc, dept_subcatg_desc, dept_category_desc, fineline_desc, cust_base_retail_amt, brand_name from ww_core_dim_vm.item_dim where current_ind = 'Y' and country_code = 'US' and dept_category_nbr = ",
  #                    categoryNbr,  " and acctg_dept_nbr = ", acctgDept, ") i on i.mds_fam_id = s.item_nbr;")
  #     # query = paste0("select s.*, i.upc_nbr from (select store_nbr, item_nbr, wm_yr_wk, report_code, sell_price, wkly_qty, wkly_sales from sku_dly_pos where wm_yr_wk > ",
  #     #                startWeek, #" and store_nbr in (", paste(storeNbr, collapse = ', '),
  #     #                " and report_code in (0,7)) s join (select mds_fam_id, upc_nbr, item1_desc, acctg_dept_desc, dept_subcatg_desc, dept_category_desc, fineline_desc, cust_base_retail_amt, brand_name from ww_core_dim_vm.item_dim where current_ind = 'Y' and country_code = 'US' and dept_category_nbr = ",
  #     #                categoryNbr,  " and acctg_dept_nbr = ", acctgDept, ") i on i.mds_fam_id = s.item_nbr;")
  #     # print(query)
  #     try({
  #       data$selectedStore_2yr_data <- GetTeradataData(query)
  #       data_2yr <<- data$selectedStore_2yr_data
  #       #write_rds(data_100Stores_2yr, 'data_250Stores_2yr_2.rds')
  #     })
  #     
  #     if(is.null(data$selectedStore_2yr_data)){
  #       #output$timeInfo <- renderValueBox(valueBox(value = 'Could not connect to Teradata ', subtitle = h2('The UserID, Password or Account is Invalid'), color = 'red'))
  #       createAlert(session, 'connection', alertId = 'Alert1', title = 'Could not connect to Teradata. The UserID, Password or Account is Invalid.', content = '', style = 'danger', dismiss = TRUE)
  #     }else{
  #       incProgress(amount = 0.2, detail = 'Collating Data into R dataframe')
  #       endTime <- Sys.time()
  #       print(c(endTime, startTime, difftime(endTime, startTime)))
  #       #output$timeInfo <- renderValueBox(valueBox(value = paste('Required Data Fetched from Teradata in ', difftime(endTime, startTime, units = 'secs') %>% round(., 0), ' seconds.'), subtitle = ''))
  #       createAlert(session, 'connection', alertId = 'Alert1', title = paste('Required Data Fetched from Teradata in ', difftime(endTime, startTime, units = 'secs') %>% round(., 0), ' seconds.'), content = '', style = 'success')
  #       show(id = 'externalData', anim = TRUE)
  #     }
  #   })
  # })
  # 
  # observeEvent(input$attributeData,{
  #   attributeFile <- input$attributeData
  #   if(!is.null(input$attributeData))
  #     data$attributeData <- data.frame(fread(attributeFile$datapath, na.strings = "n/a", integer64 = 'numeric')) %>% clean_dfname()
  #   
  #   if(!is.null(data$attributeData)){
  #     attributeAllColumns <- toupper(colnames(data$attributeData))
  #     upcCols <- c('UPC', 'UPC.NBR', 'UPC_NBR', 'UPCNBR', 'UPC NBR', 'ROLLUP.ID', 'ROLLUP_ID', 'ROLLUPID', 'ROLLUP ID')
  #     colnames(data$attributeData)[which(attributeAllColumns %in% upcCols)] <- 'upc_nbr'
  #     data$attributeData %<>% filter(!is.na(upc_nbr))
  #     attributeColumns <- attributeAllColumns[!attributeAllColumns %in% upcCols]
  #     updateSelectInput(session, inputId = 'attributes', choices = attributeColumns)
  #     show(id = 'attributes', anim = TRUE)
  #   }
  # })
  # 
  # observeEvent(input$model,{
  #   source('dataPrep.R')
  #   posData <<- data$selectedStore_2yr_data
  #   attrData <<- data$posAttributeDataFinal
  #   print(str(data$selectedStore_2yr_data))
  #   print(str(data$attributeData))
  #   data$posAttributeDataFinal <- dataPrep(selectedStoreData = data$selectedStore_2yr_data, 
  #                                          attributeData = data$attributeData, 
  #                                          attribute = input$attributes)
  # })
  #
  
  #####
  value <- reactiveValues(table = NULL, delete = NULL, add = data.frame(upc_nbr = numeric(0), price = numeric(0)), walkoff = NULL)
  
  observe({
  # observeEvent(input$newCat, {
  #   rm(list = ls())
  #   load(input$newCat$datapath, environment())
  #   value$env <<- TRUE
    #print(MNL_withoutSimilarity_Prediction_Multiple_Drop)
    #print(ls())
    
    value$table <- NULL
    value$delete <- NULL
    value$add <- data.frame(upc_nbr = numeric(0), price = numeric(0))
    updateCheckboxInput(session, 'add', value = FALSE)
    shinyjs::hide(id = 'add')
    shinyjs::hide(id = 'addition')
    
    output$fullUI <- renderUI({
    fluidPage(
      column(width = 3,
             selectizeInput('store', label = 'Please select a store :', choices = names(allFunc3), multiple = FALSE, options = NULL),
             selectInput('community', label = paste('Please select a level of', segment_by, ':'), choices = list(), multiple = FALSE)),
      column(width = 3, offset = 6, valueBoxOutput('walkoff', width = '100%')),
      column( width = 12,
              h3('Please select items to delete from the table below:'),
              dataTableOutput('preDeleteMetrics')),
      column(width = 12,
             h3('Quantity-wise plots'),
             plotOutput('transfer', height = '600px'),
             shinyjs::hidden(checkboxInput('add', label = h3('Would you like to add UPCs to the current assortment?'), value = FALSE, width = '100%'))),
      shinyjs::hidden(div(id = 'addition', fluidPage(
        column(width = 6, rHandsontableOutput('hot', width = '100%')),
        column(width = 6,
               h3('Please Enter the PRICE of the UPCs that are to be considered for ADDITION and click the checkbox next to it'))
        #actionButton('final', 'Add the selected UPCs to the store'))
      )))
    )
  })
  
  observeEvent(input$store,{
    #print(ls(environ$env))
    store <- which(names(allFunc3) == input$store)
    if(!is.data.frame(allFunc3[[store]]))
      updateSelectInput(session, 'community', choices = names(allFunc3[[store]]))
    value$table <- NULL
    value$delete <- NULL
    value$add <- data.frame(upc_nbr = numeric(0), price = numeric(0))
    updateCheckboxInput(session, 'add', value = FALSE)
    shinyjs::hide(id = 'add')
    shinyjs::hide(id = 'addition')
  })


  observeEvent(input$community,{
    value$table <- NULL
    value$delete <- NULL
    value$add <- data.frame(upc_nbr = numeric(0), price = numeric(0))
    updateCheckboxInput(session, 'add', value = FALSE)
    shinyjs::hide(id = 'add')
    shinyjs::hide(id = 'addition')
  })


  output$preDeleteMetrics <- renderDataTable({
    store <- which(names(allFunc3) == input$store)
    #print(store)
    if(is.data.frame(allFunc3[[store]])){
      disable(id = 'community')
      #print('disabled')
      table <- allFunc3[[store]] %>%
        mutate_each(funs(round(.,3)), dollar, quantity, price, outside, lprice, lnsr) %>%
        ungroup() %>%
        select(-c(outside, lnsr, lprice))
    }else{
      #print('normal')
      community <- which(names(allFunc3[[store]]) == input$community)
      table <- allFunc3[[store]][[community]] %>%
        mutate_each(funs(round(.,3)), dollar, quantity, price, outside, lprice, lnsr) %>%
        ungroup() %>%
        select(-c(outside, lnsr, lprice))
    }
    value$table <- table
    #print(head(value$table))
    return(value$table)
  }, rownames = FALSE, selection = 'multiple', options = list(scrollX = TRUE, sDom = '<"top">ft<"bottom">ip'))
  
  getHot <- reactive({
    d <- value$add
    if(input$add){
      shinyjs::show(id = 'addition')
      if(!is.null(input$hot)){
        d <- hot_to_r(input$hot)
      }
    }
    return(d)
  })
  
  deletePlot <- reactive({
    #if(input$add){
    shinyjs::show(id = 'add')
      value$add <- getHot()
      df_temp <<- value$add %>% filter(add == TRUE)
      df_temp$price <- as.numeric(df_temp$price)
      df_temp2 <<- value$table %>% mutate(add = FALSE)
      df <- full_join(df_temp2, df_temp)
    #}else{
      #df <- value$table %>% mutate(add = FALSE)
    #}
    store <- which(names(allFunc3) == input$store)
    if(is.data.frame(allFunc3[[store]])){
      outputDF <- try(MNL_withoutSimilarity_Prediction_Multiple_Drop(df, mnl_wos, value$delete, FALSE, attribute, segment_by))
    }else{
      print('normal')
      outputDF <- try(MNL_withoutSimilarity_Prediction_Multiple_Drop(df, mnl_wos, value$delete, TRUE, attribute, segment_by))
    }
    print(head(outputDF))
    value$walkoff <- outputDF$walkoff %>% unique()
    outputDF$diff <- outputDF$adjustedPredictedQuantityPostDrop - outputDF$quantity
    outputDF$diff[which(outputDF$add)] <- outputDF$adjustedPredictedQuantityPostDrop[which(outputDF$add)]
    whichDec <- which(sign(outputDF$diff) == 0)
    outputDF$deleted <- NA
    outputDF$deleted[value$delete] <- outputDF$quantity[value$delete]
    outputDF$deleted[whichDec] <- abs(outputDF$diff[whichDec])
    outputDF$diff[whichDec] <- NA

    SAVE <<- outputDF
    df1 <- outputDF %>% ungroup() %>% select(c(upc_nbr, diff)) %>% set_colnames(c('upc_nbr', 'quantity_sold')) %>% mutate(key = 'A')
    df2 <- outputDF %>% ungroup() %>% select(c(upc_nbr, quantity)) %>% set_colnames(c('upc_nbr', 'quantity_sold')) %>% mutate(key = 'B')
    df2$quantity_sold[value$delete] <- NA
    df3 <- outputDF %>% ungroup() %>% select(c(upc_nbr, deleted)) %>% set_colnames(c('upc_nbr', 'quantity_sold')) %>% mutate(key = 'C')
    df <- rbind(df1, df2, df3)
    SAVE2 <<- df
    df$upc_nbr <- factor(df$upc_nbr, rev(df$upc_nbr))
    ggplot(data = df, aes(x = upc_nbr, y = quantity_sold, fill = factor(key, levels = c('A', 'B', 'C')))) +
      geom_bar(stat = 'identity', show.legend = FALSE) +
      scale_fill_manual(values = c("mediumaquamarine", "steelblue4", "indianred1")) +
      coord_flip() +
      ggtitle('Post-Delete Sale Scenario')
  })

  observe({
    value$delete <- input$preDeleteMetrics_rows_selected
  })

  output$transfer <- renderPlot({
    if(is.null(value$delete)){
      df <- value$table %>% ungroup() %>% select(c(upc_nbr, quantity))
      df$upc_nbr <- factor(df$upc_nbr, rev(df$upc_nbr))
      plot1 <- ggplot(data = df, aes(x = upc_nbr, y = quantity)) +
        geom_bar(stat = 'identity', show.legend = FALSE) +
        coord_flip() +
        ggtitle('Pre-Delete Sale Scenario')
      return(plot1)
    }else{
      return(deletePlot())
    }
  })

  output$walkoff <- renderValueBox({
    if(is.null(value$delete)){
      return(NULL)
    }else{
      return(valueBox(value$walkoff, 'Walkoff', color = 'orange', width = 12))
    }
  })


  output$hot <- renderRHandsontable({
    if(input$add){
      store <- which(names(allFunc3) == input$store)
      if(is.data.frame(allFunc3[[store]])){
        leftUPCs <- anti_join(attributeData, value$table, by = 'upc_nbr') %>% mutate(price = '<>') %>% mutate(add = FALSE)
      }else{
        leftUPCs <- anti_join(attributeData %>% filter_(.dots = paste0(segment_by, ' == ', input$community)), value$table, by = 'upc_nbr') %>% mutate(price = '<>') %>% mutate(add = FALSE)
      }
      # if(nrow(leftUPCs) != 0){
        rhandsontable(leftUPCs, selectCallback = TRUE, readOnly = FALSE)
      # }else{
      #   return(rhandsontable(data.frame('Error' = 'All UPCs of this community are present in the store.')))
      # }
    }
  })
  
  })
  # observe({
  #   print(paste('its me', value$env))
  #   if(!is.null(value$env)){
  #     if(input$add){
  #       shinyjs::show(id = 'addition')
  #       if(!is.null(input$hot)){
  #         value$add <- hot_to_r(input$hot)
  #       }
  #     }
  #   }
  # })
  
})