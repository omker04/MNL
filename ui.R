# source('setupPkgs.R')
# library(shiny)
# library(shinydashboard)
# library(shinyjs)
# library(shinyBS)
# library(DT)
# library(rhandsontable)

packages_required <- c('dplyr', 'magrittr', 'tibble', 'ggplot2', 'purrr', 'data.table',
                       'caret', 'parallel', 'textreg', 'readr', 'stringr',
                       'shiny', 'shinydashboard', 'shinyjs', 'shinyBS', 'DT', 'rhandsontable')

# pack_lib_dir <- '/home/AzrRootAdminUser/rom_sma_R_packages/'
# packages_installed <- rownames(installed.packages(lib.loc = pack_lib_dir))
# packages_to_install <- setdiff(packages_required, packages_installed)
# lapply(packages_to_install,
#        function(pkg){install.packages(pkg,
#                                       dependencies = TRUE,
#                                       repos = "http://cran.wal-mart.com",
#                                       lib = pack_lib_dir)})
# lapply(packages_required,
#        function(pkg){library(pkg,
#                              character.only = TRUE,
#                              lib.loc = pack_lib_dir)}) # Server
# rm(packages_required, packages_installed, packages_to_install)
# 
lapply(packages_required,
       function(pkg){library(pkg,
                             character.only = TRUE)})

header <- dashboardHeader(title = strong('MNL Tool for Demand Transference'), titleWidth = '500px')
sidebar <- dashboardSidebar(disable = TRUE)

body <- dashboardBody(fluidPage(
  useShinyjs(),
  tags$style(type="text/css", ".shiny-output-error { visibility: hidden; }", ".shiny-output-error:before { visibility: hidden; }"),
  #####
  # tabBox(id = 'tabs', width = 12,
  #   tabPanel(title = 'Acquire Required Data', width = 12, fluidPage(
  #     box(title = 'Get Data From Teradata', status = 'primary', solidHeader = TRUE, width = 12,
  #         # valueBox(value = h3(strong('Warning!!')), subtitle = h2('You must have Teradata Read Access before trying to fetch data.'), color = 'orange', width = 12, icon = icon('warning')),
  #         bsAlert('warning'),
  #         valueBox(value = h4('Please Enter the following details so as to fetch the required data'), subtitle = '', color = 'aqua', width = 12),
  #         column(width = 6, 
  #                textInput('categoryNbr', label = 'Category Nbr', placeholder = 'Please enter a Category Nbr'),
  #                textInput('storeNbr', label = 'Store Nbr', placeholder = 'Please enter a Store Nbr'),
  #                textInput('userID', label = 'AD User ID', placeholder = 'Please enter your AD User-ID')
  #                ),
  #         column(width = 6, 
  #                textInput('startWeek', label = 'Start Week', placeholder = 'Please enter the start wm_yr_wk'),
  #                textInput('acctgNbr', label = 'Acctg Dept Nbr', placeholder = 'Please enter the acctg_dept_nbr', value = 11),
  #                passwordInput('password', label = 'LogIn Password', placeholder = 'Please enter your LogIn Password'),
  #                column(offset = 6, width = 6, actionButton('getData', label = strong('Fetch Required Data'), icon = icon('download'), width = '100%'))
  #                ),
  #         br(),
  #         br(),
  #         bsAlert('connection')
  #         #valueBoxOutput('timeInfo', width = 12)
  #         ),
  #     hidden(div(id = 'externalData', 
  #                box(title = 'Get External Attribute Data', status = 'primary', solidHeader = TRUE, width = 12,
  #                    
  #                    column(width = 6, fileInput('attributeData', label = 'Please upload the corresponding Attribute Data for the selected category (.csv or .txt)', accept = c("text/csv"), width = '100%')),
  #                    column(width = 6, hidden(selectInput('attributes', label = 'Select the attributes to use', choices = list(), multiple = TRUE, width = '100%'))),
  #                    column(offset = 9, width = 3, actionButton('model', label = strong('Start Modeling'), icon = icon('recycle'), width = '100%'))
  #                )
  #                ))
  #     ))
  # )
  #####
  ## fileInput('newCat', label = h3('Please upload the .RData file for the category under consideration:'), accept = '.RData', width = '100%'),
  uiOutput('fullUI')
  
  # fluidPage(
  #   column(width = 3,
  #          selectizeInput('store', label = 'Please select a store :', choices = names(allFunc3), multiple = FALSE, options = NULL),
  #          selectInput('community', label = 'Please select a community :', choices = list(), multiple = FALSE)),
  #   column(width = 3, offset = 6, valueBoxOutput('walkoff', width = '100%')),
  #   column( width = 12,
  #     h3('Please select items to delete from the table below:'),
  #     dataTableOutput('preDeleteMetrics')),
  #   column(width = 12,
  #          h3('Quantity-wise plots'),
  #          plotOutput('transfer', height = '600px'),
  #          shinyjs::hidden(checkboxInput('add', label = h3('Would you like to add UPCs to the current assortment?'), value = FALSE, width = '100%'))),
  #   shinyjs::hidden(div(id = 'addition', fluidPage(
  #     column(width = 6, rHandsontableOutput('hot', width = '100%')),
  #     column(width = 6,
  #            h3('Please Enter the PRICE of the UPCs that are to be considered for ADDITION and click the checkbox next to it'))
  #            #actionButton('final', 'Add the selected UPCs to the store'))
  #   )))
  # )
))

ui <- dashboardPage(header, sidebar, body, title = 'MNL Demand Transference', skin = 'yellow')