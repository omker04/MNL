packages_required <- c('dplyr', 'magrittr', 'tibble', 'ggplot2', 'purrr', 'data.table',
                       'caret', 'parallel', 'textreg', 'readr', 'stringr',
                       'shiny', 'shinydashboard', 'shinyjs', 'shinyBS', 'DT', 'rhandsontable', 'RJDBC')

packages_installed <- row.names(installed.packages()) # already installed packages
packages_to_install <- setdiff(packages_required, packages_installed) # installation required

lapply(packages_to_install, function(pkg){install.packages(pkg, dependencies = TRUE)}) # installing
lapply(packages_required, function(pkg){library(pkg, character.only = TRUE)}) # loading in directory

rm(packages_required, packages_installed, packages_to_install)
