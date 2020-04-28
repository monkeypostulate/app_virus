library(shiny)
library(shinyjs)
library(shiny)
library(igraph)
library(tidyverse)
library(reshape2)
#library(cowplot)
library(plotly)

source('src/tab1.R')
source('src/tab2.R')
source('src/tab3.R')
source('src/tab4.R')
source('src/functions.R')



# Define UI for application that draws a histogram
fluidPage(
  includeCSS("www/StyleSheet1.css"),
  # Add google Analytics
  tags$head(includeScript("www/google-analytics.js")),
  # Add Javascript Functions
  tags$head(includeScript("www/shiny_functions.js")),
  
  # Use pachage shinyjs
  useShinyjs(),
  titlePanel('abelgabel: outbreak', windowTitle = 'abelgabel: outbreak'),
  # ##################################################
  # Navigation bar
  # ##################################################
  
  navbarPage(id = "navbar",
             p("Virus"), # Application title
           # #######################  Begin Tab 1
           tab1(),

             ########################## End Tab 1
tab2(),
tab3(),
tab4()

  )
)
