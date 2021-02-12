library(shiny)
library(shinythemes)
library(tidyverse)
library(magrittr)
library(ggplot2)
library(sf)
library(ggmap)
library(sp)
library(rgdal)
library(leaflet)
library(readxl)
library(data.table)
library(plotly)
library(mapview)
library(mapedit)
library(shinycssloaders)
library(here)
library(matlib)
library(nngeo)
library(shinyWidgets)
library(dplyr)
library(DT)
library(leaflet.extras)
library(rhandsontable)
library(shinyBS)
library(kableExtra)
library(shinyWidgets)
library(shinyFiles)



shinyServer(function(input, output, session){

  shinyFileChoose(input,'button1',roots=getVolumes())
  path1 <- reactive({
    parseFilePaths(getVolumes(), input$button1)
  })
  output$filenames <- renderPrint({ path1()$datapath })

  
    
})






