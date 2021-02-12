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


shinyUI(
  tagList(
    tags$script(HTML(
      "document.body.style.backgroundColor = 'sapphire';"
    )),
    tags$script(HTML(
      "document.body.style.fontFamily = 'Verdana';"
    )),
    navbarPage(fluid = TRUE,
               div(img(src="CERES Logo.png", 
                       height = 30,
                       width = 40), "CERES|CropDist"),
               theme = shinytheme("slate"),
               tabPanel("MAIN",
                        fluidRow(
                          column(width = 6, div(img(src ="LOGO_CERES.png", height = 418, width = 569), style="text-align: center;")),
                          column(width = 6,
                                 h3("Project: CERES"),
                                 p("Description: Shiny app for the purpose of crop disturbance analysis from temporal Sentinel 2 data."),
                                 p("Authors: CERES team", href="https://ceres.rs/en/team/"),
                                 p("University of Belgrade, Faculty of Civil Engineering, Department of geodesy and geoinformatics")
                          )
                          
                        )
               ),
               tabPanel("DATA",
                        p(""),
                        shinyFilesButton('button1',label='Select satellite images', title='Choose time series of satellite images per parcel: ', multiple=TRUE),
                        p(""),
                        textInput(inputId = "parcelID", "Please set parcel ID: " , value = "1"),
                        p(""),
                        dateRangeInput("daterange1", "Date range:", start = "2015-07-15", end   = "2020-10-26"),
                        p("Geovisualisation"),
                        checkboxGroupInput(inputId = "visSelect", label = "Types of geovisualisation :",choices =  
                                             c("Color composite - RGB values" = "v.rgb",
                                               "Falsh color composite - NirRG values" = "v.falsh",
                                               "NDVI index" = "v.ndvi",
                                               "NDVI histogram" = "v.ndvihist",
                                               "Productivity" = "v.prod",
                                               "Chlorophile" = "v.chl",
                                               "Label" = "v.label",
                                               "Parcel polygon" = "v.polygon",
                                               "Draw polygon" = "v.draw")),
                        p(""),
                        verbatimTextOutput("filenames")
                        
               ),
               tabPanel("LABELING",
                        
                        
               )
               
               
               
    )
  )
)







































