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
library(raster)
library(stars)
library(RStoolbox)
library(gridExtra)
library(viridis)

shinyUI(
  tagList(
    tags$script(HTML(
      "document.body.style.backgroundColor = 'sapphire';"
    )),
    tags$script(HTML(
      "document.body.style.fontFamily = 'Verdana';"
    )),
    tags$head(tags$style(HTML("table.dataTable.hover tbody tr:hover, table.dataTable.display tbody tr:hover {
                                  background-color: #9c4242 !important;
                                  }
                                  "))),
    tags$style(HTML(".dataTables_wrapper .dataTables_length, .dataTables_wrapper .dataTables_filter, .dataTables_wrapper .dataTables_info, .dataTables_wrapper .dataTables_processing,.dataTables_wrapper .dataTables_paginate .paginate_button, .dataTables_wrapper .dataTables_paginate .paginate_button.disabled {
            color: #ffffff !important; 
        }
        tbody {
        color: #000000;
        }
                    ")),
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
                        fluidRow(
                          column(width = 6,
                                 p(""),
                                 shinyFilesButton('button1', 
                                                  label='Select satellite images', 
                                                  title='Choose time series of satellite images per parcel: \n [Please take a look at file size!]', 
                                                  multiple=TRUE,
                                                  class = "btn-danger btn-block"),
                                 p(""),
                                 textInput(inputId = "parcelID", "Please set parcel ID: " , value = "1"),
                                 p(""),
                                 #verbatimTextOutput("adates"),
                                 #dateRangeInput("daterange1", "Date range:", start = "2015-07-15", end   = "2020-10-26"),
                                 p(""),
                                 #airDatepickerInput(inputId = "mdates", label = "Select multiple dates:", multiple = TRUE),#, view = 'days', startView = as.Date("2015-07-15"), highlightedDates = as.vector(textOutput("ido"))),
                                 #verbatimTextOutput("res"),
                                 
                                 p("Geovisualisation and data analysis"),
                                 p(""),
                                 checkboxGroupInput(inputId = "visSelect", 
                                                    label = "Types of geovisualisation: ",
                                                    choices = c("Color composite - RGB values" = "v.rgb",
                                                                "Falsh color composite - NirRG values" = "v.falsh"
                                                    )),
                                 p(""),
                                 checkboxGroupInput(inputId = "indSelect", 
                                                    label = "Types of indicies to calculate: ",
                                                    choices = c("NDVI index" = "v.ndvi",
                                                                "NDVI histogram" = "v.ndvihist",
                                                                "Productivity" = "v.prod",
                                                                "Chlorophile" = "v.chl"
                                                    )),
                                 p(""),
                                 checkboxGroupInput(inputId = "vecSelect", 
                                                    label = "Vector data: ",
                                                    choices = c("Labels" = "v.label",
                                                                "Parcel polygon" = "v.polygon",
                                                                "Draw polygon" = "v.draw"
                                                    )),
                                 
                                 
                                 verbatimTextOutput("filenames"),
                                 verbatimTextOutput("imgStack"),
                                 p(""),
                                 actionButton(inputId ='label', label='LABEL', class = "btn-danger btn-block")
                                 ),
                          column(width = 6,
                                 p(""),
                                 p("Select minimum 4 dates!"),
                                 DT::dataTableOutput("dates") %>% withSpinner(color="#0dc5c1")
                                 )
                        ),
               ),
               tabPanel("LABELING",
                    p(""),
                    fluidRow(
                      # Geovisualisation
                      uiOutput("selector"),
                      column(width = 6, 
                             leafletOutput("geovis1", height = 400) %>% withSpinner(color="#0dc5c1"), #uiOutput
                             leafletOutput("geovis3", height = 400) %>% withSpinner(color="#0dc5c1")                              
                             ),
                      column(width = 6,
                             leafletOutput("geovis2", height = 400) %>% withSpinner(color="#0dc5c1"),
                             leafletOutput("geovis4", height = 400) %>% withSpinner(color="#0dc5c1") 
                             )
                    ),
                    p(""),
                    fluidRow(
                      # Map edit - draw polygon
                      
                      
                      
                      
                    ),
                    p(""),
                    fluidRow(
                     column(width = 6,
                            # icons https://fontawesome.com/icons?d=gallery&p=2&q=next
                            actionButton(inputId ='previousButton', label='PREVIOUS', icon("backward"), class = "btn-warning btn-block")
                            ),
                     column(width = 6,
                            actionButton(inputId ='nextButton', label='NEXT', class = "btn-warning btn-block", icon("forward"))
                            )
                    )   
               )
               
               
               
    )
  )
)







































