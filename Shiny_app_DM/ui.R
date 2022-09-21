library(raster)
library(shiny)
library(shinydashboard)
library(leaflet)
library(rgdal)
library(tidyverse)
library(rgeos)
library(rsconnect)


ui <- dashboardPage(
  skin = "green",
  dashboardHeader(
    title = "Obelezavanje promena na poljoprivrednim parcelama na osnovu biofizickih parametara",
    titleWidth = "60%"
  ),
  dashboardSidebar(
    sidebarMenu(
      menuItem("Ulazni podaci", icon = icon("mouse"),
               fileInput("file1", "Ucitaj rastere",
                         multiple = TRUE,
                         accept = c("image/*"),
                         buttonLabel = "Ucitaj...",
                         placeholder = "Nema ucitanih rastera"
               ),
               fileInput("filemap", "Ucitaj poligone",
                         multiple = TRUE,
                         accept = c('.shp','.dbf','.sbn','.sbx','.shx',".prj"),
                         buttonLabel = "Ucitaj...",
                         placeholder = "Nema ucitanih poligona"
               )
      ),
      menuItem("Izlazni podaci", icon = icon("table"),
               textInput("outputFile", h6("Sacuvaj kao"),
                         value = "parcela"),
               downloadButton("downloadData", "Preuzmi")
      )
    )
  ),
  dashboardBody(
    fluidRow(
      tabBox(
        title = "Selektuj piksel", id = "panel1",
        tabPanel("NDVI",
                 uiOutput("all_rasters"),
                 plotOutput("raster_iz_liste", click = "raster_klik")
        )
      ),
      tabBox(
        title = "InfoPanel", id = "panel2",
        tabPanel("Histogram",
                 checkboxInput(inputId = 'opcija', label = 'Gustina', value = FALSE),
                 conditionalPanel(
                   condition = 'input.opcija == true',
                   plotOutput('gustinaPlot')
                 ),
                 conditionalPanel(
                   condition = 'input.opcija == false',
                   plotOutput('histo')
                 ),
                 sliderInput("bin", "Broj barova", min = 10, max = 100, value = 50)
        ),
        tabPanel("Svojstva rastera",
                 tableOutput("tablicaPx")
        ),
        tabPanel("Metapodaci",
                 HTML("<b>Broj ucitanih rastera:</b>"),
                 textOutput("brojLejera"),
                 HTML("<b>Rezolucija:</b>"),
                 textOutput("rezolucija"),
                 HTML("<b>Kartografska projekcija:</b>"),
                 textOutput("projekcija"),
                 h4("Selektovane koordinate"),
                 textOutput("ispisKoord")
        )
      ),
      fluidRow(
        box(
          title = "Mapa", status = "info", solidHeader = TRUE, width = 8, leafletOutput("map")
        ),
        box(
          title = "Labeliranje", status = "warning", solidHeader = TRUE, width = 4,
          actionButton(inputId = "labela_0", "+"),
          actionButton(inputId = "labela_1", "-"),
          actionButton(inputId = "previousImage", "Prethodno"),
          actionButton(inputId = "nextImage", "Sledece"),
          h4("Prosecna vrednost NDVI: "),
          h4(textOutput("average_ndvi"))
        )
      ),
      fluidRow(
        box(
          title = "Vremenska serija", status = "success", solidHeader = TRUE, width= 12,
          plotOutput("vremenskaSerija")
        )
      )
    )
  )
)
