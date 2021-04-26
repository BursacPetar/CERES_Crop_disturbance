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


my_theme <- function(base_size = 10, base_family = "sans"){
  theme_minimal(base_size = base_size, base_family = base_family) +
    theme(
      axis.text = element_text(size = 10),
      axis.text.x = element_text(angle = 0, vjust = 0.5, hjust = 0.5),
      axis.title = element_text(size = 12),
      panel.grid.major = element_line(color = "grey"),
      panel.grid.minor = element_blank(),
      panel.background = element_rect(fill = "#fffcfc"),
      strip.background = element_rect(fill = "#820000", color = "#820000", size =0.5),
      strip.text = element_text(face = "bold", size = 10, color = "white"),
      legend.position = "bottom",
      legend.justification = "center",
      legend.background = element_blank(),
      panel.border = element_rect(color = "grey30", fill = NA, size = 0.5)
    )
}

theme_set(my_theme())
mycolors=c("#f32440","#2185ef","#d421ef")

find_Rcenter <- function(rasLayer = rasLayer){
  x_cent <- (extent(rasLayer)[2] + extent(rasLayer)[1])/2
  y_cent <- (extent(rasLayer)[4] + extent(rasLayer)[3])/2
  df <- data_frame(x_cent = x_cent, y_cent = y_cent) %>% as.data.frame()
  return(df)
}

webmapS2parcel <- function(listS2parc = listS2parc, files.names = files.names, id = 1, v.rgb = TRUE, v.falsh = TRUE, v.ndvi = TRUE, parcels = parcels){ # listS2parcNDVI = listS2parcNDVI,
  
  centroid_coords <- find_Rcenter(rasLayer = listS2parc[[id]])
  sf_centroid <- st_as_sf(centroid_coords, coords = c("x_cent", "y_cent"), crs = 32634)
  #pol_contain <- st_contains(parcels, sf_centroid)
  #parcel <- parcels[unlist(pol_contain), ]
  parcel <- parcels[sf_centroid, ]
  
  if(v.rgb == TRUE){
    c.map <- mapview::viewRGB(x = listS2parc[[id]], 
                     r = 3, 
                     g = 2, 
                     b = 1,  
                     layer.name = files.names[id], 
                     query.type = "mousemove", 
                     map.types = "Esri.WorldImagery")
  }
  
  if(v.falsh == TRUE){
    f.map <- mapview::viewRGB(x = listS2parc[[id]], 
                     r = 7, 
                     g = 3, 
                     b = 2,  
                     layer.name = files.names[id], 
                     query.type = "mousemove", 
                     map.types = "Esri.WorldImagery")
  }
  
  if(v.ndvi == TRUE){
    ndvi <- (listS2parc[[id]][[7]] - listS2parc[[id]][[3]]) / (listS2parc[[id]][[7]] + listS2parc[[id]][[3]])
    n.map <- mapview::mapview(ndvi, 
                              na.color = NA, 
                              layer.name = "NDVI", 
                              query.type = "mousemove", 
                              map.types = "Esri.WorldImagery")
  }
  
  p.map <- mapview(parcel, color = "blue", col.regions = "transparent", alpha.regions = 0, lwd = 2) #%>% 
    #setView(centroid_coords$x_cent,centroid_coords$y_cent , zoom = 9)
  
  if(v.rgb == TRUE & v.falsh == FALSE){
    gridMap <- c.map + p.map
  } else if(v.rgb == FALSE & v.falsh == TRUE){
    gridMap <- f.map + p.map
  } else if(v.rgb == TRUE & v.falsh == TRUE & v.ndvi == FALSE){
    gridMap <- c.map + f.map + p.map
    # gridMap <- leafsync::sync(c.map, 
    #                           f.map,
    #                           ncol = 2)
  } else if(v.rgb == TRUE & v.falsh == TRUE & v.ndvi == TRUE) {
    # gridMap <- leafsync::sync(c.map, 
    #                           f.map,
    #                           n.map,
    #                           ncol = 2)
    gridMap <- c.map + f.map + n.map + p.map
  } else {
    message("v.rgb == FALSE & v.falsh == FALSE")
  }

  
  
  return(gridMap)
}


parcele <- st_read(dsn = "D:/R_projects/CERES_crop_disturbance/Podaci/Parcele.gpkg")


shinyServer(function(input, output, session){
  
  # ------------------------------------------------------------------------------------------------------
  # tabPanel DATA {files selection, dates selection, Geovisualisation and data analysis selection}
  # ------------------------------------------------------------------------------------------------------

  shinyFileChoose(input,'button1',roots=getVolumes())
  
  path1 <- reactive({
    parseFilePaths(getVolumes(), input$button1)
  })
  
  fnames <- reactive({
    names <- path1()$datapath %>% as.character()
    names
  })
  
  dfnames <- reactive({
    namesf <- fnames()
    namesf %<>% gsub("^[^0-9]+\\.|\\.[A-Za-z]+$", "", .) %>% sub('.*\\/', '', .) %>% as.data.frame()
    df <- data.frame(fn = namesf$. ) %>% tidyr::separate(fn, c("parcel_id", "date", "type"), "_") %>% 
      dplyr::mutate(date = as.Date(date))
    df
  })
  
  output$dates <- DT::renderDataTable({
    DT::datatable(
      dfnames() %>% 
        tidyr::separate(date, c("Year", "Month", "Day"), "-") %>%
        dplyr::select(parcel_id, Year, Month, Day),
      escape=F,
      extensions = list('Buttons'),
      options = list(dom = 'Bfrtip', buttons = I('colvis'),
                     deferRender = TRUE)
    )
  })
  
  selectedDates <- reactive({
    namesf <- fnames()
    selected <- input$dates_rows_selected
    namesSel <- namesf[selected]
    namesSel
  })
  
  output$filenames <- renderPrint({ 
    df <- selectedDates()
    df
  })
  
  # output$adates <- renderPrint({
  #   avdates <- dfnames()$date
  #   avdates
  # })
  
  #output$res <- renderPrint(input$mdates)
  
  #output$ido <- renderText(dfnames()$date)
  
  imgSelected <- reactive({
    sdates <- selectedDates()
    rasStack <- lapply(sdates, stack)
    rasStack
  })
  
  output$imgStack <- renderPrint({ 
    st <- imgSelected()
    st
  })

  # ------------------------------------------------------------------------------------------------------
  
  
  
  # ------------------------------------------------------------------------------------------------------
  # tabPanel LABELING
  # ------------------------------------------------------------------------------------------------------
  
  nchoices <- eventReactive(input$label, {
    inn <- input$dates_rows_selected
    vchoices <- seq(1, length(inn), by = 1)
    vchoices
  })
  
  output$selector <- renderUI({
    selectInput("nid",
                "ID number",
                choices = as.list(nchoices()),
                selected = 1)
  })
  
  observeEvent(input$previousButton, {
    current <- which(nchoices() == input$nid)
    if(current > 1){
      updateSelectInput(session, "nid",
                        choices = as.list(nchoices()),
                        selected = nchoices()[current - 1])
    }
  })
  
  observeEvent(input$nextButton, {
    current <- which(nchoices() == input$nid)
    if(current < length(nchoices())){
      updateSelectInput(session, "nid",
                        choices = as.list(nchoices()),
                        selected = nchoices()[current + 1])
    }
  })
  
  observeEvent(input$label, {
    output$geovis1 <- renderLeaflet({ #renderUI
      dfdates <- dfnames()
      selected <- input$dates_rows_selected
      namesSel <- dfdates$date[selected]
      vismap <- webmapS2parcel(listS2parc = imgSelected(), files.names = namesSel, id = as.numeric(input$nid), v.rgb = "v.rgb" %in% input$visSelect, v.falsh = "v.falsh" %in% input$visSelect, v.ndvi = "v.ndvi" %in% input$indSelect, parcels = parcele)
      vismap@map
    })
  })
  
  observeEvent(input$label, {
    output$geovis2 <- renderLeaflet({ #renderUI
      dfdates <- dfnames()
      selected <- input$dates_rows_selected
      namesSel <- dfdates$date[selected]
      vismap <- webmapS2parcel(listS2parc = imgSelected(), files.names = namesSel, id = as.numeric(input$nid)+1, v.rgb = "v.rgb" %in% input$visSelect, v.falsh = "v.falsh" %in% input$visSelect, v.ndvi = "v.ndvi" %in% input$indSelect, parcels = parcele)
      vismap@map
    })
  })
  
  observeEvent(input$label, {
    output$geovis3 <- renderLeaflet({ #renderUI
      dfdates <- dfnames()
      selected <- input$dates_rows_selected
      namesSel <- dfdates$date[selected]
      vismap <- webmapS2parcel(listS2parc = imgSelected(), files.names = namesSel, id = as.numeric(input$nid)+2, v.rgb = "v.rgb" %in% input$visSelect, v.falsh = "v.falsh" %in% input$visSelect, v.ndvi = "v.ndvi" %in% input$indSelect, parcels = parcele)
      vismap@map
    })
  })
  
  observeEvent(input$label, {
    output$geovis4 <- renderLeaflet({ #renderUI
      dfdates <- dfnames()
      selected <- input$dates_rows_selected
      namesSel <- dfdates$date[selected]
      vismap <- webmapS2parcel(listS2parc = imgSelected(), files.names = namesSel, id = as.numeric(input$nid)+3, v.rgb = "v.rgb" %in% input$visSelect, v.falsh = "v.falsh" %in% input$visSelect, v.ndvi = "v.ndvi" %in% input$indSelect, parcels = parcele)
      vismap@map
    })
  })
  
  
  # ------------------------------------------------------------------------------------------------------
  
  
})





