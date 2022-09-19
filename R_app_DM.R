library(raster)
library(shiny)
library(shinydashboard)
library(leaflet)
library(rgdal)
library(tidyverse)
library(rgeos)
library(rsconnect)



server <- function(input, output, session) {
  options(shiny.maxRequestSize=700*1024^2)
  map <- reactive({
    req(input$filemap)
    shpdf <- input$filemap
    tempdirname <- dirname(shpdf$datapath[1])
    # Rename files
    for (i in 1:nrow(shpdf)) {
      file.rename(
        shpdf$datapath[i],
        paste0(tempdirname, "/", shpdf$name[i])
      )
    }
    map <- readOGR(paste(tempdirname,
                         shpdf$name[grep(pattern = "*.shp$", shpdf$name)],
                         sep = "/"))
                         map <- spTransform(map, CRS("+proj=longlat +datum=WGS84"))
                         map
  })
  output$map <- renderLeaflet({
    if (is.null(map())) {
      return(NULL)
    }
    map <- map()
    centers <- data.frame(gCentroid(map, byid = TRUE))
    parcel_info <- as.data.frame(map)
    centers$region <- paste("Broj parcele: ",parcel_info$NUMBER,"(",parcel_info$AREA," ha )")
    l <- leaflet(map) %>%
      setView(view_extent@coords[1],view_extent@coords[2], 17) %>%
      addProviderTiles("Esri.WorldImagery") %>%
      #addTiles() %>%
      #setView(lat=10, lng=0, zoom=2) %>%
      addPolygons(data = map, color = "#444444", weight = 1, smoothFactor = 0.5,
                  opacity = 1.0, fillOpacity = 0.5,
                  fillColor = "green",
                  highlightOptions = highlightOptions(color = "white", weight = 2,
                                                      bringToFront = TRUE),
      ) %>%
      addLabelOnlyMarkers(data = centers,
                          lng = ~x, lat = ~y, label = ~region,
                          labelOptions = labelOptions(noHide = TRUE, direction = 'top', textOnly = TRUE,style = list(
                            "color" = "black",
                            "font-style" = "bold",
                            "font-size" = "20px",
                            "box-shadow" = "2px 2px rgba(0,0,0,0.25)",
                            "border-color" = "rgba(0,0,0,0.5)"
                          )))
  })
  observeEvent(input$file1,{
    chr = character()
    dates <- list()
    pth <- input$file1
    pth_sub <- pth$datapath[1]
    tmpdirname <- str_remove(pth_sub, "/0.tif")
    for(nr in 1:length(input$file1[, 1])){
      #str(input$file1)
      raster_name_tif <- input$file1[nr, 'name']
      chr[nr] <- input$file1[nr, 'datapath']
      raster_name <- str_remove(raster_name_tif, ".tif")
      dates[nr] <- raster_name
      tmpdirname <- gsub("\\\\", "/", tmpdirname)
      to_rename <- pth$datapath[nr]
      file.rename(
        to_rename,
        paste0(tmpdirname, "/", raster_name_tif)
      )
      datapath_raster <- paste(tmpdirname, "/", raster_name_tif, sep="", collapse=NULL)
      chr[nr] <- datapath_raster
    }
    lista_rastera <<- stack(chr)
    # Racunanje koordinata za leaflet mapu
    prostorni_obuhvat <- bbox(lista_rastera)
    x_sr <- (prostorni_obuhvat[1]+prostorni_obuhvat[3])/2
    y_sr <- (prostorni_obuhvat[2]+prostorni_obuhvat[4])/2
    # Kreiranje objekta SpatialPoints i dodeljivanje projekcije
    xy <- SpatialPoints(data.frame(x_sr ,y_sr))
    crs(xy) <- crs(lista_rastera)
    xy <- spTransform(xy, "+proj=longlat +ellps=WGS84 +datum=WGS84 +no_defs") #parametri projekcije
    view_extent <<- xy
    # Outputs
    output$raster_iz_liste <- renderPlot({
      plot(lista_rastera[[input$layer]], main = names(lista_rastera)[input$layer])
    })
    output$projekcija <- renderText(projection(lista_rastera))
    output$rezolucija <- renderText(res(lista_rastera))
    output$brojLejera <- renderText(nlayers(lista_rastera))
    nummax <- nlayers(lista_rastera)
    output$ispisKoord <- renderText(formatC(round(Selekcija_koordinata(),2), format = "f", digits = 2))
    Selekcija_koordinata <- reactive({
      req(input$raster_klik$x)
      c(input$raster_klik$x, input$raster_klik$y)
    })
    output$tablicaPx <- renderTable({
      svojstvaRastera <- data.frame("Layer" = names(lista_rastera), "Value" = unlist(value())[1,])
      colnames(svojstvaRastera) <- c("Naziv rasterskog fajla","Vrednost piksela")
      rownames(svojstvaRastera) <- 1:nlayers(lista_rastera)
      svojstvaRastera
    })
    value <- eventReactive(input$raster_klik$x,{
      raster::extract(lista_rastera,cellFromXY(lista_rastera, Selekcija_koordinata()))
    })
    output$histo <- renderPlot({
      hist(lista_rastera[[input$layer]], ylab = "Frekvencija", xlab = "NDVI", col = 'palegreen1', border = "black", lwd = 3, breaks = seq(0, 1, l = input$bin))
    })
    output$gustinaPlot <- renderPlot({
      plot(density(lista_rastera[[input$layer]]), main = names(lista_rastera)[input$layer], xlab = "NDVI", ylab ="Frekvencija")
    })
    output$vremenskaSerija <- renderPlot({
      req(input$raster_klik$x)
      plot(1:nlayers(lista_rastera), value(), type = "b", xlab="Vreme", ylab="Vrednost", xaxt='n', ann=FALSE)
      axis(1, at=1:nlayers(lista_rastera), labels=dates)
    })
    output$downloadData <- downloadHandler(
      filename = function() {
        paste(input$outputFile, ".csv", sep = "")
      },
      content = function(file) {
        write.csv(df_fn_label, file, row.names = FALSE)
      }
    )
    observeEvent(input$avg_ndvi,{
      layermeans <- cellStats(lista_rastera[[input$layer]], stat='mean', na.rm=TRUE)
      sredina <- mean(layermeans)
      output$average_ndvi <- renderText({toString(sredina)})
    })
    count <- 1
    df_fn_label <- data.frame(fn = character(), file_name = character(), label = numeric())
    observeEvent(input$labela_0,{
      extract_fn <- filename(lista_rastera[[input$layer]])
      f_name <- basename(extract_fn)
      df_fn_label <<- df_fn_label %>% add_row(fn = extract_fn, file_name = f_name, label = 0)
    })
    observeEvent(input$labela_1,{
      extract_fn <- filename(lista_rastera[[input$layer]])
      f_name <- basename(extract_fn)
      df_fn_label <<- df_fn_label %>% add_row(fn = extract_fn, file_name = f_name, label = 1)
    })
    output$all_rasters <- renderUI({
      sliderInput(inputId = "layer", "Vremenska linija", min = 1, max = nummax, value = 1, step = 1, width="100%", animate = TRUE)
    })
    observeEvent(input$previousImage,{
      previous_value = input$layer - 1
      num_of_layers <- nlayers(lista_rastera)
      if (previous_value == 0 ) {
        previous_value <- num_of_layers
      }
      output$raster_iz_liste <- renderPlot({
        updateSliderInput(session, inputId = "layer", value = previous_value)
        plot(lista_rastera[[input$layer]], main = names(lista_rastera)[input$layer])
      })
      layermeans <- cellStats(lista_rastera[[previous_value]], stat='mean', na.rm=TRUE)
      sredina <- mean(layermeans)
      sredina <- round(sredina, 2)
      output$average_ndvi <- renderText({toString(sredina)})
    })
    observeEvent(input$nextImage,{
      next_value = input$layer + 1
      num_of_layers <- nlayers(lista_rastera)
      if (next_value > num_of_layers ) {
        next_value <- 1
      }
      output$raster_iz_liste <- renderPlot({
        updateSliderInput(session, inputId = "layer", value = next_value)
        plot(lista_rastera[[input$layer]], main = names(lista_rastera)[input$layer])
      })
      layermeans <- cellStats(lista_rastera[[next_value]], stat='mean', na.rm=TRUE)
      sredina <- mean(layermeans)
      sredina <- round(sredina, 2)
      output$average_ndvi <- renderText({toString(sredina)})
      updateActionButton(session, inputId = "labela_0", "+")
    })
  })
}


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

shinyApp(ui = ui, server = server)