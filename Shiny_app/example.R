library(shiny)
library(shinyFiles)

# Define UI for application that draws a histogram
ui <- fluidPage(
  
  # Application title
  titlePanel("Example"),
  
  # Sidebar with a slider input for number of bins 
  sidebarLayout(
    sidebarPanel(
      shinyFilesButton('button1',label='Files',title='Choose Files',
                       multiple=TRUE),
      shinyDirButton('button2',label='Directories',title='Choose Path')
      
    ),
    
    # Show a plot of the generated distribution
    mainPanel(
      textOutput("filenames")
    )
  )
)

# Define server logic required to draw a histogram
server <- function(input, output) {
  
  shinyFileChoose(input,'button1',roots=getVolumes())
  shinyDirChoose(input,"button2",roots=getVolumes())
  path1 <- reactive({
    parseFilePaths(getVolumes(), input$button1)
  })
  output$filenames <- renderPrint({ path1()$datapath[1] })
}

# Run the application 
shinyApp(ui = ui, server = server)