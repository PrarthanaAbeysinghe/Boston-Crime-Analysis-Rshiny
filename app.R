library(shiny)
library(shinydashboard)
library(ggplot2)
library(ggvis)
library("RColorBrewer")#colour
library(DT)
library(semantic.dashboard)

# Creating the dataframe
library(dplyr)

crime <- read.csv("D:\\python\\crime\\crimes-in-boston\\crime.csv")
grouped_data <- aggregate(list(COUNT=crime$INCIDENT_NUMBER), by=list(DISTRICT=crime$DISTRICT, YEAR=crime$YEAR), FUN=length);
library(tidyr)
final <- spread(grouped_data, DISTRICT, COUNT)

finaldf <- final[-1]
row.names(finaldf) <- final$YEAR
View(finaldf)

# Define UI
ui= dashboardPage (
  dashboardHeader(
    color = "blue",
    title = "Kaggle Boston Crime Incidents",
    titleWidth = 350,
    inverted = TRUE
  ),
  dashboardSidebar(
    size = "thin", color = "teal",
    sidebarMenu(
      menuItem(tabName = "main", "Graph", icon = icon("chart-bar")),
      menuItem(tabName = "extra", "Data Table", icon = icon("table"))
    )
  ),
  
  dashboardBody(
    # tags$head(
    #   tags$link(rel = "stylesheet", type = "text/css", href = "style.css")
    # ),
    
    tabItems(
      selected = 1,
      tabItem(
        tabName = "main",
        fluidRow(  
          column (5,
                  box (width=5,
                       color = "green", 
                       selectInput("DISTRICT", "DISTRICT:", 
                                   choices=colnames(finaldf)),
                       hr(),
                       helpText("Kaggle Boston Crime Incidents Data (2015-2018)."),
                  ),
          ), 
          
          column (9,
                  box(width = 11,
                      title = "Number of Crime Incidents by District",
                      color = "green", ribbon = TRUE, title_side = "top right",
                      
                      column(width = 16, height="auto",
                             plotOutput("crimeIncidentsPlot")
                      )
                  ),
          ),  
        )
      ),
      tabItem(
        tabName = "extra",
        fluidRow(
          box(title = "Data Table", color = "green", ribbon = TRUE, width = 15,status = "primary", height = "575",solidHeader = T, dataTableOutput("crimetable"))
          
          
        ),
      )
      
    ),
  ),theme = "cerulean"
)



# Define server logic for the Shiny app
server=shinyServer(function(input, output, session ) {
  
  
  # Fill in the spot we created for a plot
  output$crimeIncidentsPlot <- renderPlot({
    
    # Render a barplot
    barplot(finaldf[,input$DISTRICT], 
            main=paste("District:", input$DISTRICT),
            ylab="Number of crimes",
            xlab="Year",
            names.arg = c( 2015:2018),
            col=brewer.pal(n = 4, name = "PRGn"),
            border = "black"
    )
    
  })
  output$crimetable <- renderDT(datatable(crime, options = list(searching = TRUE, pageLength = 5,lengthMenu = c(5, 10, 15, 20), scrollX = T)))
})

# Run the app
shinyApp(ui=ui, server=server)