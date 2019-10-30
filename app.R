library(shiny)
library(shinydashboard)
library(ggplot2)
library(ggvis)
library("RColorBrewer")#colour
library(DT)#data table
library(ggmap)# to create map
#library(semantic.dashboard)

# Creating the dataframe
library(dplyr)

# read data set
crime <- read.csv("crime.csv")
grouped_data <- aggregate(list(COUNT=crime$INCIDENT_NUMBER), by=list(DISTRICT=crime$DISTRICT, YEAR=crime$YEAR), FUN=length);
library(tidyr)
final <- spread(grouped_data, DISTRICT, COUNT)

finaldf <- final[-1]
row.names(finaldf) <- final$YEAR

library(reshape2)
group_by_year <- spread(grouped_data, YEAR, COUNT)
melt_group_by_year <- melt(group_by_year, id.vars="DISTRICT")

# Create a variable count with value 1
crime$Count <- 1
#replace month colomn by month name
crime$MONTH <- ifelse(crime$MONTH == "1", "JANUARY",crime$MONTH)
crime$MONTH <- ifelse(crime$MONTH == "2", "FEBRUARY",crime$MONTH)
crime$MONTH <- ifelse(crime$MONTH == "3", "MARCH",crime$MONTH)
crime$MONTH <- ifelse(crime$MONTH == "4", "APRIL",crime$MONTH)
crime$MONTH <- ifelse(crime$MONTH == "5", "MAY",crime$MONTH)
crime$MONTH <- ifelse(crime$MONTH == "6", "JUNY",crime$MONTH)
crime$MONTH <- ifelse(crime$MONTH == "7", "JULY",crime$MONTH)
crime$MONTH <- ifelse(crime$MONTH == "8", "AUGUST",crime$MONTH)
crime$MONTH <- ifelse(crime$MONTH == "9", "SEPTEMBER",crime$MONTH)
crime$MONTH <- ifelse(crime$MONTH == "10", "OCTOBER",crime$MONTH)
crime$MONTH <- ifelse(crime$MONTH == "11", "NOVEMBER",crime$MONTH)
crime$MONTH <- ifelse(crime$MONTH == "12", "DECEMBER",crime$MONTH)


# Define UI
ui= dashboardPage ( 
 
  dashboardHeader( title="Boston Crime Data"
  ),
  dashboardSidebar(
    
    sidebarMenu(
      menuItem(tabName = "main", "Crime over District", icon = icon("chart-bar")),
      menuItem(tabName = "extra1", "Type of Crime vs Hour", icon = icon("chart-bar")),
      menuItem(tabName = "extra2", "Crime Evaluation", icon = icon("chart-bar")),
      menuItem(tabName = "extra3", "Crime over Time", icon = icon("chart-bar")),
      menuItem(tabName = "extra4", "Map", icon = icon("map")),
      menuItem(tabName = "extra5", "Data Table", icon = icon("table"))
    )
  ),
  
  dashboardBody(
  
    tabItems(
     
      tabItem(
        tabName = "main",
        fluidRow(  
          column (5,
                  box (width=12,
                       color = "green", 
                       selectInput("DISTRICT", "DISTRICT:", 
                                   choices=colnames(finaldf)),
                       hr(),
                       helpText("Kaggle Boston Crime Incidents Data (2015-2018)."),
                  ),
                  box(width = 12,
                      title = "Number of Crime Incidents by District",
                      color = "green", ribbon = TRUE, title_side = "top right",
                      
                      column(width = 12, height="auto",
                             plotOutput("crimeIncidentsPlot")
                     )
                  ),
          ), 
          column (7,
          box(width = 12,
                      title = "Summary by District",
                      color = "green", ribbon = TRUE, title_side = "top right",
                      
                      column(width = 12, height="auto",
                             plotOutput("crimeIncidentsGroupByYearLineGraph")
                      )
                  ),
          ),          
          
        
        ),
      ),

      tabItem(
        tabName = "extra1",
        fluidRow(
          box(width = 15,
                      title = "Type of Crime vs Hours",
                      color = "green", ribbon = TRUE, title_side = "top right",
                      
                      #column(width = 12, height="auto",
                      height = "1000",
                             plotOutput("plot1")
                      #)
                  ),
          
        ),
      ),

      tabItem(
        tabName = "extra2",
        fluidRow(
          box(width = 15,
                      title = "Crime vs Type of Crime over the Years",
                      color = "green", ribbon = TRUE, title_side = "top right",
                      
                      #column(width = 12, height="auto",
                      
                             plotOutput("plot2")
                      #)
                  ),
          
        ),
      ),

      tabItem(
        tabName = "extra3",
        fluidRow(
          column (6,
          box(width = 15,
                      title = "Summary by Year",
                      color = "green", ribbon = TRUE, title_side = "top right",
                      
                      column(width = 12, height="auto",
                             plotOutput("plot3")
                      )
                  ),
          box(width = 15,
                      title = "Summary by Day",
                      color = "green", ribbon = TRUE, title_side = "top right",
                      
                      column(width = 12, height="auto",
                             plotOutput("plot5")
                      )
                  ),
          ),
          column (6,
          box(width = 15,
                      title = "Summary by Month",
                      color = "green", ribbon = TRUE, title_side = "top right",
                      
                      column(width = 12, height="auto",
                             plotOutput("plot4")
                      )
                  ),
          box(width = 15,
                      title = "Summary by Hour",
                      color = "green", ribbon = TRUE, title_side = "top right",
                      
                      column(width = 12, height="auto",
                             plotOutput("plot6")
                      )
                  ),
          )

        ),
      ),
      
      tabItem(
        tabName = "extra4",
        fluidRow(
          box(width = 15,title = "Number of Crime Above 250",
                     
                      plotOutput("map")
          ),
                  
          ),
      ),

      tabItem(
        tabName = "extra5",
        fluidRow(
          box(title = "Data Table", color = "green", ribbon = TRUE, width = 12,status = "success", height = "575",solidHeader = T, dataTableOutput("crimetable"))
          
          
        ),
      )
    ),
  ),
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

 output$crimeIncidentsGroupByYearLineGraph <- renderPlot({
    
    # Render a line graph
        ggplot(melt_group_by_year , aes(x=DISTRICT, y=value,group=variable,
                         colour=variable)) +
        geom_line()
    
  })

output$plot1<- renderPlot({
  #Create aggregated object
        dd_aggr1 <- aggregate(Count ~ OFFENSE_CODE_GROUP + HOUR, data = crime, FUN = sum)
        # Plot graph
        ggplot(data = dd_aggr1, aes(x = HOUR, y = OFFENSE_CODE_GROUP)) + geom_tile(aes(fill = Count), color = "white") +
        scale_fill_gradient(low = "white", high = "steelblue") +
        theme_minimal()+ theme(axis.title.x=element_blank()) + theme(axis.title.y=element_blank()) +
        theme(legend.title = element_text(size = 10),
              legend.text = element_text(size = 6),
              axis.text.y = element_text(size= 6),
              axis.text.x = element_text(size = 10, angle = 45, hjust = 1))
         
})

output$plot2<- renderPlot({
        dd_aggr2 <- aggregate(Count~ OFFENSE_CODE_GROUP + YEAR, data = crime, FUN = sum)
        # Plot the graph
        ggplot(data=dd_aggr2, aes(x=YEAR, y=Count, group = OFFENSE_CODE_GROUP , colour = OFFENSE_CODE_GROUP)) +
        geom_line() + geom_point() + theme_minimal() + theme(axis.title.x=element_blank()) + theme(axis.title.y=element_blank())+ theme(legend.title=element_blank(),legend.position = "right",legend.text = element_text( size = 5),)
})
output$plot3<- renderPlot({
# Create aggregated object
        dd_aggr <- aggregate(Count ~ YEAR, data = crime, FUN = sum)
        # Plot the graph 
        ggplot(dd_aggr, aes(x=YEAR, y= Count)) + geom_line(colour = "steelblue") + geom_point(colour = "steelblue") + theme_minimal() + theme(axis.title.x=element_blank()) + theme(axis.title.y=element_blank())  +  labs(x = "Year", y = "Number of crimes")
})
output$plot4<- renderPlot({
              # Plot the graph 
           ggplot(crime, aes(x=MONTH),border = "black")+geom_bar(stat="Count", width=0.8, fill="steelblue")+ theme(axis.text.x = element_text(angle = 0, hjust = 1)) + labs(x = "Day", y = "Number of crimes") + theme_minimal()+ theme(axis.title.x=element_blank()) + theme(axis.title.y=element_blank()) +
            theme(axis.text.y = element_text(size= 6),axis.text.x = element_text(size = 10, angle = 90, hjust = 1))
          })

output$plot5<- renderPlot({
          # Plot the graph 
           ggplot(crime, aes(x=DAY_OF_WEEK))+geom_bar(stat="Count", width=0.8, fill=brewer.pal(n = 7, name = "PRGn"))+ theme(axis.text.x = element_text(angle = 0, hjust = 1)) + labs(x = "Day", y = "Number of crimes") + theme_minimal()+ theme(axis.title.x=element_blank()) + theme(axis.title.y=element_blank())
})
output$plot6<- renderPlot({
          # Plot the graph 
          ggplot(crime, aes(x=crime$HOUR))+geom_bar(stat="Count", width=0.8, fill="steelblue")+ theme(axis.text.x = element_text(angle = 0, hjust = 1)) +  labs(x = "Hours", y = "Number of crimes")
})
#create map
output$map<- renderPlot({
        #delete unwanted rows
        crime1 = crime[!crime$Lat == 	-1.00000, ]
        # Create aggregated object
        agg <- aggregate(Count~ Lat + Long, data = crime1, FUN = sum)
        agg_count <- subset(agg,Count=250,)
        qmplot(Long, Lat, data = agg_count, colour = I('red'), size = I(0.5), darken = .3)
})
# create data table
output$crimetable <- renderDT(datatable(crime, options = list(searching = TRUE, pageLength = 5,lengthMenu = c(5, 10, 15, 20), scrollX = T)))
})

# Run the app
shinyApp(ui=ui, server=server)