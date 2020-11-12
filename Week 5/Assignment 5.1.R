library(shiny)

ui <- fluidPage(
  headerPanel('Larceny from Motor Vehicle in Boston'),
  
  sidebarPanel(
    selectInput("district", "Choose a District:",
               choices = c("A1", "A7", "A15", "B2", "B3", "C6", "C11", "D4", "D14", "E5", "E13", "E18", "All"),
               selected = "All"),
    # checkboxGroupInput("district", label = h3("Select District(s):"), 
    #                   choices = c("A1", "A7", "A15", "B2", "B3", "C6", "C11", "D4", "D14", "E5", "E13", "E18", "All"),
    #                   selected = "All"),
    submitButton("Submit")
  ),
  
  mainPanel(
    tabsetPanel(type = 'tabs',
                tabPanel("Larceny from MV by Year ", plotOutput("larceny_year", height = 700)),
                tabPanel("East Boston and the Southern Area", plotOutput("map", height = 700)),
                tabPanel("Larceny from MV by Hour", plotOutput("larceny_hour", height = 700))
    )
  )
)



server <- function(input, output, session) {
  crimeInput <- reactive({
    crime_data <- read.csv("/Users/yuyizhang/FCR/NEU/CPS/Analytics_2018/ALY 6070_Communication and Visualization for Data Analytics/Week 5/Crime Incident Reports (August 2015-June 2020).csv")
    switch(input$district,
           "A1" = crime_data[crime_data$DISTRICT == "A1",],
           "A7" = crime_data[crime_data$DISTRICT == "A7",],
           "A15" = crime_data[crime_data$DISTRICT == "A15",],
           "B2" = crime_data[crime_data$DISTRICT == "B2",],
           "B3" = crime_data[crime_data$DISTRICT =="B3",],
           "C6" = crime_data[crime_data$DISTRICT == "C6",],
           "C11" = crime_data[crime_data$DISTRICT == "C11",],
           "D4" = crime_data[crime_data$DISTRICT == "D4",],
           "D14" = crime_data[crime_data$DISTRICT == "D14",],
           "E5" = crime_data[crime_data$DISTRICT == "E5",],
           "E13" = crime_data[crime_data$DISTRICT == "E13",],
           "E18" = crime_data[crime_data$DISTRICT == "E18",],
           "All" = crime_data[,])
  })
  
  
  output$larceny_year <- renderPlot({
    filedata <- crimeInput()
    selected <- filedata[filedata$OFFENSE_DESCRIPTION == "LARCENY THEFT FROM MV - NON-ACCESSORY", ]
    selected$yearmonth <- as.yearmon(paste(selected$YEAR, selected$MONTH), "%Y %m")
    agg_selected <- aggregate(selected$yearmonth, by=list(selected$yearmonth), FUN=length)
    colnames(agg_selected) <- c("yearmonth", "Count")
    ggplot(data = agg_selected, aes(x=yearmonth, y=Count)) + geom_line() + ggtitle("Monthly Number of Larceny by Districts") + theme(plot.title = element_text(face = 'bold')) + theme(plot.title = element_text(hjust = 0.5)) + geom_point(size = 4)
  })
  
  
  output$map <- renderPlot({
    filedata <- crimeInput()
    selected <- filedata[filedata$OFFENSE_DESCRIPTION == "LARCENY THEFT FROM MV - NON-ACCESSORY" & !is.na(filedata$Lat) & filedata$Lat>0, ]
    MainStates <- map_data("state")
    Massachusetts <- MainStates[MainStates$region=='massachusetts' & MainStates$subregion =='main',]
    g <- ggplot() + geom_polygon(data = Massachusetts, aes(x=long, y=lat), fill = 'antiquewhite1') + geom_point(data = selected, aes(x=Long, y=Lat, colour = DISTRICT), size = 3) + ggtitle("Map of Districts in Boston") + theme(plot.title = element_text(face = 'bold')) + theme(plot.title = element_text(hjust = 0.5))
    g + coord_cartesian(xlim=c(-71.20, -70.95), ylim = c(42.20, 42.40)) + labs(x='Longitude', y = 'Latitude')
  })
  
  
  output$larceny_hour <- renderPlot({
    filedata <- crimeInput()
    selected <- filedata[filedata$OFFENSE_DESCRIPTION == "LARCENY THEFT FROM MV - NON-ACCESSORY", ]
    agg_selected <- aggregate(selected$HOUR, by=list(selected$HOUR), FUN=length)
    colnames(agg_selected) <- c("Hour", "Count")
    ggplot(data = agg_selected, aes(x=Hour, y=Count)) + geom_line() + ggtitle("Hourly of Larceny by Districts") + theme(plot.title = element_text(face = 'bold')) + theme(plot.title = element_text(hjust = 0.5)) + geom_point(size = 4)
  })
}

shinyApp(ui, server)