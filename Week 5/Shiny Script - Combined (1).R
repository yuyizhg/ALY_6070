#UI Script

library(shiny)
library(ggplot2)
library(maps)
library(dplyr)

shinyUI(pageWithSidebar(
  headerPanel('Crimes in Boston'), 
  sidebarPanel(
    selectInput("DISTRICT", "Choose a District:", 
                choices = c("A1", "A7", "A15", "B2", "B3", "C6", "C11", "D4", "D14", "E5", "E13", "E18", "All")),
    #These choices will allow the audience to pick and choose the districts they want to look at. The "All" button will show all districts 
    
    submitButton("Submit")
  ),
  mainPanel(
    tabsetPanel(type = 'tabs',
                tabPanel("Top Crimes per UCR Type", plotOutput("hzplot", height = 700)), #Horizontal bar plot
                tabPanel("Number of Crimes per District", plotOutput("bar", height = 700)), #Stacked bar plot
                tabPanel("Homicides per District", plotOutput("map", height = 700)), #Map view
                tabPanel("Number of Homicides each Year", plotOutput("plot", height = 700)) #Line graph
                #This defines and labels each tab in the dashboard
    )
  )
)
)



#Server Script

library(shiny)
library(ggplot2)
library(maps)
library(dplyr)

#How I initially defined the crime data used
crimedat <- read.csv('Crime Incident Reports.csv')


shinyServer(function(input, output){
  districtInput <- reactive({
    switch(input$DISTRICT,
           "A1" = crimedat[crimedat$DISTRICT == "A1",], 
           "A7" = crimedat[crimedat$DISTRICT == "A7",],
           "A15" = crimedat[crimedat$DISTRICT == "A15",],
           "B2" = crimedat[crimedat$DISTRICT == "B2",],
           "B3" = crimedat[crimedat$DISTRICT =="B3",],
           "C6" = crimedat[crimedat$DISTRICT == "C6",],
           "C11" = crimedat[crimedat$DISTRICT == "C11",],
           "D4" = crimedat[crimedat$DISTRICT == "D4",],
           "D14" = crimedat[crimedat$DISTRICT == "D14",],
           "E5" = crimedat[crimedat$DISTRICT == "E5",],
           "E13" = crimedat[crimedat$DISTRICT == "E13",],
           "E18" = crimedat[crimedat$DISTRICT == "E18",],
           "All" = crimedat[,])
    #This is telling the dashboard to focus only on the chosen district. The audience can choose which district to focus on using the drop-down in the dashboard
  })
  #Horizontal Bar Plot
  output$hzplot <- renderPlot({
    hz_data <- districtInput()
    hz_data <- aggregate(hz_data$UCR_PART, by = list(hz_data$UCR_PART, hz_data$OFFENSE_CODE_GROUP, hz_data$DISTRICT), FUN = length)
    colnames(hz_data) <- c("UCR_Group", "Offense", "District", "Count")
    hz_data <- hz_data[with(hz_data, order(UCR_Group, District, -Count)),]
    #The aggregate function will give the total number of rows for each type of crime grouped by each district and UCR group. I also changed the column names and ordered it by UCR group, district, and Count (descending):
    #   UCR_Group                    Offense District Count
    #26  Part One                    Larceny       A1  6430
    #27  Part One Larceny From Motor Vehicle       A1  1607
    #1   Part One         Aggravated Assault       A1  1287
    #52  Part One                    Robbery       A1   820
    #3   Part One                 Auto Theft       A1   459
    #7   Part One        Commercial Burglary       A1   328
    
    topfive <- hz_data %>% select(UCR_Group, District, Count, Offense) %>% group_by(UCR_Group, District) %>% slice(1:5)
    topfive <- as.data.frame(topfive)
    #This will return only the top five crimes per UCR group for each district:
  #     UCR_Group District Count                    Offense
  #  1   Part One       A1  6430                    Larceny
  #  2   Part One       A1  1607 Larceny From Motor Vehicle
  #  3   Part One       A1  1287         Aggravated Assault
  #  4   Part One       A1   820                    Robbery
  #  5   Part One       A1   459                 Auto Theft
  #  6   Part One      A15   656                    Larceny
  #  7   Part One      A15   380 Larceny From Motor Vehicle
  #  8   Part One      A15   168         Aggravated Assault
  #  9   Part One      A15   130                 Auto Theft
  #  10  Part One      A15    96                    Robbery
    
    topfive$UCR_Group <- factor(topfive$UCR_Group, levels = c("Part One", "Part Two", "Part Three")) #Ordered the levels so the legend has the order I want
    g <- ggplot(topfive, aes(reorder(Offense, Count, sum), y = Count, fill = (UCR_Group))) + geom_bar( stat = 'identity') + ggtitle("Top 5 Crimes per UCR Type") + theme_classic() + theme(plot.title = element_text(face = 'bold')) + theme(plot.title = element_text(hjust = 0.5))
    g + coord_flip() + labs(x = "Offense", y = "Number of Crimes", fill = "UCR Group") +  scale_fill_manual(values = c("Part One" = 'firebrick4', "Part Two" = 'indianred2', "Part Three" = 'gray65'))
    #Returns the horizontal bar plot with the specific colors chosen for each UCR group. The reorder() part of the aes code puts the bars in descending order
    #This visualization highlights the top five crimes for each UCR type per district
    
  })
  #Stacked Bar Chart
  output$bar <- renderPlot({
    our_data <- districtInput()
    our_data <- our_data[our_data$UCR_PART %in% c("Part One", "Part Two", "Part Three") & our_data$DISTRICT != "",]
    #I want to only look at rows that have a valid UCR group and district
    our_data <- table(our_data$UCR_PART, our_data$DISTRICT)
    our_data <- our_data[c("Part One", "Part Two", "Part Three"), c("A15", "A7", "E5", "E18", "E13", "D14", "C6", "A1", "B3", "D4", "C11", "B2")]
    #Used the table() function to find the number of crimes committed in each district, grouped by the UCR type. I also specified the groups and districts I wanted to look at:
    #             A15    A7    E5   E18   E13   D14    C6    A1    B3    D4   C11    B2
    #Part One    1557  2729  2680  3404  4674  5019  6005 11236  6757 15906  9171 11401
    #Part Two    2439  5467  5137  6646  6942  7090  9512 15027 15266 15496 17863 22755
    #Part Three  4621  8867  9880 13229 11753 14623 15360 21351 26015 23593 29989 33471
    
    barplot(our_data, xlab = 'Year', ylab = 'Number of Crimes', main = "Number of Crimes Committed", col = c('firebrick4', 'indianred2', 'gray65'), ylim = c(0, 80000), legend = c("Part One", "Part Two", "Part Three"), args.legend = list(x=15, y=82000))
    #Used the barplot() function to plot the table, which automatically makes it a stacked bar plot. Specified my colors and formatted my legend to make it look nice
    #This visualization highlights the districts that have the most crimes (in general) and the districts that have the most Part One crimes
    
  })
  #Homicide Map View
  output$map <- renderPlot({
    mapdata <- districtInput()
    mapdata <- mapdata[mapdata$OFFENSE_CODE_GROUP=='Homicide' & !is.na(mapdata$Lat) & mapdata$Lat>0,]
    #I want my map data to only include homicide crimes so I filtered the data that way. I also took out any crime with invalid latitude/longitude points
    MainStates <- map_data("state")
    Massachusetts <- MainStates[MainStates$region=='massachusetts' & MainStates$subregion =='main',]
    #I found all the Massachusetts latitude/longitude points I needed from the map_data in the maps package. This will outline Massachusetts in general and will be used as my map background
    g <- ggplot() + geom_polygon(data = Massachusetts, aes(x=long, y=lat), fill = 'antiquewhite1') + geom_point(data = mapdata, aes(x=Long, y=Lat, colour = DISTRICT), size = 3) + ggtitle("Map of Boston") + theme(plot.title = element_text(face = 'bold')) + theme(plot.title = element_text(hjust = 0.5))
    g + coord_cartesian(xlim=c(-71.20, -70.95), ylim = c(42.20, 42.40)) + labs(x='Longitude', y = 'Latitude')
    #geom_polygon() is my Massachusetts map background and geom_point() has the actual homicide location points for each district. I limited the visual using coord_cartesian() so we just see the general Boston area
    #This visualization highlights how most homicides occur in the central and southern districts of Boston
  })
  #Homicide Line Graph
  output$plot <- renderPlot({
    line_data <- districtInput()
    line_data <- line_data[line_data$OFFENSE_CODE_GROUP=='Homicide' & line_data$YEAR != '2015' & line_data$MONTH != '12',]
    #Due to missing data in 2015, and December 2019, I excluded all of 2015 and all December data (for all years)
    line_data <- aggregate(line_data$YEAR, by = list(line_data$DISTRICT, line_data$YEAR), FUN = length)
    colnames(line_data) <- c("District", "Year", "Count")
    #The aggregate() function gives me the number of homicide crimes for each year, grouped by the District:
    #  District Year Count
    # 1       A1 2016     1
    # 2       A7 2016     4
    # 3       B2 2016    18
    # 4       B3 2016    12
    # 5      C11 2016    12
    # 6       C6 2016     1
    
    p <- ggplot(data = line_data, aes(x=Year, y=Count, colour=District, size = District)) + geom_line() + ggtitle("Number of Homicides per District") + theme(plot.title = element_text(face = 'bold')) + theme(plot.title = element_text(hjust = 0.5)) + geom_point(size = 4) + xlim(2016, 2019)
    p + scale_color_manual(values = c("A1" = "#3399FF", "A7" = "#999999", "A15"="#999999", "B2"="#990000", "B3"="orangered1", "C6"="#999999", "C11"="goldenrod3", "D4"="#99CCFF", "D14"="#999999", "E5"="#999999", "E13"="#999999", "E18"="#999999")) + scale_size_manual(values = c("A1" = 1, "A7" = 1, "A15" = 1, "B2" = 2, "B3" = 1, "C6" = 1, "C11" = 1, "D4" = 1, "D14" = 1, "E5" = 1, "E13" = 1, "E18" = 1)) + labs(y = "Number of Homicides")
    #I specified the size and color of each district to highlight district B2, which is slightly thicker in size. This district saw a huge drop in the number of homicides between 2018-2019 which I wanted to highlight
  })
})
