#Importing the needed libraries
require(ggplot2)
library(leaflet)
library(shiny)

#Reading the Coral data csv file
coral <- read.csv("coralbleach.csv")

#The Bleaching column has % symbol in it which needs to be removed to display the numbers in graph
coral$bleaching = gsub("%","", as.character(coral$bleaching))
#Converting the type of Bleaching to numeric
coral$bleaching = as.numeric(coral$bleaching)

#Removing the white Space from the coral type
coral[,c(2)] = trimws(coral[,c(2)],which = "right")

# Setting the levels for the site column to be based on the latitude
coral$site <- factor(coral$site,levels = rev(unique(coral$site[order(coral$latitude)])))

#Creating the UI for the application
ui <- fluidPage(
  headerPanel("Coral Bleaching at different sites"),
  
  sidebarLayout(
    sidebarPanel(
      selectInput("type","Coral Type", 
                  c("Hard Corals" = "hard corals", "Sea Pens"="sea pens",
                    "Blue Corals" = "blue corals", "Soft Corals"="soft corals",
                    "Sea Fans" = "sea fans")
                                         ),
      selectInput("smoother","Smoother Type",
                  c("None"='none',"Linear Model"='lm',"Generalised Linear Model"='glm',
                    "Generalized Addictive Model"='gam', "Local Regression"='loess'
                    ))
      ),
    
    mainPanel(
      h3(textOutput("caption")),
      plotOutput("coralPlot")
    )
    ),
  leafletOutput("mymap")
  
  )
  

#Creating the Server logic for the application
server <- function(input, output) {
  output$mymap <- renderLeaflet(
    {
      leaflet(data=coral) %>% 
        addTiles() %>% 
        addCircleMarkers(~longitude,~latitude,label = ~as.character(site),
                   labelOptions = labelOptions(noHide=T,textsize = "10px"),
                   radius = 2, stroke = FALSE, fillOpacity = 0.5
                   )
      
    }
  )
  
  output$caption <- renderText(paste("For ", input$type,", Sites ordered by Latitude"))
  output$coralPlot <- reactivePlot(function(){
    coral_new <- coral[coral$type == input$type,]
    #Checking if smoother is required or not
    if(input$smoother != 'none'){
    ggplot(coral_new,aes(x=year,y=bleaching)) + geom_point(aes(color=site)) + 
      facet_grid(type~site) + scale_y_continuous(limits = c(0,100)) + 
      theme(axis.text.x = element_text(angle=45,hjust = 1)) +
      geom_smooth(method = input$smoother) +
      labs(x = 'Year', y = 'Bleaching %')
    }
    else{
    ggplot(coral_new,aes(x=year,y=bleaching)) + geom_point(aes(color=site)) + 
      facet_grid(type~site) + scale_y_continuous(limits = c(0,100)) + 
      theme(axis.text.x = element_text(angle=45,hjust = 1)) +
      labs(x = 'Year', y = 'Bleaching %')
      
    }
  })
    
  
  
}

#Running the Application
shinyApp(ui, server)






                                             