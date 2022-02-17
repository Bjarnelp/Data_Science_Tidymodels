#
# This is a Shiny web application. You can run the application by clicking
# the 'Run App' button above.
#
# Find out more about building applications with Shiny here:
#
#    http://shiny.rstudio.com/
#

library(shiny)
library(tidyverse)
library(glue)

data <- read_csv("./data.csv")

geodata <- data %>% 
  select(LaunchSite,Latitude,Longitude) %>% 
  group_by(LaunchSite) %>% 
  distinct(LaunchSite,Latitude,Longitude)

launchsites <- c(geodata$LaunchSite,'All')

ui <- fluidPage(
  titlePanel("SpaceX Launch Records Dashboard"),
  fluidRow(column(12,
                  selectInput("site","Select Launch Site",launchsites,selected='All')
  )),
  fluidRow(column(12,
                  plotOutput("piechart")
  )),
  fluidRow(column(2,),
           column(8,
                  sliderInput("payload","Select Payload Mass Range",
                              0,10000,c(0,10000),step=1000,width='100%')
           ),
           column(2,)),
  fluidRow(column(12,
                  plotOutput("scatterchart")
  ))
)

server <- function(input, output, session) {
  site_select <- reactive(input$site)
  min_payload <- reactive(input$payload[1])
  max_payload <- reactive(input$payload[2])
  
  output$piechart <- renderPlot({ 
    if (site_select() == 'All') 
    {     data %>% 
        ggplot(mapping=aes(x="",y=Class,fill=factor(LaunchSite))) +
        geom_bar(stat="identity",width=1) + 
        coord_polar(theta="y") +
        labs(x="",y="",title="Distribution of successful launches on launch sites",fill="Launch Site") +
        theme(axis.text=element_blank(),
              axis.ticks = element_blank(),
              panel.grid = element_blank())
    }
    
    else 
    {     data %>% 
        filter(LaunchSite == site_select()) %>%
        group_by(Class) %>% 
        ggplot(mapping = aes(x="",y=sum(Class),fill=factor(Class))) +
        geom_bar(stat="identity",width = 1) +
        coord_polar(theta="y",start=0) +
        labs(x="",y="",title=glue("Distribution og successful and unsuccessful launches at {site_select()}")) +
        scale_fill_discrete("Outcome",labels=c("Failure","Success")) +
        theme(axis.text=element_blank(),
              axis.ticks = element_blank(),
              panel.grid = element_blank())
    }
  })
  
  output$scatterchart <- renderPlot({
    if (site_select() == 'All')
    {
      data %>% 
        filter(PayloadMass >= min_payload(),PayloadMass <= max_payload()) %>% 
        ggplot(mapping=aes(x=PayloadMass,y=Class,color=LaunchSite)) +
        geom_point(size=2) +
        labs(x="Payload Mass (kg)",y="Outcome Class",title="Outcome Class vs. Payload Mass (kg) for all launch sites")
    }
    else
    {
      data %>% 
        filter(LaunchSite == site_select()) %>%
        filter(PayloadMass >= min_payload(),PayloadMass <= max_payload()) %>% 
        ggplot(mapping=aes(x=PayloadMass,y=Class,color=LaunchSite)) +
        geom_point(size=2) +
        labs(x="Payload Mass (kg)", y="Outcome Class",title=glue("Outcome Class vs. Payload Mass (kg) for {site_select()}"))
    }
  })
}

shinyApp(ui, server)
