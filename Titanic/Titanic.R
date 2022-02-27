#
# This is a Shiny web application. You can run the application by clicking
# the 'Run App' button above.
#
# Find out more about building applications with Shiny here:
#
#    http://shiny.rstudio.com/
#

library(shiny)
library(shinyFeedback)
library(tidyverse)
library(tidymodels)
library(baguette)

classes <- c(1,2,3)
titles <- c("Mr.","Mrs.","Miss.","Ms.","Master.","Dr.","Rev.","Mlle.","Mme.","Sir.","Countess.","Lady.","Jonkheer.","Don.","Dona.","Col.","Capt.","Major.")
sexes <- c("Male","Female")
embarks <- c("Cherbourg","Queenstown","Southampton")

# Define UI for application that draws a histogram
ui <- fluidPage(
    useShinyFeedback(),

    # Application title
    titlePanel("Titanic Survival Predictor"),

    # Sidebar with a slider input for number of bins 
    sidebarLayout(
        sidebarPanel(
            selectInput("Title","Title",titles),         #Title input
            textInput("First","First Name(s)",value=""),                                  #First name input
            textInput("Last Name","FamilyName",value=""),                                  #Last name input
            radioButtons("PClass","Passenger class",classes,inline=T),                                  #Passenger Class
            radioButtons("Sex","Sex",sexes,inline=T),                                  #Sex
            numericInput("Age","Age",value=NA,min=0,max=200),
            numericInput("SibSp","Number of siblings and/or Spouses travelling with you",value=0,min=0,max=10),
            numericInput("Parch","Number of parents or children travelling with you",value=0,min=0,max=5),
            sliderInput("Fare","Fare paid",value=20,min=0,max=50,step=0.2,ticks=FALSE),
            radioButtons()
        ),

        # Show a plot of the generated distribution
        mainPanel(
            dataTableOutput("table")
            )
    )
)

# Define server logic required to draw a histogram
server <- function(input, output,session) {
      train_data <- read_csv("./tt_train_pp.csv")

      train_data <- train_data %>% 
        mutate(Survived = as_factor(Survived)) %>% 
        mutate(Pclass = as_factor(Pclass))
      
      rec <- recipe(Survived ~.,data=train_data) %>% 
        update_role(PassengerId,new_role = "ID") %>% 
        step_impute_mode(Embarked)
      
      model <- 
        bag_tree(min_n=7) %>% 
        set_engine("C5.0") %>% 
        set_mode("classification")
      
      wf <- workflow(rec,model)
      
      model_fit <- fit(wf,train_data)
      
      title_input <- reactive({input$Title})
      firstname_input <- reactive({input$First})
      familyname_input <- reactive({input$FamilyName})
      pclass_input <- reactive({input$PClass})
      sex_input <- reactive({input$Sex})
      age_input <- reactive({input$Age})
      sipsp_input <- reactive({input$SibSp})
      parch_input <- reactive({input$Parch})
      fare_input <- reactive({input$fare})
      
      pred_table <- tibble(PassengerId=c(2000),
                           Pclass = pclass_input,
                           Sex = sex_input,
                           Age = age_input,
                           SibSp = sibsp_input,
                           Parch = parch_input,
                           Fare = 
                           )
      
      output$table <- renderDataTable(pred_table)
            
}


# Run the application 
shinyApp(ui = ui, server = server)
