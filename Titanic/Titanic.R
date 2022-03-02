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
library(DT)

classes <- c(1,2,3)
titles <- c("Mr.","Mrs.","Miss.","Ms.","Master.","Dr.","Rev.","Mlle.","Mme.","Sir.","Countess.","Lady.","Jonkheer.","Don.","Dona.","Col.","Capt.","Major.")
sexes <- c("Male","Female")
embarks <- c("Cherbourg","Queenstown","Southampton")

<<<<<<< HEAD
# Define UI for application that draws a histogram
ui <- fluidPage(
    useShinyFeedback(),

=======
train_data <- read_csv("./tt_train_pp.csv",show_col_types = FALSE)

train_data <- train_data %>% 
  mutate(Survived = as_factor(Survived)) %>% 
  mutate(Pclass = as_factor(Pclass))

Impute_table <- train_data %>% 
  group_by(Title) %>% 
  summarise(mean_age=mean(Age,na.rm=T),mean_fare=mean(Fare,na.rm=T))

rec <- recipe(Survived ~.,data=train_data) %>% 
  update_role(PassengerId,new_role = "ID") %>% 
  step_impute_mode(Embarked)

model_def <- 
  bag_tree(min_n=7) %>% 
  set_engine("C5.0") %>% 
  set_mode("classification")

wf <- workflow(rec,model_def)

model <- fit(wf,train_data)

# Define UI for application that draws a histogram
ui <- fluidPage(
>>>>>>> f958590304f73bed22f80d139123952c15bff9c9
    # Application title
    titlePanel("Titanic Survival Predictor"),

    # Sidebar with a slider input for number of bins 
    sidebarLayout(
        sidebarPanel(
            selectInput("Title","Title",titles),         #Title input
            textInput("First","First Name(s)",value=""),                                  #First name input
<<<<<<< HEAD
            textInput("Last Name","FamilyName",value=""),                                  #Last name input
            radioButtons("PClass","Passenger class",classes,inline=T),                                  #Passenger Class
            radioButtons("Sex","Sex",sexes,inline=T),                                  #Sex
            numericInput("Age","Age",value=NA,min=0,max=200),
            numericInput("SibSp","Number of siblings and/or Spouses travelling with you",value=0,min=0,max=10),
            numericInput("Parch","Number of parents or children travelling with you",value=0,min=0,max=5),
            sliderInput("Fare","Fare paid",value=20,min=0,max=50,step=0.2,ticks=FALSE),
            radioButtons()
=======
            textInput("FamilyName","Last Name",value=""),                                  #Last name input
            radioButtons("Pclass","Passenger class",choiceNames=c("1st","2nd","3rd"),choiceValues=classes,inline=T),                                  #Passenger Class
            radioButtons("Sex","Sex",sexes,inline=T),                                  #Sex
            numericInput("Age","Age",value=20,min=0,max=200),
            numericInput("SibSp","Number of siblings and/or Spouses travelling with you",value=0,min=0,max=10),
            numericInput("Parch","Number of parents or children travelling with you",value=0,min=0,max=5),
            sliderInput("Fare","Fare paid",value=20,min=0,max=50,step=0.2,ticks=FALSE),
            radioButtons("Embark","Point of embarkation",embarks,inline=T)
>>>>>>> f958590304f73bed22f80d139123952c15bff9c9
        ),

        # Show a plot of the generated distribution
        mainPanel(
            DT::dataTableOutput("table1"),
            DT::dataTableOutput("table2")
            )
    )
)

<<<<<<< HEAD
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
=======
server <- function(input, output,session) {

      title_input <- reactive(input$Title)
      firstname_input <- reactive(input$First)
      familyname_input <- reactive(input$FamilyName)
      pclass_input <- reactive(input$Pclass)
      sex_input <- reactive(input$Sex)
      age_input <- reactive(input$Age)
      sipsp_input <- reactive(input$SibSp)
      parch_input <- reactive(input$Parch)
      fare_input <- reactive(input$Fare)
      
<<<<<<< HEAD
      reactiveInput <- reactive({
        df1 <- tibble(Title = title_input(),
                      FamilyName = familyname_input(),
                      Pclass = pclass_input(),
                      Sex = sex_input(),
                      Age = age_input(),
                      SipSp = sipsp_input(),
                      Parch = parch_input(),
                      Fare = fare_input()
                      ) %>% 
          mutate(Title = case_when(
            str_detect(Name,coll("Mr.")) ~ "Mr",
            str_detect(Name,coll("Mrs.")) ~ "Mrs",
            str_detect(Name,coll("Ms.")) ~ "Miss",
            str_detect(Name,coll("Miss.")) ~ "Miss",
            str_detect(Name,coll("Master.")) ~ "Master",
            str_detect(Name,coll("Dr.")) ~ "Doktor",
            str_detect(Name,coll("Rev.")) ~ "Mr",
            str_detect(Name,coll("Mlle.")) ~ "Miss",
            str_detect(Name,coll("Mme.")) ~ "Mrs",
            str_detect(Name,coll("Sir.")) ~ "Noble",
            str_detect(Name,coll("Countess.")) ~ "Noble",
            str_detect(Name,coll("Lady.")) ~ "Noble",
            str_detect(Name,coll("Jonkheer.")) ~ "Noble",
            str_detect(Name,coll("Don.")) ~ "Noble",
            str_detect(Name,coll("Dona.")) ~ "Noble",
            str_detect(Name,coll("Col.")) ~ "Military",
            str_detect(Name,coll("Capt.")) ~ "Military",
            str_detect(Name,coll("Major.")) ~ "Military",
          )) %>% 
          
          
        return(df1) 
      })
=======
      pred_table <- tibble(PassengerId=c(age_input,sipsp_input),
#                           Pclass = c(pclass_input),
#                           Sex = c(sex_input),
#                           Age = c(age_input),
#                           SibSp = c(sibsp_input),
#                           Parch = c(parch_input),
#                           Fare = c(fare_input)
>>>>>>> f958590304f73bed22f80d139123952c15bff9c9
                           )
>>>>>>> 4508daf73172c59bce3dd59b2c0b81797cf5d49e
      
      reactiveData <- reactive({
        df2 <- train_data
        df2$predictions <- predict(model,new_data=train_data)
        return(df2)
      })
            
      output$table1 <- DT::renderDataTable(reactiveData())
      output$table2 <- DT::renderDataTable(reactiveInput())      
}


# Run the application 
shinyApp(ui = ui, server = server)
