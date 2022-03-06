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
titles <- tibble(Title = c("Mr.","Mrs.","Miss.","Ms.","Master.","Dr.","Rev.","Mlle.","Mme.","Sir.","Countess.","Lady.","Jonkheer.","Don.","Dona.","Col.","Capt.","Major."),
                 Sex = c("male","female","female","female","male","male","male","female","female","male","female","female","male","male","female","male","male","male"))

sexes <- c("male","female")
embarks <- c("Cherbourg","Queenstown","Southampton")

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
    # Application title
    titlePanel("Titanic Survival Predictor"),

    # Sidebar with a slider input for number of bins 
    sidebarLayout(
        sidebarPanel(
            selectInput("Title","Title",titles$Title),         #Title input
            textInput("First","First Name(s)",value=""),                                  #First name input
            textInput("FamilyName","Last Name",value=""),                                  #Last name input
            radioButtons("Pclass","Passenger class",choiceNames=c("1st","2nd","3rd"),choiceValues=classes,inline=T),                                  #Passenger Class
            radioButtons("Sex","Sex",choiceNames=c("Male","Female"),choiceValues = sexes,inline=T),                                  #Sex
            numericInput("Age","Age",value=20,min=0,max=120),
            numericInput("SibSp","Number of siblings and/or Spouses travelling with you",value=0,min=0,max=10),
            numericInput("Parch","Number of parents or children travelling with you",value=0,min=0,max=5),
            sliderInput("Fare","Fare paid",value=20,min=0,max=100,step=0.2,ticks=FALSE),
            radioButtons("Embark","Point of embarkation",choiceNames=embarks,choiceValues = c("C","Q","S"),inline=T)
        ),

        mainPanel(
          fluidRow(
            column(5,
                    textOutput("text1"),tags$head(tags$style("#text1{color: black; font-size: 32px; font-style: bold;text-align: center}")),
                    textOutput("text2"),tags$head(tags$style("#text2{color: black; font-size: 32px; font-style: bold;text-align: center}"))
            ),
            column(1),
            column(4,
                    plotOutput("picture")
                   ),
            column(1)
            ),
          fluidRow(
            column(8,
                   textOutput("string"),tags$head(tags$style("#string{color: red; font-size: 26px; font-style: bold;text-align: center}"))
                  )
          )
          )
    )
)

# Define server logic required to draw a histogram
server <- function(input, output,session) {

      title_input <- reactive(input$Title)
      firstname_input <- reactive(input$First)
      familyname_input <- reactive(input$FamilyName)
      pclass_input <- reactive(input$Pclass)
      sex_input <- reactive(input$Sex)
      age_input <- reactive(input$Age)
      sibsp_input <- reactive(input$SibSp)
      parch_input <- reactive(input$Parch)
      fare_input <- reactive(input$Fare)
      embark_input <- reactive(input$Embark)
      
      reactivePredicted <- reactive({
        df1 <- tibble(PassengerId = 0,
                      Title = title_input(),
                      FamilyName = familyname_input(),
                      Pclass = pclass_input(),
                      Sex = sex_input(),
                      Age = age_input(),
                      SibSp = sibsp_input(),
                      Parch = parch_input(),
                      Fare = fare_input(),
                      Embarked = embark_input()
                     ) %>% 
          mutate(Pclass = as_factor(Pclass)) %>% 
          mutate(Title = case_when(
            str_detect(Title,coll("Mr.")) ~ "Mr",
            str_detect(Title,coll("Mrs.")) ~ "Mrs",
            str_detect(Title,coll("Ms.")) ~ "Miss",
            str_detect(Title,coll("Miss.")) ~ "Miss",
            str_detect(Title,coll("Master.")) ~ "Master",
            str_detect(Title,coll("Dr.")) ~ "Doktor",
            str_detect(Title,coll("Rev.")) ~ "Mr",
            str_detect(Title,coll("Mlle.")) ~ "Miss",
            str_detect(Title,coll("Mme.")) ~ "Mrs",
            str_detect(Title,coll("Sir.")) ~ "Noble",
            str_detect(Title,coll("Countess.")) ~ "Noble",
            str_detect(Title,coll("Lady.")) ~ "Noble",
            str_detect(Title,coll("Jonkheer.")) ~ "Noble",
            str_detect(Title,coll("Don.")) ~ "Noble",
            str_detect(Title,coll("Dona.")) ~ "Noble",
            str_detect(Title,coll("Col.")) ~ "Military",
            str_detect(Title,coll("Capt.")) ~ "Military",
            str_detect(Title,coll("Major.")) ~ "Military",
          )) %>% 
          mutate(Family = case_when((sibsp_input() == 0 & parch_input() == 0) ~ "Single",
                                    (sibsp_input() == 1 & parch_input() == 0) ~ "Couple",
                                    between(sibsp_input()+parch_input(),1,5) ~ "Small",
                                    sibsp_input()+parch_input() > 5 ~"Large"))
        
          df1$predictions <- predict(model,new_data=df1,type="prob")[2]
        
        return(df1) 
      })


      predicted <- reactive({
        value <- reactivePredicted()$predictions[1]
        
        return(value)
      })
      
      textstring <- reactive({
        text_list <-c(
          "Do I feel lucky? Well, do ya, punk?",
          "Death never takes the wise man by surprise",
          "Life is hard. After all, it kills you",
          "Oh, I am fortunes fool",
          "Did you remember to buy a lottery ticket?"
        )
        
        string <- 
          ifelse(predicted()<0.2,text_list[1],
                 ifelse(predicted()<0.4,text_list[2],
                        ifelse(predicted()<0.6,text_list[3],
                               ifelse(predicted()<0.8,text_list[4],text_list[5]))))
        
        return(string)
      })
      
      filter_title <- reactive({
        list <- titles %>% filter(Sex == sex_input()) %>% select(Title)
      })
      
      observeEvent(input$Sex,
                   {
                     updateSelectInput(session,"Title",choices = filter_title(),selected=head(filter_title(),1))
                   })
      
      output$table2 <- DT::renderDataTable(reactivePredicted())      
      
      output$text1 <- renderText({paste0("Probability of surviving is")})
      output$text2 <- renderText({paste0(round(100*predicted(),1),"%")})
      
      output$string <- renderText({paste0(textstring())})
      
      output$picture <- renderImage({
        n <- ifelse(predicted()<0.34,3,
               ifelse(predicted()<0.67,2,1))

        filename <- normalizePath(file.path('./images',paste('smiley_new',n,'.jpeg', sep='')))
        
        list(src=filename,height=300,width=300)
      },deleteFile = FALSE)      
}

# Run the application 
shinyApp(ui = ui, server = server)
