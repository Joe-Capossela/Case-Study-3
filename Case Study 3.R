finData = read.csv("NY Realestate Pricing.csv")


library(dplyr)
library(ggplot2)
library(tidyverse)
library(ggpubr)
library(esquisse)
library(plsdepot)
library(visreg)
library(caret)
library(shiny)
library(DT)
library(shinythemes)
library(car)
library(nortest)
library(tseries)
library(RcmdrMisc)
library(lmtest)

summary(finData)

set.seed(2000)
splitData = caret::createDataPartition(finData$price, p = 0.8, list=F, times=1)

trainData = finData[splitData,]
head(trainData)
testData <- finData[-splitData,]

linReg2 = lm(price ~ number_of_reviews + reviews_per_month + latitude + longitude +days_occupied_in_2019 + minimum_nights, data=trainData)
fitted(linReg2)
resid(linReg2)

predict(linReg2, newdata=testData)
predPrice = data.frame(predict(linReg2, newdata=testData))
names(predPrice)[1] = 'Predicted'
predPrice$Reference = testData$price

PRESS = sum((predPrice$Reference - predPrice$Predicted)^2)


RMSEP = sqrt(PRESS/ nrow(predPrice))


SST = sum((predPrice$Reference - mean(predPrice$Reference))^2)


R2 = 1 - (PRESS/SST)



# Define user interface
ui <- fluidPage(
  
  titlePanel("Linear Regression for NYC Real Estate Data"),
  tabsetPanel(
    tabPanel("Home",
             
             fluidRow(column(
               
               br(),
               p("Through this application, we will explore how various variables influence the price of real estate in NYC. For this application, 
                            I'm using a linear regression to evaluate how the provided variables influence the price of real estate. This topic is interesting to me given
                            the past few years of volatility in this market driven by covid. In the early months of COVID, NYC (along with other major metro areas)
                            were disproportionately impacted by population outflows and thus decreases in rent. However, as the world has begun to return to normal, rents
                            have begun to return to normal- if not surpass the pre-pandemic levels. This dataset had price information along with various attributes of 
                            real estate that made using a linear regression a natural choice.   
                            " ,style="text-align:left" ),
               br(),
               
               br(),
               p("As discussed in lecture, regression is used to identify the relationship between a dependent variable (target variable) and one or more numeric
                            independent variables. In this case, the target variable is price of real estate. Additionally, through the linear regression process, coefficients 
                            are generated for each independent variables that illustrate the relative impact of each variable.
                            
                            Additional background on the regression model I deployed: I split the total dataset into training and testing subsets in order to assist in the
                            prediction of real estate prices. From there, the scatter plot functionality  allows a user to evaluate the fit of one variable on the price of 
                            real estate. Most variables in this dataset have a slightly positive r^2 value. This r^2 value improves once all variables are included in the
                            linear regression which assists in the predictive power of the model. The final step I followed was to apply the multivariate linear regression
                            to the holdout/testing dataset. 
                            " ,style="text-align:left" ),
               br(),  
               
               br(),
               p("Observations: this dataset unfortunately didn't exhibit a strong linear relationship with the variables (this can be observed by selecting 
                            the independent variables in the section below). As mentioned above, when you combine all the independent variables together in the regression 
                            the r^2 increases compared to any single variable used in the regression on the training dataset. The r^2 increases to 0.12 which is a very weak 
                            in an absolute sense but is a significant improvement compared to any of the univariate views. 
                            
                            This non-ideal perforamnce also manifests itself in other measures of model evaluation like RMSEP, SST, and r^2 which are shown highlighted towards
                            the bottom of the application.
                            " ,style="text-align:left" ),
               br(),                      
               
               
               p("The data leveraged in this application was accessed via Kaggle. For more information, please access the following link:",
                 br(),
                 a(href="https://www.kaggle.com/datasets/ivanchvez/ny-rental-properties-pricing?resource=download", "Here",target="_blank"),style="text-align:left;color:black"),
               
               width=10)),
             
             
             
             
             hr(),
             tags$style(".fa-database {color:#E86622}"),
             h3(p(em("Dataset "),icon("database",lib = "font-awesome"),style="color:black;text-align:center")),
             fluidRow(column(DT::dataTableOutput("finData"),
                             width = 12)),
             
             hr(),
             p(em("Developed by"),br("Joe Capossela"),style="text-align:center; font-family: times")
    )),
  
  
  # Define input options
  
  tabPanel("Detail",
           
           sidebarLayout(
             sidebarPanel(
               selectInput("var", "Select Independent Variable", 
                           choices = c("latitude", "longitude", "days_occupied_in_2019", "minimum_nights", "number_of_reviews", "reviews_per_month", "availability_2020"),
                           selected = "latitude")
             ),
             
             # Define output options
             mainPanel(
               plotOutput("scatterplot"),
               verbatimTextOutput("regression"),
               verbatimTextOutput("regression2"),
               verbatimTextOutput("prediction"),
               verbatimTextOutput("evaluation"),
               verbatimTextOutput("evaluation2"),
               verbatimTextOutput("evaluation3"),
               verbatimTextOutput("evaluation4"),
               fluidRow(column(DT::dataTableOutput("predData"),
                               width = 12)),
             )
           ))
  
  
  
)

# Define server logic
server <- function(input, output) {
  
  # Load finData dataset and prep for modeling
  set.seed(2000)
  splitData = caret::createDataPartition(finData$price, p = 0.8, list=F, times=1)
  
  trainData = finData[splitData,]
  head(trainData)
  testData <- finData[-splitData,]  
  
  
  data(finData)
  
  output$finData <-  DT::renderDataTable(
    DT::datatable({
      finData
    },
    options = list(lengthMenu=list(c(5,15,30),c('5','15','30')),pageLength=10,
                   initComplete = JS(
                     "function(settings, json) {",
                     "$(this.api().table().header()).css({'background-color': 'moccasin', 'color': '1c1b1b'});",
                     "}"),
                   columnDefs=list(list(className='dt-center',targets="_all"))
    ),
    filter = "top",
    selection = 'multiple',
    style = 'bootstrap',
    class = 'cell-border stripe',
    rownames = FALSE,
    colnames = c("F1","Neighbourhood","Latitude","Longitude","Room type","Price","days_occupied_in_2019","minimum_nights","number_of_reviews","reviews_per_month","availability_2020")
    ))
  
  
  # Define scatterplot function on entire dataset
  output$scatterplot <- renderPlot({
    plot(finData[, input$var], finData$price, 
         xlab = input$var, ylab = "Price",
         main = paste("Scatterplot of", input$var, "and price"))
  })
  
  # Define regression function on Training Data
  output$regression <- renderPrint({
    lm_fit <- lm(price ~ trainData[, input$var], data = trainData)
    summary(lm_fit)
    
  })
  
  
  # Define regression function with multiple variables on Training Data
  output$regression2 <- renderPrint({
    lm_fit <- lm(price ~ room_type + neighbourhood + number_of_reviews + reviews_per_month + latitude + longitude +days_occupied_in_2019 + minimum_nights, data = trainData)
    summary(lm_fit)
  })
  
  
  # Using linear regression trained on multiple variables to predict prices 
  output$prediction <- renderPrint({
    lm_fit <- lm(price ~  number_of_reviews + reviews_per_month + latitude + longitude +days_occupied_in_2019 + minimum_nights, data = testData)
    
    predict(lm_fit, newdata=testData)
    predPrice = data.frame(predict(lm_fit, newdata=testData))
    names(predPrice)[1] = 'Predicted'
    predPrice$Reference = testData$price
    head(predPrice)
    
  })
  
  # Evaluating Model 
  output$evaluation <- renderPrint({
    PRESS = sum((predPrice$Reference - predPrice$Predicted)^2)
    x <- c("PRESS",round(PRESS,2)) 
    x
  })
  
  output$evaluation2 <- renderPrint({  
    RMSEP = sqrt(PRESS/ nrow(predPrice))
    x <- c("RMSEP",round(RMSEP,2)) 
    x    
  }) 
  
  output$evaluation3 <- renderPrint({
    SST = sum((predPrice$Reference - mean(predPrice$Reference))^2)
    x <- c("SST",round(SST,2)) 
    x        
    
  })
  
  
  output$evaluation4 <- renderPrint({
    R2 = 1 - (PRESS/SST)
    x <- c("R2",round(R2,2)) 
    x        
    
  })
  
  
  output$predData <-  DT::renderDataTable(
    DT::datatable({
      predPrice
    },
    options = list(lengthMenu=list(c(5,15,30),c('5','15','30')),pageLength=10,
                   initComplete = JS(
                     "function(settings, json) {",
                     "$(this.api().table().header()).css({'background-color': 'moccasin', 'color': '1c1b1b'});",
                     "}"),
                   columnDefs=list(list(className='dt-center',targets="_all"))
    ),
    filter = "top",
    selection = 'multiple',
    style = 'bootstrap',
    class = 'cell-border stripe',
    rownames = FALSE,
    colnames = c("Predicted","Price")
    ))
  
}

# Run the application
shinyApp(ui = ui, server = server)