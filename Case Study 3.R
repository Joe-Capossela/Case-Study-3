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
                            I'm using a linear regression to evaluate how the provided variables influence the price of real estate." ,style="text-align:center" ),
                          br(),
                        
                          br(),
                          p("As discussed in lecture, regression is used to identify the relationship between a dependent variable (target variable) and one or more numeric
                            indpendent variables. In this case, the target variable is price of real estate. Additionally, through the linear regression process, coefficients 
                            are generated for each independent variables that illustrate the relative impact of each driver." ,style="text-align:center" ),
                          br(),  
                           
                          br(),
                          p("Observations: this dataset unfortunately didn't exhibit a strong linear relationship for most variables (this can be observed by selecting 
                            the independent variables in the section below. Interestingly enough, when you combine all the independent variables together in the regression 
                            the r^2 increases compared to any single variable used in the regression." ,style="text-align:center" ),
                          br(),                      
                          
                          
                          p("The data leveraged in this application was accessed via Kaggle. For more information please access the following link",
                            br(),
                            a(href="https://www.kaggle.com/datasets/ivanchvez/ny-rental-properties-pricing?resource=download", "Here",target="_blank"),style="text-align:center;color:black"),
                          
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
                    verbatimTextOutput("regression2")
                            )
                    ))
      
      
      
    )

# Define server logic
server <- function(input, output) {
  
  # Load finData dataset
  data(finData)
  
  output$finData <-  DT::renderDataTable(
    DT::datatable({
      finData
    },
    options = list(lengthMenu=list(c(5,15,20),c('5','10','30')),pageLength=10,
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
  
  
  # Define scatterplot function
  output$scatterplot <- renderPlot({
    plot(finData[, input$var], finData$price, 
         xlab = input$var, ylab = "Price",
         main = paste("Scatterplot of", input$var, "and price"))
  })
  
  # Define regression function
  output$regression <- renderPrint({
    lm_fit <- lm(price ~ finData[, input$var], data = finData)
    summary(lm_fit)
  })
  
  
  # Define regression function with multiple variables 
  output$regression2 <- renderPrint({
    lm_fit <- lm(price ~ room_type + neighbourhood + number_of_reviews + reviews_per_month + latitude + longitude +days_occupied_in_2019 + minimum_nights, data = finData)
    summary(lm_fit)
  })
  
}

# Run the application
shinyApp(ui = ui, server = server)