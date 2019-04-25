library(shiny)
ui=fluidPage(
  titlePanel("Saving-investing Modalities"),
  
  fluidRow(
    column(4,
           div(class = "option-group",
               sliderInput(inputId="amount",
                           label="Initial Amount",
                           value=1000,min=0,max=100000, pre = "$", sep = ",",step=500)
           )),
    column(4,
           div(class = "option-group",
               sliderInput(inputId="rate",
                           label="Return Rate (in %)",
                           value=5,min=0,max=20,step=0.1)
           )),
    column(4,
           div(class = "option-group",
               sliderInput(inputId="years",
                           label="Years",
                           value=20,min=0,max=50,step=1)
           ))
  ),
  
  fluidRow(
    column(4,
           div(class = "option-group",
               sliderInput(inputId="contrib",
                           label="Annual Contribution",
                           value=2000,min=0,max=50000,pre = "$", sep = ",",step=500)
           )),
    column(4,
           div(class = "option-group",
               sliderInput(inputId="growth",
                           label="Growth Rate (in %)",
                           value=2,min=0,max=20,step=0.1)
           )),
    column(4, 
           selectInput("input_type", "Facet?",
                       c("No","Yes"
                       )
                       
           ))
  ),
  hr(),
  h3("Timelines"),
  plotOutput("plot1"),
  
  h3("Balances"),
  verbatimTextOutput("balances")
  
  
  
)
server=function(input,output){
  output$plot1=renderPlot({
    library(ggplot2)
    #' @title function future_value
    #' @description compute the future value of an investment
    #' @param amount (numeric) rate (numeric) years (numeric)
    #' @return value (numeric)
    future_value=function(amount,rate,years)
    {
      value=amount*(1+rate)^years
      return(value)
    }
    #' @title annuity
    #' @description compute the future value of annuity
    #' @param contrib (numeric)