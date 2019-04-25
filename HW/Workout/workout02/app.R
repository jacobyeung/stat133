#
# This is a Shiny web application. You can run the application by clicking
# the 'Run App' button above.
#
# Find out more about building applications with Shiny here:
#
#    http://shiny.rstudio.com/
#

library(shiny)
library(ggplot2)




# Define UI for application that draws a histogram
ui <- fluidPage(
   
   # Application title
   titlePanel("Investment Calculator"),
          fluidRow(
            column(4,
                   div(class = "option-group",
                  sliderInput(inputId = "amount",
                               label = "Initial Amount",
                               min = 0,
                               max = 100000,
                               value = 1000,
                               step = 500,
                              pre = "$",
                              sep = ","))

            ),
            column(4,
                   div(class = "option-group",
                   sliderInput(inputId = "rrate",
                               label = "Return Rate(in %)",
                               min = 0,
                               max = 20,
                               step = 0.1,
                               value = 5))
                   
            ),
            column(4,
                   sliderInput(inputId = "years",
                               label = "Years",
                               max = 50,
                               min = 0,
                               step = 1,
                               value = 20
                               )
                   ),
          column(4,
                 div(class = "option-group",
                   sliderInput(inputId = "contribution",
                               label = "Annual Contribution",
                   min = 0,
                   max = 50000,
                   step = 500,
                   value = 2000,
                   pre = "$",
                     sep = ","))),
          column(4,
            div(class = "option-group",
                sliderInput(inputId = "grate",
                            label = "Growth Rate (in %)",
                            min = 0,
                            max = 20,
                            step = 0.1,
                            value = 2))),
          column(4,
                 selectInput(inputId = "Facet",
                             label = "Facet", c("No", "Yes")))
            ),



      # Show a plot of the generated distribution
  
        h3("Timelines"),
        plotOutput("plot"),
        verbatimTextOutput("amount"),
        h2("Balances"),
        verbatimTextOutput("modes_data")
)

# Define server logic required to draw either a facetted or not facetted mode of investing
server <- function(input, output) {
    output$plot = renderPlot({
      #' @title  fv
      #' @description calculate the future value of an investment
      #' @param value (numeric) rate (numeric) time (numeric)
      #' @return calculated future value (numeric)
    fv = function(value, rate, time){
      return(value * ((1 + rate) ** time))
    }
    #' @title annuity
    #' @description calculate the future value of a annuity
    #' @param c (numeric) r (numeric) t (numeric)
    #' @return calculated future annuity (numeric)
    annuity = function(c, r, t){
      return(c * (((1 + r) ** t) - 1) / r)
    }
    #' @title fva
    #' @description calculate the future value of a growing annuity
    #' @param c (numeric) r (numeric) g(numeric) t (numeric)
    #' @return calculated future growing annuity (numeric)
    fva = function(c, r, g, t){
      return(c * (((1 + r) ** t) - ((1 + g) ** t))/ (r - g))
    }
    year = c()
    no_contrib = c()
    fixed_contrib = c()
    growing_contrib = c()
    amount = input$amount
    rate = input$rrate / 100
    con = input$contribution
    grate = input$grate / 100
    for(i in 1:(input$years + 1)){
      year[i] = i - 1
      no_contrib[i] = fv(amount, rate, i - 1)
      fixed_contrib[i] = fv(amount, rate, i - 1) + annuity(con, rate, i - 1)
      growing_contrib[i] = fv(amount, rate, i - 1) + annuity(con, rate, i - 1) + fva(con, rate, grate, i - 1)
    }
    modes = data.frame(year, no_contrib, fixed_contrib, growing_contrib)
    year_data = rep(1:(input$years + 1), 3)
    columns = c(no_contrib, fixed_contrib, growing_contrib)
    names = gl(3, input$years + 1, labels = c("no_contrib", "fixed_contrix", "growing_contrib"))
    modes_data = data.frame(year_data, columns, names)
    with(modes_data, levels(names))
    if(input$Facet == "Yes"){
      ggplot(modes_data) + 
        geom_line(aes(year_data, columns, color = names), size = 1) +
        geom_point(aes(year_data, columns, color = names), size = 2) +
        geom_area(aes(year_data, columns, fill = names), alpha = 0.5) +
        labs(title = "Three modes of investing", x = "year", y = "value") +
        facet_grid(~names)
    } else{
      ggplot(modes_data) + 
        geom_line(aes(year_data, columns, color = names), size = 1) +
        geom_point(aes(year_data, columns, color = names), size = 2) +
        labs(title = "Three modes of investing", x = "year", y = "value")
    }
  })
  
  output$modes_data = renderPrint({
    #' @title  fv
    #' @description calculate the future value of an investment
    #' @param value (numeric) rate (numeric) time (numeric)
    #' @return calculated future value (numeric)
    fv = function(value, rate, time){
      return(value * ((1 + rate) ** time))
    }
    #' @title annuity
    #' @description calculate the future value of a annuity
    #' @param c (numeric) r (numeric) t (numeric)
    #' @return calculated future annuity (numeric)
    annuity = function(c, r, t){
      return(c * (((1 + r) ** t) - 1) / r)
    }
    #' @title fva
    #' @description calculate the future value of a growing annuity
    #' @param c (numeric) r (numeric) g(numeric) t (numeric)
    #' @return calculated future growing annuity (numeric)
    fva = function(c, r, g, t){
      return(c * (((1 + r) ** t) - ((1 + g) ** t))/ (r - g))
    }
    year = c()
    no_contrib = c()
    fixed_contrib = c()
    growing_contrib = c()
    amount = input$amount
    rate = input$rrate / 100
    con = input$contribution
    grate = input$grate / 100
    for(i in 1:(input$years + 1)){
      year[i] = i - 1
      no_contrib[i] = fv(amount, rate, i - 1)
      fixed_contrib[i] = fv(amount, rate, i - 1) + annuity(con, rate, i - 1)
      growing_contrib[i] = fv(amount, rate, i - 1) + annuity(con, rate, i - 1) + fva(con, rate, grate, i - 1)
    }
    modes_data = data.frame(year, no_contrib, fixed_contrib, growing_contrib)
    modes_data
  })

}

# Run the application 
shinyApp(ui = ui, server = server)

