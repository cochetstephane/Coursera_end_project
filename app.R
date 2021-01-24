library(shiny)
library(lubridate)
library(car)
data(mtcars)

mtcars$am <- factor(mtcars$am)

ui <- fluidPage(
    titlePanel("Linear regression of MTCARS (Outcome mpg, Up to 3 main contributors)"),
    
    sidebarPanel(
        p("The input for the Dependent Variable is already selected (Go to step2)"),
        checkboxGroupInput(inputId = "DepVar", label = "Dependent Variables",
                           selected = 'mpg', choiceNames = list("mpg (Miles per gallon"),
                           choiceValues = list("mpg")), 
        
        checkboxGroupInput(inputId = "IndVar", label = "Choose up to three main predictors:",
                           selected = "qsec", choiceNames = list("Weight (wt)", "Transmission type (am)","Acceleration (qsec)"),
                           choiceValues = list("wt", "am", "qsec")),
        
        textOutput("IndVar"),
        
        br(),br(),br(),
        
        sliderInput("row",
                    "Number of rows:",
                    min = 1,
                    max = 32,
                    value = 15),
        
        tableOutput("data"),
    ),
    
    mainPanel(
        p(today()),
        p("Here are the main results of regression linear model"),
        verbatimTextOutput("RegOut2"),
        br(),
        p("The regressors in actions are:",),
        verbatimTextOutput(outputId = "IndPrint"),
    ),
)

server <- function(input, output) {
    
    output$IndVar <- renderText({
        IndVar <- paste(input$IndVar, collapse = " + ")
        paste("You chose the predictors:", IndVar)
    })
    
    output$data <- renderTable({
        mtcars[1:input$row, c("mpg", input$IndVar), drop = FALSE]
    },rownames = TRUE)
    
    # Calculation of the linear model given the list of regressors provided
    lm1 <- reactive({lm(reformulate(input$IndVar , input$DepVar), data = mtcars)})
    
    #output$RegOut2 = renderPrint({summary(lm1())[4]}) 
    output$RegOut2 = renderPrint({summary(lm1())}) 
    output$IndPrint <- renderPrint({input$IndVar})
}

shinyApp(ui, server)
