library(shiny)

shinyUI(fluidPage(
        
        titlePanel("The Word Predictor"),
        
        sidebarLayout(
                sidebarPanel(
                        textInput("obs", "Enter Word/Phrase:"),
                        submitButton("Predict!")
                        ),
                
                mainPanel(
                        h4("Your Input:"),
                        textOutput("Original"),
                        br(),
                        h4("Reformatted Input:"),
                        textOutput("Translated"),
                        br(),
                        br(),
                        h4("Predicted Next Word:"),
                        div(textOutput("BestGuess"), style = "color:red"),
                        br(),
                        h4("Data Used For Prediction:"),
                        tableOutput("view")
                        )
                )
        ))