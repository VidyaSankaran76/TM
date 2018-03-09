#
# This is the user-interface definition of a Shiny web application. You can
# run the application by clicking 'Run App' above.
#
# Find out more about building applications with Shiny here:
# 
#    http://shiny.rstudio.com/
#

library(shiny)

# Define UI for application that draws a histogram
shinyUI(fluidPage(
  
  # Application title
  titlePanel("Text Predictor Application"),
  textOutput("App to predict the text for given input"),
  textOutput("Input at the max two words"),
  # Sidebar with a slider input for number of bins 
  sidebarLayout(
    sidebarPanel(
      selectInput("text_type", label = "Type of Text:",
                  choices = c("blogs", "news", "tweets"), selected = "blogs"),
      sliderInput("samp_size", label = "Sample Size of text lines:",
                  min = 500, max = 1000, value = 1000, step = 50),
      textInput("inptext", label="Text to be Predicted for:", value="wor*"),
      submitButton(text="Done")
    ),
    
    # Show a plot of the generated distribution
    mainPanel(
       verbatimTextOutput("text1"),
       verbatimTextOutput("text2"),
       plotOutput("Out_Plot"),
       tableOutput("Out_Table"),
       tableOutput("Out_Table2")
    )
  )
))
