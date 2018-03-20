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
  h6(" This app predict the next text for given input.This app uses coursera swiftkey files as base.Due to mobile phone application, only first 5000 lines of textfiles has been used for creating text minining package, which shall form as a database for predicting the text."),
  h6("Hence use at the max 4 grams as input text from first 5000 lines of files to predict. "),
  h6("Note1: that Ngram max size is 4 words for this application and this application predicts the fifth word. Note2: While changing the text type, use in*, wor*, an* etc, so that generic unigram words can be predicted rather than getting no valid match for the words in input text."),
  h6("Note3: While using * in words, use it in the end of the Ngram like as it w*, a bloo*."),
  h6("Note4: The greying out of the graph means, the words are being predicted."),
  h5("While changing text type to Blogs or news or Tweets, the prediction output takes longer as it creates database before predicting the word. So please be patient. But once the data base of particular text created, the prediction of words within that same text takes very less time."),
  h6("Note4: The choice of answers given, for you to choose in Uniword and bigrams. This gives a path to make a sentance.Ex. was a beautiful ... big, blow etc. will lead to different sentance predictions."),
  # Sidebar with a slider input for number of bins 
  sidebarLayout(
    sidebarPanel(
      selectInput("text_type", label = "Type of Text:",
                  choices = c("blogs", "news", "tweets"), selected = "blogs"),
      #sliderInput("samp_size", label = "Sample Size of text lines:",
       #           min = 500, max = 4500, value = 1000, step = 50),
      textInput("inptext", label="Text to be Predicted for:", value="wor*"),
      submitButton(text="Done")
    ),
    
    # Show a plot of the generated distribution
    mainPanel(
       verbatimTextOutput("text1"),
       verbatimTextOutput("text2"),
       verbatimTextOutput("text3"),
       tableOutput("Out_Table"),
       tableOutput("Out_Table2"),
       verbatimTextOutput("text4"),
       tableOutput("Out_Table3"),
       plotOutput("Out_Plot")
    )
  )
  
))
