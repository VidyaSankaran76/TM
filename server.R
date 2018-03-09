#
# This is the server logic of a Shiny web application. You can run the 
# application by clicking 'Run App' above.
#
# Find out more about building applications with Shiny here:
# 
#    http://shiny.rstudio.com/
#

library(ggplot2)
library(dplyr)
library(tidyr)
library(devtools)
library(plotly)
library(caret)
library(stringi)
require(stringi)
library(tm)
library(RWeka)
library(RWekajars)
library(RColorBrewer)
library(stringr)
library(ngram)
library(wordcloud)
library(SnowballC)
library(ngram)
library(stringr)
library(NLP)
library(shiny)



# Define server logic required to draw a histogram
shinyServer(function(input, output) {
  

  
  # file download
  file_path <- "./final/en_US/"
  
  if (file.exists("./final/en_US/en_US.blogs.txt")== FALSE){
    download.file("https://d396qusza40orc.cloudfront.net/dsscapstone/dataset/Coursera-SwiftKey.zip", destfile="./Coursera-SwiftKey.zip")
    unzip("Coursera-SwiftKey.zip")
    
  }
  set.seed(1425)
  
  #reading text files of training set
  file_path <- "./final/en_US/"
  con <-  file(paste0(file_path, "en_US.blogs.txt"),"rt", blocking=FALSE)
  blogs <- readLines(con, n=1000, encoding="UTF-8")
  close(con)
  #unique(Encoding(blogs))
  
  con <-file(paste0(file_path, "en_US.twitter.txt"),"rt", blocking=FALSE)
  TL <- readLines(con, n=1000, encoding="UTF-8")
  close(con)
  #unique(Encoding(TL))
  
  con <-file(paste0(file_path, "en_US.news.txt"),"rt", blocking=FALSE)
  news <- readLines(con, n=1000, encoding="UTF-8")
  close(con)
  
  words_comp <- 0
  words_pred <- 0
  
  setPredictedCount <- function(g_w_pr){
    con <-  file("./pred_counter.txt","w", blocking=FALSE)
    writeLines(as.character(g_w_pr), con)
    close(con)
  }
  
  setCompletedCount <- function(g_w_comp) {
    con <-  file("./comps_counter.txt","w", blocking=FALSE)
    writeLines(as.character(g_w_comp), con)
    close(con)
  }
  
  getPredictedCount <- function(){
    con <-  file("./pred_counter.txt","rt", blocking=FALSE)
    g_w_pr <- readLines(con, encoding="UTF-8")
    close(con)
    return (g_w_pr)
  }
  
  getCompletedCount <- function(){
    con <-  file("./comps_counter.txt","rt", blocking=FALSE)
    g_w_comp <- readLines(con, encoding="UTF-8")
    close(con)
    return (g_w_comp)
  }
  
  setCompletedCount(words_comp)
  setPredictedCount(words_pred)
  
  # samp_size
  sample_txt_size <- 0
  g_Source <- "none"
  
  myclean <- function(docs){
   #print(length(docs))
    for (i in 1:length(docs)){
      docs[i] <- iconv(docs[i], to = "ASCII//TRANSLIT")  
      docs[i] <- gsub( "[[:punct:]]", " ", docs[i])
      docs[i] <- gsub( "[[:graph:]]", " ", docs[i])
      docs[i] <- gsub( "[[:cntrl:]]", " ", docs[i])
      docs[i] <- gsub( "[^[:alnum:]]", " ", docs[i])
      docs[i] <- gsub("[^[:alnum:][:space:]\']", "",docs[i])
      docs[i] <- gsub("^[ ]{1,10}", "",docs[i])
      docs[i] <- gsub("[ ]{2,10}", "",docs[i])
      docs[i] <- gsub("€", "", docs[i])
      docs[i] <- gsub("Ã", "", docs[i], fixed=TRUE)
      docs[i] <- gsub("â", "", docs[i], fixed=TRUE)
      docs[i] <- gsub("€", "", docs[i], fixed=TRUE)
      docs[i] <- gsub("™", "", docs[i], fixed=TRUE)
      docs[i] <- gsub("â€œ", "", docs[i], fixed=TRUE)
      #  docs[i] <- iconv(docs[i], to = "ASCII//TRANSLIT")
      docs[i] <- iconv(docs[i], 'UTF-8', 'ASCII')
      eval.parent(docs[i]<- docs[i])
    }  
  }
  
  # random sampling for the given lines of inputs
  sampletextInput <- function (txtlist, sample_len) {
    
    #pick randomlines using rbinorm function.
    txtLen <-  length(txtlist)
    index <- rbinom(n=sample_len, size=txtLen, p=0.8)
    return (txtlist[index])
  }
  
  
  # write the sampled data in to file for reading
  setSource <- function (s_txt_size=1000, source="blogs") {
    
    if ((file.exists("./source.txt")== TRUE) && 
        (file.exists("./samp_size.txt")== TRUE)){
      con <-  file("./source.txt","rt", blocking=FALSE)
      g_Source <- readLines(con, encoding="UTF-8")
      close(con)
      
      con <-  file("./samp_size.txt","rt", blocking=FALSE)
      g_s_txt_size <- readLines(con, encoding="UTF-8")
      close(con)
    }
    
    if( (g_Source != source) || (g_s_txt_size != s_txt_size)){
      
      if (source == "news" ){
        s_text <- sampletextInput(news, s_txt_size)
        g_Source <- "news"
        
      }
      else if (source == "tweets"){
        s_text <- sampletextInput(TL, s_txt_size)
        g_Source <- "tweets"
      }
      else {
        s_text <- sampletextInput(blogs, s_txt_size)
        g_Source <- "blogs"
      } 
      
      if (file.exists("./sample.txt")== TRUE){
        rm("./sample.txt")   
      }
  
      con <-  file("./sample.txt","w", blocking=FALSE)
      writeLines(as.character(s_text), con)
      close(con)
      
      con <-  file("./source.txt","w", blocking=FALSE)
      writeLines(as.character(g_Source), con)
      close(con)
      
      con <-  file("./samp_size.txt","w", blocking=FALSE)
      writeLines(as.character(s_txt_size), con)
      close(con)
    
    }
    
  }
  
 
  
  # reading the sample lines from sample source files 
  getSource <- function() {
    con <-  file("./source.txt","rt", blocking=FALSE)
    g_Source1 <- readLines(con, encoding="UTF-8")
    close(con)
   
    
    con <-  file("./sample.txt","rt", blocking=FALSE)
    sample_t <- readLines(con, encoding="UTF-8")
    close(con)
    
    myclean(sample_t)
    # Sample Size taken for analysis
    wordcount(sample_t)
    length(sample_t)
    vs <-  VectorSource(sample_t)
    return (vs)
    
  }
  
  
  # make unigrams from the sample source
  getCorpusUnigramTDM <- function() {
    
    vs <-  getSource()  
    
    read_val <- Corpus(vs,  
                       readerControl = list(reader = readPlain,  language = "en",  load = TRUE))
    toSpace <- content_transformer(function(x, pattern) {return (gsub(pattern," ",
                                                                      x))})
    
    read_val<- tm_map(read_val,toSpace,"[^[:graph:]]")
    read_val <- tm_map(read_val, removePunctuation)
    read_val <- tm_map(read_val, removeNumbers)
    read_val <- tm_map(read_val, removeWords, stopwords("english"))
    read_val <- tm_map(read_val, stripWhitespace)
    read_val <- tm_map(read_val, content_transformer(tolower))
#    read_val <- tm_map(read_val, stemDocument)
    readTDM <- TermDocumentMatrix(read_val)
    
    return (readTDM)
    
  }
  
  
  # Make bigrams or trigrams from the source
  getCorpusNgramTDM <- function(y=2){
    
    vs <-  getSource()  
    
    if(!(y == 2 || y==3  || y==4)) return (NULL)
    
    read_val <- VCorpus(vs)
    toSpace <- content_transformer(function(x, pattern) {return (gsub(pattern," ",
                                                                      x))})
    
    read_val<- tm_map(read_val,toSpace,"[^[:graph:]]")
    
    read_val <- tm_map(read_val, removePunctuation)
    read_val <- tm_map(read_val, removeNumbers)
    read_val <- tm_map(read_val, stripWhitespace)
    read_val <- tm_map(read_val, content_transformer(tolower))
    read_val <- tm_map(read_val, PlainTextDocument)
#    read_val <- tm_map(read_val, stemDocument, language="english")
    
    if (y== 2) {
      Ngram<-function(x) NGramTokenizer(x,Weka_control(min=2,max=2))
    } 
    if (y==3) {
      Ngram<-function(x) NGramTokenizer(x,Weka_control(min=3,max=3))
    }
    if (y==4) {
      Ngram<-function(x) NGramTokenizer(x,Weka_control(min=4,max=4))
    }
    
    
    Ngramtab<-TermDocumentMatrix(read_val,control=list(tokenize=Ngram))
    return (Ngramtab)
  }

  CreateNgrams <-  function() {
    
    unigram.tdm <- getCorpusUnigramTDM()
    
    DocMat <- as.matrix(unigram.tdm)
    
    v <- sort(rowSums(DocMat),decreasing=TRUE)
    d <- data.frame(word =names(v),freq=v)
    head(d, n=20)
    
    rownames(d) <- 1: nrow(d)
    write.csv(d, file=paste0(file_path, "unigram.csv", sep=" "))
    rm(v)
    rm(d)
    rm(DocMat)
    rm(unigram.tdm)
    
    
    bigram.tdm <- getCorpusNgramTDM(2)
    
    DocMat <- as.matrix(bigram.tdm)
    
    v <- sort(rowSums(DocMat),decreasing=TRUE)
    d <- data.frame(word = names(v),freq=v)
    head(d, n=20)
    
    rownames(d) <- 1: nrow(d)
    
    
    write.csv(d, file=paste0(file_path, "bigram.csv", sep=" "))
    rm(v)
    rm(d)
    rm(DocMat)
    rm(bigram.tdm)
    
    
    trigram.tdm <- getCorpusNgramTDM(3)
    
    DocMat <- as.matrix(trigram.tdm)
    
    v <- sort(rowSums(DocMat),decreasing=TRUE)
    d <- data.frame(word = names(v),freq=v)
    head(d, n=20)
    
    rownames(d) <- 1: nrow(d)
    
    
    write.csv(d, file=paste0(file_path, "trigram.csv", sep=" "))
    
    rm(v)
    rm(d)
    rm(DocMat)
    rm(trigram.tdm)
    
    quadgram.tdm <- getCorpusNgramTDM(4)
    
    DocMat <- as.matrix(quadgram.tdm)
    
    v <- sort(rowSums(DocMat),decreasing=TRUE)
    d <- data.frame(word = names(v),freq=v)
    head(d, n=20)
    
    rownames(d) <- 1: nrow(d)
    
    
    write.csv(d, file=paste0(file_path, "quadgram.csv", sep=" "))
    
    rm(v)
    rm(d)
    rm(DocMat)
    rm(quadgram.tdm)
    
  }
  
 
  # function to clean the input 
  inputclean <- function(inptxt){
    if (wordcount(inptxt)>= 4){
      output$text2 <- renderPrint({ "Words exceeding the limit, Kindly give words less than or equal to 3 in the text"})
      return (NULL)
    }
    
    if (grepl("\\*", inptxt)){
      output$text1 <- renderPrint({"Text contains asterik partial regular exp functionality implemented"})
      inptxt <- gsub("\\*", "", inptxt )
      
    }
    if (grepl("[[:punct:]]", inptxt)) {
      output$text1 <- renderPrint({"Punctionations shall be removed and the generic text alone shall be considered"})
      
      inptxt <- gsub("[[:punct:]]", "", inptxt )
    }
    if (grepl("[[:digit:]]", inptxt)) {
      output$text1 <- renderPrint({ "Digits shall be removed, and the generic text alone shall be considered"})
      inptxt <- gsub("[^[:alnum:][:space:]\']", "",inptxt)
      inptxt <- gsub("[[:digit:]]", "", inptxt )
      
      
    }
    return (inptxt)
    
  }
  
 
 
  updatePredictedCounter <- function()
  {
    g_w_pr <- getPredictedCount()
    
    g_w_pr <- as.numeric(g_w_pr)+1
    setPredictedCount(g_w_pr)
  }
  
  updateCompletedCounter <- function()
  {
    
    g_w_comp <- getCompletedCount()
    
    g_w_comp <- as.numeric(g_w_comp)+1
    setCompletedCount(g_w_comp)
    
  }
  
  
  # predict function to predict the text
  predict_ngram <- function(size, text, source="blogs") {
    
    output$text1<- renderText({NULL})
    output$text2<- renderText({NULL})
    
    output$text1 <- renderPrint({ "In predicting functionality..."})
    
   # output$text1 <- renderPrint({"Source and sample setting in progress"})
    #print(source)
    setSource(size, source)
    CreateNgrams()
   # output$text1 <- renderPrint({"Input Processing in progress.."})

    inptxt <- tolower(text)  
    text <- inputclean(inptxt)
    if (is.null(text)){
      return (NULL)
    }
    if (wordcount(text) == 1){
      #check in Bigram table
      readtable <- read.csv(paste0(file_path, "bigram.csv"))[, -1]
      if( grepl("\\*", inptxt)){
        #print(text)
        readtable <- read.csv(paste0(file_path, "unigram.csv"))[, -1]
      }
    }
    if (wordcount(text)== 2){
      #check in trigram table
      splitword <- strsplit(inptxt, " ")
      firstword <- splitword[[1]][1]
      secondword <- splitword[[1]][2]
      if (grepl("\\*",firstword)){
       # print("in split word")
        readtable <- read.csv(paste0(file_path, "unigram.csv"))[, -1]
        text <- strsplit(text, " ")[[1]][1]
        #print(text)
      }
      else if ( grepl("\\*", secondword)){
        readtable <- read.csv(paste0(file_path, "unigram.csv"))[, -1]
        text <- strsplit(text, " ")[[1]][2]
        #print("bigramcsv")
      }
      else {
        readtable <- read.csv(paste0(file_path, "trigram.csv"))[, -1]
        #print("trigramcsv")
      }
    }
    if (wordcount(text)== 3){
      splitword <- strsplit(inptxt, " ")
      firstword <- splitword[[1]][1]
      secondword <- splitword[[1]][2]
      thirdword  <- splitword[[1]][3]
      if (grepl("\\*",firstword)){
        # print("in split word")
        readtable <- read.csv(paste0(file_path, "unigram.csv"))[, -1]
        text <- strsplit(text, " ")[[1]][1]
        #print(text)
      }
      else if ( grepl("\\*", secondword)){
        readtable <- read.csv(paste0(file_path, "unigram.csv"))[, -1]
        text <- strsplit(text, " ")[[1]][2]
        #print("unigramcsv")
      }
      else if ( grepl("\\*", thirdword)){
        readtable <- read.csv(paste0(file_path, "unigram.csv"))[, -1]
        text <- strsplit(text, " ")[[1]][3]
        #print("unigramcsv")
      }
      else {
        readtable <- read.csv(paste0(file_path, "quadgram.csv"))[, -1]
        #print("quadgramcsv")
      }
      
    }
    
    x <- grep(paste0("^",text),readtable$word)
    
    pred_word <- readtable[x,]
    if (nrow(pred_word) == 0){
      return (NULL)
    }
    if (grepl("\\*", inptxt)){
      # increase the no of words completed counter
      updateCompletedCounter()
    }
    #increase the no of words predicted counter
    updatePredictedCounter()
    
    
    return (head(pred_word, n=10))
  }
  
  # This set source call is for seting default sample code
 # setSource(sample_txt_size, g_Source)
#  CreateNgrams()
  
  output$Out_Plot <- renderPlot ({
    samp_size <- input$samp_size
    text_type <- input$text_type
    text_pred  <- input$inptext
    ptext <- predict_ngram(samp_size, text_pred, text_type)
    
    if(is.null(ptext)){
      output$text1 <- renderPrint({ "No valid match found for the given input"})
      return (NULL)
    }
  
    ptext
    t <- as.data.frame(ptext)
    t$word <- factor(t$word, levels=t$word[order(t$freq, decreasing =TRUE) ])
    t$word_predicted <- word(t$word, -1)
    output$Out_Table <- renderTable({t})
    
    words_pred <- getPredictedCount()
    words_comp <- getCompletedCount()
    word_stats <- data.frame("WordsPredicted"=words_pred, "WordsCompleted"=words_comp)
    output$Out_Table2 <- renderTable({word_stats })
   
 
     ggplot(t, aes(x=(word), y=freq))+geom_bar(stat = "identity")+
        ggtitle("Predicted words with frequency")+
        theme(axis.text.x=element_text(angle=90,hjust=1))
    

   })
  
})
  
