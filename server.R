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

# file download
#file_path <- "./final/en_US/"

#if (file.exists("./final/en_US/en_US.blogs.txt")== FALSE){
 # download.file("https://d396qusza40orc.cloudfront.net/dsscapstone/dataset/Coursera-SwiftKey.zip", destfile="Coursera-SwiftKey.zip")
  #unzip(".Coursera-SwiftKey.zip")
  
#}
#memory.limit(size=56000)
set.seed(1425)

#reading text files of training set
#file_path <- "./final/en_US/"
file_path <- "./Out/"

file_out_path <- "./Out/"

con <-  file(paste0(file_path, "en_US.blogs.txt"),"rt", blocking=FALSE)
blogs <- readLines(con, n=-1L, encoding="UTF-8")
close(con)
#unique(Encoding(blogs))

con <-file(paste0(file_path, "en_US.twitter.txt"),"rt", blocking=FALSE)
TL <- readLines(con, n=-1L, encoding="UTF-8")
close(con)
#unique(Encoding(TL))

con <-file(paste0(file_path, "en_US.news.txt"),"rt", blocking=FALSE)
news <- readLines(con, n=-1L, encoding="UTF-8")
close(con)

words_comp <- 0
words_pred <- 0

setPredictedCount <- function(g_w_pr){
  con <-  file(paste0(file_out_path,"pred_counter.txt"),"w", blocking=FALSE)
  writeLines(as.character(g_w_pr), con)
  close(con)
}

setCompletedCount <- function(g_w_comp) {
  con <-  file(paste0(file_out_path,"comps_counter.txt"),"w", blocking=FALSE)
  writeLines(as.character(g_w_comp), con)
  close(con)
}

getPredictedCount <- function(){
  con <-  file(paste0(file_out_path,"pred_counter.txt"),"rt", blocking=FALSE)
  g_w_pr <- readLines(con, encoding="UTF-8")
  close(con)
  return (g_w_pr)
}

getCompletedCount <- function(){
  con <-  file(paste0(file_out_path,"comps_counter.txt"),"rt", blocking=FALSE)
  g_w_comp <- readLines(con, encoding="UTF-8")
  close(con)
  return (g_w_comp)
}

setCompletedCount(words_comp)
setPredictedCount(words_pred)


setTextSampleSize <- function(s_txt_size){
  
  con <-  file(paste0(file_out_path,"samp_size.txt"),"w", blocking=FALSE)
  writeLines(as.character(s_txt_size), con)
  close(con)
  
  
}

setSourceText <- function(source) {
  con <-  file(paste0(file_out_path,"source.txt"),"w", blocking=FALSE)
  writeLines(as.character(source), con)
  close(con)
  
}

getSourceText <- function() {
  con <-  file(paste0(file_out_path,"source.txt"),"rt", blocking=FALSE)
  g_Source1 <- readLines(con, encoding="UTF-8")
  close(con)
  return (g_Source1)
}

getTextSampleSize <- function(){
  
  con <-  file(paste0(file_out_path,"samp_size.txt"),"rt", blocking=FALSE)
  g_s_txt_size <- readLines(con, encoding="UTF-8")
  close(con)
  
  return (g_s_txt_size)
}



myclean <- function(docs){
  #print(length(docs))
  for (i in 1:length(docs)){
    docs[i] <- iconv(docs[i], to = "ASCII//TRANSLIT")  
    docs[i] <- gsub( "[[:punct:]]", " ", docs[i])
    docs[i] <- gsub( "[[:graph:]]", " ", docs[i])
    docs[i] <- gsub( "[[:cntrl:]]", " ", docs[i])
    docs[i] <- gsub( "[^[:alnum:]]", " ", docs[i])
    docs[i] <- gsub("[^[:alnum:][:space:]\']", "",docs[i])
    docs[i] <- gsub("\\s+"," ",docs[i])
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


writeSampleText <- function(s_text){
  
  if (file.exists(paste0(file_out_path,"sample.txt"))== TRUE){
    fname <-paste0(file_out_path,"sample.txt")
    file.remove(fname)   
  }
  
  con <-  file(paste0(file_out_path,"sample.txt"),"w", blocking=FALSE)
  writeLines(as.character(s_text), con)
  close(con)
}

readSampleText <- function() {
  
  con <-  file(paste0(file_out_path,"sample.txt"),"rt", blocking=FALSE)
  sample_t <- readLines(con, encoding="UTF-8")
  close(con)
  
  return (sample_t)
}


# write the sampled data in to file for reading
setSource <- function (source="blogs") {
  
  #if ((file.exists(paste0(file_out_path,"source.txt"))== TRUE) && 
  #    (file.exists(paste0(file_out_path,"samp_size.txt"))== TRUE)){
    
 #   g_Source <- getSourceText()
  #  g_s_txt_size <- getTextSampleSize()

 # }
  
  if (file.exists(paste0(file_out_path,"source.txt"))== TRUE) {
    
    g_Source <- getSourceText()
    #  g_s_txt_size <- getTextSampleSize()
    
  }

#  print ("Source Value")
 # print(source)
  
 # print("g_Source Value")
 # print (g_Source)
  
#  if( ((g_Source != source) == TRUE) || ((g_s_txt_size != s_txt_size)== TRUE)){
  if ( (g_Source != source) == TRUE){
    if (source == "news" ){
      #s_text <- news
      g_Source <- "news"
 
      
    }
    else if (source == "tweets"){
      #s_text <- TL
      g_Source <- "tweets"
  

    }
    else {
      #s_text <- blogs
      g_Source <- "blogs"
 
    } 

    #writeSampleText(s_text)
    setSourceText(g_Source)
   # setTextSampleSize(s_txt_size)
    
    return (TRUE)
    
  }

  return (FALSE)

}



# reading the sample lines from sample source files 
getSource <- function() {
  
  g_Source1 <- getSourceText()
  
 # print(g_Source1)
  
  #sample_t <- readSampleText()
  
  if (g_Source1 == "news")
  {
    sample_t <- news
    
  }else if (g_Source1 == "tweets")
  {
    sample_t <- TL
    
  }else {
    sample_t <- blogs
  }
 
#  print(sample_t)
  
 
  # Sample Size taken for analysis
  #wordcount(sample_t)
  #print(wordcount(sample_t))
  #length(sample_t)
  vs <-  VectorSource(sample_t)
  return (vs)
  
}


# make unigrams from the sample source
getCorpusUnigramTDM <- function(vs) {
  
  
  
  #vs <- VectorSource(blogs)
  
  read_val <- Corpus(vs,  
                     readerControl = list(reader = readPlain,  language = "en",  load = TRUE))
  toSpace <- content_transformer(function(x, pattern) {return (gsub(pattern," ",
                                                                    x))})
  
  read_val<- tm_map(read_val,toSpace,"[^[:graph:]]")
  read_val <- tm_map(read_val, removePunctuation)
  read_val <- tm_map(read_val, removeNumbers)
  read_val <- tm_map(read_val, stripWhitespace)
  #read_val <- tm_map(read_val, removeWords, stopwords("english"))
  read_val <- tm_map(read_val, content_transformer(tolower))
  #    read_val <- tm_map(read_val, stemDocument)
  readTDM <- TermDocumentMatrix(read_val)
  
  return (readTDM)
  
}


# Make bigrams or trigrams from the source
getCorpusNgramTDM <- function(vs, y=2){
  

  
  if(!(y == 2 || y==3  || y==4|| y==5)){return (NULL)}
  
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
  if (y==5) {
    Ngram<-function(x) NGramTokenizer(x,Weka_control(min=5,max=5))
  }
  
  Ngramtab<-TermDocumentMatrix(read_val,control=list(tokenize=Ngram))
  return (Ngramtab)
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

writecsvfile <- function(dataset, fname) {
  
#  print(head(dataset, n=6))
 # print("in writecsv file")
 # print(fname)
  if (file.exists(fname)== TRUE) {
    file.remove(fname)
  }
#  print(fname)
 # print(head(dataset,n=6))
  write.csv(dataset, fname)
  
  
}

readcsvfile <- function(fname) {
  
  readval <- read.csv(fname, header=TRUE, sep=",")[, -1]
 # print(head(readval, n=6))
  return (readval)
}

CreateNgrams <-  function() {
  
  #print("In createngrams")
  
  currentGetSource <- reactive ({
    vs <-  getSource()  
    return (vs)
    
  })
  
  
  vs <-currentGetSource()

  
  
  unigram.tdm <- getCorpusUnigramTDM(vs)
  
  #print(unigram.tdm$ncol)
  #print(unigram.tdm$nrow)
  
  DocMat <- (unigram.tdm$dimnames$Terms)
  
 # print(length(DocMat))
  v <- rep(1, length(DocMat) )
  
  #v <- sort(summary(DocMat),decreasing=TRUE)
  d <- data.frame(word = DocMat,freq=v)
  #print(head(d, n=20))
  
  #rownames(d) <- 1: nrow(d)
  
  fileN <- paste0(file_out_path, "unigram.csv")
  writecsvfile(d, fileN)
  
  rm(v)
  rm(d)
  rm(DocMat)
  rm(unigram.tdm)
  
  
  bigram.tdm <- getCorpusNgramTDM(vs,2)
  
  
  DocMat <- (bigram.tdm$dimnames$Terms)
  
  #print(length(DocMat))
  v <- rep(1, length(DocMat) )
  d <- data.frame(word = DocMat,freq=v)
  
  
  #DocMat <- as.factor(as.matrix(bigram.tdm$dimnames$Terms))
  
  #v <- sort(summary(DocMat),decreasing=TRUE)
  #d <- data.frame(word = names(v),freq=v)
  #print(head(d, n=20))
  
  rownames(d) <- 1: nrow(d)

  fileN <- paste0(file_out_path, "bigram.csv")
  writecsvfile(d, fileN)
  
  rm(v)
  rm(d)
  rm(DocMat)
  rm(bigram.tdm)
  
  
  trigram.tdm <- getCorpusNgramTDM(vs, 3)
  DocMat <- (trigram.tdm$dimnames$Terms)
  
  #print(length(DocMat))
  v <- rep(1, length(DocMat) )
  d <- data.frame(word = DocMat,freq=v)
  
  #DocMat <- as.factor(as.matrix(trigram.tdm$dimnames$Terms))
  
  #v <- sort(summary(DocMat),decreasing=TRUE)
  #d <- data.frame(word = names(v),freq=v)
  # print(head(d, n=20))
  
  rownames(d) <- 1: nrow(d)
  
  fileN <- paste0(file_out_path, "trigram.csv")
  writecsvfile(d, fileN)
  
  
  rm(v)
  rm(d)
  rm(DocMat)
  rm(trigram.tdm)
  
  quadgram.tdm <- getCorpusNgramTDM(vs, 4)
  
  DocMat <- (quadgram.tdm$dimnames$Terms)
  
  #print(length(DocMat))
  v <- rep(1, length(DocMat) )
  d <- data.frame(word = DocMat,freq=v)
  
  #DocMat <- as.factor(as.matrix(quadgram.tdm$dimnames$Terms))
  
  #v <- data.frame((sort(summary(DocMat),decreasing=TRUE)))
  #d <- data.frame(word = rownames(v),freq=v)
  #print(head(d, n=20))
  
  #rownames(d) <- 1: nrow(d)
  
  fileN <- paste0(file_out_path, "quadgram.csv")
  writecsvfile(d, fileN)
  
  
  rm(v)
  rm(d)
  rm(DocMat)
  rm(quadgram.tdm)
  
  
  pentagram.tdm <- getCorpusNgramTDM(vs,5)
  
  DocMat <- (pentagram.tdm$dimnames$Terms)
  
  #print(length(DocMat))
  v <- rep(1, length(DocMat) )
  d <- data.frame(word = DocMat,freq=v)
  
  #DocMat <- as.factor(as.matrix(pentagram.tdm$dimnames$Terms))
  
  #v <- sort(summary(DocMat),decreasing=TRUE)
  #d <- data.frame(word = names(v),freq=v)
  #print(head(d, n=20))
  
  rownames(d) <- 1: nrow(d)
  
  
  fileN <- paste0(file_out_path, "pentagram.csv")
  writecsvfile(d, fileN)
  
  rm(v)
  rm(d)
  rm(DocMat)
  rm(pentagram.tdm)
  
}

# samp_size
sample_txt_size <- 0
setTextSampleSize(sample_txt_size)

setSourceText("none")
checktoCreate <- FALSE

g_Source <- getSourceText()


# Define server logic required to draw a histogram
shinyServer(function(input, output) {

  # function to clean the input 
  inputclean <- function(inptxt){
    
    #removing extra white space
    inptxt <- tolower(inptxt)
   # inptxt <- gsub("\\s+"," ",inptxt)
    
    #print(inptxt)
    
    if (wordcount(inptxt)>= 5){
      output$text2 <- renderPrint({ "Words exceeding the limit, Kindly give words less than or equal to 4 in the text"})
      return (NULL)
    }
    
    if (grepl("\\*", inptxt)){
      output$text1 <- renderPrint({"Text contains asterik partial regular exp functionality implemented"})
      inptxt <- gsub("\\*", "", inptxt)
     # print(inptxt)
      
    }
    inptxt <- removePunctuation(inptxt,
                      preserve_intra_word_contractions = FALSE,
                      preserve_intra_word_dashes = FALSE
                      )
    #print("after tm removepunct")
    #print(inptxt)
    inptxt <- gsub("[^[:graph:]]", " ", inptxt)
    #removing extra white space
    inptxt <- gsub("\\s+"," ",inptxt)
    
    if (grepl("[[:punct:]]", inptxt)) {
      output$text2 <- renderPrint({"Punctionations shall be removed and the generic text alone shall be considered"})
      inptxt <- gsub("[[:punct:]]", "", inptxt )
      #removing extra white space
      #inptxt <- gsub("\\s+"," ",inptxt)
    }
    if (grepl("[[:digit:]]", inptxt)) {
      output$text2 <- renderPrint({ "Digits shall be removed, and the generic text alone shall be considered"})
      inptxt <- gsub("[[:digit:]]", "", inptxt )
    
      inptxt <- gsub("\\s+"," ",inptxt)
      inptxt <- gsub("[^[:alnum:][:space:]\']", "",inptxt)
      
      
    }
   
    inptxt <- trimws(inptxt)
    #print("after white space removal")
    #print(inptxt)
    return (inptxt)
    
  }
  # predict function to predict the text
  predict_ngram <- function(text, source="blogs") {
 
    inptxt <- tolower(text)  
    #print(text)
    text <- inputclean(text)
    if (is.null(text)){
      
      return (NULL)
    }
    
    values <- reactiveValues(readtable=NULL)
    readUnigram <- reactive({
      fname <- paste0(file_out_path, "unigram.csv")
      values$readtable <- readcsvfile(fname)
    })
    readBigram <- reactive({
      fname <- paste0(file_out_path, "bigram.csv")
      values$readtable <- readcsvfile(fname)
    })
    
    readTrigram <- reactive({
      fname <- paste0(file_out_path, "trigram.csv")
      values$readtable <- readcsvfile(fname)
    })
    
    readQuadgram <- reactive({
      fname <- paste0(file_out_path, "quadgram.csv")
      values$readtable <- readcsvfile(fname)
    })
    readPentagram <- reactive({
      fname <- paste0(file_out_path, "pentagram.csv")
      values$readtable <- readcsvfile(fname)
    })
    
    
        if (wordcount(text) == 1){
        if( grepl("\\*", inptxt)){
          readtable <- readUnigram()
        } else {
          readtable <- readBigram()
        }
      }
      if (wordcount(text)== 2){
        #check in trigram table
        if (grepl("\\*",inptxt)){
          readtable <- readBigram()
        }
        else {
          readtable <-readTrigram()
        }
      }
      if (wordcount(text)== 3){
 
        if (grepl("\\*",inptxt)){
          readtable <- readTrigram()
        }
        else {
          readtable <- readQuadgram()
          
        }
      }
    if (wordcount(text)== 4){

      if (grepl("\\*",inptxt)){
        readtable <- readQuadgram()
      }
      else {
        readtable <-readPentagram()
        
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
  
  output$Out_Plot <- renderPlot ({
   
    output$text1<- renderText({NULL})
    output$text2<- renderText({NULL})
    output$text3 <- renderText({NULL})
    output$text4 <- renderText({NULL})
    output$Out_Table3 <- renderTable({NULL})

    #samp_size <- input$samp_size
    text_type <- input$text_type
    text_pred  <- input$inptext
    
    currentSet <- reactive({
       checktoCreate <-  setSource(input$text_type)
    })
    
    checktoCreate <- currentSet()
    
    currentSetNgrams <- reactive({
        CreateNgrams()
    })
    
    if (checktoCreate == TRUE){
      currentSetNgrams()
    }
    
    
    currentObs <- reactive({
        ptext <- predict_ngram(input$inptext, input$text_type)
    })
    ptext <- currentObs()

 #   print (ptext)
    if(is.null(ptext)){
      output$text1 <- renderPrint({ "No valid match found for the given input"})
      return (NULL)
    }
  
    ptext
    t <- as.data.frame(ptext)
    
    t$word <- factor(t$word, levels=t$word[order(t$freq, decreasing =TRUE) ])
    t$word_predicted <- word(t$word, -1)
    if (nrow(t)== 1)
    {
      output$text3     <- renderText({"Suggested Word:"})  
      output$Out_Table <- renderTable({t[, -2]})
    }
    else
    {
      output$text3     <- renderText({"Suggested Word:"})  
      output$Out_Table <- renderTable({t[1,-2]})
      output$text4     <- renderText({"Optional Words to Explore:"})  
      output$Out_Table3 <- renderTable({t[-1,-2]})
    }

  
    words_pred <- getPredictedCount()
    words_comp <- getCompletedCount()
    word_stats <- data.frame("WordsPredicted"=words_pred, "WordsCompleted"=words_comp)
    output$Out_Table2 <- renderTable({word_stats })
   
 
     ggplot(t, aes(x=(word), y=freq))+geom_bar(stat = "identity")+
        ggtitle("Predicted words")+
        theme(axis.text.x=element_text(angle=90,hjust=1))
    
  })
 
  
  
})
  
