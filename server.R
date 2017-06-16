
#***************************************************************
#             Data Science Capstone App
#***************************************************************
library(shiny)
library(shinyjs)
library(dplyr)
library(tm)
library(tau)
library(tidyr)
library(stringr)
library(wordnet)
library(rJava)
library(qdap)
library(qdapTools)
library(qdapDictionaries)
library(rtrim)
    
# Read in sampled text for the prediction model and profanity text ***********
  options(stringsAsFactors = FALSE)
  wordpair5<-read.csv("wordpair5.csv")
  profanity<-read.csv("profanity.csv")
  as.character(profanity[,1])
  wordpair5<-filter(wordpair5,!word2 %in% as.character(profanity[,1]))
  stopwords<-stopwords("SMART")
    
#******************************************************************************
# Function to find synonym in the event that the word is not in the text sample
#******************************************************************************
synonyms_frame <- function(synonym.list, prior.frame) {
      
      synonym.list <- lapply(synonym.list, function(x) {
        if(is.list(x)) {
          x
        } else {
          list(x)
        }
      })
      phase1 <- lapply(synonym.list, lapply, paste, collapse = ", ")
      phase2 <- mapply(seqs, phase1, lapply(phase1, function(x) 1:length(x))) 
      phase3 <- list2df(lapply(phase2, paste, collapse = " @@@@ "), 
                        col2 = "word", col1 = "match.string")[2:1]
      phase3[] <- lapply(phase3, as.character)
      
      if (!missing(prior.frame)) {
        
        class(prior.frame) <- "data.frame"
        suppressWarnings(colnames(prior.frame) <- colnames(phase3))
        
        phase3 <- data.frame(rbind(phase3, 
                                   prior.frame[!prior.frame[, "word"] %in%  phase3[, "word"], ]
        ), stringsAsFactors = FALSE)
      }
      hash(phase3)
    }
    
    #' @rdname synonyms
    #' @export
    syn_frame <- synonyms_frame
    
    seqs <- function(x, y) sprintf("[%s]%s", y, x)
    
    qdap_recoder <- function(x, envr, missing){                               
      x <- as.character(x)                                                         
      sapply(x, hash_look, USE.NAMES = FALSE, envir=envr, missing = missing)                       
    }   
    
    
    rcst <- function(x) {  
      if (is.na(x)) return(NA)            
      y <- c(sapply(strsplit(x, "@@@@"), Trim))
      nms <- bracketXtract(y, "square")
      y <- bracketX(y, "square")
      names(y) <- paste0("def_", nms)
      lapply(lapply(y, strsplit, "\\,"), function(x){
        Trim(unlist(x))
      })
    }

    
    pred<-NA
    sentence<-"data"
    
#*************************************************************************************
#                              Shiny App
#*************************************************************************************
shinyServer(
  function(input, output, session){
    
    output$pred<-renderPrint({
    sentence<-tolower(word(input$word1,-1))
    
    suppressMessages(
    xsyn<-synonyms(sentence)[[1]][1]
    )
   
    preddf<-filter(wordpair5,word1==sentence)[1:3,]
    preddf2<-filter(wordpair5,word1==sentence, !word2 %in% stopwords)[1:3,]
   
    if (preddf2[1,2] %in% NA & !is.null(xsyn)){
      preddf2<-filter(wordpair5,word1==xsyn)[1:3,]}
    
    check<-as.character(preddf[,2])
    preddf2<-preddf2[,2]
    preddf<-preddf[,2]

    # Insert non-stopword next words into dataframe if empty
    j=1
    for (i in 1:3){
      if (preddf2[i] %in% NA){
        for (j in 1:3){
          if(!check[j] %in% preddf2){
            preddf2[i]<-preddf[j]
          }
        }
      }}

  

    # Insert a stopword if still NA 
    pred<-as.character(preddf2[1])
    if(pred %in% NA){pred<-sample(stopwords,1)[1]}
    pred2<-as.character(preddf2[2])
    if(pred2 %in% NA){pred2<-sample(stopwords,1)[1]}
    pred3<-as.character(preddf2[3])
    if(pred3 %in% NA){pred3<-sample(stopwords,1)[1]}
    
    # Output to the app  
    pred<-paste(pred,pred2,pred3,sep="   |   ")
    cat(pred)
    })

 session$onSessionEnded(function() {
    stopApp()
})   
     
})
    
    




