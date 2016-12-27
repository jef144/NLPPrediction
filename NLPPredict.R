#Week 2 Exploratory and milestone
#library("RWeka")
library("tm")   
#library(ngram) 
#library(parallel)
library(data.table)
library(stringr)
      
#Get to the file
setwd("~/NLPCapstone")
#dtBigram <- readRDS( file="NLP.dtBigram.RDS")

#Bring the data.tables into memory
if (file.exists("NLP.dtBigram.RDS")) {
  dtBigram <- readRDS(file="NLP.dtBigram.RDS")
}  else  {  stop('Err, could not find NLP.dtBigram.RDS')} 

if (file.exists("NLP.dtTrigram.RDS")) {
  dtTrigram <- readRDS(file="NLP.dtTrigram.RDS")
}  else  {  stop('Err, could not find NLP.dtTrigram.RDS')} 

if (file.exists("NLP.dt4gram.RDS")) {
  dt4gram <- readRDS(file="NLP.dt4gram.RDS")
}  else  {  stop('Err, could not find NLP.dt4gram.RDS')} 

if (file.exists("NLP.dt5gram.RDS")) {
  dt5gram <- readRDS(file="NLP.dt5gram.RDS")
}  else  {  stop('Err, could not find NLP.dt5gram.RDS')} 


#Define the precition function
 predictWord <-  function(pre_grams) {
  
   #Results of prediction are a dataframe with the preducted word and a  ranked indicated of
   # where in the fallback sequence the word was found
 
   
   #Apply the same logic to the search parameters as use for the corpus
   corpusSearch <- Corpus(VectorSource(pre_grams))
   corpusSearch <- tm_map(corpusSearch, removeNumbers)
   corpusSearch <- tm_map(corpusSearch, removePunctuation)
   corpusSearch <- tm_map(corpusSearch, content_transformer(tolower) )
   corpusSearch <- tm_map(corpusSearch, stemDocument, language = "english") 
   corpusSearch <- tm_map(corpusSearch , stripWhitespace)
   pre_grams1 <- corpusSearch[[1]]$content
   pre_grams1 <- str_trim(pre_grams1)               
    
   #Establish an integer counter for fall back sequence, and an empty result data frame
   fallbackSeq = as.integer(0)
   dfResult <- data.frame(post_gram = character(), fallback = integer())
   
   
   #Isolate the last four words
   fourWords = str_extract(pre_grams1, '\\S+ \\S+ \\S+ \\S+$') 
   #Look for match in the same sequence
   dtPred <-dt5gram[pre_gram == fourWords]
   #Pleace in descending order of the pregram, and if a tie, find the most-used postgram word
   setorder(dtPred, -prob, -totAll)
   fallbackSeq <- fallbackSeq + 1
   if (nrow(dtPred) > 0) {
      dfResult <- rbind( dfResult, as.data.frame(cbind(head(dtPred[,post_gram], 3), fallbackSeq)))
   }   
   
 if (nrow(dfResult)  < 7 )   {
    #Try the 5gram Bag appraoch, need that all the words be found but ANY sequence is OK -- cf Bag of Words conccept
     chrWords <- str_split(fourWords, " ",  simplify=TRUE)
     #dtPred <-dt4gram[pre_gram %like% chrWords[1]  & pre_gram %like% chrWords[2] & pre_gram %like% chrWords[3] ]  
     regSearch = paste ('(?=.*', chrWords, ')', sep="", collapse="")
     dtPred <-dt5gram[grep(regSearch, pre_gram,  perl=TRUE)] 
     setorder(dtPred,  -totAll)
     fallbackSeq <- fallbackSeq + 1
     if (nrow(dtPred) > 0) {
       dfResult <- rbind( dfResult, as.data.frame(cbind(head(dtPred[,post_gram], 3), fallbackSeq)))
       dfResult <- dfResult[!duplicated(dfResult[,c('V1' )]),]
     }      
 } 
    
 if (nrow(dfResult)  < 7 )   {  
      #Isolate the last three words
      threeWords = str_extract(pre_grams1, '\\S+ \\S+ \\S+$')
      #Look for match in the same sequence
      dtPred <-dt4gram[pre_gram == threeWords]
      #setorder(dtPred, -prob)
      setorder(dtPred, -prob, -totAll)
      fallbackSeq <- fallbackSeq + 1
      if (nrow(dtPred) > 0) {
        dfResult <- rbind( dfResult, as.data.frame(cbind(head(dtPred[,post_gram], 3), fallbackSeq)))
        dfResult <- dfResult[!duplicated(dfResult[,c('V1' )]),]
      }   
 }
      
 if (nrow(dfResult)  < 7 )   {      
      #Try the 4gram appraoch, need that all the words be found but ANY sequence is OK -- cf Bag of Words conccept
      chrWords <- str_split(threeWords, " ",  simplify=TRUE)
      #dtPred <-dt4gram[pre_gram %like% chrWords[1]  & pre_gram %like% chrWords[2] & pre_gram %like% chrWords[3] ]  
      regSearch = paste ('(?=.*', chrWords, ')', sep="", collapse="")
      dtPred <-dt4gram[grep(regSearch, pre_gram,  perl=TRUE)] 
      setorder(dtPred,  -totAll)
      fallbackSeq <- fallbackSeq + 1
      if (nrow(dtPred) > 0) {
        dfResult <- rbind( dfResult, as.data.frame(cbind(head(dtPred[,post_gram], 3), fallbackSeq)))
        dfResult <- dfResult[!duplicated(dfResult[,c('V1' )]),]
      }   
 }    
  
   
if (nrow(dfResult)  < 7 )   {          
    #fall back to two word predictor
    twoWords = str_extract(pre_grams1, '\\S+ \\S+$')
    dtPred <-dtTrigram[pre_gram == twoWords]
    setorder(dtPred, -prob, -totAll)
    fallbackSeq <- fallbackSeq + 1
    if (nrow(dtPred) > 0) {
      dfResult <- rbind( dfResult, as.data.frame(cbind(head(dtPred[,post_gram], 3), fallbackSeq)))
      dfResult <- dfResult[!duplicated(dfResult[,c('V1' )]),]
    }   
}    
   
   
if (nrow(dfResult)  < 7 )   {  
      oneWord = str_extract(pre_grams1, '\\S+$')
      dtPred <-dtBigram[pre_gram == oneWord]
      setorder(dtPred, -prob, -totAll)
      fallbackSeq <- fallbackSeq + 1
      if (nrow(dtPred) > 0) {
        dfResult <- rbind( dfResult, as.data.frame(cbind(head(dtPred[,post_gram], 3), fallbackSeq)))
        dfResult <- dfResult[!duplicated(dfResult[,c('V1' )]),]
      }   
}
   
   
  return(dfResult )
  
   
   
}

 predictWord("a back seat to")
 