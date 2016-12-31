?#Week 2 Exploratory and milestone
#library("RWeka")
library(tm)  
library('tm')
library(SnowballC)
#library(ngram) 
#library(parallel)
library(data.table)
library(stringr)
      
#Get to the file
#setwd("~/NLPCapstone")
#dtBigram <- readRDS( file="NLP.dtBigram.RDS")

RESULT_LIMIT=14
HEAD_LIMIT = 3
FALLBACK_DISCOUNT = .4

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

   #Go farther and get just the significant words
   corpusSearch <- tm_map(corpusSearch, removeWords, stopwords("english"))
   pre_gramsNoStop <- corpusSearch[[1]]$content
   pre_gramsNoStop <- str_trim(pre_gramsNoStop)     
     
   #Establish an integer counter for fall back sequence, and an empty result data frame
   fallbackSeq = as.integer(0)
   dfResult <- data.frame(post_gram = character(), 
                          prob = numeric(), 
                          fallbackSeq = integer(), 
                          fallbackName = character(), 
                          stringsAsFactors = FALSE)
   
   
   #Isolate the last four words for dt5gram
   caseName <- "dt5gram"
   fourWords = str_extract(pre_grams1, '\\S+ \\S+ \\S+ \\S+$') 
   #Look for match in the same sequence
   dtPred <-dt5gram[pre_gram == fourWords]
   #Pleace in descending order of the pregram, and if a tie, find the most-used postgram word
   setorder(dtPred, -prob, -totAll)
   #Adjust the probabilty to reflect the post_gram popularity and the fallback position
   #dtPred[, prob] := dtPred[, prob] * FALLBACK_DISCOUNT^fallbackSeq
   fallbackSeq <- fallbackSeq + 1
   if (nrow(dtPred) > 0) {
      dfResult <- rbind( dfResult, as.data.frame(cbind(head(dtPred[,post_gram, prob], 3), fallbackSeq, caseName), stringAsFactors=False))
   }   
   
     
  #Target 4 grams 
   caseName <- "dt4gram"
 if (nrow(dfResult)  < RESULT_LIMIT )   {  
      #Isolate the last three words
      threeWords = str_extract(pre_grams1, '\\S+ \\S+ \\S+$')
      #Look for match in the same sequence
      dtPred <-dt4gram[pre_gram == threeWords]
      #setorder(dtPred, -prob)
      setorder(dtPred, -prob, -totAll)
      fallbackSeq <- fallbackSeq + 1
      if (nrow(dtPred) > 0) {
        dfResult <- rbind( dfResult, as.data.frame(cbind(head(dtPred[,post_gram, prob], HEAD_LIMIT), fallbackSeq, caseName)))
        dfResult <- dfResult[!duplicated(dfResult[,'post_gram']),]
      }   
 }
     
   #Trigram
   caseName <- "dtTrigram"
   if (nrow(dfResult)  < RESULT_LIMIT )   {  
     twoWords = str_extract(pre_grams1, '\\S+ \\S+$')
     dtPred <-dtTrigram[pre_gram == twoWords]
     setorder(dtPred, -prob, -totAll)
     fallbackSeq < - fallbackSeq + 1
     if (nrow(dtPred) > 0) {
       dfResult <- rbind( dfResult, as.data.frame(cbind(head(dtPred[,post_gram, prob], HEAD_LIMIT), fallbackSeq, caseName)))
       dfResult <- dfResult[!duplicated(dfResult[,'post_gram']),]
     }   
   }
   
   
   
   
#Get some non-stopwords and search for them in ANY  seqeunce in the 5 grams
   caseName <- "dt5gramBag"
   if (nrow(dfResult)  < RESULT_LIMIT )   {
     #Try the 5gram Bag appraoch, need that all the words be found but ANY sequaence is OK -- cf Bag of Words conccept
     chrWords <- t(str_split(pre_gramsNoStop, "\\s+",  simplify=TRUE))
     chrWords <- tail(chrWords, 3)
     #chrWords <- word(pre_gramsNoStop)
     #dtPred <-dt4gram[pre_gram %like% chrWords[1]  & pre_gram %like% chrWords[2] & pre_gram %like% chrWords[3] ]  
     regSearch = paste ('(?=.*', chrWords, ')', sep="", collapse="")
     dtPred <-dt5gram[grep(regSearch, pre_gram,  perl=TRUE)] 
     setorder(dtPred,  -totAll)
     fallbackSeq <- fallbackSeq + 1
     if (nrow(dtPred) > 0) {
       dfResult <- rbind( dfResult, as.data.frame(cbind(head(dtPred[,post_gram, prob], HEAD_LIMIT), fallbackSeq, caseName)))
       dfResult <- dfResult[!duplicated(dfResult[,'post_gram']),]
     }      
   } 
   
   
    
 #Get some non-stopwords and search for them in ANY  seqeunce in the 4 grams
   caseName <- "dt4gramBag"
 if (nrow(dfResult)  < RESULT_LIMIT )   {      
      #Try the 4gram appraoch, need that all the words be found but ANY sequence is OK -- cf Bag of Words conccept
      chrWords <- t(str_split(pre_gramsNoStop, "\\s+",  simplify=TRUE))
      chrWords <- tail(chrWords, 2)
      #dtPred <-dt4gram[pre_gram %like% chrWords[1]  & pre_gram %like% chrWords[2] & pre_gram %like% chrWords[3] ]  
      regSearch = paste ('(?=.*', chrWords, ')', sep="", collapse="")
      dtPred <-dt4gram[grep(regSearch, pre_gram,  perl=TRUE)] 
      setorder(dtPred,  -totAll)
      fallbackSeq <- fallbackSeq + 1
      if (nrow(dtPred) > 0) {
        dfResult <- rbind( dfResult, as.data.frame(cbind(head(dtPred[,post_gram, prob], HEAD_LIMIT), fallbackSeq, caseName)))
        dfResult <- dfResult[!duplicated(dfResult[,'post_gram']),]
      }   
 }    
 

   
#Bigram
caseName <- "dtBigram"
if (nrow(dfResult)  < RESULT_LIMIT )   {  
      oneWord = str_extract(pre_grams1, '\\S+$')
      dtPred <-dtBigram[pre_gram == oneWord]
      setorder(dtPred, -prob, -totAll)
      fallbackSeq < - fallbackSeq + 1
      if (nrow(dtPred) > 0) {
        dfResult <- rbind( dfResult, as.data.frame(cbind(head(dtPred[,post_gram, prob], HEAD_LIMIT), fallbackSeq, caseName)))
        dfResult <- dfResult[!duplicated(dfResult[,'post_gram']),]
      }   
}
   
   
  return(dfResult )
  
   
   
}

# predictWord("a back seat to")
 