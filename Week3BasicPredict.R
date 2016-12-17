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

 predictWord <-  function(pre_grams) {
  
   #Apply the same logic to the search parameters as use for the corpus
   corpusSearch <- Corpus(VectorSource(pre_grams))
   corpusSearch <- tm_map(corpusSearch, removeNumbers)
   corpusSearch <- tm_map(corpusSearch, removePunctuation)
   corpusSearch <- tm_map(corpusSearch, content_transformer(tolower) )
   corpusSearch <- tm_map(corpusSearch, removeWords, stopwords("english")) 
   #corpusSearch <- tm_map(corpusSearch, stemDocument, language = "english") 
   corpusSearch <- tm_map(corpusSearch , stripWhitespace)
   pre_grams1 <- corpusSearch[[1]]$content
   pre_grams1 <- str_trim(pre_grams1)               
    
    
   
  #Isolate the last three words
  threeWords = str_extract(pre_grams1, '\\S+ \\S+ \\S+$')
  dtPred <-dt4gram[pre_gram == threeWords]
  #setorder(dtPred, -prob)
  setorder(dtPred, -prob, -totAll)
  top4Hits <- head(dtPred , 9)
  
  #fall back to two word predictor
  twoWords = str_extract(pre_grams1, '\\S+ \\S+$')
  dtPred <-dtTrigram[pre_gram == twoWords]
  setorder(dtPred, -prob, -totAll)
  topTriHits <- head(dtPred , 9)

  oneWord = str_extract(pre_grams1, '\\S+$')
  dtPred <-dtBigram[pre_gram == oneWord]
  setorder(dtPred, -prob, -totAll)
  topBiHits <- head(dtPred ,9)
  
  return(list(pre_grams,  pre_grams1,  top4Hits$post_gram,  top4Hits$prob ,topTriHits$post_gram,  topTriHits$prob , topBiHits$post_gram, topBiHits$prob))
  
   
   
}

predictWord("united states")
