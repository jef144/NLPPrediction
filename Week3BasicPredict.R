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

 predictWord <-  function(pre_grams) {
  
   #Apply the same logic to the search parameters as use for the corpus
   corpusSearch <- Corpus(VectorSource(pre_grams))
   corpusSearch <- tm_map(corpusSearch, removeNumbers)
   corpusSearch <- tm_map(corpusSearch, removePunctuation)
   corpusSearch <- tm_map(corpusSearch, content_transformer(tolower) )
   corpusSearch <- tm_map(corpusSearch, removeWords, stopwords("english")) 
   corpusSearch <- tm_map(corpusSearch, stemDocument, language = "english") 
   corpusSearch <- tm_map(corpusSearch , stripWhitespace)
   pre_grams1 <- corpusSearch[[1]]$content
   pre_grams1 <- str_trim(pre_grams1)               
    
   
   
   #Isolate the last four words
   fourWords = str_extract(pre_grams1, '\\S+ \\S+ \\S+ \\S+$')
   dtPred <-dt5gram[pre_gram == fourWords]
   #setorder(dtPred, -prob)
   setorder(dtPred, -prob, -totAll)
   top5Hits <- head(dtPred , 9)
   
   #Try the 5gram appraoch, need that all the words be found but ANY sequence is OK -- cf Bag of Words conccept
   chrWords <- str_split(fourWords, " ",  simplify=TRUE)
   #dtPred <-dt4gram[pre_gram %like% chrWords[1]  & pre_gram %like% chrWords[2] & pre_gram %like% chrWords[3] ]  
   regSearch = paste ('(?=.*', chrWords, ')', sep="", collapse="")
   dtPred <-dt5gram[grep(regSearch, pre_gram,  perl=TRUE)] 
   setorder(dtPred,  -totAll)
   topBag5Hits <- head(dtPred , 9) 
    
   
  #Isolate the last three words
  threeWords = str_extract(pre_grams1, '\\S+ \\S+ \\S+$')
  dtPred <-dt4gram[pre_gram == threeWords]
  #setorder(dtPred, -prob)
  setorder(dtPred, -prob, -totAll)
  top4Hits <- head(dtPred , 9)
  
  #Try the 4gram appraoch, need that all the words be found but ANY sequence is OK -- cf Bag of Words conccept
  chrWords <- str_split(threeWords, " ",  simplify=TRUE)
  #dtPred <-dt4gram[pre_gram %like% chrWords[1]  & pre_gram %like% chrWords[2] & pre_gram %like% chrWords[3] ]  
  regSearch = paste ('(?=.*', chrWords, ')', sep="", collapse="")
  dtPred <-dt4gram[grep(regSearch, pre_gram,  perl=TRUE)] 
  setorder(dtPred,  -totAll)
  topBag4Hits <- head(dtPred , 9)
  
  #fall back to two word predictor
  twoWords = str_extract(pre_grams1, '\\S+ \\S+$')
  dtPred <-dtTrigram[pre_gram == twoWords]
  setorder(dtPred, -prob, -totAll)
  topTriHits <- head(dtPred , 9)

  oneWord = str_extract(pre_grams1, '\\S+$')
  dtPred <-dtBigram[pre_gram == oneWord]
  setorder(dtPred, -prob, -totAll)
  topBiHits <- head(dtPred ,9)
  
  return(list(pre_grams,  pre_grams1, top5Hits$post_gram, topBag5Hits$post_gram,  top4Hits$post_gram,  topBag4Hits$post_gram ,topTriHits$post_gram,   topBiHits$post_gram ))
  
   
   
}

predictWord("united states america captain")