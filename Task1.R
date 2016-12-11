#Week 2 Exploratory and milestone
#library("RWeka")
library("tm")   
library(ngram) 
library(parallel)
    
#Get to the file
setwd("~/NLPCapstone")
#Access the data.  Note that the subset folder has small files
folder <-  "~/NLPCapstone/final/en_US/subset" 

#Determine if we need to read the corpus text files, or just re-load the corpus object
if ( file.exists("NLP.task1.RDS")) { 
    task1 <- readRDS(file="NLP.task1.RDS")
}  else  {
 
    #Access the data.  Note that the subset folder has small files
    (task1 <- VCorpus(DirSource(folder, encoding = "UTF-8"),
                        readerControl= list(language="english")) )
    task1 <- tm_map(task1, removeNumbers)
    gc() 
    task1 <- tm_map(task1, removePunctuation)
    gc()
    task1 <- tm_map(task1 , stripWhitespace)
    gc()
    #task1 <- tm_map(task1, tolower)
    task1 <- tm_map(task1, removeWords, stopwords("english")) 
    gc()
    #task1 <- tm_map(task1, stemDocument, language = "english") 
    saveRDS(task1, file="NLP.task1.RDS")
} 
#end of reading task1 if no RDS file found
if (file.exists("NLP.dtm1.RDS")) {
  dtm1 <- readRDS(file="NLP.dtm1.RDS")
}  else  {
    dtm1 <- DocumentTermMatrix(task1)
    gc()
    saveRDS(dtm1, file="NLP.dtm1.RDS")
    dtm1 <-removeSparseTerms(dtm1, 0.75) 
    gc()
    saveRDS(dtm1, file="NLP.dtm1.RDS")
}

#inspect(dtm1[1:3, 1000:1020])
 
    
#Progress to bigrams andn
 
#BigramTokenizer <-  function(x) NGramTokenizer(x, Weka_control(min = 2, max = 2, sep = " "))
#BigramTokenizer <-  function(x) NGramTokenizer(x, Tokenize-AsWeka(min = 2, max = 2, sep = " "))
BigramTokenizer <-
  function(x)
    unlist(lapply(ngrams(words(x), 2), paste, collapse = " "), use.names = FALSE)
#TrigramTokenizer <- function(x)  NGramTokenizer(x, Weka_control(min = 3, max = 3))
TrigramTokenizer <- 
  function(x)
  unlist(lapply(ngrams(words(x), 3), paste, collapse = " "), use.names = FALSE)

gc()
if (file.exists("NLP.dtmBigram.RDS")) {
  dtmBigram <- readRDS(file="NLP.dtmBigram.RDS")
}  else  {
    dtmBigram <- TermDocumentMatrix(task1, control = list(tokenize = BigramTokenizer))
    m
    saveRDS(dtmBigram, file="NLP.dtmBigram.RDS")
} 
 
gc()
if (file.exists("NLP.dtmTrigram.RDS")) {
  dtmTrigram <- readRDS(file="NLP.dtmTrigram.RDS")
}  else  {
    dtmTrigram <- TermDocumentMatrix(task1, control = list(tokenize = TrigramTokenizer ))
    dtmTrigram <- removeSparseTerms(dtmTrigram, .5)
    saveRDS(dtmTrigram, file="NLP.dtmTrigram.RDS")
} 

inspect(dtmBigram[340:345,1:3])
inspect(dtmTrigram[340:345,1:3]) 
