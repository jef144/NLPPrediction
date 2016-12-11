#Week 2 Exploratory and milestone
#library("RWeka")
library("tm")   
library(ngram) 
library(parallel)
library(data.table)
    
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

#Week three stuff -- convert to data tables and build probabilites
#Eventually store these as RDS files

dtBigram <-as.data.table(as.matrix(dtmBigram), keep.rowname = TRUE)

#Build a new column that sums the three documents
#dt$newval <- rowSums(dt[,.(en_US.blogs.txt, en_US.twitter.txt, en_US.news.txt)])
dtBigram$newval <- rowSums(dtBigram[,.(en_US.blogs.txt, en_US.twitter.txt, en_US.news.txt)])



#Isolate the first and last words
dtBigram$pre_gram <- str_trim(str_replace(dtBigram$rn, "\\S+$", ""))
dtBigram$post_gram <- str_extract(dtBigram$rn, "\\S+$")


#key the pre-column
setkey(dtBigram, pre_gram)

#build anothew data table with gorup-by syntax
#TODO :clean up first thtree rows
dtBiGroup <-dtBigram[3:.N, sum(newval), by=pre_gram]
setkey(dtBiGropu, pre_gram)

#Do a join to pick up the total number of pre_grams.  Then divide the counts of each post_gram by the grouped pre_gram
# and populate a new column, prop
#Logically, we do not expect any zero values in the group-by expression, so no divide by zero erros
dtBigram$prob <- dtBigram$newval / dtBigram[dtBigramGroup]$V1

