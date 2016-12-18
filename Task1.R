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
if ( file.exists("NLP.corpus1.RDS")) { 
    corpus1 <- readRDS(file="NLP.corpus1.RDS")
}  else  {
 
    #Access the data.  Note that the subset folder has small files
    (corpus1 <- VCorpus(DirSource(folder, encoding = "UTF-8"),
                        readerControl= list(language="english")) )
    corpus1 <- tm_map(corpus1, removeNumbers)
    gc() 
    corpus1 <- tm_map(corpus1, removePunctuation)
    gc()
    corpus1 <- tm_map(corpus1 , stripWhitespace)
    gc()
    corpus1 <- tm_map(corpus1, content_transformer(tolower) )
    corpus1 <- tm_map(corpus1, removeWords, stopwords("english")) 
    gc()
    corpus1 <- tm_map(corpus1, stemDocument, language = "english") 
    corpus1 <- tm_map(corpus1 , stripWhitespace)
    saveRDS(corpus1, file="NLP.corpus1.RDS")
} 
#end of reading corpus1 if no RDS file found
if (file.exists("NLP.dtm1.RDS")) {
  dtm1 <- readRDS(file="NLP.dtm1.RDS")
}  else  {
    dtm1 <- DocumentTermMatrix(corpus1)
    gc()
    saveRDS(dtm1, file="NLP.dtm1.RDS")
    dtm1 <-removeSparseTerms(dtm1, 0.75) 
    gc()
    saveRDS(dtm1, file="NLP.dtm1.RDS")
}

#inspect(dtm1[1:3, 1000:1020])
 
#Todo:  I'd give 
#perl -ane '($p, $m) = /(\S+) (eat|sleep|die|give)/i; print lc("$p $m\n") if $m;' *txt | grep "i'd" | sort | uniq -c | sort -nr | more
#perl -ane '($p1, $p2, $p3, $p4,  $m) = /(jury) (\S+) (\S+) (\S+) (matter|case|incident|account)/i; print lc("$p1 $p2 $p3 $p4 $m\n") if $m;' *txt |  sort | uniq -c | sort -nr | more

    
#Progress to bigrams andn
 
BigramTokenizer <-   function(x)     unlist(lapply(ngrams(words(x), 2), paste, collapse = " "), use.names = FALSE)
#TrigramTokenizer <- function(x)  NGramTokenizer(x, Weka_control(min = 3, max = 3))
TrigramTokenizer <-   function(x)   unlist(lapply(ngrams(words(x), 3), paste, collapse = " "), use.names = FALSE)
FourgramTokenizer <-  function(x)   unlist(lapply(ngrams(words(x), 4), paste, collapse = " "), use.names = FALSE)
FivegramTokenizer <-  function(x)   unlist(lapply(ngrams(words(x), 5), paste, collapse = " "), use.names = FALSE)



gc()
if (file.exists("NLP.dtmBigram.RDS")) {
  dtmBigram <- readRDS(file="NLP.dtmBigram.RDS")
}  else  {
    dtmBigram <- TermDocumentMatrix(corpus1, control = list(tokenize = BigramTokenizer))
    
    saveRDS(dtmBigram, file="NLP.dtmBigram.RDS")
} 
rm(dtmBigram) 
gc()

if (file.exists("NLP.dtmTrigram.RDS")) {
  dtmTrigram <- readRDS(file="NLP.dtmTrigram.RDS")
}  else  {
    dtmTrigram <- TermDocumentMatrix(corpus1, control = list(tokenize = TrigramTokenizer ))
    #dtmTrigram <- removeSparseTerms(dtmTrigram, .5)
    saveRDS(dtmTrigram, file="NLP.dtmTrigram.RDS")
} 
rm(dtmTrigram)
gc()

if (file.exists("NLP.dtm4gram.RDS")) {
  dtm4gram <- readRDS(file="NLP.dtm4gram.RDS")
}  else  {
  dtm4gram <- TermDocumentMatrix(corpus1, control = list(tokenize = FourgramTokenizer ))
  #dtm4gram <- removeSparseTerms(dtm4gram, .5)
  saveRDS(dtm4gram, file="NLP.dtm4gram.RDS")
}
rm(dtm4gram)
gc()


##five grams
if (file.exists("NLP.dtm5gram.RDS")) {
  dtm4gram <- readRDS(file="NLP.dtm5gram.RDS")
}  else  {
  dtm5gram <- TermDocumentMatrix(corpus1, control = list(tokenize = FivegramTokenizer ))
  saveRDS(dtm5gram, file="NLP.dtm5gram.RDS")
}
rm(dtm5gram)
gc()


