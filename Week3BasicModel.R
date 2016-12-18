#Week 2 Exploratory and milestone
#library("RWeka")
library("tm")   
library(ngram) 
library(parallel)
library(data.table)
library(stringr)
     
#Get to the file
setwd("~/NLPCapstone")
#Access the data.  Note that the subset folder has small files
folder <-  "~/NLPCapstone/final/en_US/subset" 


###Bigrams
if (file.exists("NLP.dtmBigram.RDS")) {
  dtmBigram <- readRDS(file="NLP.dtmBigram.RDS")
}  else  {
  stop('Err, could not find NLP.dtmBigram.RDS')
} 
#Convert the DTM to data.table for more functionality and higher performance in later steps
dtBigram <-as.data.table(as.matrix(dtmBigram), keep.rowname = TRUE)
#remove(dtmBigram)
gc()

#Build a new column that sums the three documents
dtBigram$newval <- rowSums(dtBigram[,.(en_US.blogs.txt, en_US.twitter.txt, en_US.news.txt)])

#Isolate the first and last words into variable pre_gram and post_gram. Key the pre_gram column
dtBigram$pre_gram <- str_trim(str_replace(dtBigram$rn, "\\S+$", ""))
dtBigram$post_gram <- str_extract(dtBigram$rn, "\\S+$")
setkey(dtBigram, pre_gram)

#Get a count  by pre_gram as a step towards calcualting the probability of post_grams, then calc the probalbity
dtBigram[, totPre :=  sum(newval), by = pre_gram]
dtBigram$prob <- dtBigram$newval / dtBigram$totPre

#Also calc the  overall number of times a post_gram appears in the table.  This value will be used in a simplified Kneser-Ney prioritization
dtBigram[, totAll :=  sum(newval), by = post_gram]


#Save the object for use in the runtime prediction model
saveRDS(dtBigram[newval > 1], file="NLP.dtBigram.RDS") 
rm(dtBigram)
gc()
 

###Trigrams
if (file.exists("NLP.dtmTrigram.RDS")) {
  dtmTrigram <- readRDS(file="NLP.dtmTrigram.RDS")
}  else  {
  stop('Err, could not find NLP.dtmTrigam.RDS')
} 
#Convert the DTM to data.table for more functionality and higher performance in later steps
dtTrigram <-as.data.table(as.matrix(dtmTrigram), keep.rowname = TRUE)
remove(dtmTrigram)
gc()

#Build a new column that sums the three documents
dtTrigram$newval <- rowSums(dtTrigram[,.(en_US.blogs.txt, en_US.twitter.txt, en_US.news.txt)])

#Isolate the first and last words into variable pre_gram and post_gram. Key the pre_gram column
dtTrigram$pre_gram <- str_trim(str_replace(dtTrigram$rn, "\\S+$", ""))
dtTrigram$post_gram <- str_extract(dtTrigram$rn, "\\S+$")
setkey(dtTrigram, pre_gram)

#Get a count  by pre_gram as a step towards calcualting the probability of post_grams, then calc the probalbity
dtTrigram[, totPre :=  sum(newval), by = pre_gram]
dtTrigram$prob <- dtTrigram$newval / dtTrigram$totPre


#Also calc the  overall number of times a post_gram appears in the table.  This value will be used in a simplified Kneser-Ney prioritization
dtTrigram[, totAll :=  sum(newval), by = post_gram]
dtTrigram$popular  <- dtTrigram$newval / dtTrigram$totAll

#Save the object for use in the runtime prediction model
saveRDS(dtTrigram[newval > 1], file="NLP.dtTrigram.RDS") 
rm(dtTrigram)


###4grams
if (file.exists("NLP.dtm4gram.RDS")) {
  dtm4gram <- readRDS(file="NLP.dtm4gram.RDS")
}  else  {
  stop('Err, could not find NLP.dtm4gam.RDS')
} 
#Convert the DTM to data.table for more functionality and higher performance in later steps
dt4gram <-as.data.table(as.matrix(dtm4gram), keep.rowname = TRUE)
remove(dtm4gram)
gc()

#Build a new column that sums the three documents
dt4gram$newval <- rowSums(dt4gram[,.(en_US.blogs.txt, en_US.twitter.txt, en_US.news.txt)])

#Isolate the first and last words into variable pre_gram and post_gram. Key the pre_gram column
dt4gram$pre_gram <- str_trim(str_replace(dt4gram$rn, "\\S+$", ""))
dt4gram$post_gram <- str_extract(dt4gram$rn, "\\S+$")
setkey(dt4gram, pre_gram)

#Get a count  by pre_gram asdt a step towards calcualting the probability of post_grams, then calc the probalbity
dt4gram[, totPre :=  sum(newval), by = pre_gram]
dt4gram$prob <- dt4gram$newval / dt4gram$totPre


#Also calc the  overall number of times a post_gram appears in the table.  This value will be used in a simplified Kneser-Ney prioritization
dt4gram[, totAll :=  sum(newval), by = post_gram]
dt4gram$popular  <- dt4gram$newval / dt4gram$totAll

#Save the object for use in the runtime prediction model
saveRDS(dt4gram[newval > 1 | totAll <2000  ], file="NLP.dt4gram.RDS") 
saveRDS(dt4gram, file="NLP.dt4gram.RDS") 
rm(dt4gram)

gc()



###5grams
if (file.exists("NLP.dtm5gram.RDS")) {
  dtm5gram <- readRDS(file="NLP.dtm5gram.RDS")
}  else  {
  stop('Err, could not find NLP.dtm5gam.RDS')
} 
#Convert the DTM to data.table for more functionality and higher performance in later steps
dt5gram <-as.data.table(as.matrix(dtm5gram), keep.rowname = TRUE)
remove(dtm5gram)
gc()

#Build a new column that sums the three documents
dt5gram$newval <- rowSums(dt5gram[,.(en_US.blogs.txt, en_US.twitter.txt, en_US.news.txt)])

#Isolate the first and last words into variable pre_gram and post_gram. Key the pre_gram column
dt5gram$pre_gram <- str_trim(str_replace(dt5gram$rn, "\\S+$", ""))
dt5gram$post_gram <- str_extract(dt5gram$rn, "\\S+$")
setkey(dt5gram, pre_gram)

#Get a count  by pre_gram asdt a step towards calcualting the probability of post_grams, then calc the probalbity
dt5gram[, totPre :=  sum(newval), by = pre_gram]
dt5gram$prob <- dt5gram$newval / dt5gram$totPre


#Also calc the  overall number of times a post_gram appears in the table.  This value will be used in a simplified Kneser-Ney prioritization
dt5gram[, totAll :=  sum(newval), by = post_gram]
dt5gram$popular  <- dt5gram$newval / dt5gram$totAll

#Save the object for use in the runtime prediction model
saveRDS(dt5gram[newval > 1 | totAll <2000  ], file="NLP.dt5gram.RDS") 
saveRDS(dt5gram, file="NLP.dt5gram.RDS") 
rm(dt5gram)

gc()


