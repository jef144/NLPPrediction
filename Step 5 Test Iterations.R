#Week 2 Exploratory and milestone
#library("RWeka")
#library("tm")   
#library(ngram) 
#library(parallel)
library(data.table)
library(stringr)
      
#Get to the files
setwd("~/NLPCapstone")
#dtBigram <- readRDS( file="NLP.dtBigram.RDS")

#Assume that the data objects and the prediction function are already in RStudio memory
#Run Step 4 Prediction first if neecessary
  
#Fail here if the function is not loaded
 predictWord("a back seat to")
 
 set.seed(2016)
 
 #How many test iterations
 TESTLIMIT <- 100
 
 tstCount <- 0 
 
 #Counter of successful matches on the first try
 correctFirst <- 0
 
 #Dataframe holding counts for all successful predictions, even if down the probability list 
 dfSuccesses <- data.frame( fallbackSeq = (1:15), 
                            fallbackStrategy = "",
                            successes = 0,
                               stringsAsFactors = FALSE)
 
 
 inputFile <- "./final/en_US/tst/en_US.tst" 
 con  <- file(inputFile, open = "r")
 
 
 #Read each test case,  which happens to be a sentence as parsed by the Step 1 script
 while (length(oneLine <- readLines(con, n = 1, warn = FALSE)) > 0  &   tstCount <= TESTLIMIT) {
    
   tstCount = tstCount + 1
    numSentenceWords <- str_count(oneLine, pattern="\\S+")
    if (numSentenceWords > 3) {    
         cutOff <-  sample(2:numSentenceWords -1 , 1)
         #print(c( "Observation", numSentenceWords, cutOff,   oneLine) )   
         predictors <- str_c(word(oneLine, 1:(cutOff-1)), collapse = " ")
         toPredict  <- word(oneLine,  cutOff )
         print(oneLine)
         
         #Call the preduction routine
         dfPredicted <- predictWord(predictors)
         
         #Keep a count of correct top-of-list predictions
         x <- 1
         if (nrow(dfPredicted) > 0 ) {
             if (toPredict ==    dfPredicted[1, 'post_gram']  )  {
                correctFirst = correctFirst + 1
             }
         }   
         
         #Capture more nuance by incrementing the dfSuccesses if a match is found there.
         
         for (i in 1:nrow(dfPredicted)) {
           if (nrow(dfPredicted) > 0 ) { 
               if (toPredict == dfPredicted[i, 'post_gram'] ) {
                    #increment dfSuccesses
                    successfulFallback = dfPredicted[i, 'fallbackSeq']
                    dfSuccesses[successfulFallback, 'successes']  = 1 + dfSuccesses[successfulFallback, 'successes']  
                  
               }
           }   
         } 
   }
 }  
 View(dfSuccesses)
 print ("Correct", correctFirst)