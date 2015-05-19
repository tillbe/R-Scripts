library(topicmodels)
library(tm)
library(parallel)

# data is stored in variable `text`
# to create the corpus with `tm`
corpus <- Corpus(VectorSource(text))

# create document term matrix. adjust parameters as needed.  
dtm <- DocumentTermMatrix(corpus,
                         control = list(stemming = TRUE,  # snowball has to be installed
                                        stopwords = TRUE, 
                                        minWordLength = 3,
                                        removeNumbers = TRUE, 
                                        removePunctuation = TRUE))

# runs the LDA models with a given number of topics. 
# make sure you fix the seed to be able to compare models.

runLDA <- function(k, SEED = 2015){
  print(paste0("Model with ",k," topics started!"))
  tm = LDA(dtm, k = k, 
           method = "Gibbs",
           control = list(alpha=0.01, # adjust as needed.
                          seed = SEED))
  l =  topicmodels::logLik(tm) # calculates loglikelihood fit
  # saves the model so we don't have to run it again later
  saveRDS(tm, paste0("model_",k,".rds"))
  print(paste0("Model with ",k," topics done!"))
  return(l)
}

cores=detectCores() 
ks = seq(20,200,20) # number of topics we want to compare
# runs the function on all cores for all ks
output <- unlist(mclapply(X=ks, FUN=runLDA, mc.cores=cores))
saveRDS(output, "output.rds")

