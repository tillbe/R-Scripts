library(topicmodels)
library(tm)
library(parallel)

# data is stored in variable `text`
# to create the corpus with `tm`
corpus <- Corpus(VectorSource(text))
  
tm <- DocumentTermMatrix(corpus,
                         control = list(stemming = TRUE,  # snowball has to be installed
                                        stopwords = TRUE, 
                                        minWordLength = 3,
                                        removeNumbers = TRUE, 
                                        removePunctuation = TRUE))

runLDA <- function(dtm, k, SEED = 2015){
  print(paste0("Model with ",k," topics started!"))
  tm = LDA(dtm, k = k, 
           method = "Gibbs",
           control = list(alpha=0.01,
                          seed = SEED))
  l =  topicmodels::logLik(tm)
  saveRDS(tm, paste0("model_",k,".rds"))
  print(paste0("Model with ",k," topics done!"))
  return(l)
}

cores=detectCores()
ks = seq(20,200,20)
output <- unlist(mclapply(X=ks, FUN=runLDA, mc.cores=cores))
saveRDS(output, "output.rds")

