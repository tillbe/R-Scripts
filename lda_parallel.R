library(topicmodels)
library(tm)
library(parallel)
library(ggplot2)
if (!require("SnowballC")) install.packages("SnowballC")


# data is stored in variable `text`
text = # insert data here in form of vector...

# remove all abstracts with less than 100 characters.
# you can adjust the number of characters as desired.
# make sure to at least remove elements with no text 
# as otherwise LDA model won't run
len = lapply(text, nchar)
text = text[which(len>100)]

corpus = Corpus(VectorSource(text))

# create document term matrix. adjust parameters as needed.  
dtm = DocumentTermMatrix(corpus,
                         control = list(stemming = TRUE,  # SnowballC has to be installed
                                        stopwords = TRUE, 
                                        minWordLength = 3,
                                        removeNumbers = TRUE, 
                                        removePunctuation = TRUE))



# function that runs the LDA models with a given number of topics. 
# needed for the mclapply call later.
# make sure you fix the seed to be able to compare models.
# adjust parameters as needed, especially alpha

runLDA <- function(k, SEED = 2015){
  print(paste0("Model with ", k," topics started!"))
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

# number of topics we want to compare
# adjust this as needed to get best fit
ks = seq(20,200,20) 

# runs the function on all cores for all ks
cores=detectCores() 
output = unlist(mclapply(X=ks, FUN=runLDA, mc.cores=cores))

# save model fits for later plotting and analysis
df = data.frame(k = ks, loglik = output)
saveRDS(df, "model_fits.rds") 

# best model fit with k models
final_k = df$k[which.max(df$loglik)]

# if you want to remove unessary model files, run this function
deleteModels <- function(k){
  remove_ks = ks[which(ks!=k)] 
  lapply(paste0("model_",remove_ks,".rds"), file.remove)
}
# deleteModels(final_k)

### Function to plot the model fit
plotFit <- function(df){
  gp = ggplot(df, aes(x = k, y = loglik)) + 
    geom_line() +
    geom_point(size = 3) +
    xlab("Number of topics (k)") +
    ylab("Log-Likelihood") +
    ggtitle("Model fit for different number of topics")
  return(gp)
}
# plotFit(df)
