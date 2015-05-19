library(tm)
library(topicmodels)
library(dplyr)
library(tidyr)
library(ggplot2)

# load model from topicmodels package
fh = "~/Desktop/model_80.rds"
tm = readRDS(fh)


# word distribution for topics
createbetaDF <- function(t){
  betaDF = as.data.frame(t@beta)
  names(betaDF) = t@terms
  betaDF$topic = 1:nrow(betaDF)
  betaDF = betaDF %>% 
              gather(word, probability, -topic) %>%
              mutate_each(funs(as.character))
  return(betaDF)
}

# returns most common words along with probability (beta value)
getTerms <- function(t, n=5, bdf = betaDF, tm.mod = tm){
  w = as.data.frame(terms(tm.mod, n)[,t])
  w = w %>% 
        mutate_each(funs(as.character)) %>%
        gather(topic, word) %>%
        separate(topic, c("del", "topic"), " ") %>%
        select(-del) %>%
        mutate_each(funs(as.character))
 
  for (i in 1:nrow(w)) {
      p =   bdf %>%
        filter(topic == w[i, "topic"], word == w[i, "word"]) %>%
        select(probability)
      w[i,"prob"] = as.numeric(p)
  }
  return(w)
}


betaDF = createbetaDF(tm)
foo = getTerms(c(35,41,23,20,30), n=5)
ggplot(foo, aes(x=topic,y=prob, col = topic)) +
  geom_text(aes(label = word), 
            position = position_jitter(w = 0.5)) 


######## Topic Distribution

sums= melt(gammaDF %>%
             summarise_each(funs(sum)))
sums %>% arrange(desc(value)) %>% head(10)



gammaDF = as.data.frame(tm@gamma)
names(gammaDF) = paste0("topic", 1:length(names(gammaDF)))

betaDF = as.data.frame(tm@beta)
names(betaDF) = tm@terms
betaDF$topic = paste0("topic", 1:nrow(betaDF))

x=gather(betaDF, word, probability, -topic)
topic_distribution = 
  gather(gammaDF %>%
       summarise_each(funs(sum)),
       topic,
       sum)