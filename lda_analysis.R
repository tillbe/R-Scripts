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

mostCommonTopics = function(gdf, n = 10){
  df = gdf %>%
    summarise_each(funs(sum)) %>%
    gather(topic, totalProb) %>%
    arrange(desc(totalProb)) %>%
    #separate(topic, c("del", "topic"), 5) %>%
    #select(-del) %>%
    head(n)
  return(df)
}
gammaDF = as.data.frame(tm@gamma)
names(gammaDF) = paste0("topic", 1:length(names(gammaDF)))

mc = mostCommonTopics(gammaDF, 5)
df = gammaDF[,mc$topic]
df2 = gather(df, topic, prob)
ggplot(df, aes(x=topic35)) + geom_density(fill = "red", alpha =0.5) + 
  xlim(0,1) +
  geom_density(aes(x=topic41), fill = "blue", alpha = 0.5) +
  geom_density(aes(x=topic60), fill = "white", alpha = 0.5)

ggplot(filter(df2, prob > 0.01), aes(x=prob)) + 
  geom_density(aes(fill = topic, col = topic), alpha = .5) +
  xlim(0, 1)
