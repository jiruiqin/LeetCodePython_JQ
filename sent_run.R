dyn.load('/Library/Java/JavaVirtualMachines/jdk1.8.0_20.jdk/Contents/Home/jre/lib/server/libjvm.dylib')
setwd('~/Finance/Programming/R/')
# install.packages("RTextTools")
# install.packages('e1071')
# install.packages('rvest')
# install.packages('rnn')
# install.packages('stringr')
# install.packages('RSentiment')
# install.packages('rJava')
# install.packages('twitteR')
#install.packages('Rserve')
#install.packages('sentimentr')
library(Rserve)
library(RTextTools)
library(e1071)
library(rvest)
library(stringr)
library(RSentiment)
library(qdap)
library(dplyr)
library(devtools)
install_github("mananshah99/sentR", force=TRUE)
library(sentR)
library(plyr)
require(sentiment)
library(twitteR)
library(ggplot2)
library(wordcloud)
library(RColorBrewer)
library(sentimentr)
pos <- read.csv("positive-words.csv")
neg <- read.csv("negative-words.csv")
speechurl <- read.csv('speechurl.csv')
speakers <- read.csv('speakers.csv')
colnames(speechurl)[1]<-'date'
colnames(speechurl)[2]<-'url'
colnames(speakers)[1]<-'date'
colnames(speakers)[2]<-'speaker'
speechdat <- merge(speechurl, speakers, by="date")

cum_words<- list()
cum_res <- matrix(nrow=nrow(speechdat), ncol=6)
colnames(cum_res) <- c()
rownames(cum_res) <- speechdat[,1]
for(i in 1:nrow(speechdat)){

  speaker <- gsub("  ", '', gsub('\n', '', as.character(speechdat[i,3])))
  speaker <- gsub("\\s", " ", speaker)
  webpage <- read_html(as.character(speechdat[i,2]))
  speech_data <- as.character(html_text(webpage))
  
  b <- str_locate(speech_data, speaker)[2]+1
  speech <- str_sub(speech_data, b)
  speech <- gsub(speechdat[i,1], "", speech)
  speech <- unlist(unlist(str_split(speech, '\n')))
  
  speech <- paste( gsub("U.S.", "US", speech[sapply(speech, function (x) tryCatch({nchar(x)}, error=function(e){FALSE}))>10]), collapse='')
  speech <- gsub("\r", "", gsub("\t", "", speech))
  speech <-  paste(strsplit(speech, split="    ")[[1]], collapse='')
  speech <-  paste(strsplit(speech, split="  ")[[1]], collapse='')
  speech <-  strsplit(speech, "[.]")[[1]]
  speech <- speech[sapply(speech, nchar)>10]
  speech <- speech[sapply(speech, function(x) !str_detect(x, '\\|'))]
  # run sentiment analysis apis
  out_agg <- classify.aggregate(speech,pos,neg)
  cum_res[i, 1] <- mean(out_agg$score)
  
  out_NB <- classify.naivebayes(speech)
  out_NB <- as.numeric(out_NB[,3])
  cum_res[i, 2] <- mean(out_NB/(abs(max(out_NB)) + abs(min(out_NB))))
  
  out_scor <- calculate_score(speech)
  cum_res[i, 3] <- mean(out_scor/(abs(max(out_scor)) + abs(min(out_scor))))
  
  out_qdap <- qdap::polarity(speech)
  cum_res[i, 4] <- out_qdap$group$ave.polarity
  
  out_sentr <- sentimentr::sentiment(speech)
  cum_res[i, 5] <- mean(out_sentr$sentiment)
  
  out_sent <- sentiment::sentiment(speech)
  out_sent <- out_sent[which(!is.na(out_sent))]
  cum_res[i, 6] <- mean(out_sent/(abs(max(out_sent)) + abs(min(out_sent))))
  
  
  # add all words
  emo.docs <- levels(factor(unlist(sapply(speech, function(x) strsplit(x, split=" ")))))
  emo.docs <- sapply(emo.docs, tolower)
  emo.docs <- NLP::words(speech)
  t<-removeWords(emo.docs, stopwords("english"))
  t<-str_replace_all(t, "[[:punct:]]", " ")
  t<-str_replace_all(t, "[^[:alnum:]]", " ")
  t<-gsub('[[:digit:]]+', '', t)
  t<-removeWords(t, letters)
  out_words <- as.data.frame(all_words(t))
  if(i==1){
    cum_words <- out_words
  } else{
    cum_words <- bind_rows(cum_words,out_words) %>% group_by(WORD) %>% summarise_all(sum)
  }
}

# create corpus
library(wordcloud)
wordcloud(cum_words$WORD, cum_words$FREQ, min.freq=3)

# plot and granger causality


# Add Market prices
consumersent_ind <- read.csv("consumersent_indices.csv")
market_ind <- read.csv("market_indices.csv")
putcall_ratios <- read.csv('putcall_ratios.csv')
trin_ind <- read.csv('trin.csv')

