dyn.load('/Library/Java/JavaVirtualMachines/jdk1.8.0_20.jdk/Contents/Home/jre/lib/server/libjvm.dylib')
setwd('~/Finance/Programming/R/')
install.packages("devtools")
install.packages('RTextTools',dependencies=TRUE)
install.packages('e1071')
install.packages('rvest')
install.packages('stringr')
install.packages('RSentiment')
install.packages('sentiment')
install.packages('rJava')
install.packages('curl')
install.packages('Rserve')
install.packages('sentimentr')
install.packages('qdap')
install.packages('ggplot2')
install.packages('dplyr')
install_github("cran/wordcloud")
install_github("")
install_github("mananshah99/sentR", force=TRUE)
install.packages("tm")  # for text mining
install.packages("SnowballC") # for text stemming
install.packages("https://cran.r-project.org/src/contrib/wordcloud_2.5.tar.gz") # word-cloud generator 
install.packages("RColorBrewer") # color palettes
# Load
library(tm)
library(SnowballC)
library(wordcloud)
library(RColorBrewer)

library(rJava)
library(NLP)
library(zoo)
library(Rserve)
library(RTextTools)
library(e1071)
library(rvest)
library(stringr)
library(RSentiment)
library(qdap)
library(dplyr)
library(devtools)
library(sentR)
library(plyr)
library(sentiment)
library(ggplot2)
library(sentimentr)

memory.limit(size = 2500)

pos <- read.csv("positive-words.csv")
neg <- read.csv("negative-words.csv")
#speechurl <- read.table('postspeechurl.csv', header = TRUE, sep = ",")
sentdata <- read.table('speechdata_NOSTEM.csv', header = TRUE, sep = ",")
# colnames(speechurl)[1]<-'date'
# colnames(speechurl)[2]<-'url'
# colnames(speakers)[1]<-'date'
# colnames(speakers)[2]<-'speaker'
# speechdat <- merge(speechurl, speakers, by="date")

#data <- read.csv('speechdata_NOSTEM.csv')
sentdata <- sentdata[,-1]
#cum_words<- list()
cum_res <- matrix(nrow=nrow(data), ncol=2)
colnames(cum_res) <- c( "qdap", "sentr")
rownames(cum_res) <- speechdat[,1]


for(i in 100219:nrow(data)){

  # speaker <- gsub("  ", '', gsub('\n', '', as.character(speechdat[i,3])))
  # speaker <- gsub("\\s", " ", speaker)
  # webpage <- read_html(as.character(speechdat[i,2]))
  # speech_data <- as.character(html_text(webpage))
  # 
  # b <- str_locate(speech_data, speaker)[2]+1
  # speech <- str_sub(speech_data, b)
  # speech <- gsub(speechdat[i,1], "", speech)
  # speech <- unlist(unlist(str_split(speech, '\n')))
  # 
  # speech <- paste( gsub("U.S.", "US", speech[sapply(speech, function (x) tryCatch({nchar(x)}, error=function(e){FALSE}))>10]), collapse='')
  # speech <- gsub("\r", "", gsub("\t", "", speech))
  # speech <-  paste(strsplit(speech, split="    ")[[1]], collapse='')
  # speech <-  paste(strsplit(speech, split="  ")[[1]], collapse='')
  # speech <-  strsplit(speech, "[.]")[[1]]
  # speech <- speech[sapply(speech, nchar)>10]
  # speech <- speech[sapply(speech, function(x) !str_detect(x, '\\|'))]
  # run sentiment analysis apis
  sentence <- data[i, 3]
  sentence<-str_replace_all(sentence, "[^[:alnum:]]", " ")
  sentence<-gsub('[[:digit:]]+', '', sentence)
  if (nchar(as.character(sentence)) > 5){
    # 
    # out_NB <- classify.naivebayes(sentence)
    # cum_res[i, 2] <- ifelse(length(as.numeric(out_NB))==0, 0, out_NB)
    # 
    #cum_res[i, 3] <- calculate_score(sentence)/100
    
    #out_qdap <- qdap::polarity(sentence)
    #cum_res[i, 1] <- out_qdap$group$ave.polarity
    
    out_sentr <- sentimentr::sentiment(sentence)
    cum_res[i, 2] <- out_sentr$sentiment
    
    # out_sent <- sentiment::sentiment(speech)
    # out_sent <- out_sent[which(!is.na(out_sent))]
    # cum_res[i, 6] <- mean(out_sent/(abs(max(out_sent)) + abs(min(out_sent))))
    
    
    # add all words
    # t<-removeWords(levels(factor(sentence)), stopwords("english"))
    # t<-str_replace_all(t, "[[:punct:]]", " ")
    # 
    # t<-removeWords(t, letters)
    # 
    # out_words <- as.data.frame(qdap::all_words(t))
    # if(i==1){
    #   cum_words <- out_words
    # } else{
    #   cum_words <- bind_rows(cum_words,out_words) %>% group_by(WORD) %>% summarise_all(sum)
    # }
  }
}

graphdat<-aggregate(sentdata$Sentiment_Vader, by=list(sentdata$Datestamp,sentdata$Speaker), FUN=mean)
colnames(graphdat)<-c("Date","Speaker","Val")
t<-melt(graphdat,id.vars="date")

p<-ggplot(data=graphdat, aes(x=Date,y=Val)) + geom_line(aes(group=Speaker,color=Speaker))+
  ggtitle("Speaker Sentiments")+ theme(legend.position="none") +
  xlab("Date")+ylab("Sentiment Rating")

ggplotly(p)
p <- plot_ly(data, x = ~graphdat, y = ~Tree1, type = 'scatter', mode = 'lines') %>%
  add_trace(y = ~Tree2) %>%
  add_trace(y = ~Tree3) %>%
  add_trace(y = ~Tree4) %>%
  add_trace(y = ~Tree5) %>%
  layout(showlegend = FALSE)

# Create a shareable link to your chart
# Set up API credentials: https://plot.ly/r/getting-started
chart_link = api_create(p, filename="legend/hiding")


ggplotly(p)
sentdata[which(sentdata$Speaker=="Chairman Ben S.Bernanke"),]
greens<- sentdata[which(sentdata$Speaker=="Chairman Alan Greenspan"),]

greens<- data.frame("date"=aggregate(greens$Sentiment_Vader, by=list(greens$Datestamp), FUN=mean)[1],
                    "vader"=aggregate(greens$Sentiment_Vader, by=list(greens$Datestamp), FUN=mean)[2],
                       "sentr"=aggregate(greens$sentr, by=list(greens$Datestamp), FUN=mean)[2])
colnames(greens)<-c("date","vader","sentr")
greens$date<-as.Date(greens$date,format="%Y-%m-%d")
bern<- sentdata[which(sentdata$Speaker=="Governor Ben S. Bernanke"),]
bern<- data.frame("date"=aggregate(bern$Sentiment_Vader, by=list(bern$Datestamp), FUN=mean)[1],
                  "vader"=aggregate(bern$Sentiment_Vader, by=list(bern$Datestamp), FUN=mean)[2],
                  "sentr"=aggregate(bern$sentr, by=list(bern$Datestamp), FUN=mean)[2])
colnames(bern)<-c("date","vader","sentr")
bern$date<-as.Date(bern$date,format="%Y-%m-%d")
yellen<- sentdata[which(sentdata$Speaker=="Chair Janet L. Yellen"),]
yellen<- data.frame("date"=aggregate(yellen$Sentiment_Vader, by=list(yellen$Datestamp), FUN=mean)[1],
                    "vader"=aggregate(yellen$Sentiment_Vader, by=list(yellen$Datestamp), FUN=mean)[2],
                    "sentr"=aggregate(yellen$sentr, by=list(yellen$Datestamp), FUN=mean)[2])
colnames(yellen)<-c("date","vader","sentr")
yellen$date<-as.Date(yellen$date,format="%Y-%m-%d")
test<-full_join(greens, bern, by=date)

sentdata$sentr<-cum_res[,2]

greens<- sentdata[which(sentdata$Speaker=="Chairman Alan Greenspan"),]
greensagg<- data.frame(aggregate(greens$Sentiment_Vader, by=list(greens$Datestamp), FUN=mean),
                       aggregate(greens$sentr, by=list(greens$Datestamp), FUN=mean)[2])

docs <- Corpus(VectorSource(greens$Token))
toSpace <- content_transformer(function (x , pattern ) gsub(pattern, " ", x))
docs <- tm_map(docs, toSpace, "/")
docs <- tm_map(docs, toSpace, "@")
docs <- tm_map(docs, toSpace, "\\|")
docs <- tm_map(docs, content_transformer(tolower))
docs <- tm_map(docs, removeNumbers)
docs <- tm_map(docs, removeWords, stopwords("english"))
docs <- tm_map(docs, removeWords, c("blabla1", "blabla2")) 
docs <- tm_map(docs, removePunctuation)
docs <- tm_map(docs, stripWhitespace)

dtm <- TermDocumentMatrix(docs)
m <- as.matrix(dtm)
v <- sort(rowSums(m),decreasing=TRUE)
d <- data.frame(word = names(v),freq=v)
head(d, 10)
set.seed(1234)
wordcloud(words = d$word, freq = d$freq, min.freq = 10,
          max.words=200, random.order=FALSE, rot.per=0.35, 
          colors=brewer.pal(8, "Dark2"))


greens<- sentdata[which(sentdata$Speaker=="Chair Janet L. Yellen"),]
greensagg<- data.frame(aggregate(greens$Sentiment_Vader, by=list(greens$Datestamp), FUN=mean),
                       aggregate(greens$sentr, by=list(greens$Datestamp), FUN=mean)[2])

docs <- Corpus(VectorSource(greens$Token))
toSpace <- content_transformer(function (x , pattern ) gsub(pattern, " ", x))
docs <- tm_map(docs, toSpace, "/")
docs <- tm_map(docs, toSpace, "@")
docs <- tm_map(docs, toSpace, "\\|")
docs <- tm_map(docs, content_transformer(tolower))
docs <- tm_map(docs, removeNumbers)
docs <- tm_map(docs, removeWords, stopwords("english"))
docs <- tm_map(docs, removeWords, c("blabla1", "blabla2")) 
docs <- tm_map(docs, removePunctuation)
docs <- tm_map(docs, stripWhitespace)

dtm <- TermDocumentMatrix(docs)
m <- as.matrix(dtm)
v <- sort(rowSums(m),decreasing=TRUE)
d <- data.frame(word = names(v),freq=v)
head(d, 10)
set.seed(1234)
wordcloud(words = d$word, freq = d$freq, min.freq = 10,
          max.words=200, random.order=FALSE, rot.per=0.35, 
          colors=brewer.pal(12, "Paired"))



bernanke<- sentdata[which(sentdata$Speaker=="Governor Ben S. Bernanke"),]
group_by(bernanke$Datestamp)

sentdata <- sentdata[which(nchar(as.character(sentdata$Token))>5),]
sentdata$Token<-sapply(sentdata$Token, function (x) gsub('[[:digit:]]+', '', x))
sentdata$qdap<-sapply(sentdata$Token, function(x) qdap::polarity(x,constrain=TRUE)$group$ave.polarity)

# create corpus
library(wordcloud)
wordcloud(cum_words$WORD, cum_words$FREQ, min.freq=3)
set.seed(1234)
wordcloud(words = cum_words$WORD, freq = cum_words$FREQ, min.freq = 10,
          max.words=200, random.order=FALSE, rot.per=0.35, 
          colors=brewer.pal(8, "Dark2"))

barplot(cum_words[1:10,]$FREQ, las = 2, names.arg = cum_words[1:10,]$WORD,
        col ="lightblue", main ="Most frequent words",
        ylab = "Word frequencies")

# plot and granger causality
data$

# Add Market prices
consumersent_ind <- read.csv("consumersent_indices.csv")
market_ind <- read.csv("market_indices.csv")
putcall_ratios <- read.csv('putcall_ratios.csv')
trin_ind <- read.csv('indices.csv')
market_ind_info <- colnames(market_ind)
colnames(market_ind) <- sapply(market_ind[1,], as.character)
market_ind <- market_ind[-1,]
rownames(market_ind) <- as.Date(market_ind$Date, format="%d-%b-%y")
market_ind <-market_ind[,-1]
log_vix <- sapply(market_ind$VIX, function(x) log(as.numeric(x)))
vix_diff <- data.frame("VIX"=log_vix-lag(log_vix))
rownames(vix_diff) <- rownames(market_ind)
rownames(consumersent_ind)<-as.yearmon(consumersent_ind$TIME, format="%Y-%m")
consumersent_ind <- consumersent_ind[, -1]
consumersent_ind <- as.data.frame(apply(consumersent_ind, c(1,2), function(x) log(as.numeric(x))))
consumersent_ind$BCI <- consumersent_ind$BCI-lag(consumersent_ind$BCI)
consumersent_ind$CCI <- consumersent_ind$CCI-lag(consumersent_ind$CCI)
consumersent_ind$CLI <- consumersent_ind$CLI-lag(consumersent_ind$CLI)

rownames(trin_ind)<- as.Date(trin_ind$Date, format="%m/%d/%y")
trin_ind <- trin_ind[,-1]
trin_ind <- as.data.frame(apply(trin_ind, c(1,2), function(x) log(as.numeric(x))))
trin_ind$ARMS.Index <- trin_ind$ARMS.Index-lag(trin_ind$ARMS.Index)
trin_ind$EUR.Curncy <- trin_ind$EUR.Curncy-lag(trin_ind$EUR.Curncy)
trin_ind$SPX.Index <- trin_ind$SPX.Index-lag(trin_ind$SPX.Index)
trin_ind$USGG10YR.Index <- trin_ind$USGG10YR.Index-lag(trin_ind$USGG10YR.Index)

pc_ratios <- data.frame("pc_equity" = putcall_ratios$Equity.P.C.Ratio, "pc_ind" = putcall_ratios$Indices.P.C.Ratio, "pc_total" = putcall_ratios$Total.P.C.Ratio)
rownames(pc_ratios) <- as.character(as.Date(putcall_ratios$Date,format="%m/%d/%y"))
pc_ratios <- as.data.frame(apply(pc_ratios, c(1,2), function(x) log(as.numeric(x))))
pc_ratios$pc_equity <- pc_ratios$pc_equity-lag(pc_ratios$pc_equity)
pc_ratios$pc_ind <- pc_ratios$pc_ind-lag(pc_ratios$pc_ind)
pc_ratios$pc_total <- pc_ratios$pc_total-lag(pc_ratios$pc_total)

write.csv(pc_ratios, file = "pc_ratios_out.csv")
write.csv(trin_ind, file = "market_ind_out.csv")
write.csv(consumersent_ind, file = "consumersent_ind.csv")
write.csv(vix_diff, file = "vix_diff.csv")

rownames(cum_res) <- as.character(as.Date(rownames(cum_res), format="%B %d,%Y"))

left_join(cum_res, market_ind, by=rowname)

