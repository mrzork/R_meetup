library(rtweet)
library(quanteda)
library(ggplot2)
library(data.table)

## insert your appname
appname <- "myappname"

## insert your api key
key <- "myapikey"

## insert your api secret
secret <- "myapisecret"

## create a token, storing it as object 'twitter_token'
twitter_token <- create_token(
  app = appname,
  consumer_key = key,
  consumer_secret = secret
)






#Search API (REST)
tw.search1<-search_tweets(q = "colombia", type = "recent",  parse = T, 
                        include_rts = T, n = 500, token = twitter_token)
View(tw.search1)

kw<-"@claudialopez"
tw.search<-search_tweets(q = kw, type = "recent",  parse = T, lang = "es",
                        include_rts = T, n = 10000, token = twitter_token)

#tw.search<-tw.dt
View(tw.search)

#user extraction from data.frame attributes 
tw.users<-as.data.table(attr(tw.search,"users"))
setkey(tw.users, user_id)
tw.users<-unique(tw.users)
View(tw.users)


#Stream API (STREAMING) filter
tw.stream.track<-stream_tweets(q = "@JuanManSantos", timeout = 60, parse = T, 
                               language = "es", token = twitter_token)
View(tw.stream.track)

#Stream API (STREAMING) location
geocoords<-lookup_coords("Colombia")

tw.stream.geo<-stream_tweets(geocoords, timeout = 60, parse = T,
                        token = twitter_token)
View(tw.stream.geo)


#Get account Timeline
tw.timeline<-get_timeline(user = "@JuanManSantos", n = 200, parse = T, 
                          token = twitter_token)
View(tw.timeline)

#Get Colombia trending topics
tw.trends.colombia<-get_trends(woeid = "23424787", token = twitter_token)
View(tw.trends.colombia)

#Get Cali trending topics
tw.trends.cali<-get_trends(woeid = "368149", token = twitter_token)
View(tw.trends.cali)

#Get followers
tw.followers<-get_followers("AndreaMamprin")
View(tw.followers)

#Users lookup
tw.lookupusers<-lookup_users(tw.followers)
View(tw.lookupusers)

#data exploration
#screen_name
head(tw.search$screen_name, 10)
tw.search<-as.data.table(tw.search) #data.table do it better
tw.search.sn<-tw.search[, .N, by = "screen_name"]
head(tw.search.sn[order(-N)],10)

#source
head(tw.search$source, 20)
tw.search.sr<-tw.search[, .N, by = "source"]
head(tw.search.sr[order(-N)],10)

#tweets per hour
attr(tw.search$created_at, "tzone")<-"America/Bogota"
tw.search[, time.dh:=as.POSIXct(format(created_at, "%Y-%m-%d %H:00:00"))]
tw.search.f<-tw.search[, .N, by = "time.dh"]
ggplot() + 
  geom_line(data = tw.search.f, aes(x=time.dh, y=N)) + 
  theme(axis.text.x = element_text(angle=90, size=12)) + 
  xlab("Time") + ylab("N tweets") 







#defino el corpus de trinos
tw.search.sl<-tw.search[is_retweet==F]
mycorpus <- corpus(tw.search.sl$text)
head(summary(mycorpus))
mycorpus <- tolower(mycorpus)


#tokenization
mycorpus.wd <- tokens(mycorpus, what = "word")
mycorpus.fw <- tokens(mycorpus, what = "fastestword")
mycorpus.ch <- tokens(mycorpus, what = "character")
mycorpus.sn <- tokens(mycorpus, what = "sentence")

i<-5
mycorpus[i] #sentences?
mycorpus.wd[i]
mycorpus.fw[i]
mycorpus.ch[i]
mycorpus.sn[i]


#remove special characters: url
mycorpus.adv1 <- tokens(mycorpus, what = "word", remove_url = T)
mycorpus[1:3]
mycorpus.adv1[1:3]


#remove special characters: numbers
mycorpus.adv2 <- tokens(mycorpus, what = "word", remove_url = T, remove_numbers = T)
mycorpus.adv1[1:3]
mycorpus.adv2[1:3]

#remove special characters: punctuation, symbols, hyphens, twitter symbols
mycorpus.adv3 <- tokens(mycorpus, what = "word", remove_url = T, remove_numbers = T,
                        remove_punct = T, remove_symbols = T, remove_separators = T, 
                        remove_hyphens = T, remove_twitter = T)
mycorpus.adv2[1:3]
mycorpus.adv3[1:3]


#remove stopwords
stopwords(kind = "spanish")
kwf<-tolower(gsub("[@#]","",kw, perl = T))   #normalize keyword with regex
stopw <- c(stopwords(kind = "spanish"), kwf, "rt")
mycorpus.adv4<-removeFeatures(mycorpus.adv3, stopw)
mycorpus.adv3[1]
mycorpus.adv4[1]


#stemming
mycorpus.adv5<-tokens_wordstem(mycorpus.adv4, language = "es")
mycorpus.adv4[1]
mycorpus.adv5[1]

#dtm creation and changes
dtm1 <- dfm(mycorpus.adv1)
dtm2 <- dfm(mycorpus.adv2)
dtm3 <- dfm(mycorpus.adv3)
dtm4 <- dfm(mycorpus.adv4)
dtm5 <- dfm(mycorpus.adv5)
dtm1
dtm2
dtm3
dtm4
dtm5

#remove stems with nchar<=2 
dtm5.w2<-dfm_select(dtm5, min_nchar = 3)
dtm5
dtm5.w2

#visualizing dtm
View(dtm5.w2)

#reduce dtm sparsity 
#remove no frequent terms
dtm5.rs<-dfm_trim(dtm5.w2, min_count = 20, min_docfreq = 2)
dtm5.w2
dtm5.rs

#remove zero documents
rowTotals <- apply(dtm5.rs , 1, sum) #Find the sum of terms in each Document
dtm5.rs.rr <- dtm5.rs[rowTotals> 0, ] #remove all docs term words
dtm5.rs
dtm5.rs.rr



#explore dtm
#most frequent stems
topfeatures(dtm5.rs.rr, 20)

#similarity
tf<-topfeatures(dtm5.rs.rr, 5) #top 5 stems
freqstem<-names(tf)
sim<-textstat_simil(dtm5.rs.rr, freqstem, margin = "features", method = "correlation")
sim[1:5,]

#wordcloud
set.seed(100)
textplot_wordcloud(dtm4, min.freq = 40, random.order = FALSE,
                   rot.per = .25, scale = c(2, 0.5),
                   colors = RColorBrewer::brewer.pal(8,"Dark2"))

#ngrams
bigram<-tokens_ngrams(mycorpus.adv4, 2, concatenator = " ")
bigram.dfm<-dfm(bigram)
topfeatures(bigram.dfm,10)

trigram<-tokens_ngrams(mycorpus.adv4, 3, concatenator = " ")
trigram.dfm<-dfm(trigram)
topfeatures(trigram.dfm,10)


#hashtags
head(tw.search$hashtags, 100)
ht<-tw.search[is.na(hashtags)==F, .SD, .SDcols = "hashtags"]
ht.dtm<-dfm(corpus(ht$hashtags))
topfeatures(ht.dtm, 15)

#mentions
head(tw.search$mentions_screen_name, 10)
mn<-tw.search[is.na(mentions_screen_name)==F, .SD, .SDcols = "mentions_screen_name"]
mn.dtm<-dfm(corpus(mn$mentions_screen_name))
topfeatures(mn.dtm, 20)