# Library -----------------------------------------------------------------
require(tm); require(RWeka)

# Parameters --------------------------------------------------------------
tm_stopwords <- c("admin")
max_ngram <- 2

# Load --------------------------------------------------------------------
df <- 
tm_corpus <- Corpus(DataframeSource(df)

# remove punc and to lowercase
tm_corpus <- tm_map(tm_corpus, removePunctuation)
tm_corpus <- tm_map(tm_corpus, tolower)
# tm_corpus <- tm_map(tm_corpus, stemDocument)

# remove generic and custom stopwords
tm_corpus <- tm_map(tm_corpus, removeWords, tm_stopwords)# stopwords("english")

# build a term-document matrix
tdm <- TermDocumentMatrix(tm_corpus, control = list(tokenize = function(x) NGramTokenizer(x, Weka_control(min = 1, max = max_ngram))))

# Inspect -----------------------------------------------------------------
# inspect the document-term matrix
tdm

# inspect most popular words
# findfreqTerms(tdm, lowfreq=1000)

tdm2 <- removeSparseTerms(tdm, sparse = 0.999)
tdm2

# build word count
df_wordcount <- sort(rowSums(as.matrix(tdm2)), decreasing = TRUE)
df_wordcount <- data.frame(word = names(df_wordcount), freq = df_wordcount)


require(wordcloud);
# png("wordcloud.png", width=1280,height=800)
wordcloud(words = df_word$word, freq = df_word$freq,
          scale = c(8,.3), min.freq = 2, max.words = 100,   
          random.order = TRUE, rot.per=.15, color = brewer.pal(9, "RdYlBu")
          )
# vfont=c("sans serif","plain")
# dev.off()