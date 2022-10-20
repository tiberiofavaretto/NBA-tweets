#' Title: NBA tweets case I
#' Purpose: Analyzing tweets
#' License: GPL>=3
#' Date: Mar 13 2022
#'

# Set the working directory
setwd("C:/Users/tiber/Desktop/Università/Master/II Semester/NLP/Case I/Data")

# Libs
library(NLP)
library(qdapDictionaries)
library(qdapRegex)
library(qdapTools)
library(RColorBrewer)
library(tm)
library(qdap)
library(ggplot2)
library(ggthemes)
library(ggdendro)
library(stringi)
library(wordcloud)


# Options & Functions
options(stringsAsFactors = FALSE)
Sys.setlocale('LC_ALL','C')

tryTolower <- function(x){
  # return NA when there is an error
  y = NA
  # tryCatch error
  try_error = tryCatch(tolower(x), error = function(e) e)
  # if not an error
  if (!inherits(try_error, 'error'))
    y = tolower(x)
  return(y)
}

cleanCorpus<-function(corpus, customStopwords){
  corpus <- tm_map(corpus, content_transformer(qdapRegex::rm_url))
  corpus <- tm_map(corpus, content_transformer(replace_contraction)) 
  corpus <- tm_map(corpus, removeNumbers)
  corpus <- tm_map(corpus, removePunctuation)
  corpus <- tm_map(corpus, stripWhitespace)
  corpus <- tm_map(corpus, content_transformer(tryTolower))
  corpus <- tm_map(corpus, removeWords, customStopwords)
  return(corpus)
}

# load the case data in an object
dec2019 <- read.csv('C_Dec2019.csv')
aug2020 <- read.csv('K_Aug2020.csv')
sep2020 <- read.csv('L_Sep2020.csv')
oct2020 <- read.csv('M_Oct2020.csv')

#### 
# Write a function accepting a text column
# use gsub subsituting 'http\\S+\\s*' for '' which removes URLS
# use gsub substituting '(RT|via)((?:\\b\\W*@\\w+)+)' for '' which removes "RT" exactly
# use tolower in the function on the text
# return the changed text
basicSubs <- function(x){
  x <- gsub('http\\S+\\s*', '', x)
  x <- gsub('(RT|via)((?:\\b\\W*@\\w+)+)', '', x)
  x <- tolower(x)
  return(x)
}

# apply the function to JUST THE TEXT COLUMN
text <- basicSubs(dec2019$text)

# replacing the original text column with the new one
dec2019[["text"]] <- text

# Create custom stop words
stops <- c(stopwords('SMART'), 'smh', 'rofl', 'nba', 'game')

# sampling the dataframe
index <- sample(1:nrow(dec2019), size = 0.03*nrow(dec2019))
small_dec2019 <- dec2019[index,]

#### 
# Use sum with stri_count on the newt text object
lebron  <- sum(stri_count(small_dec2019$text, fixed ='lebron'))
giannis  <- sum(stri_count(small_dec2019$text, fixed ='giannis'))
butler <- sum(stri_count(small_dec2019$text, regex ='butler'))

# Organize term objects into a data frame
termFreq <- data.frame(terms = c('LeBron','Giannis','Butler'),
                       freq  = c(lebron,giannis, butler))

# Examine
termFreq

# Plot a geom_bar with ggplot2 by filling in the correct data,
# adding a layers "theme_gdocs() + theme(legend.position = "none")"
ggplot(termFreq, aes(x = reorder(terms, freq), y = freq,fill=freq)) + 
  geom_bar(stat = "identity") + coord_flip() + 
  theme_gdocs() + theme(legend.position = "none")

# Apply the VCorpus Function to a VectorSource of the original text object
# Hint: only pass in the vector NOT the entire dataframe using a $
cleanTxt <- VCorpus(VectorSource(small_dec2019$text))

# Clean the Corpus with your cleanCorpus function, this will take a few seconds
cleanTxt <- cleanCorpus(cleanTxt, stops)

# Construct a DTM in an object called cleanMat
cleanMatm  <- DocumentTermMatrix(cleanTxt)

# Construct a TDM in an object called cleanMat
cleanMatTDMm <- TermDocumentMatrix(cleanTxt)

# Switch this to a simple matrix still called cleanMat
cleanMatTDM <- as.matrix(cleanMatTDMm)

# Switch this to a simple matrix still called cleanMat
cleanMat <- as.matrix(cleanMatm)

# Get the most frequent terms
topTermsA <- colSums(cleanMat)

# Add the terms
topTermsA <- data.frame(terms = colnames(cleanMat), freq = topTermsA)

# Remove row attributes
rownames(topTermsA) <- NULL

# Review
head(topTermsA)

# Simple barplot; values greater than 15
topWords      <- subset(topTermsA, topTermsA$freq >= 1000) 
topWords      <- topWords[order(topWords$freq, decreasing=F),]

# Chg to factor for ggplot
topWords$terms <- factor(topWords$terms, 
                         levels=unique(as.character(topWords$terms))) 

ggplot(topWords, aes(x=terms, y=freq)) + 
  geom_bar(stat="identity", fill='darkred') + 
  coord_flip()+ theme_gdocs() +
  geom_text(aes(label=freq), colour="white",hjust=1.25, size=3.0)

# Inspect word associations
associations <- findAssocs(cleanMatm, 'lebron', 0.30)
associations

# Organize the word associations
assocDF <- data.frame(terms=names(associations[[1]]),
                      value=unlist(associations))
assocDF$terms <- factor(assocDF$terms, levels=assocDF$terms)
rownames(assocDF) <- NULL
assocDF

# Make a dot plot
ggplot(assocDF, aes(y=terms)) +
  geom_point(aes(x=value), data=assocDF, col='#c00c00') +
  theme_gdocs() + 
  geom_text(aes(x=value,label=value), colour="red",hjust="inward", vjust ="inward" , size=3)

# Which term is the most frequent?
idx <- which.max(topTermsA$freq)
topTermsA[idx, ]

# Order
exampleReOrder <- topTermsA[order(topTermsA$freq, decreasing = T),]

# Remove row attributes
rownames(exampleReOrder) <- NULL
head(exampleReOrder)

# Reduce TDM
reducedTDM <- removeSparseTerms(cleanMatTDMm, sparse=0.985) #shoot for ~50 terms; 1.5% of cells in row have a value  
reducedTDM

# Organize the smaller TDM
reducedTDM <- as.data.frame(as.matrix(reducedTDM))

# Basic Hierarchical Clustering
hc <- hclust(dist(reducedTDM))
plot(hc,yaxt='n')

ggdendrogram(hc, rotate=FALSE)

# Get Row Sums & organize
decTDMv <- sort(rowSums(cleanMatTDM), decreasing = TRUE)
decDF   <- data.frame(word = names(decTDMv), freq = decTDMv)

# Review all Palettes
display.brewer.all()

# Choose a color & drop light ones
pal <- brewer.pal(8, "Blues")
pal <- pal[-(1:2)]

# Make simple word cloud
# Reminder to expand device pane
wordcloud(decDF$word,
          decDF$freq,
          max.words    = 50,
          random.order = FALSE,
          colors       = pal,
          scale        = c(2,1))



# apply the function to JUST THE TEXT COLUMN
text <- basicSubs(aug2020$text)

# replacing the original text column with the new one
aug2020[["text"]] <- text

# Create custom stop words
stops <- c(stopwords('SMART'), 'smh', 'rofl', 'nba', 'game')

# sampling the dataframe
index <- sample(1:nrow(aug2020), size = 0.02*nrow(aug2020))
small_aug2020 <- aug2020[index,]

#### 
# Use sum with stri_count on the newt text object
lebron  <- sum(stri_count(small_aug2020$text, fixed ='lebron'))
giannis  <- sum(stri_count(small_aug2020$text, fixed ='giannis'))
butler <- sum(stri_count(small_aug2020$text, regex ='butler'))

# Organize term objects into a data frame
termFreq <- data.frame(terms = c('LeBron','Giannis','Butler'),
                       freq  = c(lebron,giannis, butler))

# Examine
termFreq

# Plot a geom_bar with ggplot2 by filling in the correct data,
# adding a layers "theme_gdocs() + theme(legend.position = "none")"
ggplot(termFreq, aes(x = reorder(terms, freq), y = freq,fill=freq)) + 
  geom_bar(stat = "identity") + coord_flip() + 
  theme_gdocs() + theme(legend.position = "none")

# Apply the VCorpus Function to a VectorSource of the original text object
# Hint: only pass in the vector NOT the entire dataframe using a $
cleanTxt <- VCorpus(VectorSource(small_aug2020$text))

# Clean the Corpus with your cleanCorpus function, this will take a few seconds
cleanTxt <- cleanCorpus(cleanTxt, stops)

# Construct a DTM in an object called cleanMat
cleanMatm  <- DocumentTermMatrix(cleanTxt)

# Construct a TDM in an object called cleanMat
cleanMatTDMm <- TermDocumentMatrix(cleanTxt)

# Switch this to a simple matrix still called cleanMat
cleanMatTDM <- as.matrix(cleanMatTDMm)

# Switch this to a simple matrix still called cleanMat
cleanMat <- as.matrix(cleanMatm)

# Get the most frequent terms
topTermsA <- colSums(cleanMat)

# Add the terms
topTermsA <- data.frame(terms = colnames(cleanMat), freq = topTermsA)

# Remove row attributes
rownames(topTermsA) <- NULL

# Review
head(topTermsA)

# Simple barplot; values greater than 15
topWords      <- subset(topTermsA, topTermsA$freq >= 600) 
topWords      <- topWords[order(topWords$freq, decreasing=F),]

# Chg to factor for ggplot
topWords$terms <- factor(topWords$terms, 
                         levels=unique(as.character(topWords$terms))) 

ggplot(topWords, aes(x=terms, y=freq)) + 
  geom_bar(stat="identity", fill='darkred') + 
  coord_flip()+ theme_gdocs() +
  geom_text(aes(label=freq), colour="white",hjust=1.25, size=3.0)

# Inspect word associations
associations <- findAssocs(cleanMatm, 'lebron', 0.30)
associations

# Organize the word associations
assocDF <- data.frame(terms=names(associations[[1]]),
                      value=unlist(associations))
assocDF$terms <- factor(assocDF$terms, levels=assocDF$terms)
rownames(assocDF) <- NULL
assocDF

# Make a dot plot
ggplot(assocDF, aes(y=terms)) +
  geom_point(aes(x=value), data=assocDF, col='#c00c00') +
  theme_gdocs() + 
  geom_text(aes(x=value,label=value), colour="red",hjust="inward", vjust ="inward" , size=3)

# Which term is the most frequent?
idx <- which.max(topTermsA$freq)
topTermsA[idx, ]

# Order
exampleReOrder <- topTermsA[order(topTermsA$freq, decreasing = T),]

# Remove row attributes
rownames(exampleReOrder) <- NULL
head(exampleReOrder)

# Reduce TDM
reducedTDM <- removeSparseTerms(cleanMatTDMm, sparse=0.985) #shoot for ~50 terms; 1.5% of cells in row have a value  
reducedTDM

# Organize the smaller TDM
reducedTDM <- as.data.frame(as.matrix(reducedTDM))

# Basic Hierarchical Clustering
hc <- hclust(dist(reducedTDM))
plot(hc,yaxt='n')

ggdendrogram(hc, rotate=FALSE)

# Get Row Sums & organize
decTDMv <- sort(rowSums(cleanMatTDM), decreasing = TRUE)
decDF   <- data.frame(word = names(decTDMv), freq = decTDMv)

# Review all Palettes
display.brewer.all()

# Choose a color & drop light ones
pal <- brewer.pal(8, "Blues")
pal <- pal[-(1:2)]

# Make simple word cloud
# Reminder to expand device pane
wordcloud(decDF$word,
          decDF$freq,
          max.words    = 50,
          random.order = FALSE,
          colors       = pal,
          scale        = c(2,1))



# apply the function to JUST THE TEXT COLUMN
text <- basicSubs(sep2020$text)

# replacing the original text column with the new one
sep2020[["text"]] <- text

# Create custom stop words
stops <- c(stopwords('SMART'), 'smh', 'rofl', 'nba', 'game')

# sampling the dataframe
index <- sample(1:nrow(sep2020), size = 0.03*nrow(sep2020))
small_sep2020 <- sep2020[index,]

#### 
# Use sum with stri_count on the newt text object
lebron  <- sum(stri_count(small_sep2020$text, fixed ='lebron'))
giannis  <- sum(stri_count(small_sep2020$text, fixed ='giannis'))
butler <- sum(stri_count(small_sep2020$text, regex ='butler'))

# Organize term objects into a data frame
termFreq <- data.frame(terms = c('LeBron','Giannis','Butler'),
                       freq  = c(lebron,giannis, butler))

# Examine
termFreq

# Plot a geom_bar with ggplot2 by filling in the correct data,
# adding a layers "theme_gdocs() + theme(legend.position = "none")"
ggplot(termFreq, aes(x = reorder(terms, freq), y = freq,fill=freq)) + 
  geom_bar(stat = "identity") + coord_flip() + 
  theme_gdocs() + theme(legend.position = "none")

# Apply the VCorpus Function to a VectorSource of the original text object
# Hint: only pass in the vector NOT the entire dataframe using a $
cleanTxt <- VCorpus(VectorSource(small_sep2020$text))

# Clean the Corpus with your cleanCorpus function, this will take a few seconds
cleanTxt <- cleanCorpus(cleanTxt, stops)

# Construct a DTM in an object called cleanMat
cleanMatm  <- DocumentTermMatrix(cleanTxt)

# Construct a TDM in an object called cleanMat
cleanMatTDMm <- TermDocumentMatrix(cleanTxt)

# Switch this to a simple matrix still called cleanMat
cleanMatTDM <- as.matrix(cleanMatTDMm)

# Switch this to a simple matrix still called cleanMat
cleanMat <- as.matrix(cleanMatm)

# Get the most frequent terms
topTermsA <- colSums(cleanMat)

# Add the terms
topTermsA <- data.frame(terms = colnames(cleanMat), freq = topTermsA)

# Remove row attributes
rownames(topTermsA) <- NULL

# Review
head(topTermsA)

# Simple barplot; values greater than 15
topWords      <- subset(topTermsA, topTermsA$freq >= 840) 
topWords      <- topWords[order(topWords$freq, decreasing=F),]

# Chg to factor for ggplot
topWords$terms <- factor(topWords$terms, 
                         levels=unique(as.character(topWords$terms))) 

ggplot(topWords, aes(x=terms, y=freq)) + 
  geom_bar(stat="identity", fill='darkred') + 
  coord_flip()+ theme_gdocs() +
  geom_text(aes(label=freq), colour="white",hjust=1.25, size=3.0)

# Inspect word associations
associations <- findAssocs(cleanMatm, 'lebron', 0.30)
associations

# Organize the word associations
assocDF <- data.frame(terms=names(associations[[1]]),
                      value=unlist(associations))
assocDF$terms <- factor(assocDF$terms, levels=assocDF$terms)
rownames(assocDF) <- NULL
assocDF

# Make a dot plot
ggplot(assocDF, aes(y=terms)) +
  geom_point(aes(x=value), data=assocDF, col='#c00c00') +
  theme_gdocs() + 
  geom_text(aes(x=value,label=value), colour="red",hjust="inward", vjust ="inward" , size=3)

# Which term is the most frequent?
idx <- which.max(topTermsA$freq)
topTermsA[idx, ]

# Order
exampleReOrder <- topTermsA[order(topTermsA$freq, decreasing = T),]

# Remove row attributes
rownames(exampleReOrder) <- NULL
head(exampleReOrder)

# Reduce TDM
reducedTDM <- removeSparseTerms(cleanMatTDMm, sparse=0.985) #shoot for ~50 terms; 1.5% of cells in row have a value  
reducedTDM

# Organize the smaller TDM
reducedTDM <- as.data.frame(as.matrix(reducedTDM))

# Basic Hierarchical Clustering
hc <- hclust(dist(reducedTDM))
plot(hc,yaxt='n')

ggdendrogram(hc, rotate=FALSE)

# Get Row Sums & organize
decTDMv <- sort(rowSums(cleanMatTDM), decreasing = TRUE)
decDF   <- data.frame(word = names(decTDMv), freq = decTDMv)

# Review all Palettes
display.brewer.all()

# Choose a color & drop light ones
pal <- brewer.pal(8, "Blues")
pal <- pal[-(1:2)]

# Make simple word cloud
# Reminder to expand device pane
wordcloud(decDF$word,
          decDF$freq,
          max.words    = 50,
          random.order = FALSE,
          colors       = pal,
          scale        = c(2,1))



# apply the function to JUST THE TEXT COLUMN
text <- basicSubs(oct2020$text)

# replacing the original text column with the new one
oct2020[["text"]] <- text

# Create custom stop words
stops <- c(stopwords('SMART'), 'smh', 'rofl', 'nba', 'game')

# sampling the dataframe
index <- sample(1:nrow(oct2020), size = 0.05*nrow(oct2020))
small_oct2020 <- oct2020[index,]

#### 
# Use sum with stri_count on the newt text object
lebron  <- sum(stri_count(small_oct2020$text, fixed ='lebron'))
giannis  <- sum(stri_count(small_oct2020$text, fixed ='giannis'))
butler <- sum(stri_count(small_oct2020$text, regex ='butler'))

# Organize term objects into a data frame
termFreq <- data.frame(terms = c('LeBron','Giannis','Butler'),
                       freq  = c(lebron,giannis, butler))

# Examine
termFreq

# Plot a geom_bar with ggplot2 by filling in the correct data,
# adding a layers "theme_gdocs() + theme(legend.position = "none")"
ggplot(termFreq, aes(x = reorder(terms, freq), y = freq,fill=freq)) + 
  geom_bar(stat = "identity") + coord_flip() + 
  theme_gdocs() + theme(legend.position = "none")

# Apply the VCorpus Function to a VectorSource of the original text object
# Hint: only pass in the vector NOT the entire dataframe using a $
cleanTxt <- VCorpus(VectorSource(small_oct2020$text))

# Clean the Corpus with your cleanCorpus function, this will take a few seconds
cleanTxt <- cleanCorpus(cleanTxt, stops)

# Construct a DTM in an object called cleanMat
cleanMatm  <- DocumentTermMatrix(cleanTxt)

# Construct a TDM in an object called cleanMat
cleanMatTDMm <- TermDocumentMatrix(cleanTxt)

# Switch this to a simple matrix still called cleanMat
cleanMatTDM <- as.matrix(cleanMatTDMm)

# Switch this to a simple matrix still called cleanMat
cleanMat <- as.matrix(cleanMatm)

# Get the most frequent terms
topTermsA <- colSums(cleanMat)

# Add the terms
topTermsA <- data.frame(terms = colnames(cleanMat), freq = topTermsA)

# Remove row attributes
rownames(topTermsA) <- NULL

# Review
head(topTermsA)

# Simple barplot; values greater than 15
topWords      <- subset(topTermsA, topTermsA$freq >= 840) 
topWords      <- topWords[order(topWords$freq, decreasing=F),]

# Chg to factor for ggplot
topWords$terms <- factor(topWords$terms, 
                         levels=unique(as.character(topWords$terms))) 

ggplot(topWords, aes(x=terms, y=freq)) + 
  geom_bar(stat="identity", fill='darkred') + 
  coord_flip()+ theme_gdocs() +
  geom_text(aes(label=freq), colour="white",hjust=1.25, size=3.0)

# Inspect word associations
associations <- findAssocs(cleanMatm, 'lebron', 0.30)
associations

# Organize the word associations
assocDF <- data.frame(terms=names(associations[[1]]),
                      value=unlist(associations))
assocDF$terms <- factor(assocDF$terms, levels=assocDF$terms)
rownames(assocDF) <- NULL
assocDF

# Make a dot plot
ggplot(assocDF, aes(y=terms)) +
  geom_point(aes(x=value), data=assocDF, col='#c00c00') +
  theme_gdocs() + 
  geom_text(aes(x=value,label=value), colour="red",hjust="inward", vjust ="inward" , size=3)

# Which term is the most frequent?
idx <- which.max(topTermsA$freq)
topTermsA[idx, ]

# Order
exampleReOrder <- topTermsA[order(topTermsA$freq, decreasing = T),]

# Remove row attributes
rownames(exampleReOrder) <- NULL
head(exampleReOrder)

# Reduce TDM
reducedTDM <- removeSparseTerms(cleanMatTDMm, sparse=0.985) #shoot for ~50 terms; 1.5% of cells in row have a value  
reducedTDM

# Organize the smaller TDM
reducedTDM <- as.data.frame(as.matrix(reducedTDM))

# Basic Hierarchical Clustering
hc <- hclust(dist(reducedTDM))
plot(hc,yaxt='n')

ggdendrogram(hc, rotate=FALSE)

# Get Row Sums & organize
decTDMv <- sort(rowSums(cleanMatTDM), decreasing = TRUE)
decDF   <- data.frame(word = names(decTDMv), freq = decTDMv)

# Review all Palettes
display.brewer.all()

# Choose a color & drop light ones
pal <- brewer.pal(8, "Blues")
pal <- pal[-(1:2)]

# Make simple word cloud
# Reminder to expand device pane
wordcloud(decDF$word,
          decDF$freq,
          max.words    = 50,
          random.order = FALSE,
          colors       = pal,
          scale        = c(2,1))
