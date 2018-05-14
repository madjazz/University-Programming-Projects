# Load Libraries

library(rvest)
library(stringr)
library(rlist)
library(tm)
library(SnowballC)
library(topicmodels)
library(ggplot2)
library(RColorBrewer)
library(colorRamps)
library(ggthemes)
library(viridis)
library(wordcloud)
library(dplyr)
library(reshape2)
library(tikzDevice)

# Build Functions

# Webscraper

scrape <- function(url) {
  read_html(url) %>% 
    html_nodes("p") %>% 
    html_text()
}

# List Cleaner

list_clear <- function(list, n) {
  list[n][c(grep(" ", list[n]))]
}

# Text Cleaner

c_clear <- function(chunk) {
  chunk <- gsub("\\W", " ", chunk)
  chunk <- gsub("\\s+", " ", chunk)
  chunk <- gsub("^\\s", "", chunk)
  chunk <- gsub("\\s+$", "", chunk)
  chunk <- tolower(chunk)
}


# China 1998

## Download Text

c98 <- read_html("http://www.china.org.cn/e-white/5/index.htm")
url1 <- c98 %>%  html_node("a") %>% html_attr("href")
url2 <- c98 %>%  html_nodes("strong a") %>% html_attr("href")
urls <- c(url1, url2)
urls <- paste("http://www.china.org.cn/e-white/5/", urls, sep = "")

c98 <- lapply(urls, scrape)

## Clean Text

c98 <- gsub("\n", "", unlist(c98))
c98 <- list_clear(c98, seq(length(c98)))
c98 <- c_clear(c98)
c98 <- c98[-length(c98)]
c98 <- paste(c98, collapse = '')

# China 2000

c00 <- read_html("http://www.china.org.cn/e-white/2000/index.htm")
urls <- c00 %>%  html_nodes("h4 a , p a, table~ a") %>% html_attr("href")
urls <- paste("http://www.china.org.cn/e-white/2000/", urls, sep = "")

c00 <- lapply(urls, scrape)

c00 <- gsub("\n", "", unlist(c00))
c00 <- list_clear(c00, seq(length(c00)))
c00 <- c_clear(c00)
c00 <- c00[-length(c00)]
c00 <- paste(c00, collapse = '')

# China 2002

c02 <- read_html("http://www.china.org.cn/e-white/20021209/index.htm")
urls <- c02 %>% html_nodes("font a") %>% html_attr("href")
urls <- paste("http://www.china.org.cn/e-white/20021209/", urls, sep = "")

c02 <- lapply(urls, scrape)
c02 <- c02[1:8]

c02 <- gsub("\n", "", unlist(c02))
c02 <- list_clear(c02, seq(length(c02)))
c02 <- c_clear(c02)
c02 <- c02[-length(c02)]
c02 <- paste(c02, collapse = '')

# China 2004

c04 <- read_html("http://www.china.org.cn/e-white/20041227/index.htm")
urls <- c04 %>% html_nodes("font a") %>% html_attr("href")
urls <- paste("http://www.china.org.cn/e-white/20041227/", urls, sep = "")

c04 <- lapply(urls, scrape)
c04 <- c04[1:11]

c04 <- gsub("\n", "", unlist(c04))
c04 <- list_clear(c04, seq(length(c04)))
c04 <- c_clear(c04)
c04 <- c04[-length(c04)]
c04 <- paste(c04, collapse = '')

# China 2006

c06 <- read_html("http://www.china.org.cn/english/features/book/194421.htm")
urls <- c06 %>% html_nodes("div a") %>% html_attr("href")
urls <- urls[1:11]
urls <- paste("http://www.china.org.cn", urls, sep = "")

c06 <- lapply(urls, scrape)

c06 <- gsub("\n", "", unlist(c06))
c06 <- list_clear(c06, seq(length(c06)))
c06 <- c_clear(c06)
c06 <- paste(c06, collapse = '')

# China 2008

c08 <- read_html("http://www.china.org.cn/government/whitepaper/node_7060059.htm")
urls <- c08 %>% html_nodes("div a") %>% html_attr("href")
urls <- urls[1:15]
urls <- paste("http://www.china.org.cn/government/whitepaper/", urls, sep = "")

c08 <- lapply(urls, scrape)

c08 <- gsub("\n", "", unlist(c08))
c08 <- list_clear(c08, seq(length(c08)))
c08 <- c_clear(c08)
c08 <- paste(c08, collapse = '')

# China 2010

c10 <- read_html("http://www.china.org.cn/government/whitepaper/node_7114675.htm")
urls <- c10 %>% html_nodes("a") %>% html_attr("href")
urls <- urls[1:11]
urls <- paste("http://www.china.org.cn/government/whitepaper/", urls, sep = "")

c10 <- lapply(urls, scrape)

c10 <- gsub("\n", "", unlist(c10))
c10 <- list_clear(c10, seq(length(c10)))
c10 <- c_clear(c10)
c10 <- paste(c10, collapse = '')

# China 2013

c13 <- read_html("http://news.xinhuanet.com/english/china/2013-04/16/c_132312681.htm")
urls <- c13 %>% html_nodes(".page-Article") %>% html_attr("href")
urls <- urls[1:5]
urls[1] <- "http://news.xinhuanet.com/english/china/2013-04/16/c_132312681.htm"

c13 <- lapply(urls, scrape)
c13 <- c13[1:4]

c13 <- gsub("\n", "", unlist(c13))
c13 <- list_clear(c13, seq(length(c13)))
c13 <- c_clear(c13)
c13 <- c13[-length(c13)]
c13 <- paste(c13, collapse = '')

# China 2015

c15 <- read_html("http://news.xinhuanet.com/english/china/2015-05/26/c_134271001.htm")
urls <- c15 %>% html_nodes(".page-Article") %>% html_attr("href")
urls <- urls[1:7]
urls[1] <- "http://news.xinhuanet.com/english/china/2015-05/26/c_134271001.htm"

c15 <- lapply(urls, scrape)

c15 <- gsub("\n", "", unlist(c15))
c15 <- list_clear(c15, seq(length(c15)))
c15 <- c_clear(c15)
c15 <- paste(c15, collapse = '')

# Create Corpus

china <- Corpus(VectorSource(c(c98, c00, c02, c04, c06, c08, c10, c13, c15)))

# Remove Stop Words, White Space and Numbers

china <- tm_map(china, removeWords, stopwords("SMART"))
china <- tm_map(china, stripWhitespace)
china <- tm_map(china, removeNumbers)

# Stem Documents

china <- tm_map(china, stemDocument)

# Create Document Term Matrix

china <- DocumentTermMatrix(china)
rownames(china) <- c("1998", "2000", "2002", "2004", "2006", "2008", "2010", "2013", "2015")

# US 1998

us98 <- read_html("http://nssarchive.us/national-security-strategy-1998/")
urls <- us98 %>% html_nodes(".page-num") %>% html_text()
urls <- paste("http://nssarchive.us/national-security-strategy-1998/", seq(length(urls)), sep = "")

us98 <- lapply(urls, scrape)
us98[[1]][1] <- us98[[1]][-1]

us98 <- strsplit(unlist(us98), "\n")
pat <- "Pages: 1 2 3 4 5 6 7 8 9 10 11 12 13 14 15 16 17 18 19 20 21 22 23 24 25 26 27 28 29 30 31 32 33 34"
us98 <- gsub(pat, "", unlist(us98))
us98 <- list_clear(us98, seq(length(us98)))
us98 <- c_clear(us98)
us98 <- paste(us98, collapse = '')

## US 2000

us00 <- read_html("http://nssarchive.us/national-security-strategy-2000-2/")
urls <- us00 %>% html_nodes(".page-num") %>% html_text()
urls <- paste("http://nssarchive.us/national-security-strategy-2000-2/", seq(length(urls)), sep = "")

us00 <- lapply(urls, scrape)
us00 <- strsplit(unlist(us00), "\n")
pat <- "Pages: 1 2 3 4 5 6 7 8 9 10 11 12 13 14 15 16 17 18 19 20 21 22 23 24 25 26 27 28 29"
us00 <- gsub(pat, "", unlist(us00)) 
us00 <- gsub("&nbsp", "", unlist(us00))
us00 <- list_clear(us00, seq(length(us00)))
us00 <- c_clear(us00)
us00 <- us00[-c(1:10)]
us00 <- paste(us00, collapse = '')

# US 2001

us01 <- read_html("http://nssarchive.us/national-security-strategy-2000/")
urls <- us01 %>% html_nodes(".page-num") %>% html_text()
urls <- paste("http://nssarchive.us/national-security-strategy-2000/", seq(length(urls)), sep = "")

us01 <- lapply(urls, scrape)

us01 <- strsplit(unlist(us01), "\n")
pat <- "Pages: 1 2 3 4 5 6 7 8 9 10 11 12 13 14 15 16 17 18 19 20 21 22 23 24 25 26 27 28 29 30 31 32 33 34 35 36 37 38 39 40"
us01 <- gsub(pat, "", unlist(us01))
us01 <- list_clear(us01, seq(length(us01)))
us01 <- c_clear(us01)
us01 <- us01[-c(1:15)]
us01 <- paste(us01, collapse = '')

# US 2002

us02 <- read_html("http://nssarchive.us/national-security-strategy-2002/")
urls <- us02 %>% html_nodes(".page-num") %>% html_text()
urls <- paste("http://nssarchive.us/national-security-strategy-2002/", seq(length(urls)), sep = "")

us02 <- lapply(urls, scrape)

us02 <- strsplit(unlist(us02), "\n")
pat <- "Pages: 1 2 3 4 5 6 7 8 9 10 11 12"
us02 <- gsub(pat, "", unlist(us02))
us02 <- list_clear(us02, seq(length(us02)))
us02 <- c_clear(us02)
us02 <- us02[-c(1:8)]
us02 <- paste(us02, collapse = '')

# US 2006

us06 <- read_html("http://nssarchive.us/national-security-strategy-2006/")
urls <- us06 %>% html_nodes(".page-num") %>% html_text()
urls <- paste("http://nssarchive.us/national-security-strategy-2006/", seq(length(urls)), sep = "")

us06 <- lapply(urls, scrape)

us06 <- strsplit(unlist(us06), "\n")
pat <- "Pages: 1 2 3 4 5 6 7 8 9 10 11 12 13 14 15 16 17 18"
us06 <- gsub(pat, "", unlist(us06))
us06 <- list_clear(us06, seq(length(us06)))
us06 <- c_clear(us06)
us06 <- paste(us06, collapse = '')

# US 2010

us10 <- read_html("http://nssarchive.us/national-security-strategy-2010/")
urls <- us10 %>% html_nodes(".page-num") %>% html_text()
urls <- paste("http://nssarchive.us/national-security-strategy-2010/", seq(length(urls)), sep = "")

us10 <- lapply(urls, scrape)

us10 <- strsplit(unlist(us10), "\n")
pat <- "Pages: 1 2 3 4 5 6 7 8 9 10 11 12 13 14 15 16 17 18 19 20 21 22 23 24 25 26 27 28 29 30 31 32 33 34 35 36 37 38 39 40 41 42 43 44 45 46 47 48 49 50 51 52 53 54 55 56 57 58 59"
us10 <- gsub(pat, "", unlist(us10))
us10 <- list_clear(us10, seq(length(us10)))
us10 <- c_clear(us10)
us10 <- us10[-c(1:40)]
us10 <- paste(us10, collapse = '')

# US 2015

us15 <- read_html("https://raw.githubusercontent.com/madjazz/thesis/master/2015.txt") %>% html_text()

us15 <- gsub("\r", "", us15)
us15 <- gsub("\n", " ", us15)
us15 <- strsplit(us15, "%")
us15 <- c_clear(us15)

# Create Corpus

usa <- Corpus(VectorSource(c(us98, us00, us01, us02, us06, us10, us15)))

# Remove Stop Words, White Space and Numbers

usa <- tm_map(usa, removeWords, stopwords("SMART"))
usa <- tm_map(usa, stripWhitespace)
usa <- tm_map(usa, removeNumbers)

# Stem Documents

usa <- tm_map(usa, stemDocument)

# Create Document Term Matrix

usa <- DocumentTermMatrix(usa)
rownames(usa) <- c("1998", "2000", "2001", "2002", "2006", "2010", "2015")

# Harmonic Mean

harmonicMean <- function(logLikelihoods, precision=2000L) {
  library("Rmpfr")
  llMed <- median(logLikelihoods)
  as.double(llMed - log(mean(exp(-mpfr(logLikelihoods,
                                       prec = precision) + llMed))))
}

# China

# Set Parameters

k = 20
burnin = 1000
iter = 1000
keep = 50

fitted <- LDA(usa, k = k, method = "Gibbs",control = list(burnin = burnin, iter = iter, keep = keep) )

logLiks <- fitted@logLiks[-c(1:(burnin/keep))]

harmonicMean(logLiks)

sequ <- seq(2, 50, 1) 

fitted_many <- lapply(sequ, function(k) LDA(china, k = k, method = "Gibbs",control = list(burnin = burnin, iter = iter, keep = keep) ))

logLiks_many <- lapply(fitted_many, function(L)  L@logLiks[-c(1:(burnin/keep))])

hm_many <- sapply(logLiks_many, function(h) harmonicMean(h))

plot(sequ, hm_many, type = "l")

sequ[which.max(hm_many)]

c.dat <- cbind(sequ, hm_many)

# USA

k = 20
burnin = 1000
iter = 1000
keep = 50

fitted <- LDA(usa, k = k, method = "Gibbs",control = list(burnin = burnin, iter = iter, keep = keep) )

logLiks <- fitted@logLiks[-c(1:(burnin/keep))]

harmonicMean(logLiks)

# generate numerous topic models with different numbers of topics
sequ <- seq(2, 50, 1) # in this case a sequence of numbers from 1 to 50, by ones.
fitted_many <- lapply(sequ, function(k) LDA(usa, k = k, method = "Gibbs",control = list(burnin = burnin, iter = iter, keep = keep) ))

logLiks_many <- lapply(fitted_many, function(L)  L@logLiks[-c(1:(burnin/keep))])

hm_many <- sapply(logLiks_many, function(h) harmonicMean(h))

plot(sequ, hm_many, type = "l")

sequ[which.max(hm_many)]

u.dat <- cbind(sequ, hm_many)

# Harmonic Mean Visualization

data <- left_join(as.data.frame(c.dat), as.data.frame(u.dat), by = "sequ")
names(data) <- c("seq", "China", "USA")

data <- melt(data, id = "seq")
names(data) <- c("seq", "Country", "value")
data <- group_by(data, Country) %>% mutate(color = (max(value) == value))


p_hm <- ggplot(data, aes(x = seq, y = value, color = Country)) + 
  geom_line() +
  geom_point(aes(color = color)) +
  scale_color_manual("Legend", labels = c("China", NA, "Optimum", "USA"), values = c("red", NA, "black", "blue"), breaks = c("China", NA, "Optimum", "USA")) +
  ylab("") + 
  xlab("") + 
  labs(title="Optimal Number of Topics") +
  theme_tufte() +
  theme(plot.title = element_text(lineheight=.8, face="bold", vjust=1))


# Topic-Models

# Example

# Set parameters for Gibbs sampling

burnin <- 4000
iter <- 2000
thin <- 500
seed <-list(2003,5,63,100001,765)
nstart <- 5
best <- TRUE

# Select Number of Topics

k <- 7

# Run LDA using Gibbs Sampling

ex_lda <-LDA(china,k, method="Gibbs", control=list(nstart=nstart, seed = seed, best=best, burnin = burnin, iter = iter, thin=thin))

# Topic Distribution per Document

ex_lda.topics <- as.matrix(topics(c_lda))
write.table(ex_lda.topics, "ex_topics.txt")

# Top 15 Terms per Topic

ex_lda.terms <- as.matrix(terms(ex_lda,15))

write.table(ex_lda.terms, "ex_terms.txt")

# Topic Probabilities

ex_lda.probs <- as.data.frame(ex_lda@gamma)
write.table(ex_lda.probs, "ex_probs.txt")

# Visualizations

exprobs.viz <- ex_lda.probs
exprobs.viz$year <- rownames(ex_lda.topics)
colnames(exprobs.viz) <- c("Topic 1", "Topic 2", "Topic 3", "Topic 4", "Topic 5", "Topic 6", "Topic 7", "Year")
exprobs.viz <- melt(exprobs.viz)
names(exprobs.viz) <- c("Year", "Topic", "Value")

# Time-Series

p_exprobs.viz <- ggplot(exprobs.viz, aes(x = Year, y = Value, group = Topic,                       colour = Topic)) +
  geom_freqpoly(stat = "identity") +
  theme_tufte(base_family="Helvetica") +
  labs(title="Example: Topic Probabilities China 1998-2015",
       x = " ", y = " ") +
  theme(plot.title = element_text(lineheight=.8, face="bold",                   vjust=1)) +
  scale_colour_brewer(palette = "Set2")

# Heat Map

ex.viz <- ggplot(cprobs.viz, aes(x=Year, y=Topic, fill=Value)) +
  geom_tile(color="white", size=0.1) +
  scale_fill_viridis(name="Probability", option = "B") +
  coord_equal() +
  labs(x=" ", y=" ", title=" Example: Topic Probabilities China                       1998-2015") +
  theme_tufte(base_family="Helvetica") +
  theme(plot.title = element_text(lineheight=.8, face="bold",                   vjust=1)) +
  theme(axis.ticks=element_blank()) +
  theme(axis.text=element_text(size=7)) +
  theme(legend.title=element_text(size=8)) +
  theme(legend.text=element_text(size=6))

# Word Cloud

ex.wc <- posterior(ex_lda)$terms
ex.wc <- as.data.frame(ex.wc)
ex.wc <- ex.wc[c(1),]
ex.wc <- melt(ex.wc)

set.seed(1234)
p_ex.wc <- wordcloud(words = ex.wc$variable, scale = c(2.5,0.5), freq =                              ex.wc$value, max.words = 50, rot.per=0.35, 
                     colors=brewer.pal(8, "Dark2"))

# China

# Set parameters for Gibbs sampling

burnin <- 4000
iter <- 2000
thin <- 500
seed <-list(2003,5,63,100001,765)
nstart <- 5
best <- TRUE

# Select Number of Topics

k <- 11

# Run LDA using Gibbs Sampling

c_lda <-LDA(china,k, method="Gibbs", control=list(nstart=nstart, seed = seed, best=best, burnin = burnin, iter = iter, thin=thin))

# Topic Distribution per Document

c_lda.topics <- as.data.frame(topics(c_lda, 11))
write.table(c_lda.topics, "china_topics.txt")

# Top 15 Terms per Topic

c_lda.terms <- as.matrix(terms(c_lda,15))
write.table(c_lda.terms, "china_terms.txt")

# Topic Probabilities

c_lda.probs <- as.data.frame(c_lda@gamma)
write.table(c_lda.probs, "china_probs.txt")

# Visualization

cprobs.viz <- c_lda.probs
cprobs.viz$year <- colnames(c_lda.topics)
colnames(cprobs.viz) <- c("Topic 1", "Topic 2", "Topic 3", "Topic 4", "Topic 5", "Topic 6", "Topic 7", "Topic 8", "Topic 9", "Topic 10", "Topic 11","Year")
cprobs.viz <- melt(cprobs.viz)
names(cprobs.viz) <- c("Year", "Topic", "Value")

col_nr <- length(unique(cprobs.viz$Topic))
gen_pal <- colorRampPalette(brewer.pal(9, "Set1"))

p_cprobs.viz <- ggplot(cprobs.viz, aes(x = Year, y = Value, group = Topic,                       colour = Topic)) +
  geom_freqpoly(stat = "identity") +
  theme_tufte(base_family="Helvetica") +
  labs(title="Topic Probabilities China 1998-2015",
       x = " ", y = " ") +
  theme(plot.title = element_text(lineheight=.8, face="bold",                   vjust=1)) +
  scale_colour_manual(values = gen_pal(col_nr))

# Set parameters for Gibbs sampling

burnin <- 4000
iter <- 2000
thin <- 500
seed <-list(2003,5,63,100001,765)
nstart <- 5
best <- TRUE

# Select Number of Topics

k <- 7

# Run LDA using Gibbs Sampling

us_lda <-LDA(usa, k, method="Gibbs", control=list(nstart=nstart, seed = seed, best=best, burnin = burnin, iter = iter, thin=thin))

# Topic Distribution per Document

us_lda.topics <- as.data.frame(topics(us_lda, 7))
write.table(us_lda.topics, "usa_topics.txt")

# Top 15 Terms per Topic

us_lda.terms <- as.matrix(terms(us_lda,15))
write.table(us_lda.terms, "usa_terms.txt")

# Topic Probabilities

us_lda.probs <- as.data.frame(us_lda@gamma)
write.table(us_lda.probs, "usa_probs.txt")

# Visualization

usprobs.viz <- us_lda.probs
usprobs.viz$year <- rownames(us_lda.topics)
colnames(usprobs.viz) <- c("Topic 1", "Topic 2", "Topic 3", "Topic 4", "Topic 5", "Topic 6", "Topic 7", "Year")
usprobs.viz <- melt(usprobs.viz)
names(usprobs.viz) <- c("Year", "Topic", "Value")

col_nr.us <- length(unique(usprobs.viz$Topic))

p_usprobs.viz <- ggplot(usprobs.viz, aes(x = Year, y = Value, group = Topic,                       colour = Topic)) +
  geom_freqpoly(stat = "identity") +
  theme_tufte(base_family="Helvetica") +
  labs(title="Topic Probabilities USA 1998-2015",
       x = " ", y = " ") +
  theme(plot.title = element_text(lineheight=.8, face="bold",                   vjust=1)) +
  scale_colour_manual(values = gen_pal(col_nr.us))