require("twitteR")
require("dplyr")
require("caret")
require("randomForest")

# Download Tweets (Twitter OAuth not included!)

setup_twitter_oauth(…)

guardian <- userTimeline(87818409, n=3200)
independent <- userTimeline(16973333, n=3200)
sun <- userTimeline(34655603, n=3200)
mirror <- userTimeline(16887175, n=3200)

df_guard <- twListToDF(strip_retweets(guardian, strip_manual=TRUE, strip_mt=TRUE))
df_ind <- twListToDF(strip_retweets(independent, strip_manual=TRUE, strip_mt=TRUE))
df_sun <- twListToDF(strip_retweets(sun, strip_manual=TRUE, strip_mt=TRUE))
df_mir <- twListToDF(strip_retweets(mirror, strip_manual=TRUE, strip_mt=TRUE))

df <- rbind(df_guard, df_ind, df_sun, df_mir)

# Download Twitter Data for Reproduction

download.file("https://www.dropbox.com/s/bonrzrphx24c0rs/df.csv?dl=0", destfile = "df.csv")
df <- read.csv("df.csv")

# Download AFINN Dataset

download.file("http://www2.imm.dtu.dk/pubdb/views/edoc_download.php/6010/zip/imm6010.zip", destfile = "afinn.zip")
unzip("afinn.zip")
afinn <- read.table( "./AFINN/AFINN-111.txt", fill = TRUE)

# Compare Datasets

list <- strsplit(df$text, " ")
df_text <- data.frame(id=rep(df$id, sapply(list, length)), words=unlist(list))
df_text$words <- gsub("[^[:alnum:] ]", "", df_text$words)

names(afinn) <- c("words", "score")

temp <- left_join(df_text, afinn)
temp$score <- gsub("([a-zA-Z])", "", temp$score)
temp <- filter(temp, !is.na(temp$score))
temp <- left_join(temp, df)
temp <- temp[, c(1, 2, 3, 13)]

# Remove duplicated ID's according to lowest score

a <- do.call(rbind, by(temp, INDICES=list(temp$id), FUN=function(x) head(x[order(x$score), ], 1)))

# Standardize number of tweets according to lowest number using random samples

no <- count(a, screenName)

set.seed(42)

b <- rbind(sample_n(subset(a, a$screenName == "guardian"), min(no$n)), 
           sample_n(subset(a, a$screenName == "Independent"), min(no$n)), 
           sample_n(subset(a, a$screenName == "TheSun"), min(no$n)), 
           sample_n(subset(a, a$screenName == "DailyMirror"), min(no$n)))

# Classify tweets according to quality

b$class <- ifelse(b$screenName == "guardian", 1, ifelse(b$screenName == "Independent", 1, 0))
b <- filter(b, !is.na(b$score))
b$score <- factor(b$score, levels=c("-5", "-4", "-3", "-2", "-1", "0", "1", "2", "3", "4", "5"))
b$class <- factor(b$class)

# Split Dataset into Train and Test Set

smp_size <- floor(0.75 * nrow(b))

set.seed(123)
train_ind <- sample(seq_len(nrow(b)), size = smp_size)

train <- b[train_ind, ]
test <- b[-train_ind, ]

# Logistic Regression

m1 <- train(class ~ score,  data=train, method="glm", family="binomial")

m1
summary(m1)

pred_m1 = predict(m1, newdata=test)
confusionMatrix(data=pred_m1, test$class)


# Random Forest

m2 <- train(class ~ score, 
            data = train, 
            method = "rf") 
m2

pred_m2 = predict(m2, newdata=test)
confusionMatrix(data=pred_m2, test$class)