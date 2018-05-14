#############################
###     Assignment 1      ###
#############################

### Read data into working directory

library("readr")

setwd("~/Documents/Universität/Københavns Universitet/Security Risk Management/3. Semester/Social Data Science/Assignment 1")
df = read_csv("https://raw.githubusercontent.com/MuseumofModernArt/collection/master/Artworks.csv")

### Create a new dataframe of the stock of paintings at MOMA for each month in the year.

library(zoo)
library(plyr)

# New Dataframe
df$DateAcquired <- as.yearmon(df$DateAcquired)
stock_my <- count(na.omit(df$DateAcquired))

# Binwidth

h = (max(as.numeric(stock_my$x)) - min(as.numeric(stock_my$x)) / sqrt(732))

library(ggplot)
       
ggplot(stock_my, aes(x = as.Date(x))) + 
  geom_bar(color = "red", binwidth = h) + 
  scale_x_date(labels = date_format("%Y"), breaks = "10 years", limits = c(min(as.Date(stock_my$x)), max(as.Date(stock_my$x)))) +
  labs(title = "MOMA Stock of Paintings 1929 - 2015", x = "Year", y = "Stock of Paintings") +
  theme(plot.title = element_text(lineheight=.8, face="bold"))
  
### Create the same plot but this time the color should reflect the stock of paintings for curator approved and non-curator approved paintings, respectively

# New dataframe

stock_cu <- df[, c(11, 12)]
stock_cu <- count(stock_cu)

# Plot

ggplot(stock_cu, aes(x = as.Date(DateAcquired), fill = CuratorApproved)) + 
  geom_bar(color = "red", binwidth = h, position = "dodge") + 
  scale_x_date(labels = date_format("%Y"), breaks = "10 years", limits = c(min(as.Date(stock_my$x)), max(as.Date(stock_my$x)))) +
  scale_fill_manual(values=c("red", "green")) +
  labs(title = "MOMA Stock of Paintings 1929 - 2015", x = "Year", y = "Stock of Paintings") +
  theme(plot.title = element_text(lineheight=.8, face="bold"))

### Create a new dataframe of the stock of paintings grouped by what department the painting belongs to

# New dataframe

stock_dp <- df[, c(10, 11)]
stock_dp <- count(stock_dp)

### Plot this data frame using ggplot2

ggplot(stock_dp, aes(x = as.Date(DateAcquired))) + 
  geom_freqpoly(binwidth = h, aes(colour = Department)) + 
  scale_x_date(labels = date_format("%Y"), breaks = "10 years", limits = c(min(as.Date(stock_my$x)), max(as.Date(stock_my$x)))) +
  scale_colour_brewer(palette="Set1") +  
  labs(title = "MOMA Stock of Paintings 1929 - 2015", x = "Year", y = "Stock of Paintings") +
  theme(plot.title = element_text(lineheight=.8, face="bold")) +
  guides(colour = guide_legend(override.aes = list(size=10)))

# Which department has had the highest increase in their stock of paintings?
  # Painting & Sculpture

### Write a piece of code that counts the number of paintings by each artist in the dataset. List the 10 painters with the highest number of paintings in MOMA’s collection. 

# New dataframe

count_ar <- count(df$Artist)
count_ar <- count_ar[-1, ]

# Order dataframe

count_ar <- count_ar[order(-as.numeric(count_ar$freq), decreasing = FALSE),]

# List the 10 painters with the highest number of paintings in MOMA’s collection. 

top_10 <- head(count_ar, n = 10)
