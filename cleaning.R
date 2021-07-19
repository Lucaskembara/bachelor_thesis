# This script is for the cleaning of tweets from the KRO dataset.
library(plyr)
library(readr)
library(dplyr)
library(qdap)
library(tm)

set.seed(10)

# Setting wd path
setwd("~/Documents/Thesis/data")
mydir <- "covid19-tweets-master"
files <- list.files(path=mydir, pattern="*.csv", full.names=TRUE)

# Combining data and removing duplicates
dat <- ldply(files, read_csv)
dat <- distinct(dat) %>%
  tibble()

# Subset relevant columns
columns <- c("text", "source", "display_text_width", "is_quote", "is_retweet",
             "favorite_count", "retweet_count", "hashtags", "lang", "name",
             "description", "followers_count", "friends_count", "listed_count",
             "statuses_count", "favourites_count", "verified")
dat <- dat[,columns]

# Cleaning text column to a clean_text column
dat <- dat[dat$is_retweet == FALSE,] # Remove retweets
dat$clean_text <- gsub("(RT|via)((?:\\b\\W*@\\w+)+)", " ", dat$text) # Remove retweets
dat$clean_text <- gsub("@\\w+", "", dat$clean_text) # Remove mentions
dat$clean_text <- tolower(dat$clean_text) # To lowercase
dat$clean_text <- gsub("http.*\\b"," ", dat$clean_text) # Remove URLs
dat$clean_text <- gsub("https.*\\b"," ", dat$clean_text) # Remove URLs
dat$clean_text <- gsub("\\b+RT", " ", dat$clean_text) # Remove RT
dat$clean_text <- gsub("[^[:alnum:][:space:]']", "", dat$clean_text) # Remove punctuation except '
dat$clean_text <- gsub("[[:digit:]]", "", dat$clean_text) # Remove digits
dat$clean_text <- gsub("([[:alpha:]])\\1{2,}", "\\1", dat$clean_text) # Replacing repeating characters e.g aaa -> a

# Resetting row index
rownames(dat) <- NULL

# Filtering on keyword(s)
dat_filtered <- dplyr::filter(dat, grepl("chinees | chinezen", clean_text))

# Removing stopwords
dat$clean_text <- rm_stopwords(dat$clean_text, tm::stopwords("dutch"), separate = FALSE)
