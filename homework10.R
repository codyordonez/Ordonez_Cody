# Homework 10 ------------------------------------------------------------

# Pre-Flight --------------------------------------------------------------

# Load libraries
library(rtweet)

# Search for 1000 tweets using the "Astroworld" hashtag
rt <- search_tweets(
  "#Astroworld", n = 1000, include_rts = FALSE
)

# Preview tweets data
rt

# Preview users data
users_data(rt)

# Get all live tweets for 10 seconds
rtLive <- stream_tweets(timeout=10)

# Preview live tweets data
rtLive

# Preview live tweets users data
users_data(rtLive)

# The rt and rtLive datasets are already rectangular and ready for analyses
