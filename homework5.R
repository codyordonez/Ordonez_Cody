# Homework #4 -------------------------------------------------------------

# This script 1) Imports the appropriate data from the shared data sets. 2) Analyzes the speaking complexity of each speaking turn using an appropriate complexity metric 3) Aggregates the data by candidate and returns a two column data frame: speaker | ave_complexity in descending order of complexity and 4) compares the ave_complexity of Trump with Bush, Cruz, and Walker and calculates the mean difference and displays a plot. 

# Pre-flight --------------------------------------------------------------

# Load libraries 
library(tidyverse)
library(tidytext)

# Set the working directory
setwd("/home/cao2246/R/Ordonez_Cody_2")

# Load data
data <- read_csv("datasets/gop_debates.csv")

#Save word data of most common words excluding stop words for analyses
data_word_n <- data %>%
  unnest_tokens(word, text) %>%
  #Use anti_join() to remove stop words
  anti_join(stop_words) %>%
  #Filter out "fffd" which is listed many times in the data due to the code being unable to read a character
  filter(!word %in% "fffd") %>%
  count(date, word, sort = TRUE)

# TF-IDF

# Get word count by debate compared to total count of each word
total_words <- data_word_n %>% 
  group_by(date) %>% 
  summarize(total = sum(n))

# Create a new data frame combining information on total number of words per debate and word frequency counts
data_word_n <- left_join(data_word_n, total_words)

# Get term frequency (TF), inverse document frequency (IDF), and TF-IDF 
data_word_n <- data_word_n %>%
  # Calculates TF, IDF, and TF-IDF from word totals and TFs
  bind_tf_idf(word, date, n)


# Display 10 most salient words for each debate (in chronological order)

data_word_n %>%
  select(-total) %>%
  arrange(desc(tf_idf)) %>% 
  filter(date == "2015-08-06") %>% 
  slice(1:10)

data_word_n %>%
  select(-total) %>%
  arrange(desc(tf_idf)) %>% 
  filter(date == "2015-08-28") %>% 
  slice(1:10)

data_word_n %>%
  select(-total) %>%
  arrange(desc(tf_idf)) %>% 
  filter(date == "2015-09-16") %>% 
  slice(1:10)

data_word_n %>%
  select(-total) %>%
  arrange(desc(tf_idf)) %>% 
  filter(date == "2015-12-15") %>% 
  slice(1:10)

data_word_n %>%
  select(-total) %>%
  arrange(desc(tf_idf)) %>% 
  filter(date == "2016-01-14") %>% 
  slice(1:10)

data_word_n %>%
  select(-total) %>%
  arrange(desc(tf_idf)) %>% 
  filter(date == "2016-01-28") %>% 
  slice(1:10)

data_word_n %>%
  select(-total) %>%
  arrange(desc(tf_idf)) %>% 
  filter(date == "2016-02-06") %>% 
  slice(1:10)

data_word_n %>%
  select(-total) %>%
  arrange(desc(tf_idf)) %>% 
  filter(date == "2016-02-13") %>% 
  slice(1:10)

data_word_n %>%
  select(-total) %>%
  arrange(desc(tf_idf)) %>% 
  filter(date == "2016-02-25") %>% 
  slice(1:10)

data_word_n %>%
  select(-total) %>%
  arrange(desc(tf_idf)) %>% 
  filter(date == "2016-03-03") %>% 
  slice(1:10)

data_word_n %>%
  select(-total) %>%
  arrange(desc(tf_idf)) %>% 
  filter(date == "2016-09-12") %>% 
  slice(1:10)


# Plot TF-IDF for each of the debates (by 10 most salient words per debate)

# This has the plots for the first 4 debates
data_word_n %>%
  arrange(desc(tf_idf)) %>%
  mutate(word = factor(word, levels = rev(unique(word)))) %>% 
  group_by(date) %>%
  filter(date == "2015-08-06" | date == "2015-08-28" | date == "2015-09-16" | date == "2015-12-15") %>%
  slice(1:10) %>% 
  ungroup() %>%
  ggplot(aes(word, tf_idf, fill = date)) +
  geom_col(show.legend = FALSE) +
  labs(x = NULL, y = "tf-idf") +
  facet_wrap(~date, ncol = 2, scales = "free") +
  coord_flip()

# This has the plots for the next 4 debates
data_word_n %>%
  arrange(desc(tf_idf)) %>%
  mutate(word = factor(word, levels = rev(unique(word)))) %>% 
  group_by(date) %>%
  filter(date == "2016-01-14" | date == "2016-01-28" | date == "2016-02-06" | date == "2016-02-13") %>%
  slice(1:10) %>% 
  ungroup() %>%
  ggplot(aes(word, tf_idf, fill = date)) +
  geom_col(show.legend = FALSE) +
  labs(x = NULL, y = "tf-idf") +
  facet_wrap(~date, ncol = 2, scales = "free") +
  coord_flip()

# This has the plots for the last 3 debates
data_word_n %>%
  arrange(desc(tf_idf)) %>%
  mutate(word = factor(word, levels = rev(unique(word)))) %>% 
  group_by(date) %>%
  filter(date == "2016-02-25" | date == "2016-03-03" | date == "2016-09-12") %>%
  slice(1:10) %>% 
  ungroup() %>%
  ggplot(aes(word, tf_idf, fill = date)) +
  geom_col(show.legend = FALSE) +
  labs(x = NULL, y = "tf-idf") +
  facet_wrap(~date, ncol = 2, scales = "free") +
  coord_flip()