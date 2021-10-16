# Practium 1 -------------------------------------------------------------

# Research Question: What are the most salient spam vs. ham content differences when comparing spam and ham (spam and not spam) from test (SMS) messages from the spam-ham dataset?
  
# This script 1) Imports the appropriate data from the spam-ham dataset. 2) Prepares the data for analysis. 3) Conducts a TF-IDF analysis comparing spam and ham and describing the most salient spam vs. ham content differences, which is done by finding and displaying both the top 15 most salient monograms and top 15 most salient bigrams for each classification (spam and ham).  4) Visualizes the results with a plot, specifically the top 15 most salient monograms and top 15 most salient bigrams for each of the two classifications (spam and ham). 

# Pre-flight --------------------------------------------------------------

# Load libraries
library(tidyverse)
library(tidytext)

# Set the working directory
setwd("/home/cao2246/R/Ordonez_Cody_2")

# Load data
data <- read_csv("spam_ham.csv") # This dataset was downloaded from Canvas


# TF-IDF Analysis  --------------------------------------------------------

# Monograms  --------------------------------------------------------

# Save word data (by classification - ham or spam) of most common words excluding stop words for analyses
data_word_n <- data %>%
  unnest_tokens(word, Message) %>% # Tokenize by word; Message is the text of the SMS message
  anti_join(stop_words) %>% # anti_join() is used to remove stop words
  count(Type, word, sort = TRUE) # Type is the classification of the SMS message (ham or spam)

# Get word count by classification compared to total count of each word
total_words <- data_word_n %>% 
  group_by(Type) %>% 
  summarize(total = sum(n))

# Create a new data frame combining information on total number of words per classification and term frequency counts
data_word_n <- left_join(data_word_n, total_words)

# Get term frequency (TF), inverse document frequency (IDF), and TF-IDF 
data_word_n <- data_word_n %>%
  bind_tf_idf(word, Type, n) # Calculates TF, IDF, and TF-IDF from word totals and TFs

# Display 15 most salient monograms in order of most salient for each of the two classifications (ham and spam)

data_word_n %>%
  select(-total) %>%
  arrange(desc(tf_idf)) %>% 
  filter(Type == "ham") %>% 
  slice(1:15)

data_word_n %>%
  select(-total) %>%
  arrange(desc(tf_idf)) %>% 
  filter(Type == "spam") %>% 
  slice(1:15)


# Bigrams  --------------------------------------------------------

stop_words_bounded <- paste0("\\b", stop_words$word, "\\b", collapse = "|") # Will be used to deal with stop words

bigrams <- data %>%
  unnest_tokens(bigram, Message, token = "ngrams", n=2) %>% # Tokenize by bigram
  filter(str_count(bigram,stop_words_bounded) < 2) %>% # Remove double stop words
  count(Type, bigram, sort = TRUE)

# Get bigram count by classification compared to total count of each bigram
total_bigrams <- bigrams %>% 
  group_by(Type) %>% 
  summarize(total = sum(n))

# Create a new data frame combining information on total number of bigrams per classification and term frequency counts
bigrams <- left_join(bigrams, total_bigrams)

# Get term frequency (TF), inverse document frequency (IDF), and TF-IDF 
bigrams <- bigrams %>%
  bind_tf_idf(bigram, Type, n) # Calculates TF, IDF, and TF-IDF from bigram totals and TFs

# Display 15 most salient bigrams in order of most salient for each of the two classifications (ham and spam)

bigrams %>%
  select(-total) %>%
  arrange(desc(tf_idf)) %>% 
  filter(Type == "ham") %>% 
  slice(1:15)

bigrams %>%
  select(-total) %>%
  arrange(desc(tf_idf)) %>% 
  filter(Type == "spam") %>% 
  slice(1:15)


# Visualize frequency of the top 15 most salient monograms for each of the two classifications (ham and spam)

# Plot TF-IDF for each of the classifications (by 15 most salient monograms per type)
data_word_n %>%
  arrange(desc(tf_idf)) %>%
  group_by(Type) %>%
  slice(1:15) %>% 
  ungroup() %>%
  mutate(word = factor(word, levels = rev(unique(word)))) %>%
  ggplot(aes(word, tf_idf, fill = Type)) +
  geom_col(show.legend = FALSE) +
  labs(x = NULL, y = "tf-idf") +
  coord_flip() +
  facet_wrap(~Type, ncol = 2, scales = "free_y")


# Visualize frequency of the top 15 most salient bigrams for each of the two classifications (ham and spam)

# Plot TF-IDF for each of the classifications (by 15 most salient bigrams per type)
bigrams %>%
  arrange(desc(tf_idf)) %>%
  group_by(Type) %>%
  slice(1:15) %>% 
  ungroup() %>%
  mutate(bigram = factor(bigram, levels = rev(unique(bigram)))) %>% 
  ggplot(aes(bigram, tf_idf, fill = Type)) +
  geom_col(show.legend = FALSE) +
  labs(x = NULL, y = "tf-idf") +
  coord_flip() +
  facet_wrap(~Type, ncol = 2, scales = "free_y")
