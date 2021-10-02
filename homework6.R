# Homework #6 -------------------------------------------------------------

# This script 1) Imports the appropriate data from the shared data sets. 2) Conducts a TF-IDF analysis comparing each of the 2015 debates. 3) Visualizes the top 10 most salient terms for each of the 2015 debates. 

# Pre-flight --------------------------------------------------------------

# Load libraries 
library(tidyverse)
library(tidytext)
library(textstem)

# Set the working directory
setwd("/home/cao2246/R/Ordonez_Cody_2")

# Load data
data <- read_csv("datasets/gop_debates.csv")

# Stem and lemmatize
stem_strings("immigration border deport refugee visa citizen alien undocumented naturalization mexico")
lemmatize_strings("immigration border deport refugee visa citizen alien undocumented naturalization mexico")

# Save terms for later
immigration_terms <- "migrat|border|deport|refuge|visa|citizen|alien|document|natural|mexic"

# Address stop words
stop_words_bounded <- paste("\\b", stop_words$word, "\\b", collapse = "|")

# Immigration framegram (top 10 immigration terms) - Trump
data %>%
  filter(who == "TRUMP") %>%
  unnest_tokens(trigram, text, token = "ngrams", n=3) %>% 
  count(trigram, sort = TRUE) %>%
  filter(str_detect(trigram,immigration_terms)) %>% 
  filter(str_count(trigram,stop_words_bounded) < 1) %>% 
  filter(n > 1) %>%
  mutate(trigram = reorder(trigram, n)) %>%
  slice(1:10) %>%
  ggplot(aes(x=trigram, y=n)) +
  geom_col() +
  xlab(NULL) +
  coord_flip()

# Immigration framegram (top 10 immigration terms) - Cruz
data %>%
  filter(who == "CRUZ") %>%
  unnest_tokens(trigram, text, token = "ngrams", n=3) %>% 
  count(trigram, sort = TRUE) %>%
  filter(str_detect(trigram,immigration_terms)) %>% 
  filter(str_count(trigram,stop_words_bounded) < 1) %>% 
  filter(n > 1) %>%
  mutate(trigram = reorder(trigram, n)) %>%
  slice(1:10) %>%
  ggplot(aes(x=trigram, y=n)) +
  geom_col() +
  xlab(NULL) +
  coord_flip()

# Immigration framegram (top 10 immigration terms) - Rubio
data %>%
  filter(who == "RUBIO") %>%
  unnest_tokens(trigram, text, token = "ngrams", n=3) %>% 
  count(trigram, sort = TRUE) %>%
  filter(str_detect(trigram,immigration_terms)) %>% 
  filter(str_count(trigram,stop_words_bounded) < 1) %>% 
  filter(n > 1) %>%
  mutate(trigram = reorder(trigram, n)) %>%
  slice(1:10) %>%
  ggplot(aes(x=trigram, y=n)) +
  geom_col() +
  xlab(NULL) +
  coord_flip()
