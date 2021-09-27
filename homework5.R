# Homework #4 -------------------------------------------------------------

# This script 1) Imports the appropriate data from the shared data sets. 2) Conducts a TF-IDF analysis comparing each of the 2015 debates. 3) Visualizes the top 10 most salient terms for each of the 2015 debates. 

# Pre-flight --------------------------------------------------------------

# Load libraries 
library(tidyverse)
library(tidytext)

# Set the working directory
setwd("/home/cao2246/R/Ordonez_Cody_2")

# Load data
data <- read_csv("datasets/gop_debates.csv")

# Save word data of most common words excluding stop words for analyses
data_word_n <- data %>%
  unnest_tokens(word, text) %>%
  # Use anti_join() to remove stop words
  anti_join(stop_words) %>%
  # Filter out "fffd" which is listed many times in the data due to the code being unable to read or translate some characters
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

# Display 10 most salient words (in order of most salient) for each debate (in chronological order)

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


# As we can see from the data, plots, and results of the TF-IDF analyses, the most salient topics discussed for each of the 2015 GOP primary debates are as follows:

# 2015-08-06: Veteran Affairs (va), Education (curriculum)
# 2015-08-28: Healthcare (medicare), Fantasy football? (fantasy, football). Fantasy football doesn't exactly seem like a political issue... but we can run some code to see if "fantasy football" appears as a bigram. I will do this at the end.
# 2015-09-16: Drug policy (marijuana), Prison system (jail), Vaccine regulation/mandates (vaccines)
# 2015-12-15: Foreign affairs (gadhafi, assad), Immigration (refugees), Internet safety/privacy (metadata)
# 2016-01-14: Taxes (tariff, vat), Gun control (guns, gun)
# 2016-01-28: Racial profiling (profiling), Economic policy (entrepeneur), Puerto Rico (puerto)
# 2016-02-06: Fifth Amendment/Eminent domain (eminent, domain, property), Military/Veteran Affairs (v.a, veteran, draft)
# 2016-02-13: Fifth Amendment/Eminent domain (eminent, domain), New Supreme Court nomination (scalia, nominate), Poverty (antipoverty)
# 2016-02-25: Immigration (hispanic, deportation, daca), Foreign affairs (palestinians, israel)
# 2016-03-03: Interrogation tactics/Torture (waterboarding, breathe)
# 2016-09-12: Monetary policy (banks, dollar, reserve), Carbon emissions/Carbon footprint (carbon)


# 2015-08-28 Question: Does "fantasy football appear as a bigram?

bigrams <- data %>%
  #Tokenize by bigram
  unnest_tokens(bigram, text, token = "ngrams", n=2) %>%
  #Filter only the relevant date
  filter(date == "2015-08-28") %>%
  #Count and sort by word
  count(bigram, sort = TRUE)

# Separate bigrams into individual consecutive words
bigrams_separated <- bigrams %>%
  separate(bigram, c("word1", "word2"), sep = " ")

# Display results filtering "fantasy" and "football" as word1 and word2
bigrams_separated %>%
  filter(word1 == "fantasy", word2 == "football")

# 2015-08-28 Update: As we can see, "fantasy football" does in fact appear as a bigram four times. Thus, we can conclude that fantasy football was relevant to a political issue or topic during this debate.
