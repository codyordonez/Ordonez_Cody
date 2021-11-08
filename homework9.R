# Homework 9------------------------------------------------------------

# Pre-Flight --------------------------------------------------------------

# Load libraries
library(tidyverse)
library(stringr)
library(tidytext)

# Load data and create labels
data <- read_csv("spam_ham.csv") %>%
  select(Type, Message) %>% 
  mutate(spam = ifelse(Type=="spam","yes","no")) %>% 
  unique()

# Use summative content analysis to do feature engineering, note that the the following terms indicate that the message should be considered spam
data_engineered <- data %>% 
  mutate(Message = str_replace_all(Message, "[^[:graph:]]", " "),
         Message =tolower(Message),
         mobile = str_count(Message, "mobile"),
         claim = str_count(Message, "claim"),
         prize = str_count(Message, "prize"),
         won = str_count(Message, "won"),
         win = str_count(Message, "win"),
         nokia = str_count(Message, "nokia"),
         urgent = str_count(Message,"urgent"),
         camera = str_count(Message, "camera"),
         customer = str_count(Message, "customer"),
         rate = str_count(Message,"rate"),
         cash = str_count(Message, "cash"),
         free = str_count(Message,"free"),
         number = str_count(Message, "0") + str_count(Message, "1") + str_count(Message, "2") + str_count(Message, "3") + str_count(Message, "4") + str_count(Message, "5") + str_count(Message, "6") + str_count(Message, "7") + str_count(Message, "8") + str_count(Message, "9")) %>% 
  select(Message, spam, mobile:number)

#  Create Train & Test Sets -----------------------------------------------

# Create training set based on an 80/20 split
training_set <- data_engineered %>% slice_sample(prop =.80)

# Create testing set
test_set <- data_engineered %>% anti_join(training_set, by="Message")

# Create training set with No Message
training_set_no_id <- training_set %>% select(-Message)

# Predictive models are created and compared in "homework9_2.R"
