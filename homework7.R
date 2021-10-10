# This script 1) Imports the appropriate data from the shared data sets. 2) Conducts a sentiment analysis to show the frequency of fear words used per talking turn when comparing Bush, Cruz, and Fiorina to Trump. 3) Visualizes the data for each of the candidates.

# Pre-flight --------------------------------------------------------------

# Load libraries 
library(tidyverse)
library(tidytext)
library(textstem)
library(fuzzyjoin)
library(dabestr)
library(ggplot2)

# Set the working directory
setwd("/home/cao2246/R/Ordonez_Cody_2")

# Load data
data <- read_csv("datasets/gop_debates.csv")

# Get NRC sentiment dictionary and filter by fear
nrc_fear <- get_sentiments("nrc") %>% 
  filter(sentiment == "fear")
  
nrc_analysis_Trump <- data %>% 
  filter(who == "TRUMP") %>%
  unnest_tokens(word, text, token = "words") %>%
  inner_join(nrc_fear) %>%
  count(word, sort = TRUE)

trump_fear_words <- sum(nrc_analysis_Trump$n)

trump_turns <- data %>%
  filter(who == "TRUMP") %>%
  count(who, sort = TRUE) %>%
  summarise(Trump_speaking_turns = sum(n))

trump_turns <- trump_turns$Trump_speaking_turns

trump_avg = trump_turns / trump_fear_words

print(paste0(round(trump_avg, digits=6), " is the average number of fear words used per talking turn by Trump"))

nrc_analysis_Bush <- data %>% 
  filter(who == "BUSH") %>%
  unnest_tokens(word, text, token = "words") %>%
  inner_join(nrc_fear) %>%
  count(word, sort = TRUE)

bush_fear_words <- sum(nrc_analysis_Bush$n)

bush_turns <- data %>%
  filter(who == "BUSH") %>%
  count(who, sort = TRUE) %>%
  summarise(Bush_speaking_turns = sum(n))

bush_turns <- bush_turns$Bush_speaking_turns

bush_avg = bush_turns / bush_fear_words

print(paste0(round(bush_avg, digits=6), " is the average number of fear words used per talking turn by Bush"))

nrc_analysis_Cruz <- data %>% 
  filter(who == "CRUZ") %>%
  unnest_tokens(word, text, token = "words") %>%
  inner_join(nrc_fear) %>%
  count(word, sort = TRUE)

cruz_fear_words <- sum(nrc_analysis_Cruz$n)

cruz_turns <- data %>%
  filter(who == "CRUZ") %>%
  count(who, sort = TRUE) %>%
  summarise(Cruz_speaking_turns = sum(n))

cruz_turns <- cruz_turns$Cruz_speaking_turns

cruz_avg = cruz_turns / cruz_fear_words

print(paste0(round(cruz_avg, digits=6), " is the average number of fear words used per talking turn by Cruz"))

nrc_analysis_Fiorina <- data %>% 
  filter(who == "FIORINA") %>%
  unnest_tokens(word, text, token = "words") %>%
  inner_join(nrc_fear) %>%
  count(word, sort = TRUE)

fiorina_fear_words <- sum(nrc_analysis_Fiorina$n)

fiorina_turns <- data %>%
  filter(who == "FIORINA") %>%
  count(who, sort = TRUE) %>%
  summarise(Fiorina_speaking_turns = sum(n))

fiorina_turns <- fiorina_turns$Fiorina_speaking_turns

fiorina_avg = fiorina_turns / fiorina_fear_words

print(paste0(round(fiorina_avg, digits=6), " is the average number of fear words used per talking turn by Fiorina"))

results <- data.frame(candidate = c("Trump", "Bush", "Cruz", "Fiorina"), avg_fear_words = c(trump_avg, bush_avg, cruz_avg, fiorina_avg))

results %>%
  arrange(avg_fear_words) %>%
  mutate(candidate=factor(candidate, levels=candidate)) %>%
  ggplot(aes(x=candidate, y=avg_fear_words, fill=as.factor(candidate))) + 
    geom_bar(stat = "identity") +
    scale_fill_manual(values = c("purple", "orange", "yellow", "red") ) +
    theme(legend.position="none") +
    coord_flip() +
    xlab("Candidate") +
    ylab("Average Number of Fear Words per Speaking Turn")

