# This script 1) Imports the appropriate data from the shared data sets. 2) Conducts a sentiment analysis to show the frequency of fear words used per talking turn when comparing Bush, Cruz, and Fiorina to Trump. 3) Visualizes the data for each of the candidates.

# Pre-flight --------------------------------------------------------------

# Load libraries 
library(tidyverse)
library(tidytext)
library(textstem)
library(fuzzyjoin)
library(dabestr)

# Set the working directory
setwd("/home/cao2246/R/Ordonez_Cody_2")

# Load data
data <- read_csv("datasets/gop_debates.csv")

# Get NRC sentiment dictionary and filter by fear
nrc_fear <- get_sentiments("nrc") %>% 
  filter(sentiment == "fear")

# Conduct NRC analysis for Trump
nrc_analysis_Trump <- data %>% 
  filter(who == "TRUMP") %>% # Filter for words said by Trump
  unnest_tokens(word, text, token = "words") %>%
  inner_join(nrc_fear) %>%
  count(word, sort = TRUE)

# Save total number of fear words said by Trump as a variable
trump_fear_words <- sum(nrc_analysis_Trump$n)

# Find total number of talking turns by Trump
trump_turns <- data %>%
  filter(who == "TRUMP") %>%
  count(who, sort = TRUE) %>%
  summarise(Trump_speaking_turns = sum(n))

# Save total number of talking turns by Trump as a variable
trump_turns <- trump_turns$Trump_speaking_turns

# Find and save average number of fear words said by Trump per talking turn as a variable
trump_avg = trump_turns / trump_fear_words

# Print result for Trump
print(paste0(round(trump_avg, digits=6), " is the average number of fear words used per talking turn by Trump"))

# Conduct NRC analysis for Bush
nrc_analysis_Bush <- data %>% 
  filter(who == "BUSH") %>%
  unnest_tokens(word, text, token = "words") %>%
  inner_join(nrc_fear) %>%
  count(word, sort = TRUE)

# Save total number of fear words said by Bush as a variable
bush_fear_words <- sum(nrc_analysis_Bush$n)

# Find total number of talking turns by Bush
bush_turns <- data %>%
  filter(who == "BUSH") %>%
  count(who, sort = TRUE) %>%
  summarise(Bush_speaking_turns = sum(n))

# Save total number of talking turns by Bush as a variable
bush_turns <- bush_turns$Bush_speaking_turns

# Find and save average number of fear words said by Bush per talking turn as a variable
bush_avg = bush_turns / bush_fear_words

# Print result for Bush
print(paste0(round(bush_avg, digits=6), " is the average number of fear words used per talking turn by Bush"))

# Conduct NRC analysis for Cruz
nrc_analysis_Cruz <- data %>% 
  filter(who == "CRUZ") %>%
  unnest_tokens(word, text, token = "words") %>%
  inner_join(nrc_fear) %>%
  count(word, sort = TRUE)

# Save total number of fear words said by Cruz as a variable
cruz_fear_words <- sum(nrc_analysis_Cruz$n)

# Find total number of talking turns by Cruz
cruz_turns <- data %>%
  filter(who == "CRUZ") %>%
  count(who, sort = TRUE) %>%
  summarise(Cruz_speaking_turns = sum(n))

# Save total number of talking turns by Cruz as a variable
cruz_turns <- cruz_turns$Cruz_speaking_turns

# Find and save average number of fear words said by Cruz per talking turn as a variable
cruz_avg = cruz_turns / cruz_fear_words

# Print result for Cruz
print(paste0(round(cruz_avg, digits=6), " is the average number of fear words used per talking turn by Cruz"))

# Conduct NRC analysis for Fiorina
nrc_analysis_Fiorina <- data %>% 
  filter(who == "FIORINA") %>%
  unnest_tokens(word, text, token = "words") %>%
  inner_join(nrc_fear) %>%
  count(word, sort = TRUE)

# Save total number of fear words said by Fiorina as a variable
fiorina_fear_words <- sum(nrc_analysis_Fiorina$n)

# Find total number of talking turns by Fiorina
fiorina_turns <- data %>%
  filter(who == "FIORINA") %>%
  count(who, sort = TRUE) %>%
  summarise(Fiorina_speaking_turns = sum(n))

# Save total number of talking turns by Cruz as a variable
fiorina_turns <- fiorina_turns$Fiorina_speaking_turns

# Find and save average number of fear words said by Fiorina per talking turn as a variable
fiorina_avg = fiorina_turns / fiorina_fear_words

# Print result for Fiorina
print(paste0(round(fiorina_avg, digits=6), " is the average number of fear words used per talking turn by Fiorina"))

# Get results
results <- data.frame(candidate = c("Trump", "Bush", "Cruz", "Fiorina"), avg_fear_words = c(trump_avg, bush_avg, cruz_avg, fiorina_avg))

# Plot results
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

#Get mean difference
mean_diff <- data.frame(comparison = c("Trump - Bush", "Trump - Cruz", "Trump - Fiorina"), trump_diff = c(trump_avg - bush_avg, trump_avg - cruz_avg, trump_avg - fiorina_avg))

#Display mean difference
mean_diff


# As we can see from the results, mean differences, and plot, we can say the following:

# When comparing the average fear words per speaking turn between Trump and Bush, there is a large difference with Trump saying 0.6695 more fear words per talking turn on average.
# Trump says 94% more fear words per speaking turn on average compared to Bush.

# When comparing the average fear words per speaking turn between Trump and Cruz, there is a very large difference with Trump saying 0.8751 more fear words per talking turn on average.
# Trump says 174% more fear words per speaking turn on average compared to Cruz.

# When comparing the average fear words per speaking turn between Trump and Fiorina, there is a very large difference with Trump saying 0.8901 more fear words per talking turn on average.
# Trump says 182% more fear words per speaking turn on average compared to Fiorina.

# Overall, the answer to the research question is yes, there is a meaningful difference in the frequency of fear words per talking turn.
# Trump says significantly more fear words per talking turn on average compared to the other three candidates.

