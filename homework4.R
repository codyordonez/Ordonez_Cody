# Homework #4 -------------------------------------------------------------

# This script 1) Imports the appropriate data from the shared data sets. 2) Analyzes the speaking complexity of each speaking turn using an appropriate complexity metric 3) Aggregates the data by candidate and returns a two column data frame: speaker | ave_complexity in descending order of complexity and 4) compares the ave_complexity of Trump with Bush, Cruz, and Walker and calculates the mean difference and displays a plot. 

# Pre-flight --------------------------------------------------------------

# Load libraries 
library(tidyverse)
library(dabestr)
library(quanteda)
library(quanteda.textstats)

# Set the working directory
setwd("/home/cao2246/R/Ordonez_Cody_2")

# Load data
data <- read_csv("datasets/gop_debates.csv")

# Analyze Speaking Complexity ---------------------------------------------
data <- data %>% 
  bind_cols(textstat_readability(.$text,measure = "ELF")) # bind ELF score to each row

# Aggregate & Arrange Data ------------------------------------------------
data %>% 
  select(speaker=who,ELF) %>% # select columns of interest and rename who as speaker 
  group_by(speaker) %>%
  summarise(ave_complexity = mean(ELF)) %>% # get average speaking complexity  
  arrange(desc(ave_complexity)) %>% # arrange descending by ave_complexity
  filter(speaker == "TRUMP" | speaker == "BUSH" | speaker == "CRUZ" | speaker == "WALKER") # filters to include only the four relevant candidates

# Calculate mean difference
Trump_Bush_Cruz_Walker_mean_diff <- data %>%
  dabest(who, ELF, 
         idx = c("TRUMP", "BUSH", "CRUZ", "WALKER"), 
         paired = FALSE) %>% mean_diff()

# Display mean difference and the 95% confidence interval for each of the three comparisons
Trump_Bush_Cruz_Walker_mean_diff

# Visualize data
Trump_Bush_Cruz_Walker_mean_diff %>% plot()

