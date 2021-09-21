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


# As we can see from the mean differences, 95% confidence intervals, and plot, we can say the following with high confidence (95%):

# When comparing the mean difference in average complexity of Trump and Bush, there is a moderate difference with Bush scoring between 0.687 and 1.31 higher complexity than Trump according to the NDC metric.
# We can say with 95% certainty that Bush speaks with complexity between 32.4% and 61.8% higher than Trump.

# When comparing the mean difference in average complexity of Trump and Cruz, there is a large difference with Cruz scoring between 1.64 and 2.33 higher complexity than Trump according to the NDC metric.
# We can say with 95% certainty that Cruz speaks with complexity between 77.4% and 109.9% higher than Trump.

# When comparing the mean difference in average complexity of Trump and Walker, there is a moderate to very large difference with Walker scoring between 1.13 and 2.67 higher complexity than Trump according to the NDC metric.
# We can say with 95% certainty that Walker speaks with complexity between 53.3% and 125.9% higher than Trump.

# As seen in the plot, the total number of speaking turns is very different between politicians, with Trump having 738, Bush having 279, Cruz having 348, and Walker having 39.
# This discrepancy explains the large difference in the range of the confidence interval for Trump and Walker, because there were far fewer data points to average for Walker compared to the other candidates.

# Overall, the answer to the research question is yes, there is a meaningful difference in speaking complexity.
# We can say with at least 95% confidence that all three other politicians speak with higher complexity than Trump according to the NDC metric.
# This meaningful difference is also displayed and can be seen on the plot.
