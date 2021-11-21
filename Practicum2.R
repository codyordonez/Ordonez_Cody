# Practicum 2 ------------------------------------------------------------

# Pre-Flight --------------------------------------------------------------

# Summative Content Analysis Method --------------------------------------------------------------

# Load libraries
library(tidyverse)
library(tidytext)
library(stringr)
library(textstem)
library(irr)

# Load data and create labels
data <- read_csv("hedge_data.csv") %>%
  select(hedge, text) %>% 
  mutate(hedged = ifelse(hedge=="1","yes","no")) %>% 
  unique()

# The following list of hedged terms was initially taken from http://www.uefap.com/writing/feature/hedge.htm but heavily edited after running numerous IRR agreement tests to diagnose issues in agreement (see code below) 
hedged_terms <- "suggest|could|may|might|indicate|seem|potentially|comparably|slightly|mildly|moderately|unlikely|negatively|limited conditions|close to|not better than|generally superior|although further|with the comparison to|more discriminative than|in turn|moderate-|statistically significantly|non-significant|no significant changes|no significant difference between|shown potential|showing potential|has potential|the potential to|the potential of|\\) but|, but|but lower"

# Add a column called robot_hedged and give each entry a "yes" or "no" regarding if it uses any hedged language
data <- data %>%
  mutate(text = tolower(text),
         robot_hedged = ifelse(str_detect(text,hedged_terms),"yes","no"))


# The following IRR analysis was used many times to see accuracy comparing robot_hedge to hedge, which was then used to edit the hedged_terms list to improve accuracy (this process was repeated until agreement > 96.5% and Kappa > 0.8)

# Auto-code for hedged terms 
data_compare <- data %>%
  mutate(text = tolower(text),
         robot_hedge = ifelse(str_detect(text,hedged_terms),1,0))

# Check agreement 
agree(data.frame(data_compare$hedge,data_compare$robot_hedge))

# Check reliability 
kappa2(data.frame(data_compare$hedge,data_compare$robot_hedge))

# Diagnose issues 
issues <- data_compare %>% 
  mutate(agree = hedge + robot_hedge) %>% 
  filter(agree == 1)

# Final data frame has only three variables: text, hedged, and robot_hedged
data <- data %>%
  select(text, hedged, robot_hedged)

# Print results
print(data)

