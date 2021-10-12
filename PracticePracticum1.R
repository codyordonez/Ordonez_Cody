# Practice Practium 1 -------------------------------------------------------------

# This script 1) Imports the appropriate data from the AG News dataset. 2) Prepares the data for analysis. 3) Conducts an analysis comparing the lexical diversity of the entries by article class and calculating the mean difference. 4) Visualizes the results with a plot.  

# Pre-flight --------------------------------------------------------------

# Load libraries
library(textdata)
library(tidyverse)
library(tidytext)
library(dabestr)
library(quanteda)
library(quanteda.textstats)

# Load data
data <- dataset_ag_news()

# We need to get the first 100 entries for each class. What are the classes?
data %>%
  distinct(class)

# We can see that here are 4 distinct classes: "Business", "Sci/Tech", "Sports", and "World".

# Filter for the first 100 "Business" entries
business_data_100 <- data %>%
  filter(class == "Business") %>%
  slice(1:100)

# Filter for the first 100 "Sci/Tech" entries
sci_tech_data_100 <- data %>%
  filter(class == "Sci/Tech") %>%
  slice(1:100)

# Filter for the first 100 "Sports" entries
sports_data_100 <- data %>%
  filter(class == "Sports") %>%
  slice(1:100)

# Filter for the first 100 "World" entries
world_data_100 <- data %>%
  filter(class == "World") %>%
  slice(1:100)

# Aggregate the data to combine the entries of all 4 classes (400 total entries)
aggregated_data <- rbind(business_data_100, sci_tech_data_100, sports_data_100, world_data_100)

aggregated_data <- aggregated_data %>%
  mutate(doc_id = rownames(aggregated_data))

corpus <- aggregated_data %>% corpus(text_field = "description", unique_docnames = FALSE)
corpus_tokens <- corpus %>% tokens()
corpus_dfm <- corpus_tokens %>% dfm(remove = stopwords('en'))
lexdiv <- corpus_dfm %>% textstat_lexdiv(measure=c("TTR"))
full_data <- aggregated_data %>% left_join(lexdiv, by=c("doc_id" = "document")) %>% unique()


# Aggregate & Arrange Data to determine which class's entries are the most lexical diverse. 

full_data %>% 
  group_by(class) %>% 
  summarise(ave_lexdiv = mean(TTR, na.rm = TRUE)) %>%
  arrange(desc(ave_lexdiv)) # arrange descending by ave_lexdiv

# Calculate mean differences for each class compared to all other classes

# Compare Business to Sci/Tech, Sports, and World
business_mean_diff <- full_data %>%
  dabest(class, TTR, 
         idx = c("Business", "Sci/Tech", "Sports", "World"), 
         paired = FALSE) %>% mean_diff()

# Compare Sci/Tech to Business, Sports and World
sci_tech_mean_diff <- full_data %>%
  dabest(class, TTR, 
         idx = c("Sci/Tech", "Business", "Sports", "World"), 
         paired = FALSE) %>% mean_diff()

# Compare Sports to Sci/Tech, Business, and World
sports_mean_diff <- full_data %>%
  dabest(class, TTR, 
         idx = c("Sports", "Sci/Tech", "Business", "World"), 
         paired = FALSE) %>% mean_diff()

# Compare World to Sports, Sci/Tech, and Business
world_mean_diff <- full_data %>%
  dabest(class, TTR, 
         idx = c("World", "Sports", "Sci/Tech", "Business"), 
         paired = FALSE) %>% mean_diff()

# Display results of all mean differences and the 95% confidence interval for each comparison
business_mean_diff
sci_tech_mean_diff
sports_mean_diff
world_mean_diff

# Visualize data
business_mean_diff %>% plot
sci_tech_mean_diff %>% plot
sports_mean_diff %>% plot
world_mean_diff %>% plot

# There are neither measurable nor meaningful differences in average lexical diversity when comparing any class (Sci/Tech, Sports, or World) with Business.
# There are neither measurable nor meaningful differences in average lexical diversity when comparing Sports with World.
# There is a very small meaningful difference in average lexical diversity when comparing Sports with Sci/Tech and when comparing World with Sci/Tech.

