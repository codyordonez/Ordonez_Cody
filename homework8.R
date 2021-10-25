# Homework 8------------------------------------------------------------

# Pre-Flight --------------------------------------------------------------

# Load libraries 
library(tidyverse)
library(tidytext)
library(stringr)
library(textstem)
library(irr)

# Load data
data <- read_csv("utreddit.csv")

# Edit data such that if there is no text, add the word "image0" to replace "NA"
data$post_text[is.na(data$post_text)] <- "image0" # If there is not text in a post, then it must be an image

# Get terms as REGEX query by category (Note that this is already approximately in the form of the "better" regex so there is no need to do it again using a fancy function)
course_scheduling_terms <- "schedul|course|class|syllab|lecture|requir|prereq|instructor|professor|teacher"
major_selection_terms <- "major|minor|subject|certificat|transfer"
policy_questions_terms <- "policy|procedure|protocol|rule|regulat|guideline|deadline"
financial_aid_terms <- "financial aid|grant|scholarship|fafsa"
housing_terms <- "hous|dorm|apartment|roommate"
food_terms <- "food|restaurant|cook|cafeteria|meal|to eat"
entertainment_terms <- "entertain|movie|tv|television|game|sport|football|party|parti|nightlife|bar|club|frat"
humor_terms <- "funny|joke|meme|haha|image0" # Note that many images are memes that are meant to be funny, while other images are not meant to be funny. Therefore, the images must be looked at individually to tell if it should really go in the "humor" category

# Looking only at the text of the titles of the posts, give each post a score of "0" or "1" for each category, with a "1" indicating that the post belongs in that category
data <- data %>%
  mutate(title = tolower(title),
          sched = ifelse(str_detect(title,course_scheduling_terms),1,0)) %>%
  mutate(title = tolower(title),
          major = ifelse(str_detect(title,major_selection_terms),1,0)) %>%
  mutate(title = tolower(title),
          policy = ifelse(str_detect(title,policy_questions_terms),1,0)) %>%
  mutate(title = tolower(title),
          finaid = ifelse(str_detect(title,financial_aid_terms),1,0)) %>%
  mutate(title = tolower(title),
          housing = ifelse(str_detect(title,housing_terms),1,0)) %>%
  mutate(title = tolower(title),
          food = ifelse(str_detect(title,food_terms),1,0)) %>%
  mutate(title = tolower(title),
          entertain = ifelse(str_detect(title,entertainment_terms),1,0)) %>%
  mutate(title = tolower(title),
          humor = ifelse(str_detect(title,humor_terms),1,0))

# Same process as before, but now looking at the text of the post rather than the title. Adds another "0" or "1" for each category, with an addition of "1" indicating that the post belongs in that category
data <- data %>%
  mutate(post_text = tolower(post_text),
         sched = ifelse(str_detect(post_text,course_scheduling_terms),1+sched,sched)) %>%
  mutate(post_text = tolower(post_text),
         major = ifelse(str_detect(post_text,major_selection_terms),1+major,major)) %>%
  mutate(post_text = tolower(post_text),
         policy = ifelse(str_detect(post_text,policy_questions_terms),1+policy,policy)) %>%
  mutate(post_text = tolower(post_text),
         finaid = ifelse(str_detect(post_text,financial_aid_terms),1+finaid,finaid)) %>%
  mutate(post_text = tolower(post_text),
         housing = ifelse(str_detect(post_text,housing_terms),1+housing,housing)) %>%
  mutate(post_text = tolower(post_text),
         food = ifelse(str_detect(post_text,food_terms),1+food,food)) %>%
  mutate(post_text = tolower(post_text),
         entertain = ifelse(str_detect(post_text,entertainment_terms),1+entertain,entertain)) %>%
  mutate(post_text = tolower(post_text),
         humor = ifelse(str_detect(post_text,humor_terms),1+humor,humor))

# Determine if a post matches any category
data$row_sum = rowSums(data[,c(4:11)])

# If a post does not belong in any category, give a score of "1" to the category "other"; else if the post does belong in another category, give a score of "0" to the category "other"
data <- data %>%
  mutate(other = if_else(row_sum == 0, 1, 0))

# Remove the column row_sum as it is no longer needed
data$row_sum = NULL

# Note that for each category, posts with a "0" mean they are relatively unlikely to belong in that category, posts with a "1" mean they are fairly likely to belong in that category, and posts with a "2" mean they are highly likely to belong in that category

