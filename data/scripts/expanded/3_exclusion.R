# This script implements exclusion criteria for the similarity task
# Subjects with more than 10% of trials over 10 seconds are excluded entirely. 
# Any trials over 10 seconds are excluded.
# (The emotion task had a time limit so this doesn't apply, but we do check amount of missing data.)
setwd("/Users/rcassan2/Documents/GitHub/cover_songs_for_pub_repo/data/scripts/expanded/")
library(tidyverse) 
library(magrittr) 

# load the ratings
sim_ratings <- read_csv('../../processed/expanded/similarity/main_task.csv')

# find subjects with more than 10% of trials over 10 seconds
subs_to_exclude <- sim_ratings %>%
  # keep only >10s trials
  filter(rt > 10) %>%
  # count how many >10s trials each participant had
  group_by(participant) %>%
  summarize(count = n()) %>%
  # subjects with more than 7 will be excluded
  filter(count > 7) %>%
  pull(participant)

# filter out subjects to exclude and any trials over 10 seconds
sim_ratings_filtered <- sim_ratings %>%
  # remove subjects to be excluded
  filter(!participant %in% subs_to_exclude) %>% # 4 subjects
  # remove remaining >10s trials
  filter(rt < 10) # 53 other trials

# compute mean and standard deviation of similarity ratings per original-cover pair
mean_sim_ratings <- sim_ratings_filtered %>%
  group_by(song_id) %>%
  summarize(mean_sim = mean(rating),
            sd_sim = sd(rating))

# scale
mean_sim_ratings_scaled <- as.data.frame(scale(mean_sim_ratings$mean_sim))
mean_sim_ratings$scaled <- mean_sim_ratings_scaled$V1

#write_csv(mean_sim_ratings, '../../processed/expanded/similarity/mean_ratings.csv')



# load the emotion ratings
emot_ratings <- read_csv('../../processed/expanded/emotion/main_task.csv')
# how much missing data does each participant have?
emot_ratings %>% 
  filter(is.na(rt)) %>%
  group_by(participant) %>%
  summarize(count = n())
# 6 subjects missed one trial, 3 subjects missed 2, 1 subject missed 11 (which is 7.9%)
