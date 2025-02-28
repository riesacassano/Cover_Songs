setwd("/Users/rcassan2/Documents/GitHub/cover_songs_for_pub_repo/data/scripts/dense/")
library(tidyverse)
library(magrittr)

# load the similarity data
data <- read_csv('../../raw/dense/similarity.csv')

# how many subjects are there, and how many completed the experiment?
incomplete_subs <- data %>% 
  group_by(exp_subject_id) %>% 
  summarize(count = n()) %>%
# complete subjects have 54 rows
  filter(count < 54) %>%
  pull(exp_subject_id)

# use the incomplete subjects list to filter the data
complete_data <- data %>%
  filter(!exp_subject_id %in% incomplete_subs)
# this dataframe has 2538 rows (47 subjects x 54 rows per subject)
# save this dataframe
write_csv(complete_data, '../../processed/dense/similarity/complete_subjects.csv')


# filter out the main task data
main_task <- complete_data %>%
  # select relevant columns
  select(c(exp_subject_id, Task_Name, Trial_Id, similarity)) %>%
  # keep rows pertaining to main task only
  filter(grepl('main', Task_Name))
# each clip is assigned a block ("Main Task 1") and Trial ID
# for example "Main Task 2", Trial ID 3 is 2010_1 (Tik Tok)

# load the trial-clip assignments
clip_info <- read_csv('../../raw/dense/similarity_clip_assignments.csv')

# join
main_task_with_songs <- main_task %>%
  # join in trial-clip assignments
  left_join(., clip_info, 
            by = join_by("Task_Name" == "block", "Trial_Id" == "trial_id"), 
            relationship = "many-to-one") %>%
  # those columns are now not useful
  # (if we wanted to look at order effects we should have selected Trial_Nr above
  relocate(song_id, .after = exp_subject_id) %>%
  mutate(Task_Name = NULL, Trial_Id = NULL)
# save this dataframe
write_csv(main_task_with_songs, '../../processed/dense/similarity/main_task.csv')

# average over all subjects to get mean ratings per song
mean_ratings <- main_task_with_songs %>%
  group_by(song_id) %>%
  summarize(mean_sim = mean(similarity),
            sd_sim = sd(similarity))
write_csv(mean_ratings, '../../processed/dense/similarity/mean_ratings.csv')


# filter survey data
survey <- complete_data %>%
  filter(Task_Name == 'post survey')
# not going to worry about selecting out unnecessary columns
write_csv(survey, '../../processed/dense/similarity/survey.csv')
