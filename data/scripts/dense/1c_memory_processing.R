setwd("/Users/rcassan2/Documents/GitHub/cover_songs_for_pub_repo/data/scripts/dense/")
library(tidyverse)
library(magrittr)

# load the both sessions of the memory data
data1 <- read_csv('../../raw/dense/memory_session1.csv')
data2 <- read_csv('../../raw/dense/memory_session2.csv')

# for the first session, how many subjects are there, and how many completed the experiment?
incomplete_subs1 <- data1 %>% 
  group_by(exp_subject_id) %>% 
  summarize(count = n()) %>%
  # complete subjects have 54 rows
  filter(count < 54) %>%
  pull(exp_subject_id)

# use the incomplete subjects list to filter the session 1data
complete_data1 <- data1 %>%
  filter(!exp_subject_id %in% incomplete_subs1)
# this dataframe has 5724 rows (106 subjects x 54 rows per subject)
# save this dataframe
write_csv(complete_data1, '../../processed/dense/memory/complete_subjects_session1.csv')

# repeat the process for session 2
incomplete_subs2 <- data2 %>% 
  group_by(exp_subject_id) %>% 
  summarize(count = n()) %>%
  # complete subjects have 54 rows
  filter(count < 54) %>%
  pull(exp_subject_id)

# use the incomplete subjects list to filter the session 1data
complete_data2 <- data2 %>%
  filter(!exp_subject_id %in% incomplete_subs2)
# this dataframe has 4968 rows (92 subjects x 54 rows per subject)
# save this dataframe
write_csv(complete_data2, '../../processed/dense/memory/complete_subjects_session2.csv')


# Prolific IDs are the same across both sessions, but `exp_subject_id` is not
# we'll assign internal subject IDs 

# get the session 2 experiment IDs
id_register <- complete_data2 %>%
  select(c(prolific_ID, exp_subject_id)) %>%
  filter(!is.na(prolific_ID)) %>%
  rename(exp_id2 = exp_subject_id)

sess1_IDs <- complete_data1 %>%
  select(c(prolific_ID, exp_subject_id)) %>%
  filter(!is.na(prolific_ID)) %>%
  rename(exp_id1 = exp_subject_id)

id_register %<>% 
  full_join(., sess1_IDs, by = join_by(prolific_ID))

# surprisingly, there are 3 prolific IDs that don't show up in session 1
# there is one pair of prolific IDs that is only one letter off from each other
# do a little manual repair to make those the same participant
id_register[79,]$exp_id1 <- 598175
# there are no other obvious matches, so we'll have to remove them
id_register %<>% 
  filter(!is.na(exp_id1)) %>%
# as well as the participants who didn't complete session 2
  filter(!is.na(exp_id2))
# this leaves us with 90 subjects that completed both sessions
id_register$'internal_id' <- seq(1:90)
# save this dataframe
write_csv(id_register, '../../processed/dense/memory/id_register.csv')

# add the internal assignments
data_assg1 <- id_register %>%
  select(c(exp_id1, internal_id)) %>%
  left_join(complete_data1, ., by = join_by('exp_subject_id' == 'exp_id1'), relationship = "many-to-one") %>%
  # remove subjects who don't have an internal ID (that didn't complete the second session)
  filter(!is.na(internal_id))
data_assg2 <- id_register %>%
  select(c(exp_id2, internal_id)) %>%
  left_join(complete_data2, ., by = join_by('exp_subject_id' == 'exp_id2'), relationship = "many-to-one") %>%
  # remove subjects who don't have an internal ID (missing for some reason)
  filter(!is.na(internal_id))

# grab the survey data from session 1
survey_data <- data_assg1 %>%
  filter(Task_Name == 'post survey')
write_csv(survey_data, '../../processed/dense/memory/survey.csv')


# combine the data from session 1 and session 2
# add session assignments 
# in the minimally processed data, this is redundant - would be able to figure out which session is which
# because of date info and exp_subject_id from session 2 is greater than session 1 (6XXXXX vs 5XXXXX)
data_assg1$session <- 1
data_assg2$session <- 2
# use bind_rows instead of rbind since session 1 data includes more columns (for the survey data)
complete_data_both_sessions <- bind_rows(data_assg1, data_assg2)
write_csv(complete_data_both_sessions, '../../processed/dense/memory/complete_data_both_sessions.csv')


# pull out main task data
main_task_all_cols <- complete_data_both_sessions %>%
  filter(grepl('main', Task_Name))
# filter columns only used for survey
main_task_no_survey <- main_task_all_cols %>%
  select(where(function(x) any(!is.na(x))))
# filter unnecessary "info" cols
main_task <- main_task_no_survey %>%
  relocate(c(internal_id, session)) %>%
  select(-c("Block_Nr", "Block_Name", "Task_Nr", "Trial_Nr", "Condition_Id", 
            "completed", "end_time", "exp_subject_id", "group_name", 
            "rec_session_id", "session_name", "session_nr", "start_time", 
            "time_delay_offset", "unlocked"))

# load in song info
clip_info1 <- read_csv('../../raw/dense/memory1_clip_assignments.csv')
clip_info2 <- read_csv('../../raw/dense/memory2_clip_assignments.csv')
# add session column and combine
clip_info1$session <- 1
clip_info2$session <- 2
clip_info <- bind_rows(clip_info1, clip_info2)

main_task_with_songs <- 
  # join in trial-clip assignments
  left_join(main_task, clip_info, 
            by = join_by("Task_Name" == "block", "Trial_Id" == "trial_id", 
                         "session" == "session"), 
            relationship = "many-to-one") %>%
  relocate(c(song_id, orig_or_cover), .after = session) %>%
  mutate(Task_Name = NULL, Trial_Id = NULL) # keep session to look at potential priming effects

# check age at event
print(unique(main_task_with_songs$age_at_event))
# fix manually after we save
write_csv(main_task_with_songs, '../../processed/dense/memory/main_task.csv')
