setwd("/Users/rcassan2/Documents/GitHub/cover_songs_for_pub_repo/data/scripts/expanded/")
library(tidyverse)
library(magrittr)

# load data
raw <- read.csv('../../raw/expanded/emotion/raw_combined.csv')
# reading as a tibble (using read_csv) causes some issues, so we use read.csv

# order effect is ignored
main_data <- raw %>% select(c(participant, clip, mouse_rating.x, mouse_rating.y, 
                              mouse_rating.time, task.started))
# use task.started to filter main task
# filter the rows where the clip played (may be missing data - there was a time limit for each trial)
main_data %<>% 
  filter(!is.na(task.started)) %>%
  mutate(task.started = NULL)
  
# did all subjects hear all stimuli?
trials_by_sub <- main_data %>%
  group_by(participant) %>%
  summarize(count = n())
# check that all subjects heard all 140 stimuli


# for song_id, keep only Song_Artist
main_data %<>%
  # remove "song_indices/" at the beginning
  mutate(clip = str_split_i(clip, '/', -1)) %>%
  # remove ".wav" at the end
  mutate(clip = str_split_i(clip, '.wav', 1)) %>%
  # rename variables to make them more readable
  rename(x = mouse_rating.x) %>%
  rename(y = mouse_rating.y) %>%
  rename(rt = mouse_rating.time) %>%
  rename(song_id = clip)
  

# save the data!
write_csv(main_data, '../../processed/expanded/emotion/main_task.csv')


# survey data
survey_data <- raw %>%
  select(c(participant, matches("survey")))

# filter the rows that have the data
survey_data %<>% filter(!is.na(post_survey.age))
# might cause an issue if subjects didn't respond to this particular question
# but it looks like all of our current subjects did

# remove "survey" prefix
colnames(survey_data) <- gsub("post_survey.", "", colnames(survey_data))

# save the data!
write_csv(survey_data, '../../processed/expanded/emotion/survey.csv')


# timing info
# grab any columns with "started", "stopped", or "rt"
# rt might be redundant in our data (stopped - started for each routine, usually)
timing <- raw %>%
  select(c(participant, matches("started|stopped|rt")))

# save the data!
write_csv(timing, '../../processed/expanded/emotion/timing.csv')


# quick visualizations of timing - more in "4_emotion_data_exploration.R"
# slider.rt - time they took on each individual trial
main_data %>% 
  select(participant, rt) %>%
  filter(!is.na(rt)) %>%
  ggplot(aes(x = factor(participant), y = rt)) +
  geom_jitter(width = 0.05, alpha = 0.5) +
  theme(axis.text.x = element_blank()) +
  xlab("participant") +
  ylab("response time (seconds)")
ggsave('../../processed/expanded/figures/emotion_timing/response_time_per_trial.png', width=7, height=5)
# what was the mean and standard deviation of response times?
ggplot(main_data, aes(rt)) +
  geom_histogram()
m_rt <- mean(main_data$rt, na.rm = TRUE) # mean is 1.838 seconds
sd_rt <- sd(main_data$rt, na.rm = TRUE) # standard deviation is 2.166 seconds

# space_to_move_on.rt - how long of a break they took
timing %>%
  filter(!is.na(space_to_move_on.rt)) %>%
  ggplot(aes(x = (space_to_move_on.rt / 60))) +
  geom_histogram() +
  xlab('break length (minutes)')
ggsave('../../processed/expanded/figures/emotion_timing/break_length.png', width=5, height=5)
