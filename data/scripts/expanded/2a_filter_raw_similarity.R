setwd("/Users/rcassan2/Documents/GitHub/cover_songs_for_pub_repo/data/scripts/expanded/")
library(tidyverse)
library(magrittr)

# load data
raw <- read.csv('../../raw/expanded/similarity/raw_combined.csv')
# reading as a tibble (using read_csv) causes some issues, so we use read.csv

# similarity
# main task data
main_data <- raw %>% select(c(participant, song_id, slider.response, slider.rt))
# for the sake of the initial analysis, I'll ignore the trial order (`blocks.thisN` and `songs.thisN`) 
# and whether or not they heard original or cover first ('orig_or_cover')

# filter the rows that have slider responses
main_data %<>% filter(!is.na(slider.response))
  
# did all subjects hear all stimuli?
trials_by_sub <- main_data %>%
  group_by(participant) %>%
  summarize(count = n())
# check that all subjects heard all 70 pairs of stimuli


# for song_id, keep only Song_Artist
main_data %<>%
  # remove "song_indices/" at the beginning
  mutate(song_id = str_split_i(song_id, '/', -1)) %>%
  # remove ".csv" at the end
  mutate(song_id = str_split_i(song_id, '.csv', 1)) %>%
  # rename variables to make them more readable
  rename(rating = slider.response) %>%
  rename(rt = slider.rt)
  

# save the data!
write_csv(main_data, '../../processed/expanded/similarity/main_task.csv')



# survey data
survey_data <- raw %>%
  select(c(participant, matches("survey")))

# filter the rows that have the data
survey_data %<>% filter(!is.na(survey.age))
# might cause an issue if subjects didn't respond to this particular question
# but it looks like all of our current subjects did

# remove "survey" prefix
colnames(survey_data) <- gsub("survey.", "", colnames(survey_data))

# save the data!
write_csv(survey_data, '../../processed/expanded/similarity/survey.csv')



# timing info
# grab any columns with "started", "stopped", or "rt"
# rt might be redundant in our data (stopped - started for each routine, usually)
timing <- raw %>%
  select(c(participant, matches("started|stopped|rt")))

# save the data!
write_csv(timing, '../../processed/expanded/similarity/timing.csv')



# quick visualizations of timing
# rt - time they took on each individual trial
timing %>% 
  select(participant, slider.rt) %>%
  filter(!is.na(slider.rt)) %>%
  ggplot(aes(x = factor(participant), y = slider.rt)) +
  geom_jitter(width = 0.05, alpha = 0.5) +
  theme(axis.text.x = element_blank()) +
  xlab("participant") +
  ylab("response time (seconds)")
ggsave('../../processed/expanded/figures/similarity_timing/response_time_per_trial.png', width=7, height=5)
# what was the mean and standard deviation of response times?
ggplot(timing, aes(slider.rt)) +
  geom_histogram()
m_rt <- mean(timing$slider.rt, na.rm = TRUE) # mean is 3.155 seconds
sd_rt <- sd(timing$slider.rt, na.rm = TRUE) # standard deviation is 7.706 seconds
nrow(filter(timing, slider.rt > (m_rt + 3*sd_rt))) # there are 38 instances when the response time is 3SD above the mean
unique(filter(timing, slider.rt > (m_rt + 3*sd_rt))$participant) # 14 participants had at least one of these

# space_to_move_on.rt - how long of a break they took
timing %>%
  filter(!is.na(space_to_move_on.rt)) %>%
  ggplot(aes(x = (space_to_move_on.rt / 60))) +
  geom_histogram() +
  xlab('break length (minutes)')
ggsave('../../processed/expanded/figures/similarity_timing/break_length.png', width=5, height=5)
