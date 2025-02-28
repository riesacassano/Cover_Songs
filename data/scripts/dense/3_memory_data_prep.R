# This script prepares the memory data for analysis.

# The steps include: 
# 1. tagging which memory descriptions are "Not a Memory"
# 2. filtering instances where a participant had a memory for the original
# 3. filtering instances where a participant had a memory for the original and
# the cover and compute memory quality differences
# 4. generating sentence embeddings using BERT and compute semantic distances
# between original-cover memory pairs
# There are a few manual steps and references to other scripts.


setwd("/Users/rcassan2/Documents/GitHub/cover_songs_for_pub_repo/data/scripts/dense/")
library(tidyverse)
library(magrittr) 

# Step 1: tag memories that are not a memory ("NaM")
# these are memory descriptions that have no episodic information
# read in memory task data
main_task <- read_csv('../../processed/dense/memory/main_task.csv')
# add a column for memory/non-memory tags
# "0" if no memory reported, "mem" for memory reported
main_task_NaM <- main_task %>%
  mutate(NaM = ifelse(is.na(description), "0", "mem")) %>%
  relocate(NaM, .before = "description")
#write_csv(main_task_NaM, '../../processed/dense/memory/main_task_NaM.csv')

# manually: go through and mark "NaM" if memory was reported but there was
# no episodic info in description (or participant clicked accidentally)
main_task_NaM <- read_csv('../../processed/dense/memory/main_task_NaM.csv') 
# check that we have NaMs now...
print(filter(main_task_NaM, NaM == 'NaM'))



# Step 2: filter instances  where a participant had a memory for the original
# create a boolean column where memory = TRUE, no memory = FALSE
main_task_NaM %<>% 
  mutate(memory = ifelse(NaM == 'mem', TRUE, FALSE)) %>%
  relocate(memory, .after = NaM)

# pivot the data wider so each song has a row for each participant
main_wider <- main_task_NaM %>%
  # remove session info - not looking at order effects right now
  select(-c(session)) %>% 
  pivot_wider(id_cols = c(internal_id, song_id), 
              names_from = 'orig_or_cover', values_from = seq(4,19))
# save this dataframe
#write_csv(main_wider, '../../processed/dense/memory/main_task_wider.csv')

# filter instances where the participant had a memory for the original
orig_mems <- main_wider %>%
  mutate(NaM_orig = NULL,
         NaM_cover = NULL) %>%
  filter(memory_orig)
# memory_orig is now not informative, but we'll leave it just for confidence
# save this dataframe
#write_csv(orig_mems, 
#          '../../processed/dense/memory/original_memories-w_or_wo_cover_mem.csv')
# we'll use this for likelihood analysis



# Step 3: filter instances where a participant had a memory for the original and
# the cover and compute memory quality differences

# filter rows where a memory was evoked
memories_only <- main_task_NaM %>%
  filter(memory) %>%
  mutate(NaM = NULL,
         memory = NULL)
# this is helpful if we want to look at all memories, regardless of whether they
# were evoked by an original or cover or whether the corresponding original or
# cover evoked a memory
#write_csv(memories_only, '../../processed/dense/memory/all_memories.csv')

# now filter instances where a memory was evoked for both the original and the cover
memory_pairs <- main_wider %>%
  filter(memory_orig) %>%
  filter(memory_cover) %>%
  select(-c(NaM_orig, NaM_cover, memory_orig, memory_cover))

# make all of the ratings into factors
common_rating <- c('not at all', 'a little', 'somewhat', 'very', 'extremely')
# this rating scheme is used for multiple ratings
memory_pairs_factors <- memory_pairs %>%
  mutate(control_orig = factor(control_orig,
                               levels = c('completely deliberate',
                                          'somewhat deliberate',
                                          'neither',
                                          'somewhat spontaneous',
                                          'completely spontaneous'),
                               ordered = TRUE),
         control_cover = factor(control_cover, 
                                levels = c('completely deliberate',
                                           'somewhat deliberate',
                                           'neither',
                                           'somewhat spontaneous',
                                           'completely spontaneous'),
                                ordered = TRUE),
         emot_content_orig = factor(emot_content_orig, 
                                    levels = c('very negative',
                                               'somewhat negative',
                                               'neutral',
                                               'somewhat positive',
                                               'very positive'),
                                    ordered = TRUE),
         emot_content_cover = factor(emot_content_cover, 
                                     levels = c('very negative', 
                                                'somewhat negative',
                                                'neutral',
                                                'somewhat positive',
                                                'very positive'),
                                    ordered = TRUE),
         emot_intensity_orig = factor(emot_intensity_orig, 
                                      levels = common_rating,
                                      ordered = TRUE),
         emot_intensity_cover = factor(emot_intensity_cover, 
                                       levels = common_rating,
                                       ordered = TRUE),
         energetic_orig = factor(energetic_orig, 
                                 levels = common_rating,
                                 ordered = TRUE),
         energetic_cover = factor(energetic_cover, 
                                  levels = common_rating,
                                  ordered = TRUE),
         familiarity_orig = factor(familiarity_orig, 
                                   levels = common_rating,
                                   ordered = TRUE),
         familiarity_cover = factor(familiarity_cover, 
                                    levels = common_rating,
                                    ordered = TRUE),
         important_orig = factor(important_orig, 
                                 levels = common_rating,
                                 ordered = TRUE),
         important_cover = factor(important_cover, 
                                  levels = common_rating,
                                  ordered = TRUE),
         preference_orig = factor(preference_orig, 
                                  levels = c('not at all',
                                             'a little',
                                             'somewhat',
                                             'a lot',
                                             'love it!'),
                                  ordered = TRUE),
         preference_cover = factor(preference_cover, 
                                   levels = c('not at all',
                                              'a little',
                                              'somewhat',
                                              'a lot',
                                              'love it!'),
                                   ordered = TRUE),
         social_orig = factor(social_orig, 
                              levels = common_rating,
                              ordered = TRUE),
         social_cover = factor(social_cover, 
                               levels = common_rating,
                               ordered = TRUE),
         unique_orig = factor(unique_orig, 
                              levels = common_rating,
                              ordered = TRUE),
         unique_cover = factor(unique_cover, 
                               levels = common_rating,
                               ordered = TRUE),
         vivid_orig = factor(vivid_orig, 
                             levels = common_rating,
                             ordered = TRUE),
         vivid_cover = factor(vivid_cover, 
                              levels = common_rating,
                              ordered = TRUE)
         )
# could have done this earlier to save ourselves some repetition, but this is where it's applicable
# and I didn't want to deal with "NA" as a level

# check column types
str(memory_pairs_factors)

# compare the qualities
# in general, + means original memory is rated higher than cover memory
memory_pairs_differences <- memory_pairs_factors %>%
  mutate(age_at_event_diff = age_at_event_orig - age_at_event_cover, .after = age_at_event_cover) %>%
  mutate(control_diff = as.numeric(control_orig) - as.numeric(control_cover), .after = control_cover) %>%
  # + means original memory is more spontaneous than cover memory
  mutate(emot_content_diff = as.numeric(emot_content_orig) - as.numeric(emot_content_cover), .after = emot_content_cover) %>%
  # + means original is more positive than cover
  mutate(emot_intensity_diff = as.numeric(emot_intensity_orig) - as.numeric(emot_intensity_cover), .after = emot_intensity_cover) %>%
  mutate(energetic_diff = as.numeric(energetic_orig) - as.numeric(energetic_cover), .after = energetic_cover) %>%
  mutate(familiarity_diff = as.numeric(familiarity_orig) - as.numeric(familiarity_cover), .after = familiarity_cover) %>%
  mutate(important_diff = as.numeric(important_orig) - as.numeric(important_cover), .after = important_cover) %>%
  mutate(preference_diff = as.numeric(preference_orig) - as.numeric(preference_cover), .after = preference_cover) %>%
  mutate(social_diff = as.numeric(social_orig) - as.numeric(social_cover), .after = social_cover) %>%
  mutate(unique_diff = as.numeric(unique_orig) - as.numeric(unique_cover), .after = unique_cover) %>%
  mutate(vivid_diff = as.numeric(vivid_orig) - as.numeric(vivid_cover), .after = vivid_cover)
  
#write_csv(memory_pairs_differences, '../../processed/dense/memory/memory_pairs.csv')
# we'll use this for memory qualities analysis



# Step 4: generate sentence embeddings using BERT and compute semantic distances
# between original-cover memory pairs

# sentence embeddings are generated and semantic distances are computed in
# `3a_sentence_embeddings.ipynb`
# all embeddings are in `all_memory_embeddings.csv`
# semantic distances are in `memory_pairs_semantic_distances.csv`

# recombine with the ratings
semantic_distances <- read_csv('../../processed/dense/memory/memory_pairs_semantic_distances.csv')
memory_pairs_differences_wsemantic <- memory_pairs_differences %>%
  left_join(., semantic_distances, by = join_by(internal_id, song_id)) %>%
  relocate(semantic_distance, .after = description_cover)
#write_csv(memory_pairs_differences_wsemantic, '../../processed/dense/memory/memory_pairs.csv')

# manual codings of same/similar/different are added 
# and saved in `memory_pairs_coded.csv`
# we'll use this for memory content analysis
