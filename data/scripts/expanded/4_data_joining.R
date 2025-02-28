# This script generates four files: two with all of the features (unscaled and scaled) 
# and two with all of the feature differences (unscaled and scaled) and similarity ratings
# all features includes acoustic, musical, and emotional features
setwd("/Users/rcassan2/Documents/GitHub/cover_songs_for_pub_repo/data/scripts/expanded/")
library(tidyverse) # for... basically everything
library(magrittr) # for bidirectional pipes (not technically tidy)
library(readxl) # for reading Excel files
library(labelled) # for removing attributes from columns (after scaling)

# Step 1: load the necessary files

# similarity ratings
sim_ratings <- read_csv('../../processed/expanded/similarity/mean_ratings.csv')
# acoustic features
acoustic_feat <- read_xlsx('../../../stimuli/expanded_corpus_1955-2022/features_acoustic_expanded.xlsx')
mfcc_dist <- read_csv('../../../stimuli/expanded_corpus_1955-2022/mfccs_distance.csv')
# musical features
musical_feat <- read_xlsx('../../../stimuli/expanded_corpus_1955-2022/features_musical_expanded.xlsx')
# emotional features
emot_ratings <- read_csv('../../processed/expanded/emotion/main_task.csv')


# Step 2: clean and prepare the data

# compute mean and standard deviation of emotion ratings per clip
# ignoring missing data in these calculations
mean_emot_ratings <- emot_ratings %>%
  group_by(song_id) %>%
  summarize(mean_valence = mean(x, na.rm = TRUE),
            sd_valence = sd(x, na.rm = TRUE),
            mean_arousal = mean(y, na.rm = TRUE),
            sd_arousal = sd(y, na.rm = TRUE))
#write_csv(mean_emot_ratings, '../../processed/expanded/emotion/mean_ratings.csv')

# remove .wav suffix from acoustic features
acoustic_feat %<>% 
  mutate(song_id = gsub('.wav', '', clip)) %>%
  mutate(clip = NULL) %>%
  # move song_id to the beginning
  relocate(song_id, .before = rms)

# remove title, artist, year, and half-decade column from musical features
musical_feat %<>% select(-c(`...1`, year, title, artist))
# separate musical features into actual features and feature differences
musical_feat_diffs <- musical_feat %>%
  # remove any columns specific to original or cover
  select(-contains("orig"), -contains("cover"))
musical_feat %<>%
  # remove any columns with pre-computed differences
  select(-contains("diff"))
# pivot the feature matrix longer to have separate rows for each clip
musical_feat %<>%
  # parallel pivot, as described here: https://stackoverflow.com/questions/59253987/parallel-pivot-longer-of-two-sets-of-columns
  pivot_longer(cols = -song_id, names_to = c('orig_or_cover', '.value'), names_sep = '_') %>%
  # combine song_id and orig_or_cover in way that matches acoustic and emotional features
  mutate(orig_or_cover = ifelse(orig_or_cover == 'orig', 'Original5S', 'Cover5S')) %>% # the lazy way to do this
  mutate(song_id = paste(song_id, orig_or_cover, sep = "_")) %>%
  mutate(orig_or_cover = NULL)

# scale the pre-computed differences 
# scale key and voice type
musical_feat_diffs_scaled <- musical_feat_diffs %>%
  select(-c(song_id, tempo_difference)) %>%
  # we care about absolute differences, so take absolute value here before scaling
  mutate(key_difference = abs(key_difference),
         treble_difference = abs(treble_difference),
         tbb_difference = abs(tbb_difference),
         voice_type_difference = abs(voice_type_difference))
musical_feat_diffs_scaled <- as.data.frame(scale(musical_feat_diffs_scaled))

# scale MFCCs distance
mfcc_dist_scaled <- as.data.frame(scale(mfcc_dist$mfccs_dist))


# Step 3: combine the actual features

all_features <- full_join(acoustic_feat, musical_feat, by = join_by(song_id))
all_features <- full_join(all_features, mean_emot_ratings, by = join_by(song_id))
# save
#write_csv(all_features, '../../processed/expanded/joined_with_features/all_features.csv')

# scale each feature and save
# do it in a hacky way
all_feat_numeric <- all_features %>%
  # exclude SD columns because it doesn't make sense to scale those
  select(-c(sd_valence, sd_arousal)) %>%
  # remove non-numeric columns (treating genre as categorical)
  select(-c(song_id, key, genre)) 
all_feat_numeric <- as.data.frame(scale(all_feat_numeric))

all_features_scaled <- all_features %>% 
  select(c(song_id, key, genre)) %>%
  cbind(., all_feat_numeric) %>%
  relocate(key, .after = tempo) %>%
  relocate(genre, .after = tbb)

# check for no attributes
str(all_features_scaled)
#write_csv(all_features_scaled, '../../processed/expanded/joined_with_features/all_features_scaled.csv')


# Step 4: combine computed feature differences with similarity ratings

all_features_and_sim_ratings <- all_features %>%
  # compute acoustic and emotional feature differences
  # get the original/cover distinction from current song ID
  separate_wider_delim(song_id, '_', names = c('title', 'artist', 'orig_or_cover')) %>%
  # reconstruct the song_id column
  mutate(song_id = paste(title, artist, sep = '_')) %>%
  relocate(song_id) %>%
  mutate(title = NULL,
         artist = NULL) %>%
  
  # remove the musical features (we'll add those back in later)
  select(-c(tempo, key, treble, tbb, genre)) %>%
  # remove SD columns 
  select(-c(sd_valence, sd_arousal)) %>%
  
  # pivot longer so each row is a clip-feature pair
  pivot_longer(cols = -c(song_id, orig_or_cover), names_to = 'feature', values_to = 'value') %>%
  # pivot wider so each row is a song-feature pair
  pivot_wider(id_cols = c(song_id, feature), names_from = orig_or_cover, values_from = value) %>%
  # compute differences between originals and covers on each feature
  mutate(difference = Original5S - Cover5S) %>%
  # drop the original/cover value columns 
  mutate(Original5S = NULL, Cover5S = NULL) %>%
  # finally, pivot wider so each song has its own row
  pivot_wider(id_cols = song_id, names_from = feature, values_from = difference) %>%
  # add "difference" suffix
  rename_with(~ paste0(., '_difference')) %>%
  rename(song_id = song_id_difference) %>%

  # add the pre-computed musical feature differences
  left_join(., musical_feat_diffs, by = join_by(song_id)) %>%
  # relocate valence and arousal to end 
  relocate(mean_valence_difference, .after = voice_type_difference) %>%
  relocate(mean_arousal_difference, .after = mean_valence_difference) %>%
  
  # add pre-computed MFCCs distance
  left_join(., mfcc_dist, by = join_by(song_id)) %>%
  # relocate to just after all of the MFCCs
  relocate(mfccs_dist, .after = mfcc12_difference) %>%
  
  # join the similarity ratings
  left_join(sim_ratings, ., by = join_by(song_id)) %>%
  select(-c(scaled))

# save
#write_csv(all_features_and_sim_ratings, 
#          '../../processed/expanded/joined_with_features/similarity_and_feature_differences.csv')


# combine SCALED computed feature differences with similarity ratings
# i.e. do basically the same thing but with scaled features as input
# This version scales the features before taking the difference
all_features_and_sim_ratings_scaled <- all_features_scaled %>%
  # compute acoustic and emotional feature differences
  # get the original/cover distinction from current song ID
  separate_wider_delim(song_id, '_', names = c('title', 'artist', 'orig_or_cover')) %>%
  # reconstruct the song_id column
  mutate(song_id = paste(title, artist, sep = '_')) %>%
  relocate(song_id) %>%
  mutate(title = NULL,
         artist = NULL) %>%
  
  # remove key, voice type, and genre
  select(-c(key, treble, tbb, genre)) %>%
  
  # pivot longer so each row is a clip-feature pair
  pivot_longer(cols = -c(song_id, orig_or_cover), names_to = 'feature', values_to = 'value') %>%
  # pivot wider so each row is a song-feature pair
  pivot_wider(id_cols = c(song_id, feature), names_from = orig_or_cover, values_from = value) %>%
  # compute differences between originals and covers on each feature
  mutate(difference = Original5S - Cover5S) %>%
  # drop the original/cover value columns 
  mutate(Original5S = NULL, Cover5S = NULL) %>%
  # finally, pivot wider so each song has its own row
  pivot_wider(id_cols = song_id, names_from = feature, values_from = difference) %>%
  # add "difference" suffix
  rename_with(~ paste0(., '_difference')) %>%
  rename(song_id = song_id_difference) %>%
  
  # add the pre-computed scaled musical feature differences
  cbind(., musical_feat_diffs_scaled) %>%
  # relocate valence and arousal to end 
  relocate(mean_valence_difference, .after = voice_type_difference) %>%
  relocate(mean_arousal_difference, .after = mean_valence_difference) %>%
  
  # add MFCC distance
  mutate(mfccs_dist = mfcc_dist_scaled$V1) %>%
  relocate(mfccs_dist, .after = mfcc12_difference) %>%
  
  # join the similarity ratings
  left_join(sim_ratings, ., by = join_by(song_id)) %>%
  select(-c(mean_sim, sd_sim)) %>%
  rename(mean_sim = scaled) %>%
  relocate(mean_sim, .after = song_id)

# save
#write_csv(all_features_and_sim_ratings_scaled, 
#          '../../processed/expanded/joined_with_features/similarity_and_feature_differences_scaled.csv')



# This version scales the DIFFERENCES
all_features_and_sim_ratings_abs_scaled_numeric <- all_features %>%
  # compute acoustic and emotional feature differences
  # get the original/cover distinction from current song ID
  separate_wider_delim(song_id, '_', names = c('title', 'artist', 'orig_or_cover')) %>%
  # reconstruct the song_id column
  mutate(song_id = paste(title, artist, sep = '_')) %>%
  relocate(song_id) %>%
  mutate(title = NULL,
         artist = NULL) %>%
  
  # remove key, voice type, and genre
  select(-c(key, treble, tbb, genre)) %>%
  # remove SD columns 
  select(-c(sd_valence, sd_arousal)) %>%

  # pivot longer so each row is a clip-feature pair
  pivot_longer(cols = -c(song_id, orig_or_cover), names_to = 'feature', values_to = 'value') %>%
  # pivot wider so each row is a song-feature pair
  pivot_wider(id_cols = c(song_id, feature), names_from = orig_or_cover, values_from = value) %>%
  # compute differences between originals and covers on each feature
  mutate(difference = Original5S - Cover5S) %>%
  # drop the original/cover value columns 
  mutate(Original5S = NULL, Cover5S = NULL) %>%
  # take the absolute value of all of the differences
  mutate(difference = abs(difference)) %>%
  # finally, pivot wider so each song has its own row
  pivot_wider(id_cols = song_id, names_from = feature, values_from = difference) %>%
  # add "difference" suffix
  rename_with(~ paste0(., '_difference')) %>%
  rename(song_id = song_id_difference)

# remove song_id to scale
song_ids <- all_features_and_sim_ratings_abs_scaled_numeric$song_id
all_features_and_sim_ratings_abs_scaled_numeric %<>% select(-song_id)
# scale
all_features_and_sim_ratings_abs_scaled_numeric <- as.data.frame(scale(all_features_and_sim_ratings_abs_scaled_numeric))

all_features_and_sim_ratings_abs_scaled <- all_features_and_sim_ratings_abs_scaled_numeric %>%
  # add back the song IDs
  cbind(song_ids, .) %>%
  rename(song_id = song_ids) %>%
  
  # add the pre-computed scaled musical feature differences
  cbind(., musical_feat_diffs_scaled) %>%
  # relocate valence and arousal to end 
  relocate(mean_valence_difference, .after = voice_type_difference) %>%
  relocate(mean_arousal_difference, .after = mean_valence_difference) %>%
  
  # add MFCC distance
  mutate(mfccs_dist = mfcc_dist_scaled$V1) %>%
  relocate(mfccs_dist, .after = mfcc12_difference) %>%
  
  # join the similarity ratings
  left_join(sim_ratings, ., by = join_by(song_id)) %>%
  select(-c(mean_sim, sd_sim)) %>%
  rename(mean_sim = scaled) %>%
  relocate(mean_sim, .after = song_id)


#write_csv(all_features_and_sim_ratings_abs_scaled, 
#          '../../processed/expanded/joined_with_features/similarity_and_feature_scaled_differences.csv')
