# This script does the final preparation for memory analysis: joining with 
# scaled similarity ratings and musical/emotional features

# The memory data files involved are:
# `original_memories-w_or_wo_cover_mem.csv` for likelihood analysis
# `memory_pairs_coded.csv` for content and qualities analysis
# `all_memories.csv`
# `all_memory_embeddings.csv` for potential future exploratory analysis

# The memory data is joined with:
# `similarity_and_scaled_feature_differences.csv`
# `all_features_scaled.csv`

setwd("/Users/rcassan2/Documents/GitHub/cover_songs_for_pub_repo/data/scripts/dense/")
library(tidyverse)
library(magrittr) 

# load the memory data files
original_all_covers <- read_csv('../../processed/dense/memory/original_memories-w_or_wo_cover_mem.csv')
memory_pairs <- read_csv('../../processed/dense/memory/memory_pairs_coded.csv')
all_memories <- read_csv('../../processed/dense/memory/all_memories.csv')
all_memory_embeddings <- read_csv('../../processed/dense/memory/all_memory_embeddings.csv')

# load the feature files
feat_diffs <- read_csv('../../processed/dense/joined_with_features/similarity_and_scaled_feature_differences.csv')
feats <- read_csv('../../processed/dense/joined_with_features/all_features_scaled.csv')

# join!
orig_all_covers <- original_all_covers %>%
  left_join(., feat_diffs, by = join_by(song_id))
write_csv(orig_all_covers, '../../processed/dense/joined_with_features/memory_original-w_or_wo_cover_memories.csv')
mem_pairs <- memory_pairs %>%
  left_join(., feat_diffs, by = join_by(song_id))
write_csv(mem_pairs, '../../processed/dense/joined_with_features/memory_pairs.csv')

all_mem <- feats %>%
  # have to do some name repair first
  separate_wider_delim(song_id, delim = '_', 
                       names = c('year', 'rank', 'orig_or_cover')) %>%
  # reconstruct the song_id column
  mutate(song_id = paste(year, rank, sep = '_')) %>%
  relocate(song_id) %>%
  mutate(year = NULL,
         rank = NULL) %>%
  mutate(orig_or_cover = ifelse(orig_or_cover == 'Original5S', 'orig', 'cover')) %>%
  
  left_join(all_memories, ., by = join_by(song_id, orig_or_cover))
write_csv(all_mem, '../../processed/dense/joined_with_features/memory_all.csv')

all_mem_emb <- feats %>%
  # have to do some name repair first
  separate_wider_delim(song_id, delim = '_', 
                       names = c('year', 'rank', 'orig_or_cover')) %>%
  # reconstruct the song_id column
  mutate(song_id = paste(year, rank, sep = '_')) %>%
  relocate(song_id) %>%
  mutate(year = NULL,
         rank = NULL) %>%
  mutate(orig_or_cover = ifelse(orig_or_cover == 'Original5S', 'orig', 'cover')) %>%

  left_join(all_memory_embeddings, ., by = join_by(song_id, orig_or_cover)) %>%
  relocate(c(772:798), .after = orig_or_cover)
write_csv(all_mem_emb, '../../processed/dense/joined_with_features/memory_all_embeddings.csv')
