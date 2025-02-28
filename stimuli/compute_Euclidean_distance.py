# This script computes Euclidean distance between MFCCs for original-cover pairs

import numpy as np
import pandas as pd

# load the acoustic features
#features = pd.read_excel('expanded_corpus_1955-2022/features_acoustic_expanded.xlsx')
features = pd.read_excel('dense_corpus_2008-2019/features_acoustic_dense.xlsx')
mfccs = features.iloc[:,9:]

# get the song_ids 
song_ids = features['clip'].str.split('_', expand = True)
song_ids = song_ids[0] + '_' + song_ids[1]
song_ids = song_ids.unique()

distances = []

# for each song
for i in range(len(song_ids)):
	# get the corresponding two rows of the MFCCs
	mfccs_cover = mfccs.iloc[2*i, :].to_numpy()
	mfccs_original = mfccs.iloc[2*i+1, :].to_numpy()
	# calculate the distance
	distances.append(np.linalg.norm(mfccs_original - mfccs_cover))


output = pd.DataFrame({'song_id': song_ids, 'mfccs_dist': distances})
#output.to_csv('expanded_corpus_1955-2022/mfccs_distance.csv', index = False)
output.to_csv('dense_corpus_2008-2019/mfccs_distance.csv', index = False)