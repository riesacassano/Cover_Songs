import os
import numpy as np
import pandas as pd
import matplotlib.pyplot as plt
import librosa as lb

# features extracted: RMS, spectral bandwidth, spectral centroid, spectral contrast, spectral flatness, spectral rolloff, zero crossing rate, Mel-frequency cepstral coefficients (offset and 12 coefficients)
# in general, the values are the mean over all of the time bins

# create the header for output dataframe
columns = ['clip', 'rms', 'spectral_bandwidth', 'spectral_centroid', 'spectral_contrast', 'spectral_flatness', 'spectral_rolloff', 'zero_crossing_rate', 'mfccs_offset']
mfcc_cols = []
for i in range(12): mfcc_cols.append('mfcc%d'%(i+1))
# we're using 12 MFCCs
columns = columns + mfcc_cols

# set folder where stimuli are
stimulus_folder = 'dense_corpus_2008-2019/stimuli_5s/'

# test with one file
# this includes visualizing MFCCs in comments
output_all = []

obj = os.scandir(stimulus_folder)
for entry in obj:
	if 'wav' in entry.name:
		this_song = entry.name
		print(this_song)

		# start the list that we'll append the values to
		this_song_path = stimulus_folder + this_song
		this_output = [this_song] 

		# load the song
		y, sr = lb.load(this_song_path)
		
		# root-mean-square (rms)
		rms = np.mean(lb.feature.rms(y=y))
		this_output.append(rms)

		# spectral bandwidth
		bandwidth = np.mean(lb.feature.spectral_bandwidth(y=y))
		this_output.append(bandwidth)

		# spectral centroid
		centroid = np.mean(lb.feature.spectral_centroid(y=y))
		this_output.append(centroid)

		# spectral contrast
		contrast = np.mean(lb.feature.spectral_contrast(y=y))#, axis=1) #specifying axis gives by octave
		this_output.append(contrast)

		# spectral flatness
		flatness = np.mean(lb.feature.spectral_flatness(y=y))
		this_output.append(flatness)

		# spectral rolloff
		rolloff = np.mean(lb.feature.spectral_rolloff(y=y))
		this_output.append(rolloff)

		# zero crossing rate
		zcr = np.mean(lb.feature.zero_crossing_rate(y=y))
		this_output.append(zcr)

		# Mel-frequency cepstral coefficients
		mfccs = lb.feature.mfcc(y=y, sr=sr, n_mfcc=13)
		mfccs_mean = np.mean(mfccs, axis=1)

		this_output = this_output + mfccs_mean.tolist()
		output_all.append(this_output)

#print(output_all)
df_output = pd.DataFrame(output_all, columns = columns)
df_output.to_csv('dense_corpus_2008-2019/features_acoustic_dense.csv', index=False)




# for testing
#these_songs = ['2010_1_Original5S.wav', '2010_1_Cover5S.wav']
#for i in range(2):
#	this_song = these_songs[i]
#	if 'wav' in this_song: # for ease of integrating

# visualize MFCCs (this was in the loop)
		#S = lb.feature.melspectrogram(y=y, sr=sr, n_mels=128, fmax=8000)
		#fig, ax = plt.subplots(nrows=2, sharex=True)
		#img = lb.display.specshow(lb.power_to_db(S, ref=np.max), x_axis='time', y_axis='mel', fmax=8000, ax=ax[0])
		#fig.colorbar(img, ax=[ax[0]])
		#ax[0].set(title='Mel spectrogram')
		#ax[0].label_outer()
		#img = lb.display.specshow(mfccs, x_axis='time', ax=ax[1])
		#fig.colorbar(img, ax=[ax[1]])
		#ax[1].set(title='MFCC')
		#plt.show()

		#plt.savefig('visualize_2010_1_cover_MFCCs.png', dpi=300)

