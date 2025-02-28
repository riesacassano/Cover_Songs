# This script takes the raw csv files from Pavlovia and combines them (`raw_combined.csv`).

import os
import numpy as np
import pandas as pd

# set the directory
#filepath = '../../raw/expanded/similarity/by_subject/'
filepath = '../../raw/expanded/emotion/by_subject/'

combined = pd.DataFrame()

for file in os.scandir(filepath):
	if '.csv' in file.name: 
		# get the current csv file
		this_df = pd.read_csv(filepath + file.name)
		# combine this data with the data we already saw
		# (definitely not the most efficient way to do this)
		combined = pd.concat([combined, this_df], ignore_index = True)


#combined.to_csv('../../raw/expanded/similarity/raw_combined.csv', index = False)
combined.to_csv('../../raw/expanded/emotion/raw_combined.csv', index = False)