import os
import pandas as pd

#input_folder = 'similarity/by_subject/'
input_folder = 'emotion/by_subject/'
#output_folder = 'similarity/by_subject_anon/'
output_folder = 'emotion/by_subject_anon/'

file_list = os.listdir(input_folder)

# testing
#for i in range(1):
#	entry = file_list[i]
#	if '.csv' in entry:
for entry in file_list:
	if '.csv' in entry:
		print(entry)

		this_file = pd.read_csv(input_folder + entry)
		if "PROLIFIC_PID" not in this_file.columns: print('not here')
		else:
			this_file.drop("PROLIFIC_PID", axis=1, inplace=True)
			this_file.to_csv(output_folder + entry, index=False)