Corpora readme

For both the dense and expanded corpora, each folder contains: 
1) a Word Doc with a table of all of the songs. This includes a table with the genre categories (detail below)
2) an Excel file with all of the covers we found and their YouTube links. (Links may be broken if videos have been privated or removed.) This includes timestamps for the 15-second excerpt from both the originals (both corpora) and the covers (dense corpus only). Songs that appear in both the dense and the expanded corpus are only represented in "all covers (dense).xlsx".
3) separate Excel files with automatically extracted acoustic features (from librosa) and the manually coded musical features. (The script for extracting the acoustic features is in the main folder: "extract_acoustic_features.py")
4) the clips themselves as .wav files (sampling rates are either 44.1kHz or 48kHz). Clips are not publicly available, but can be provided upon request.

The "dense" corpus contains 50 songs from 2008-2019. In general these are the top 5 songs from the Billboard Hot 100 Year-End (pop) charts from that year. The 15-second clips of these songs were used in the memory experiment (study 2). The 5-second clips are also included, as they were used to gather similarity ratings for original-cover pairs and emotional valence and arousal. However, these ratings are only used in Study 2. Clips are labeled with year, Billboard rank for that year, and original or cover.

The "expanded" corpus contains 70 songs from 1955-2022. There are roughly 5 songs from each half-decade (1955-1959, 1960-1964, etc.). We primarily used the Billboard All-Time Hot 100 to find these songs, but we also considered the Rolling Stone Greatest Songs of All Time list as well as Billboard Year-End charts to fill in years that were not represented. 12 songs from the dense corpus also appear in the expanded corpus. The 5-second clips were used to gather similarity ratings for original-cover pairs and emotional valence and arousal for Study 1. Clips are labeled with title, artist, and original or cover. The "all covers (expanded).xlsx" file only has songs that were not included in the dense corpus.

Genre categories: For the dense corpus, all originals were categorized as "pop", so there are genre labels for covers only. There are also only 7 categories. For the expanded corpus, we needed additional categories and to categorize the originals to fully characterize the genre diversity of the corpus.

Note on clips and timestamps: cover clips were extracted from the same part of the song (e.g. first chorus) and the same lyrics as the original clips. When possible, we used original clips extracted by Dr. Amy Belfi.

~ RYCC 12-13-24, last updated 2-25-25