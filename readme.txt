Download example files at https://drive.google.com/drive/folders/1VEELPrru2PN0txoTX8T03x_jVFZbXmvS?usp=share_link

1. text_mining
text mining to extract n-gram keywords, sorted by occurrences or tfidf
line 18 - change folder of setwd

input: SDG1/in/SDG1.global.csv - 2009-2020 global publication list downloaded from Scopus, searched using Elsever SDG1 query 2019 (https://elsevier.digitalcommonsdata.com/datasets/87txkw7khs/1)
output: SDG1/out/sdg_kw_f - output of keywords extracted from Author keywords and Index keywords; sorted by the occurrences
        SDG1/out/distinct_bigrams - output of 2-gram keywords extracted from title and abstract; sorted by the occurrences
        SDG1/out/distinct_trigrams - output of 3-gram keywords extracted from title and abstract; sorted by the occurrences
        SDG1/out/distinct_quadgrams - output of 4-gram keywords extracted from title and abstract; sorted by the occurrences
        SDG1/out/tfidf_rank_bigrams - output of 2-gram keywords extracted from title and abstract; sorted by tfidf values
        SDG1/out/tfidf_rank_trigrams - output of 3-gram keywords extracted from title and abstract; sorted by tfidf values
        SDG1/out/tfidf_rank_quadgrams - output of 4-gram keywords extracted from title and abstract; sorted by tfidf values


2. result_validation_bootstrap_loop
resample the data and run the process 10 times
line 18 - change folder of setwd
line 23 - you can change the times of re-sampling; the example uses 10 times
line 25 - you can change the percentage of re-sampling; the example uses 0.8

input: SDG1/in/SDG1.global.csv
output: SDG1/result validation/sdg_kw_f* - similar to SDG1/out/sdg_kw_f but re-sample 80% of the data, run 10 times (1-10)
        SDG1/result validation/distinct_* - similar to above
        SDG1/result validation/tfidf_* - similar to above

3. result validation compare plot
plot the results of the 10 times resampling with the original result using all of the data to compare
line 17 - change folder of setwd

input: SDG1/out/*
       SDG1/result validation/*
output: a figure
