# text mining
# the code was written by Kevin Kang and maintained by Weiwei Wang after a handover

library(readr)
library(dplyr)
library(tidytext)
library(tidyr)
library(stats)
library(NLP)
library(tm)
library(ggplot2)
library(stringr)
#library(gtools)
#library(textclean)

SDGs <- c("SDG1")
for (sdg in SDGs) {
setwd(paste("C:/Users/wwan484/OneDrive - The University of Auckland/SDGs/paper/code/",sdg,sep=""))
getwd()

# read the combined excel
df_raw <- read.csv(paste("./in/",sdg,".global.csv",sep=""), stringsAsFactors = F, header = T)
outdir <- "./out"

#-------------------------------------------------------------------------------------------------------
#-------------------------------------------------------------------------------------------------------

stop_words_new <- stop_words %>%   # build stop words; from tidytext library; stop words: an, a, of, etc
  filter(!str_detect(word, "changes")) %>%  # include changes into stop words; can't use when do climate change
  filter(lexicon == "SMART") # The source of the stop word. Either "onix", "SMART", or "snowball"
#View(stop_words)
pre_process <- function(data){
  data <- gsub('\\?.*', "", data) # regular expression; '*' remove the words after ?
  data <- gsub('//©.*', "", data)
  data %>%
    removeNumbers() %>%  # remove numbers; but i.e., in climate change, PM2.5 is important so do not remove numbers
    removePunctuation(preserve_intra_word_contractions = TRUE) %>%
    removePunctuation(preserve_intra_word_dashes = TRUE) %>%
    tolower() %>%
    removeWords(stop_words_new$word) %>%  # remove stop words
    stripWhitespace() %>%
    as.data.frame()
}

# stop words are removed in the beginning (just after reading the data in);
# undesireable words are removed later and in function ngrams_unite (from title+abs)
# undesireable words, somehow duplicate with stop words
undesirable_words <- c("zealand", "may", "can", "also", "included", "including", "article", "elsevier", "springer", "wiley", "sons", 
                       "taylor","francis","author", "study", "studies", "royal", "austrilian", "trial", "journal", "na", "ieee",
                       "rights reserved", "randomised controlled", "confidence interval", "abstract", 'life cycle assessment',
                       "statistically significant","logistic regression", "systematic review", "primary outcome", "data collection", "main outcome")

#extract abastract, preprocess abstract
sdg16_abs <- pre_process(df_raw$Abstract)
names(sdg16_abs) <- "text"  # give a name 'text'
glimpse(sdg16_abs)  # view data structure
sdg16_abs$text <- as.character(sdg16_abs$text)  # change type to character

#extract Title combine with preprocessed abstract
df_t <- df_raw[, "Title"] # no preprocess for title
df_a_t <- cbind(sdg16_abs, df_t)
names(df_a_t) <- c("Abstract", "Title")
rm(df_t)

#extract Author and Index keywords
sdg16_kw <- df_raw[,c("Author.Keywords", "Index.Keywords")]  # Author keywords are given by author, index keywords are given by someone else
sdg16_kw <- sdg16_kw %>%
  unite(keywords, 'Author.Keywords', 'Index.Keywords', sep = "; ")  # combine, separate using ';'
sdg16_kw$keywords <- tolower(sdg16_kw$keywords)

kw_len <- max(str_count(sdg16_kw$keywords, "; "))  # count number of keywords;
temp_kw <- sdg16_kw %>%
  separate(keywords, paste0("word",1:max(str_count(sdg16_kw$keywords, "; "))), sep = "; ") # use ';' separate keywords

ifelse(kw_len %% 2 == 0, kw_len, kw_len <- kw_len - 1)

temp_kw_full <- vector()  # sort the keywords in column instead of row
for (i in seq(1, kw_len, by = 2)){
  temp <- c(temp_kw[,i], temp_kw[,i+1])
  temp_kw_full <- c(temp_kw_full, temp)
}
temp <- as.data.frame(temp_kw_full) # change to dataframe type

sdg16_kw_vec <- temp %>%
  filter(!temp_kw_full == "",
         !temp_kw_full %in% undesirable_words) %>%
  count(temp_kw_full, sort = T) %>%
  mutate(total = sum(n), p = n / total * 100) # calculate the probability of a word

rm(temp)
rm(temp_kw)
rm(temp_kw_full)
rm(df_raw)
rm(sdg16_abs)
rm(sdg16_kw)

dir.create(outdir,recursive = TRUE)
write.csv(sdg16_kw_vec, paste(outdir,'/sdg_kw_f.csv',sep="",collapse=NULL))
# until here, preprocess has been finished and keywords from 'keywords' are extracted; # below we will transfer abstract to keywords
#-------------------------------------------------------------------------------------------------------
#-------------------------------------------------------------------------------------------------------

#pre for TDIDF, ngram by title; function: sort keywords by frequency 
ngrams_unite <- function(dataset, ngram){
  if(ngram == 1){
    dataset %>%
      unnest_tokens(word1, Abstract) %>%
      filter(!word1 %in% undesirable_words) %>%
      filter(!nchar(word1) <= 2) %>%
      count(word1, sort = T)  # probably ignore the single word so doesn't count the mutate
  }
  else if(ngram == 2){
    dataset %>%
      unnest_tokens(word, Abstract, token = "ngrams", n = ngram) %>%
      separate(word, c("word1", "word2"), sep = " ") %>%
      filter(!word1 %in% undesirable_words,   # check the individual word; remove the undesired words
             !word2 %in% undesirable_words) %>%
      filter(!nchar(word1) <= 1,
             !nchar(word2) <= 2) %>%
      unite(keyword, word1, word2, sep = " ") %>%
      filter(!keyword %in% undesirable_words) %>%  # check combined words; until here, 2 columns: titles and each keywords from the paper
      count(Title, keyword, sort = TRUE) %>% # count #keywords within each title
      mutate(total = sum(n))
  } 
  else if(ngram == 3){
    dataset %>%
      unnest_tokens(word, Abstract, token = "ngrams", n = ngram) %>%
      separate(word, c("word1", "word2", "word3"), sep = " ") %>%
      filter(!word1 %in% undesirable_words,
             !word2 %in% undesirable_words,
             !word3 %in% undesirable_words) %>%
      filter(!nchar(word1) <= 1,
             !nchar(word2) <= 2,
             !nchar(word2) <= 2) %>%
      unite(keyword, word1, word2, word3, sep = " ") %>%
      filter(!keyword %in% undesirable_words) %>%
      count(Title, keyword, sort = TRUE) %>%
      mutate(total = sum(n))
  }
  else if(ngram == 4){
    dataset %>%
      unnest_tokens(word, Abstract, token = "ngrams", n = ngram) %>%
      separate(word, c("word1", "word2", "word3", "word4"), sep = " ") %>%
      filter(!word1 %in% undesirable_words,
             !word2 %in% undesirable_words,
             !word3 %in% undesirable_words,
             !word4 %in% undesirable_words) %>%
      filter(!nchar(word1) <= 1,
             !nchar(word2) <= 2,
             !nchar(word3) <= 2,
             !nchar(word4) <= 2) %>%
      unite(keyword, word1, word2, word3, word4, sep = " ") %>%
      filter(!keyword %in% undesirable_words) %>%
      count(Title, keyword, sort = TRUE) %>%
      mutate(total = sum(n))
  }
}

#calculats TDIDF; use TDIDF to sort the keywords according to importance
ngrams_tfidf <- function(dataset){
  dataset %>%
    #group_by(Title) %>% 
    mutate(rank = row_number(),  # 'dataset' before %>% is the input df to modify
           `term frequency` = n/total) %>%  # append rand and term frequency to dataset: but seems not used
    bind_tf_idf(keyword, Title, n) %>%
    arrange(desc(tf_idf))
}

#distinct n-gram;
distinct_ngram <- function(dataset){
  dataset %>%
    count(keyword, sort = T) %>%
    mutate(p = n / nrow(df_raw) * 100)
}

#generate n-gram WF; call function; word frequency result
df_bigrams <- ngrams_unite(df_a_t, 2) #df_a_t= abstract + title
df_trigrams <- ngrams_unite(df_a_t, 3)
df_quadgrams <- ngrams_unite(df_a_t, 4)

#n-gram TFIDF; call tfidf function; tfidf result
bigrams_tfidf <- ngrams_tfidf(df_bigrams)
trigrams_tfidf <- ngrams_tfidf(df_trigrams)
quadgrams_tfidf <- ngrams_tfidf(df_quadgrams)

#WF rank n-gram; output the results
distinct_bigrams <- df_bigrams %>%
  count(keyword, sort = T) %>% # only count the #papers that contain the keyword but don't consider the occurrence time in the paper
  mutate(p = n / nrow(df_a_t) * 100) # calculate probability; n of paper/total paper number
write.csv(distinct_bigrams, paste(outdir,"/distinct_bigrams.csv",sep="",collapse=NULL))
distinct_trigrams <- trigrams_tfidf %>%
  count(keyword, sort = T) %>%
  mutate(p = n / nrow(df_a_t) * 100)
write.csv(distinct_trigrams, paste(outdir,"/distinct_trigrams.csv",sep="",collapse=NULL))
distinct_quadgrams <- quadgrams_tfidf %>%
  count(keyword, sort = T) %>%
  mutate(p = n / nrow(df_a_t) * 100)
write.csv(distinct_quadgrams, paste(outdir,"/distinct_quadgrams.csv",sep="",collapse=NULL))

#TFIDF rank n-gram
tfidf_rank_bigrams <- bigrams_tfidf %>%
  distinct(keyword) # unique rows (retain the one met first); output the highest tfidf value of the word
write.csv(tfidf_rank_bigrams, paste(outdir,"/tfidf_rank_bigrams.csv",sep="",collapse=NULL))
tfidf_rank_trigrams <- trigrams_tfidf %>%
  distinct(keyword)
write.csv(tfidf_rank_trigrams, paste(outdir,"/tfidf_rank_trigrams.csv",sep="",collapse=NULL))
tfidf_rank_quadgrams <- quadgrams_tfidf %>%
  distinct(keyword)
write.csv(tfidf_rank_quadgrams, paste(outdir,"/tfidf_rank_quadgrams.csv",sep="",collapse=NULL))
}
