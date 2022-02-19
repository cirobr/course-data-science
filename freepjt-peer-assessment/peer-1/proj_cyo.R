#### Baseline Setup: Libraries and Data loading ####

# Check if all needed packages are installed
needed.packages <- c("tidyverse", "ggplot2", "tm", "stm", "quanteda")
for(pack in needed.packages){
  if(pack %in% rownames(installed.packages()) == FALSE)
  {install.packages(pack)}
}
rm(needed.packages)

library(tidyverse)
library(ggplot2)
library(tm)
library(stm)
library(quanteda)
library(lubridate)
library(stringi)
# switch this for your machine, you will not have the same file structure as I:
#setwd("E:/Projektvault/Coding/RDataCapstone/proj_chooseyourown")


#### Preprocessing: Take raw data and reduce it down to be usable ####
{
  # This work is based on data released by Twitter on IRA activity on the platform between 2009 and 2018
  # The raw data set is a .csv file which uncompressed totals a size  of ~5gb. As I highly doubt this would be usable on most machines, I stripped it down to the most influential tweets, as these should provide a clear overview of successfully used methods while disregarding methods that barely drew interactions
  # In case you want to read more about this and similar data sets, they can be found at: https://transparency.twitter.com/en/reports/information-operations.html

  # I tried packing this source file download and its processing into an automatic downloading/processing loop, but for some reason unzip() does not seem capable of handling files of over 1gb size.
  # Therefore if you want to recreate the data set yourself from the original Twitter version, you would have to manually download (link: https://storage.googleapis.com/twitter-election-integrity/hashed/2018_10/ira/ira_tweets_csv_hashed.zip, zipped download size ~1.2 gb) and unzip the file yourself.
  # Your target file system should be (your working directory)/data/ira_tweets_csv_hashed.csv to continue onward (you should have a file of ~5.3 gb size)
  
  tweets_raw <- read_csv("data/ira_tweets_csv_hashed.csv",
                         col_types = cols(tweetid = col_character(), retweet_tweetid = col_character(), in_reply_to_tweetid = col_character(),
                                          latitude = col_factor(), longitude = col_factor(), poll_choices = col_character()))
  # col_types used here because read_csv automatically data content types for columns - and for these vars the automatic assumption chooses the wrong format
  
  # Cleanup
  tweets_clean <- tweets_raw %>% 
    select(-c("poll_choices", "user_reported_location", "latitude", "longitude", "urls")) %>% # strip unused columns
    filter(tweet_language %in% c("en")) # remove non-English tweets
  rm(tweets_raw); gc() # free up RAM
  
  # Add "interactions" column (= sum of likes, retweets, replies and quote-replies)
  tweets_clean <- tweets_clean %>%
    mutate(interactions = like_count + retweet_count + reply_count + quote_count)
  
  # Examine dynamics in interaction numbers
  table(tweets_clean$interactions > 0) # tweets with at least 1 interaction
  # -> ~80% of tweets have no interactions
  table(tweets_clean$interactions >= 100) # tweets with at least 100 interaction
  # -> About 50.000 tweets with 100+ interactions -> reasonable cutoff-point, should be a manageable amount on most systems
  
  # Filter tweets to project dataset
  tweets_red <- tweets_clean %>% filter(interactions >= 100)
  rm(tweets_clean); gc()
  
  # save data files for later use
  write_csv(tweets_red, file.path("data/tweets_en-red.csv"), na = "NA", append = FALSE, col_names = T, quote_escape = "double")
  }

## Alternative: Download preprocessed data from my Github
{
  dl <- tempfile()
  download.file("https://github.com/fallenEngels/RDataCapstone/raw/main/proj_chooseyourown/data/tweets_en-red.zip", dl)
  
  tweets_red <- read_csv(unzip(dl, "tweets_en-red.csv"),
                         col_types = cols(tweetid = col_character(), retweet_tweetid = col_character(), in_reply_to_tweetid = col_character()))
}


#### Cleanup of data set ####

### Content removal
# remove tags of other users
tweets_red$tweet_text <- gsub("@[a-zA-Z0-9_]*", "", tweets_red$tweet_text)
# remove the "#" symbol before hash-tagged words
tweets_red$tweet_text <- gsub("#", "", tweets_red$tweet_text)
# remove hyperlinks: fortunately, Twitter uses a custom link shortener, that condenses links to used images, vidoes and external hyperlinks down to a t.co address, which makes removal of these extremely easy
tweets_red$tweet_text <- gsub("https?://t.co/[a-zA-Z0-9]*", "", tweets_red$tweet_text)

### Emoji recoding
# Emoji are a complex thing. They are special symbols generated locally on the user's screen based on a unique identifier that gets transmitted in text form. While most emoji are only made up of one identifier (for example a laughing face with the identifier U+1F600), others are built up in a modular fashion out of multiple identifiers: Country flags, for example, are made up of two codes (one for flags in general, and one to identify a specific country), while a family emoji can be made up of up to 7 different codes (to allow for a variety of family constellations). Different modifiers, like skin tones, only add further to this chaos.

# read emoji recode data base - available on github as well
emoji <- read_csv2("Other Files/emoji-list.txt", col_names = T, col_types = cols(code = col_character(), 
                                                                                 Replace = col_character()), locale = locale(encoding = "UTF-8"))
# add white spaces around each emoji, to keep them separate from one another - as very few people will type emoji-whitespace-emoji when chaining multiple emoji together
emoji$Replace <- paste(" ", emoji$Replace, " ")
# loop over all tweet texts, replacing emoji with their respective identifiers
# Caution: This code works, but it will not scale very well - the way it is written, it goes through all tweets once for every emote in the text file. While this will still be (somewhat) viable here for 1.809 x 46.807 search-and-replace operations, it may take hours or even days on larger data sets.
for (i in seq(1,length(emoji$Replace))){
  tweets_eng$tweet_text <- gsub(emoji$code[i], emoji$Replace[i], tweets_eng$tweet_text)
} ### !Caution, will take quite some time! (~30 mins on my system) ###
# remove duplicated white spaces introduced by replacement
tweets_red$tweet_text <- gsub("  ", " ", tweets_red$tweet_text)

# save data files for later use
write_csv(tweets_red, file.path("data/tweets_en-red.csv"), na = "NA", append = FALSE, col_names = T, quote_escape = "double")

### Text preparation
# Create a quanteda-usable object of all tweet texts from raw data, and strip punctuations and remaining special characters 
toks <- quanteda::tokens(tweets_red$tweet_text,
                         remove_symbols = TRUE,
                         remove_separators = TRUE,
                         remove_punct = TRUE)
# Further refine resulting corpus by removing capitalization and (in frequency analysis) useless stopwords
toks <- tokens_remove(tokens_tolower(toks), c(stopwords("en"), "amp"))
# Side note: the "amp" removed here is with a very high likelihood the remnant of the "&" symbol, which in HTML has the entity name "&amp;". While "&" and ";" get removed by remove_symbols and remove_punct respectively, the "amp" part remains and shows up at word frequency rank #8 when not removed.
# stemming to remove grammatical inflection of words
toks <- tokens_wordstem(toks)
# convert to document-feature-matrix, to add metadata back in
dtm <- dfm(toks)
# add metadata back in (tweet time, interactions)
docvars(dtm, "date") <- tweets_red$tweet_time
docvars(dtm, "userid") <- tweets_red$userid
docvars(dtm, "quotecount") <- tweets_red$quote_count
docvars(dtm, "replycount") <- tweets_red$reply_count
docvars(dtm, "likecount") <- tweets_red$like_count
docvars(dtm, "retweetcount") <- tweets_red$retweet_count

#### Frequency Analysis ####
textstat_frequency(dtm, n = 15)

# Bi-grams
bigram <- tokens_ngrams(toks, n = 2)
# Convert to (usable) frequency table
freq.bigram <- table(as.character(bigram))
sort(freq.bigram, decreasing=T)[1:30]



#### Natural Language Processing ####
# convert dfm to stm (as dfm is not readable by stm NLP algorithm)
stm_dtm <- convert(dtm, to = "stm")
# As the warning states, certain documents were dropped. This is because while cleaning the data, some tweets were completely emptied, as they only consisted of stop words to begin with (for example: "It's me."). These should be recorded for later
used_documents <- names(stm_dtm$documents)
used_documents <- used_documents %>% gsub("^text", "", .) %>% as.integer(.)


### Search for K
# NLP relies on a set number of topics to fill. While this value can be freely chosen, it can also be calculated based on existing data to an amount that best "explains" the underlying tweets and structures
select_k <- searchK(stm_dtm$documents, stm_dtm$vocab, data = stm_dtm$meta,
                    K = seq(5, 40, by = 5),
                    prevalence =~ s(date) + quotecount + replycount + likecount + retweetcount,
                    init.type = "Spectral", max.em.its = 10, seed = 2020)
# save file
save(select_k, file = "data/selectK.RData")

# plot results (raw R plot)
plot(select_k)
# pretty up values and tidy up for ggplot plot
selectk_df <- data.frame(K = unlist(select_k$results$K), exclus = unlist(select_k$results$exclus), semcoh = unlist(select_k$results$semcoh),
                         heldout = unlist(select_k$results$heldout), residual = unlist(select_k$results$residual),
                         bound = unlist(select_k$results$bound), lbound = unlist(select_k$results$lbound), em.its = unlist(select_k$results$em.its))
selectk_df %>% select(K, semcoh, exclus, heldout) %>%
  pivot_longer(-K, names_to = "measure", values_to = "value") %>% # melt data frame
  ggplot(aes(x = K, y = value, group = measure, color = measure)) +
  geom_line() +  facet_wrap(.~measure, scale = "free", ncol = 3) +
  labs(y = "", title = "Exclusivity and semantic coherence for K topics", subtitle = "tested with 10 iterations") +
  theme_minimal() +
  theme(legend.position="none")

### NLP model construction
stm_model_30 <- stm(stm_dtm$documents, stm_dtm$vocab, data = stm_dtm$meta,
                    K = 30,
                    prevalence =~ s(date) + quotecount + replycount + likecount + retweetcount,
                    init.type = "Spectral", max.em.its = 75, seed = 2020)
# !This may again take some time! #

save(stm_model_30, file = "data/stm_mod_30.RData")
# load("data/stm_mod_30.RData")
plot(stm_model_30, type = "summary", xlim = c(0, 0.2), n = 5)


#generate data bases for the most used and most-exclusively used terms for each topic
labels <- labelTopics(stm_model_30, topics = 30, n = 10)
prob <- list() # words with highest probability for topic
frex <- list() # words most exclusively used in topic
for(i in c(1:30)){
  prob[[i]] <- paste(labels$prob[i,], collapse = ' ')
  frex[[i]] <- paste(labels$frex[i,], collapse = ' ')
}
labels_df <- data.frame(Prob = unlist(prob), Frex = unlist(frex), Topics = 1:30)
rm(labels, prob, frex, i)

### Manually inspect each topic
top <- 1 # topic to inspect
{
  print(labels_df[top, 1]) # print 
  print(labels_df[top, 2])
  thought <- findThoughts(stm_model_30, n = 15, topics = top, text = tweets_red$tweet_text[used_documents])$docs[[1]]
  plotQuote(thought, width = 90, main = paste("Topic", top, sep = " "))
}


### Graphical representation of topic distribution

# get posting week for all tweets
dates <- paste(year(stm_dtm$meta$date), "-", isoweek(stm_dtm$meta$date), sep = "")
# unify date form (2015-8 -> 2015-08)
for (i in 1:length(dates)) {
  if (nchar(dates[i]) == 6) {
    stri_sub(dates[i], 6, 5) <- 0
  }
}
# merge with data frame of topic distribution
topic_times <- data.frame(dates, stm_model_30$theta)
# count tweets per week
counts <- topic_times %>% count(dates)
# add column names
colnames(topic_times) <- c("date", paste("topic_",1:30, sep = ""))
# average topic distribution for each week
topic_times <- aggregate(.~date, FUN = mean, data = topic_times)
# add # of occurrences to data set
topic_times <- topic_times %>% mutate(count = counts[,2])

# melt to long format
topic_times.long <- reshape2::melt(topic_times, id.vars = c("date", "count"))
p_top <- ggplot(topic_times.long, aes(x = date, y = value * count, group = variable, color = variable)) +
  geom_line() + geom_line(aes(x = date, y = count), color = "black") +
  scale_y_sqrt() + theme_minimal() + 
  scale_x_discrete(breaks = topic_times.long$date[floor(seq(1, nlevels(as.factor(topic_times.long$date)), length.out = 50))]) +
  labs(y = "# of tweets", x = "Calendar week") +
  theme(axis.text.x = element_text(angle = 90), legend.position = "none")
# save for rmd
save(p_top, file = "data/p_top.RData")
# load("data/p_top.RData")
