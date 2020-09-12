library(tidyverse)
library(gutenbergr)
library(tidytext)
options(digits = 3)

texto <- "Pride and Prejudice"
x <- str_detect(gutenberg_metadata$title, texto)
ind <- which(x)
length(ind)

gutenberg_metadata[ind,]
registro <- gutenberg_works(title == texto, languages = "en", only_languages = TRUE)
registro$gutenberg_id

words <- gutenberg_download(registro$gutenberg_id)
text_df <- tibble(text = words$text)
text_df
words_df <- text_df %>% unnest_tokens(word, text)
nrow(words_df)

words_df1 <- words_df %>% anti_join(stop_words)
head(words_df1)
nrow(words_df1)

words_df2 <- words_df1 %>% filter(!str_detect(words_df1$word, "\\d+"))
head(words_df2)
nrow(words_df2)

tab <- data.frame(table(words_df2))
tab %>% filter(Freq >= 100) %>% nrow()

afinn <- get_sentiments("afinn")
afinn_sentiments <- inner_join(words_df2, afinn)

afinn_sentiments %>% nrow()
afinn_sentiments %>% filter(value > 0) %>% nrow() / afinn_sentiments %>% nrow()
afinn_sentiments %>% filter(value == 4) %>% nrow()
