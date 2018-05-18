library(rjson)
library(jsonlite)
library(tidyverse)
library(tidytext)


source("function/stem_words.R")
stem_dict <- read_csv("data/stem_dict.csv")

data("stop_words")

json_file <- "data/Books_5.json"
start_time <- Sys.time()
review <- stream_in(file(json_file))
end_time <- Sys.time()

end_time - start_time

comments <- review %>%
    as_tibble() %>%
    select(reviewerID,asin,overall,reviewText) %>%
    filter(row_number() <= n()/10) %>%
    mutate(overall = as.factor(overall))


stemmed_comments <- comments %>%
    unnest_tokens(output = word,input = reviewText) %>%
    mutate(word = stemming_words(word,stem_dict = stem_dict))

#remove stopwords
clean_comments <- stemmed_comments %>%
    anti_join(stop_words)

#word frequency
clean_comments %>%
    group_by(overall) %>%
    count(word,sort = TRUE) %>%
    top_n(15,wt = n) %>%
    ungroup() %>%
    mutate(word = reorder(word,n)) %>%
    ggplot(aes(x = word, y = n, fill = overall)) +
    geom_col() +
    facet_wrap(~ overall, scales = "free", ncol = 2) +
    coord_flip() 

#word
