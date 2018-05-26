#This is a sub-project of project.R
library(tidyverse)
library(tidytext)
library(stringr)
library(wordcloud)
library(igraph)
library(ggraph)
library(bigrquery)
#1 find books reviews on bigquery-------------
n_book_sample <- 1000
set.seed(20070831)
selected_index <- sample(1:nrow(table1),n_book_sample)

selected_books <- table1$asin[selected_index]
#write_csv(tibble(x = selected_books),path = "output/selected_book_asin.csv")
#compose a sql
seq4_segment1 <-
  "SELECT reviewText,reviewerID,asin,overall
FROM `machinelearning-196501.machineLearningDataset.amazonBooks` 
WHERE asin IN"
selected_books_quote <- str_c('"',selected_books,'"',collapse = ",")
sql4_segment2 <- str_c("(",selected_books_quote,")")
sql4 <- str_c(seq4_segment1,sql4_segment2,sep = " ")
table4 <- query_exec(sql4, project = project,use_legacy_sql = FALSE,max_pages = Inf)
table4 <- table4 %>% 
  mutate(overall = as.integer(overall))
text_table <- table4 %>% 
  select(asin,reviewerID,overall,reviewText)
#2 tokenlization ------------------
tidy_table <- text_table %>%
  unnest_tokens(word, reviewText)
#2.1 word stemming ---------
stemming_dictionary <- read_csv("data/stem_dict.csv")
source("function/stem_words.R")
stemmed_table <- tidy_table %>%
  mutate(word = stemming_words(word,stem_dict = stemming_dictionary))
#2.2 remove stopwords
clean_table <- stemmed_table %>%
  anti_join(stop_words)
#3 word frequency/cloud in each category(positive, neutral, negative) -------
convert_rating <- function(ratings){
  ifelse(ratings %in% c(1,2),-1,
         ifelse(ratings %in% 3, 0,
                1))
}
#overall word frequency
freq_table <- clean_table %>%
  mutate(overall_rating = convert_rating(overall)) %>%
  group_by(overall_rating) %>%
  count(word) %>%
  arrange(overall_rating,desc(n))

#wordcloud negative
freq_table %>%
  filter(overall_rating == -1) %>%
  with(wordcloud(word, n, max.words = 100,random.order = FALSE))
#wordcloud neutral
freq_table %>%
  filter(overall_rating == 0) %>%
  with(wordcloud(word, n, max.words = 100,random.order = FALSE))
#wordcloud negative
freq_table %>%
  filter(overall_rating == 1) %>%
  with(wordcloud(word, n, max.words = 100,random.order = FALSE))
#4 2-gram analysis -------
tidy_table_2g <- text_table %>%
  mutate(overall_rating = convert_rating(overall)) %>%
  unnest_tokens(bigram, reviewText, token = "ngrams", n = 2)

#stem and remove stopwords
bigrams_separated <- tidy_table_2g %>%
  separate(bigram, c("word1", "word2"), sep = " ")

bigrams_filtered <- bigrams_separated %>%
  filter(!word1 %in% stop_words$word) %>%
  filter(!word2 %in% stop_words$word) %>%
  mutate(word1 = stemming_words(word1,stem_dict = stemming_dictionary),
         word2 = stemming_words(word2,stem_dict = stemming_dictionary))

bigrams_united <- bigrams_filtered %>%
  unite(bigram, word1, word2, sep = " ")

bigram_counts <- bigrams_united %>% 
  group_by(overall_rating) %>%
  count(bigram, sort = TRUE) %>%
  ungroup()
  
#word frequency
#negative
bigram_counts %>%
  filter(overall_rating == -1) %>%
  with(wordcloud(bigram, n, max.words = 30,random.order = FALSE))

#neutral
bigram_counts %>%
  filter(overall_rating == 0) %>%
  with(wordcloud(bigram, n, max.words = 30,random.order = FALSE))
#positive
bigram_counts %>%
  filter(overall_rating == 1) %>%
  with(wordcloud(bigram, n, max.words = 30,random.order = FALSE,scale = c(3,.5)))

#network of 2-grams------------
bigram_graph_master <- bigrams_filtered %>%
  group_by(overall_rating) %>%
  count(word1,word2, sort = TRUE) %>%
  top_n(20) %>%
  ungroup()

a <- grid::arrow(type = "closed", length = unit(.15, "inches"))
#negative
bigram_graph1 <- bigram_graph_master %>%
  filter(overall_rating == -1) %>%
  select(-1) %>%
  graph_from_data_frame() 
set.seed(20070831)
ggraph(bigram_graph1, layout = "fr") +
  geom_edge_link(aes(edge_alpha = n), show.legend = FALSE,
                 arrow = a, end_cap = circle(.07, 'inches')) +
  geom_node_point(color = "lightblue", size = 5) +
  geom_node_text(aes(label = name), vjust = 1, hjust = 1) +
  theme_void()

#neutral
bigram_graph2 <- bigram_graph_master %>%
  filter(overall_rating == 0) %>%
  select(-1) %>%
  graph_from_data_frame()
set.seed(20070831)
ggraph(bigram_graph2, layout = "fr") +
  geom_edge_link(aes(edge_alpha = n), show.legend = FALSE,
                 arrow = a, end_cap = circle(.07, 'inches')) +
  geom_node_point(color = "lightblue", size = 5) +
  geom_node_text(aes(label = name), vjust = 1, hjust = 1) +
  theme_void()

#positive
bigram_graph3 <- bigram_graph_master %>%
  filter(overall_rating == 1) %>%
  select(-1) %>%
  graph_from_data_frame()
set.seed(20070831)
ggraph(bigram_graph3, layout = "fr") +
  geom_edge_link(aes(edge_alpha = n), show.legend = FALSE,
                 arrow = a, end_cap = circle(.07, 'inches')) +
  geom_node_point(color = "lightblue", size = 5) +
  geom_node_text(aes(label = name), vjust = 1, hjust = 1) +
  theme_void()

saveRDS(bigram_graph_master,"data/bigram_graph_master.RData")

#sentiment scoring-----------------
set.seed(20070831)
sample_review_index <- sample(1:nrow(text_table),100)
text_to_analyze <- text_table[sample_review_index,]
#1.tokenlize words
step1 <- text_to_analyze %>%
  unnest_tokens(word, reviewText)
#2. stem words and remove stop words
step2 <- step1 %>%
  mutate(word = stemming_words(word,stem_dict = stemming_dictionary)) %>%
  anti_join(stop_words)
#3. join a dictionary that includes sentiment
step3 <- step2 %>%
  left_join(filter(sentiments,lexicon == "AFINN"),by = c("word"))
#4. final step:calculate score
final_step <-step3 %>%
  group_by(asin,reviewerID) %>%
  summarise(sentiment_score = mean(score,na.rm = TRUE),
            overall = first(overall))

final_step
