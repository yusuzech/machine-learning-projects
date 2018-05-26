#book data
bookwise <- samples %>%
  left_join(table5,by = "asin")
#user data
sql6_template <-
  "SELECT reviewerID,asin,reviewText,overall
FROM `machinelearning-196501.machineLearningDataset.amazonBooks` 
WHERE asin IN (PLEASESELECTME)"
sql6 <- str_replace(sql6_template,pattern = fixed("PLEASESELECTME"),
                    replacement = str_c('"',samples$asin,'"',collapse = ","))
table6 <-  query_exec(sql6, project = project,use_legacy_sql = FALSE,max_pages = Inf)

nlp_result1 <- gl_nlp(table6$reviewText,nlp_type = "analyzeSentiment",language = "en",type = "PLAIN_TEXT",encodingType  = "UTF8")

sent_rating1 <- bind_cols(nlp_result1$documentSentiment,tibble(overall = as.integer(table6$overall)))
#write_csv(sent_rating1,path = "data/sentiment_scores_and_overall_ratings.csv")
#sent_rating1 <- read_csv("data/sentiment_scores_and_overall_ratings.csv")
userwise <- bind_cols(table6[,c("asin","reviewerID")],sent_rating1)
#combined table
combined_table <- userwise %>%
  inner_join(bookwise,by = "asin") %>%
  inner_join(table3 %>% rename(n_user_reviews = num_reviews),by = "reviewerID") %>%
  inner_join(table1 %>% rename(n_book_reviews = num_reviews),by = "asin")
user_book_data <- combined_table[complete.cases(combined_table),]
#write_csv(user_book_data,path = "data/user_book_data.csv")

#model training ---------------
set.seed(20070831)
traning_books <- sample(user_book_data$asin,850)

train_data <- user_book_data %>%
  filter(asin %in% traning_books)
test_data <- user_book_data %>%
  filter(!(asin %in% traning_books))

model <- lm(overall ~ poly(score,3),data = train_data)
predicted.intervals <- predict(model,data = train_data,interval='confidence',level=0.99)
summary(model)
sentiment_poly <- tibble(x = train_data$score,y = predicted.intervals[,1],z = train_data$overall) %>%
  arrange(x) %>%
  group_by(x,z) %>%
  mutate(count = n()) %>%
  ggplot() +
  geom_point(aes(x = x, y = z,size = count),color = "skyblue3") +
  geom_line(aes(x = x, y = y)) +
  labs(x = "sentiment score",y = "User Rating","Polynomial") +
  theme_light();sentiment_poly
ggsave(sentiment_poly,filename = "output/sentiment_regression.png")

#model testing---------

#final model
y1_predicted =  predict(model,newdata = test_data,interval='confidence',level=0.99)
test_data_y1 <-
  bind_cols(test_data,tibble(y1 = y1_predicted[,1]))

#calculate pull weight
test_data_final <- test_data_y1 %>%
  mutate(bookPullWeight = case_when(n_book_reviews <= 40 ~ 0.1,
                                    n_book_reviews <= 80 & n_book_reviews > 40 ~ 0.2,
                                    n_book_reviews <= 120 & n_book_reviews > 80 ~ 0.25,
                                    n_book_reviews > 120 ~ 0.3),
         userPullWeight = case_when(n_user_reviews <= 10 ~ 0,
                                    n_user_reviews <= 20 & n_user_reviews > 10 ~ 0.15,
                                    n_user_reviews <= 40 & n_user_reviews > 20 ~ 0.2,
                                    n_user_reviews > 40 ~ 0.25)
         
  )
test_data_final = test_data_final %>%
  mutate(y2_unrounded = y1 + bookPullWeight * (y1 - avg_book_rating ) + userPullWeight *  (y1 - avg_user_rating),
         our_residuals = round(y2_unrounded) - overall)

mean(abs(test_data_final$our_residuals)) #But isnt that a biased sample? we need random reviews, from the raw data
