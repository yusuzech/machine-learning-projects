install.packages("googleLanguageR")
library(googleLanguageR)
gl_auth("machineLearning-95f2ba11fc15.json")

texts <- table$reviewText

nlp_result <- gl_nlp("bitch",nlp_type = "analyzeSentiment",language = "en",type = "PLAIN_TEXT",encodingType  = "UTF8")
nlp_result$documentSentiment



bind_cols(nlp_result$documentSentiment,tibble(overall = table$overall)) %>%
  mutate(product = magnitude * score) %>%
  cor()