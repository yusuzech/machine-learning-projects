#1. set up-------------------
install.packages("bigrquery")
install.packages("googleLanguageR")
install.packages("tidytext")
library(googleLanguageR)
library(bigrquery)
library(tidyverse)
library(stringr)
library(tidytext)
gl_auth("machineLearning-95f2ba11fc15.json")
project <- "machinelearning-196501"
#2. randomly select books --------------------
#choose books that only have more than 19 reviews
sql1 <- 
"SELECT *
FROM(
    SELECT asin , COUNT(*) AS num_reviews
    FROM `machinelearning-196501.machineLearningDataset.amazonBooks` 
    GROUP BY asin
    )
WHERE num_reviews >= 20"

table1 <- query_exec(sql1, project = project,use_legacy_sql = FALSE,max_pages = Inf)

#randomly select books
n_book_sample <- 1000
set.seed(20070831)
selected_index <- sample(1:nrow(table1),n_book_sample)

selected_books <- table1$asin[selected_index]
#write_csv(tibble(x = selected_books),path = "output/selected_book_asin.csv")
samples = read_csv("data/book_info_966.csv")
sample_books <- samples$asin
#3. find user's past ratings and average them---------
#3.1 find reviewerIDs------------
sql2_segment1 <- 
"SELECT DISTINCT reviewerID
FROM `machinelearning-196501.machineLearningDataset.amazonBooks` 
WHERE asin IN"
quote_books <- str_c('"',sample_books,'"',collapse = ",")
sql2_segment2 <- str_c("(",quote_books,")")
sql2 <- str_c(sql2_segment1,sql2_segment2,sep = " ")
table2 <- query_exec(sql2, project = project,use_legacy_sql = FALSE,max_pages = Inf)
#3.2 calculate reviewer average score
sql3_segment1 <-
  "SELECT reviewerID,AVG(overall) AS avg_user_rating, COUNT(*) AS num_reviews
FROM `machinelearning-196501.machineLearningDataset.amazonBooks` 
WHERE reviewerID IN"
quote_users <- str_c('"',table2$reviewerID,'"',collapse = ",")
sql3_segment2 <- str_c("(",quote_users,")")
sql3_segment3 <- "GROUP BY reviewerID"
sql3 <- str_c(sql3_segment1,sql3_segment2,sql3_segment3,sep = " ")
#getting query too big error seperate query and combine results
#seperate one query into 10,total length 50428
table3 <- tibble()
for(n_query in 1:10){
  if(n_query <=9){
    query_index <- seq.int(5000*(n_query - 1)+1,5000*n_query) 
  } else {
    query_index <- seq.int(45001,length(table2$reviewerID))
  }
  quote_users <- str_c('"',table2$reviewerID[query_index],'"',collapse = ",")
  sql3_segment2 <- str_c("(",quote_users,")")
  sql3_segment3 <- "GROUP BY reviewerID"
  sql3 <- str_c(sql3_segment1,sql3_segment2,sql3_segment3,sep = " ")
  current_query <- query_exec(sql3, project = project,use_legacy_sql = FALSE,max_pages = Inf) 
  table3 <- bind_rows(table3,current_query)
  cat("current query",n_query,",index",min(query_index),"to",max(query_index),"in",length(table2$reviewerID),"completed.\n")
  
}
#3.3 calculate average book rating-------------------------
sql5_template <-
"SELECT asin,AVG(overall) AS avg_book_rating
FROM `machinelearning-196501.machineLearningDataset.amazonBooks` 
WHERE asin IN (PLEASESELECTME)
GROUP BY asin"
sql5 <- str_replace(sql5_template,pattern = fixed("PLEASESELECTME"),
            replacement = str_c('"',samples$asin,'"',collapse = ","))
table5 <-  query_exec(sql5, project = project,use_legacy_sql = FALSE,max_pages = Inf)
#4. Do linear regression on user rating, price and sentiment---------
#4.1 text mining(methodology)----------------
#source("textmining.R")
#4.2 google NLP api
#4.2.1 correlation between sentiment and actual score
#set.seed(20070831)
#rand_index <- sample(1:nrow(table4),1000)
#texts <- table4$reviewText[rand_index]
#nlp_result <- gl_nlp(texts,nlp_type = "analyzeSentiment",language = "en",type = "PLAIN_TEXT",encodingType  = "UTF8")

#sent_rating <- bind_cols(nlp_result$documentSentiment,tibble(overall = table4$overall[rand_index]))

#5. final regression and  testing---------
#source("model_training.R)

