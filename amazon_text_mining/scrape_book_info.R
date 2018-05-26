library(tidyverse)
library(stringr)
library(rvest)
library(RSelenium)
library(seleniumPipes)

book_urls <- str_c("https://www.amazon.com/exec/obidos/ASIN/",
                   sample_books)
book_table <- tibble()

#start server
rD <- rsDriver (browser = 'chrome',chromever = "latest",port = 4445L)
#open browser
remDr <- remoteDr(browserName = "chrome",port = 4445L)
#scraping
for(i in 1:length(book_urls)){
  web_url <- book_urls[i]
  remDr %>% go(web_url)
  #asin
  book_asin <- sample_books[i]
  #title
  book_title <- remDr %>% getPageSource() %>% html_nodes("#productTitle,#ebooksProductTitle") %>% html_text()
  book_title <- ifelse(identical(book_title,character(0)),NA,book_title)
  #price
  price_text <- remDr %>% getPageSource() %>% html_nodes(".a-color-price") %>% html_text()
  book_price <- price_text[str_detect(price_text,pattern = "^\\$")] %>%
    parse_number() %>% mean()
  book_price <- ifelse(is.nan(book_price),NA,book_price)
  #make a row
  current_book <- tibble(title = book_title,price = book_price, asin = book_asin)
  #merge into table
  book_table <- bind_rows(book_table,current_book)
  cat(i,"/",length(book_urls)," item completed","values:",book_asin," ",book_title," ",book_price," ","\n")
  Sys.sleep(runif(1,min = 1, max = 3))
}
remDr %>% deleteSession()
rD[["server"]]$stop() 

write_csv(book_table,path = "data/book_info_1000.csv")