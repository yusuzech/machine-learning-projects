install.packages("bigrquery")
library(bigrquery)
library(tidyverse)
project <- "machinelearning-196501"
sql <- "SELECT asin,reviewText
FROM `machinelearning-196501.machineLearningDataset.amazonBooks`
LIMIT 1000"
table <- query_exec(sql, project = project,use_legacy_sql = FALSE,max_pages = Inf)

table %>%
  ggplot(aes(x = number_reviews)) +
  geom_histogram(binwidth = 1) +
  coord_cartesian(xlim = c(0,60))


min(table$number_reviews)
    