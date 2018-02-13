dict_url <- "http://www.lexiconista.com/Datasets/lemmatization-en.zip"
tmp <- tempfile()
download.file(dict_url, tmp)

# extract the contents
con <- unz(tmp, "lemmatization-en.txt", encoding = "UTF-8")
tab <- read_delim(con,delim = "\t",col_names = FALSE)
names(tab) <- c("stem", "term")

write_csv(tab,"data/stem_dict.csv")
