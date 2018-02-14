titles <- c("Twenty Thousand Leagues under the Sea", "The War of the Worlds",
            "Pride and Prejudice", "Great Expectations")

library(gutenbergr)

books <- gutenberg_works(title %in% titles) %>%
    gutenberg_download(meta_fields = "title")

library(stringr)

# divide into documents, each representing one chapter
by_chapter <- books %>%
    group_by(title) %>%
    mutate(chapter = cumsum(str_detect(text, regex("^chapter ", ignore_case = TRUE)))) %>%
    ungroup() %>%
    filter(chapter > 0) %>%
    unite(document, title, chapter)

