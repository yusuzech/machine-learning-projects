#load stemming dictionary into global environment first
#dictionary should be of the following format:
#w columns: stem and term
stemming_words <- function(words,stem_dict){
    match_position <- match(words,stem_dict$term)
    match_result <- ifelse(is.na(match_position),words,stem_dict$stem[match_position])
    return(match_result)
}

#demo
#stemming_words(c("likes","loves","abstract"),stem_dict = tab)
