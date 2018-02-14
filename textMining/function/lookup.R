#A dictionary is a data frame with keys in the first columns, and values in other columns
table_lookup <- function(x,dict,key = 1, value = 2){
    dict_index <- match(x,dict[[key]])
    if (any(is.na(dict_index))) {
        missing_values <- x[is.na(dict_index)]
        meessage_content <- stringr::str_c("WARNING: NA(s) in output, ",sum(is.na(dict_index)),
                                           " value(s) are not in dictionary, which are:\n",missing_values,"\n")
        message(meessage_content)
        output <- dict[[value]][dict_index]
        attributes(output)$missing_values <- missing_values
        return(output)
    } else {
        return(dict[[value]][dict_index])
    }
}
