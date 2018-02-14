#A dictionary is a data frame with keys in the first columns, and values in other columns
#Otherwise it will be converted to a data frame
table_lookup <- function(x,dict,key = 1, value = 2){
    #check if is data.frame
    if(is.vector(dict)){
        dict <- data.frame(key = names(dict),value = unname(dict))
        message("named vector dictionary converted to dataframe")
    }
    
    dict_index <- match(x,dict[[key]])
    #check if all values are in dictionary
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

# #Demo
# dictionary <- data.frame(key = letters, value1 = seq_along(letters),value2 = seq(from = 1, by = 3,length.out = length(letters)))
# #1.
# x <- sample(letters,26)
# table_lookup(x = x,dict = dictionary,key = "key",value = 2)
# #2.
# x<- c("a","b","ac")
# table_lookup(x = x,dict = dictionary,key = "key",value = "value1")
# #3.
# dictionary <- c(a = 1, b = 2, c = 3)
# table_lookup(x = x, dict = dictionary)
