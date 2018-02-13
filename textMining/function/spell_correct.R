#correct spelling using hunspell library: 
#https://hunspell.github.io/
#https://cran.r-project.org/web/packages/hunspell/index.html
#works on word-level

spell_correct <- function(words){
    library(hunspell)
    library(purrr)
    #check if spelling is correct
    check_result <- hunspell_check(words)
    #Use the first suggestion is spelling is incorrect
    corrected <- ifelse(check_result,words,
                        map_chr(hunspell_suggest(words),1))
    return(corrected)
}

#Demo
#spell_correct(c("test","naive","Yifu","Daniel","Jackson","Jack","corrrect"))
#result:
#[1] "test"    "naive"   "Iffy"    "Daniel"  "Jackson" "Jack"    "correct"
#Yifu is a Chinese name so it is identify as incorrect spelling
