library(tidyverse)
library(lubridate)
dataset <- read_csv("cluster_data_for_students.csv")

#cleaning---------
dropped <- dataset %>%
    select(-1) %>% #delete column X1
    mutate(year_month_day = ymd(str_c(str_extract(IYEAR,"[0-9]+"),
                                      str_extract(IMONTH,"[0-9]+"),
                                      str_extract(IDAY,"[0-9]+"),
                                      sep = "-"))) %>% # change month day year into a year-month-day format 
    mutate(year=year(year_month_day),
           month = month(year_month_day)) %>% # create year and month as number
    select(-IDAY,-IMONTH,-IYEAR) %>% #drop IDAY,IMONTH,IYEAR 
    mutate(days = as.integer(year_month_day)-as.integer(min(year_month_day))) %>%
    select(-year_month_day)# count days to see trend in time easier



#change encoding----------
to_categorical <- "(_STATE)|(MARITAL)|(_RACEGR3)|(MSCODE)|(EMPLOY)" # use regular expression later
missing_in_dict <- "(_RACEGR2)|(EMPLOY1)|(BPHIGH4)|(TOLDHI2)"

#function to convert numeric to character
dict_to_cate <- function(x){
    as.character(x)
}
#for those columns which only have value 1,2,7,9 and NA, convert 1 to TRUE, 2 to FALSE and others to NA ----------
dict_to_logical <- function(x){
    my_dict <- c(T,F) %>%
        setNames(c(1,2))
    if(all(unique(x) %in% c(1,2,7,9,NA))){
        return(my_dict[as.character(x)])
    } else {
        return(x)
    }
}

encoded <- dropped %>% 
    select(-matches(missing_in_dict)) %>% #drop some missing columns in data dictionary
    mutate_at(vars(matches(to_categorical)),~as.character(.x)) %>% # convert some columns to categorical
    mutate_all(dict_to_logical) %>%
    select(-DIABETE3) # column has unique values 1,2,3,4,7,9 where it is only supposed to have 1,2,4,9, drop this column

#get all complete observations ------------
#drop columns that have too many missing values
NA_pct <- map_dbl(encoded,~sum(is.na(.x))/length(.x)) %>% sort(decreasing = T)
drop_cols <- names(NA_pct[NA_pct>0.25])
encode_drop <- encoded %>%
    select(-one_of(drop_cols))

#add index encode_drop-------------------
encode_drop_indexed <- encode_drop %>%
    mutate(index = row_number())
sum(complete.cases(encode_drop))
encode_drop_indexed <- encode_drop_indexed[complete.cases(encode_drop_indexed),]

#convert all logical to 1 or 0
final <- encode_drop_indexed %>%
    mutate_if(~is.logical(.x),~as.integer(.x))


#one hot encoding------------
one_hot <- model.matrix(index ~ .,data=final)[,-1] %>% as.data.frame()
dim(one_hot)

#PCA---------------------
#rule of thumb, we have 23000 observations, so we use at most 23 variables or it may not converge
normalize_column <- function(x){
    (x-min(x))/(max(x)-min(x))
}
scaled <- map_df(one_hot,normalize_column) #normalize data

pca = princomp(scaled)
out <- capture.output(summary(pca))
out_vec <- str_split(str_c(out[seq(5,length(out),by = 4)],collapse = " "),pattern = " ") 
pca_pct <- out_vec[[1]] %>% as.numeric() %>% na.omit()
pca_table <- tibble(components = seq_along(pca_pct),y=pca_pct)
pca_table %>% 
    ggplot(aes(x = components,y=y)) +
    geom_line() +
    geom_point(color="skyblue3") +
    labs(y="% of Variance explained")
#Fisrt 13 components explained 52% variance, let's see what makes each components
abs_component_df <- as.data.frame(abs(unclass(pca$loadings)))

#see most influential variables in the first 13 components an calculate their weight
mydf <- abs_component_df[,1:13]
#Sum weight of each variable in first 13 components
important_vars <- apply(map2_df(mydf,13:1,~.x*.y),MARGIN = 1,sum) %>%
    setNames(row.names(abs_component_df)) %>% sort(decreasing = T)

#Use the first 20 variables
vars_in_cluster <- names(important_vars)[1:20]


#kmeans--------------

model_data <- scaled %>%
    select(one_of(vars_in_cluster))

kmeans_cluster <- kmeans(model_data,centers = 4,nstart = 25)
str_c("between_SS / total_SS = ",round(100*(kmeans_cluster$betweenss/kmeans_cluster$totss),2),"%")



chronic_conditions <- str_split("CVDINFR4
CVDCRHD4
CVDSTRK3
ASTHMA3
CHCSCNCR
CHCOCNCR
CHCCOPD1
HAVARTH3
ADDEPEV2
CHCKIDNY",pattern="\\\n")[[1]]

condition_rank_list <- list()
graph_list_1 <- list()
for(condition in chronic_conditions){
    current_list <- encode_drop %>%
        group_by(`_STATE`) %>%
        summarise(mean_condition = mean(UQ(rlang::sym(condition)),na.rm = T)) %>%
        arrange(desc(mean_condition)) %>% list()
    names(current_list) <- condition
    condition_rank_list <- append(condition_rank_list,current_list)
    current_graph <- current_list[[1]] %>%
        top_n(30) %>%
        ggplot(aes(x = fct_reorder(`_STATE`,mean_condition),y=mean_condition)) +
        geom_col(fill="skyblue3") +
        coord_flip() +
        labs(x = "_STATE",title = condition) 
        
    graph_list_1 <- append(graph_list_1,list(current_graph))
}

graph_list_1[3]
