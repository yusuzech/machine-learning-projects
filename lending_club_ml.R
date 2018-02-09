#libraries
library(tidyverse)
library(caret)
library(corrplot)
library(stringr)
library(h2o)

lending_data <- read.csv("https://raw.githubusercontent.com/h2oai/app-consumer-loan/master/data/loan.csv")

#glimpse at data structure
glimpse(lending_data)
#View data summary
summary(lending_data)

t(map_dbl(lending_data,~sum(is.na(.x))))

#check if these are the same observations
lending_data %>% 
    mutate(row_number = row_number()) %>% 
    filter(is.na(delinq_2yrs)) %>% 
    select(row_number,delinq_2yrs,revol_util,total_acc,longest_credit_length)

#remove some NA observations
cleaned_data <- lending_data %>%
    filter(!is.na(delinq_2yrs))


lending_data %>%
    group_by(emp_length) %>%
    summarise(bad_rate = sum(as.numeric(bad_loan))/n()) %>%
    mutate(emp_length = if_else(is.na(emp_length),"NA",as.character(emp_length))) %>%
    arrange(emp_length) %>% 
    ggplot(aes(x = emp_length, y = bad_rate)) +
    geom_col(fill = "skyblue3") +
    labs(title = "Employment length vs bad loan rate", subtitle = "when employment rate is missing, bad loan rate is higher")

#### 3.C.b. Find observations with rare values
col_class_fac <- map_chr(lending_data,class) == "factor"
map(lending_data[,col_class_fac],table)
#remove rare observations
cleaned_data <- cleaned_data %>% 
    filter(home_ownership != "ANY" | !(addr_state %in% c("IA","ID","ME","NE")))

#remove rare observations
cleaned_data <- cleaned_data %>% 
    filter(!(home_ownership == "ANY" | (addr_state %in% c("IA","ID","ME","NE"))))
#create new column is_emp_missing and remove original one
cleaned_data <- cleaned_data %>%
    mutate(is_emp_missing = if_else(is.na(emp_length),1,0)) %>%
    select(-emp_length)


#correlation heatmap with numerical variables
lending_numeric <- cbind(cleaned_data[,map_lgl(cleaned_data,is.numeric)])
corrplot(cor(lending_numeric,use = "complete.obs"),method = "number")

cleaned_data$bad_loan <- as.factor(cleaned_data$bad_loan)
processed_values <- preProcess(cleaned_data, method = c("center", "scale","medianImpute"))
data_transformed <- predict(processed_values, cleaned_data)

cleaned_data$bad_loan <- as.factor(cleaned_data$bad_loan)
cleaned_data$addr_state <- factor(cleaned_data$addr_state, levels = unique(cleaned_data$addr_state))

processed_values <- preProcess(cleaned_data, method = c("center", "scale","medianImpute"))
data_transformed <- predict(processed_values, cleaned_data)

#convert categorical values to dummy variables
cleaned_data_dum <- model.matrix(bad_loan ~ ., data = data_transformed)
cleaned_data_dum <- cbind(data.frame(bad_loan = data_transformed$bad_loan),
                          cleaned_data_dum[,-1]) #remove intercept

#prepare data for h2o package
#register cores for parellel precessing
h2o.init(nthreads = 7, #all cores
         max_mem_size = "8G")

h2o_data <- as.h2o(cleaned_data_dum)
splits <- h2o.splitFrame(data = h2o_data, 
                         ratios = c(0.7, 0.15),  #partition data into 70%, 15%, 15% chunks
                         seed = 20070831)  #setting a seed will guarantee reproducibility
h2o_train <- splits[[1]]
h2o_valid <- splits[[2]]
h2o_test <- splits[[3]]
nrow(h2o_train)
nrow(h2o_valid)
nrow(h2o_test)
y = "bad_loan"
x = setdiff(names(h2o_data), c(y, "int_rate"))


#set grid search parameters
nnet.params <- list(loss = c("Quadratic","CrossEntropy"), #loss function
                    rate = c(0.05,0.1,0.5), #learning rate
                    hidden = list(c(10,10),c(20,20))) #number of layers

# Train and validate a grid of GBMs
nnet.grid1 <- h2o.grid("deeplearning", x = x, y = y,
                      grid_id = "nnet.grid1",
                      training_frame = h2o_train,
                      validation_frame = h2o_valid,
                      seed = 20070831,
                      hyper_params = nnet.params)
