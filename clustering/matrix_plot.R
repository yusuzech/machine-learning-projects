library(stringr)
temp <- matrix(rnorm(10000),nrow = 100)

image(temp, ncol=100, nrow=100)

df <- as.data.frame(temp) %>% 
    mutate(x = row_number()) %>% 
    select(x,everything())

colnames(df) <- if_else(str_detect(colnames(df),pattern = "x"),colnames(df),str_sub(colnames(df),start = 2L))

transformed <- df %>%
    gather(key = "y", value = "value",2:ncol(df)) %>%
    mutate(x = as.numeric(x),
           y = as.numeric(y))

ggplot(transformed) +
    geom_raster(aes(x = x, y = y, fill = value))

