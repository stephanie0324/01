library(readxl)
library(tidyverse)
library(ggplot2)
library(highcharter)
library(plotly)
library(stringr)
library(viridis)
library(gridExtra)
library(tidyverse)
library(highcharter)
library(plotly)
library(htmltools)
library(purrr)
library(rjson)
library(jsonlite)
library(tidytext)
library(ggraph)
library(igraph)


data <- fromJSON("../data/train.json")
data <- as.data.frame(data)
head(flatten(data))
train <- fromJSON("../data/train.json", flatten = TRUE)
test <- fromJSON("../data/test.json", flatten = TRUE)

ingredientscombine <- function(s)
{
  a <- unlist(s)
       return(paste0(a, collapse = '',sep=' '))
}
train$ingredients <- sapply(train$ingredients,ingredientscombine)
train <- train %>%rename(text = ingredients)
test$ingredients <- sapply(test$ingredients,ingredientscombine)

test <- test %>%rename(text = ingredients)

wordcloudcuisine= function(train,cuisineName)
{   train %>%
    unnest_tokens(word, text) %>%
    filter(!word %in% stop_words$word) %>%
    filter(!word %in% most_common_words$word) %>%
    filter(cuisine == cuisineName) %>%
    count(word,sort = TRUE) %>%
    ungroup()  %>%
    head(30) 
  with(wordcloud(word, n, max.words = 30,colors=brewer.pal(8, "Dark2")))
}
wordcloudcuisine(train,"italian")

