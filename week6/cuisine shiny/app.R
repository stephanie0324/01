#
# This is a Shiny web application. You can run the application by clicking
# the 'Run App' button above.
#
# Find out more about building applications with Shiny here:
#
#    http://shiny.rstudio.com/
#

library(shiny)
library(wordcloud)
library(RColorBrewer)
library(ggplot2)
library(shinythemes)
# Define UI for application that draws a histogram
cuisine<<-list("italian","southern_us","mexican","chinese","french")
ui <- navbarPage(theme=shinytheme("cosmo"),"Worldwide Cuisine",tabPanel("前言"),
                 navbarMenu("世界",tabPanel("world",h1(fluidPage()))),
                 navbarMenu("異國料理",tabPanel("長條圖",h1(fluidPage(titlePanel("Barplot"),
                                                                       sidebarLayout(sidebarPanel(selectInput("cuisine", "選擇國家",choices=cuisine),
                                                                                     hr()),
                                                                                     mainPanel( plotOutput("cbarplot")))))),
                                       tabPanel("文字雲",h1(fluidPage(titlePanel("Word Cloud"),
                                                                       sidebarLayout(sidebarPanel(selectInput("cuisine", "選擇國家", choices = cuisine),
                                                                                     actionButton("update","Change"),
                                                                                     hr(),
                                                                                     sliderInput("freq","Minimum Frequency:",min = 1,  max = 50, value = 15),
                                                                                     sliderInput("max","Maximum Number of Words:",min = 1,  max = 300,  value = 100)),
                                                                                     mainPanel(plotOutput("wordcloudcuisine")))))))
                                                                    

                 )
# Define server logic required to draw a histogram
server <- function(input, output) {
  library(jsonlite)
  library(tidyverse)
  library(tidytext)
  library(scales)
  library(stringr)
  library(wordcloud)
  library(treemap)
  library(text2vec)
  library(glmnet)
  library(igraph)
  library(ggraph)
  library(knitr) 
  fillColor = "#FFA07A"
  fillColor2 = "#F1C40F"
  fillColorBlue = "#AED6F1"
  train <- fromJSON("../data/train.json", flatten = TRUE)
  test <- fromJSON("../data/test.json", flatten = TRUE)
  train2<-train
#prepare ingredients  
ingredientscombine <- function(s)
  {
    a <- unlist(s)
    return(paste0(a, collapse = '',sep=' '))
  }
  
  train$ingredients <- sapply(train$ingredients,ingredientscombine)
  train <- train %>%
    rename(text = ingredients)
  test$ingredients <- sapply(test$ingredients,ingredientscombine)
  test <- test %>%
    rename(text = ingredients)
#barplot
most_common_words <- train %>%
    unnest_tokens(word, text) %>%
    filter(!word %in% stop_words$word) %>%
    count(word,sort = TRUE) %>%
    ungroup() %>%
    mutate(word = factor(word, levels = rev(unique(word)))) %>%
    head(20)
  
most_common_ingredients <- train2 %>% 
    mutate(ingredients = str_split(ingredients, pattern = ",")) %>% 
    unnest(ingredients) %>% 
    mutate(ingredients = gsub(ingredients, pattern = 'c\\(', replacement = "")) %>%
    mutate(ingredients = gsub(ingredients, pattern = '"', replacement = "")) %>%
    mutate(ingredients = trimws(ingredients)) %>%
    group_by(ingredients) %>%
    summarise(Count = n()) %>%
    arrange(desc(Count)) %>%
    ungroup() %>%
    head(10)
bcuisine= function(train,cuisine)
{
  train %>% 
  filter(cuisine == input$cuisine) %>%
  mutate(ingredients = str_split(ingredients, pattern = ",")) %>% 
  unnest(ingredients) %>% 
  mutate(ingredients = gsub(ingredients, pattern = 'c\\(', replacement = "")) %>%
  mutate(ingredients = gsub(ingredients, pattern = '"', replacement = "")) %>%
  mutate(ingredients = trimws(ingredients)) %>%
  filter(!ingredients %in% most_common_ingredients$ingredients) %>%
  group_by(ingredients) %>%
  summarise(Count = n()) %>%
  arrange(desc(Count)) %>%
  ungroup() %>%
  mutate(ingredients = reorder(ingredients,Count)) %>%
  head(10)%>%
  ggplot(aes(x = ingredients,y = Count)) +
    geom_bar(stat='identity',fill= fillColor2) +
    geom_text(aes(x = ingredients, y = .01, label = paste0("( ",Count," )",sep="")),
              hjust=0, vjust=.5, size = 4, colour = 'black',
              fontface = 'bold') +
    labs(x = 'ingredients', 
         y = 'Count', 
         main=input$cuisine) +
    coord_flip() +
    theme_bw()
}
output$cbarplot <- renderPlot(bcuisine(train2,input$cuisine))

#wordcloud
wcuisine= function(train,cuisine)
{  train %>%
      unnest_tokens(word, text) %>%
      filter(!word %in% stop_words$word) %>%
      filter(!word %in% most_common_words$word) %>%
      filter(cuisine == input$cuisine) %>%
      count(word,sort = TRUE) %>%
      ungroup()  %>%
      head(30)%>% 
  with(wordcloud(word, n, max.words = 50,colors=brewer.pal(8, "Dark2")))
}
output$wordcloudcuisine<-renderPlot(wcuisine(train,input$cuisine))
}

# Run the application 
shinyApp(ui = ui, server = server)

