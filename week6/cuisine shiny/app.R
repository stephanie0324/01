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
cuisine<<-list("italian","southern_us","indian","mexican","chinese","french")
ui <- navbarPage(theme=shinytheme("flatly"),
                 "Worldwide Cuisine",
                 navbarMenu("前言",
                            tabPanel("介紹",includeMarkdown("RMDFILE.Rmd")),
                            tabPanel("資料來源",tags$p("資料來自:https://www.kaggle.com/umeshnarayanappa/recipes-tf-idf-and-bigrams/data"))),
                 navbarMenu("世界",
                            tabPanel("world",
                                     h1(fluidPage()))),
                 
                 navbarMenu("異國料理",
                             tabPanel("長條圖",
                                      h1(fluidPage(titlePanel("Barplot"),
                                                   sidebarLayout(sidebarPanel(selectInput("cuisine", 
                                                                                          "選擇國家",
                                                                                          choices=cuisine),
                                                                              hr()),
                                                                 mainPanel( plotOutput("cbarplot")))))),
                            tabPanel("文字雲",
                                     h1(fluidPage(titlePanel("Wordcloud"),
                                                  sidebarLayout(sidebarPanel(selectInput("cuisineName",
                                                                                         "選擇國家",
                                                                                         choices = cuisine),
                                                                             hr()),
                                                                mainPanel(plotOutput("plot")))))),
                            tabPanel("關係連結圖",
                                     h1(fluidPage(titlePanel("Treemap"),
                                                      sidebarLayout(sidebarPanel(selectInput("ing",
                                                                                             "選擇國家",
                                                                                             choices = cuisine)),
                                                                    mainPanel(type="tabs",
                                                                              tabsetPanel(tabPanel("數據",
                                                                                                   tableOutput("table")),
                                                                                          tabPanel("連結圖",
                                                                                                   plotOutput("treemap"))))))))) 
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
      filter(cuisine == input$cuisineName) %>%
      count(word,sort = TRUE) %>%
      ungroup()  %>%
      head(30)%>%
    with(wordcloud(word, n, max.words = 30,colors=brewer.pal(8, "Dark2")))
}
output$plot<-renderPlot(wcuisine(train,input$cuisineName))



#treemap
##bigram
count_bigrams <- function(dataset) 
{
  dataset %>%
    unnest_tokens(bigram, text, token = "ngrams", n = 2) %>%
    separate(bigram, c("word1", "word2"), sep = " ") %>%
    filter(!word1 %in% stop_words$word,
           !word2 %in% stop_words$word) %>%
    count(word1, word2, sort = TRUE)
}
visualize_bigrams <- function(bigrams) 
{
  set.seed(2016)
  a <- grid::arrow(type = "closed", length = unit(.15, "inches"))
  
  bigrams %>%
    graph_from_data_frame() %>%
    ggraph(layout = "fr") +
    geom_edge_link(aes(edge_alpha = n), show.legend = FALSE, arrow = a) +
    geom_node_point(color = "lightblue", size = 5) +
    geom_node_text(aes(label = name), vjust = 1, hjust = 1) +
    theme_void()
  
}

visualize_bigrams_individual <- function(bigrams) 
{
  set.seed(2016)
  a <- grid::arrow(type = "closed", length = unit(.15, "inches"))
  
  bigrams %>%
    graph_from_data_frame() %>%
    ggraph(layout = "fr") +
    geom_edge_link(aes(edge_alpha = n), show.legend = FALSE, arrow = a,end_cap = circle(.07, 'inches')) +
    geom_node_point(color = "lightblue", size = 5) +
    geom_node_text(aes(label = name), vjust = 1, hjust = 1) +
    theme_void()
}

trainWords<-function(train,cuisine)
{  
  train %>% 
  filter(cuisine == input$ing) %>%
  count_bigrams()
}

tmap<-function(train,cuisine)
{
  train %>% 
    filter(cuisine == input$ing) %>%
    count_bigrams()%>%
    filter(n > 200) %>%
    visualize_bigrams()
}  
    
output$table <- renderTable(trainWords(train,input$ing))
output$treemap<-renderPlot(tmap(train,input$ing))
}
# Run the application 
shinyApp(ui = ui, server = server)

