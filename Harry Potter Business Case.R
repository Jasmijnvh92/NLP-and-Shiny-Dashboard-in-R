# Read necessary libraries
library(textreadr)
library(dplyr)
library(stringr)
library(tidytext)
library(tidyverse)
library(scales)
library(ggplot2)
library(widyr)
library(ggraph)
library(igraph)
library(quanteda)
library(quanteda.textmodels)
library(RColorBrewer)
library(harrypotter)

#Importing all .txt files from Harry Potter Survey # a txt works like a csv file with multiple rows
setwd("/Users/jasmijnvanhulsen/Desktop/Classes/Module B/Text Analytics/Harry Potter Survey")
nm <- list.files(path="/Users/jasmijnvanhulsen/Desktop/Classes/Module B/Text Analytics/Harry Potter Survey")

# Bind all the documents together in a DF and renaming columns 
my_txt_text <- as.data.frame(do.call(rbind, lapply(nm, function(x) paste(read_document(file=x))))) %>%
  rename("House" = V1,
         "Patronus" = V2,
         "Animal" = V3,
         "Create" = V4,
         "Boggart" = V5,
         "Hallow" = V6,
         "Success" = V7)

# Create df with only yes answers
success_df <- my_txt_text[c(2, 3, 4, 6, 7, 8, 14, 17, 20, 21), ]

# Call new column yes (and move to front)
success_yes <- success_df %>%
  cbind(success="yes", success_df)

# Create df with only no answers
no_success_df <- my_txt_text[c(1, 5, 9, 10, 11, 12, 13, 15, 16, 17, 18, 19), ]

# Call new column no (and move to front)
success_no <- no_success_df %>%
  cbind(success="no",no_success_df)

# Bind all rows together
survey <- rbind(success_yes, success_no)

# Delete the first version of questions
survey <-  survey[ -c(1:7, 15) ]

# Gather questions in new location information and call the questions text
survey <- survey %>%
  gather("question","text",2:7)

#-----------------------------------------------------------------------------#
#                            Tokanization                                     #
#-----------------------------------------------------------------------------#
# Creating customized stopwords 
my_stopwords <- tribble(
  ~word,   ~lexicon,
  "house",  "CUSTOM",
  "colors", "CUSTOM",
  "color",  "CUSTOM",
  "meaning", "CUSTOM",
  "patronus", "CUSTOM",
  "im", "CUSTOM")

# Adding the cusomized to original stopwords 
stop_words <- stop_words %>%
  bind_rows(my_stopwords)

# Create token list by seperating all words: TOTAL
token_list_all <- survey %>%
  unnest_tokens(word, text) %>%
  anti_join(stop_words) %>%
  count(success, question, word, sort=TRUE)

# Frequent tokens
frequencies_tokens <- survey %>%
  unnest_tokens(word, text) %>%
  anti_join(stop_words) %>% 
  count(success, word, sort=TRUE)

# Creating graph with most frequent words 
freq_hist <- frequencies_tokens %>%
  mutate(word=reorder(word, n)) %>%
  filter(success == "no") %>%
  filter(n > 3) %>% 
  ggplot(aes(word, n, fill = "Harry Potter"))+
  geom_col(show.legend = FALSE)+
  xlab(NULL)+
  coord_flip() +
  scale_fill_hp(discrete = TRUE, option = "HarryPotter")
print(freq_hist)

#-----------------------------------------------------------------------------#
#                             TF-IDF                                          #
#-----------------------------------------------------------------------------#

# Create total words per article 
total_words <- token_list_all %>%
  group_by(question) %>%
  summarize(total=sum(n))

# Join ai_tidy with total_words
Harry_words <- left_join(token_list_all, total_words)

# Bind TF IDF
Harry_words <- Harry_words %>%
  bind_tf_idf(word, question, n)

Harry_words %>%
  arrange(desc(tf_idf))

# Graphing most important words 
Harry_words %>%
  arrange(desc(tf_idf)) %>%
  mutate(word=factor(word, levels=rev(unique(word)))) %>%
  group_by(question) %>%
  filter(success == "no") %>%
  top_n(3) %>%
  ggplot(aes(word, tf_idf, fill=question))+
  geom_col(show.legend=FALSE)+
  scale_fill_hp_d(option = "Gryffindor")+
  labs(x=NULL, y="tf-idf")+
  facet_wrap(~question, ncol=2, scales="free")+
  coord_flip()

#-----------------------------------------------------------------------------#
#                              Sentiment                                      #
#-----------------------------------------------------------------------------#

# Afinn sentiment analysis _TOTAL
hp_sentiments <- token_list_all %>%
  inner_join(get_sentiments("afinn"), by = "word") %>%
  group_by(question) %>%
  filter(success == "no") %>%
  summarize(score = sum(value * n) / sum(n))

# Graph Afinn
hp_sentiments %>%
  mutate(question = reorder(question, score)) %>%
  ggplot(aes(question, score, fill = score > 0)) +
  geom_col(show.legend = FALSE) +
  scale_fill_hp_d(option = "Ravenclaw")+
  coord_flip() +
  ylab("Average sentiment score")

#-----------------------------------------------------------------------------#
#                             Correlations                                    #
#-----------------------------------------------------------------------------#

my_text_df <- survey %>%
  mutate(section = row_number()) %>%
  unnest_tokens(word, text) %>%
  filter(!word %in% stop_words$word)
my_text_df

#taking out the least common words
word_cors <- my_text_df %>%
  group_by(word) %>%
  filter(n() >= 5) %>%
  pairwise_cor(word, section, sort=TRUE) 
#pairwise_cor() check correlation based on how ofter words appear in the same section

#Create different one for following 2nd plots 
compare_cors <- word_cors

# Checking any words' correlation with another one (filtering)

## Creating a correlation network
word_cors %>%
  filter(correlation > 0.3) %>%
  graph_from_data_frame() %>%
  ggraph(layout = "fr")+
  geom_edge_link(aes(edge_alpha = correlation), show.legend=F)+
  geom_node_point(color = "darkred", size=2)+
  geom_node_text(aes(label=name), repel=T)+
  theme_void()

## Creating a barcharts for correlation
## Pick particular interesting words and find the other words most associated with them

compare_cors %>%
  filter(item1 %in% c("adventurous", "people", "purple", "white")) %>%
  group_by(item1) %>%
  top_n(5) %>%
  mutate(item2 = reorder(item2, correlation)) %>%
  ggplot(aes(item2, correlation, fill = "Slytherin", show.legend = FALSE)) +
  geom_col(show.legend = FALSE) +
  scale_fill_hp_d(begin = 1, option = "Slytherin")+
  facet_wrap(~ item1, scales = "free") +
  coord_flip()

#-----------------------------------------------------------------------------#
#                             Naive Bayes                                     #
#-----------------------------------------------------------------------------#

#Importing all .txt files from Harry Potter Survey # a txt works like a csv file with multiple rows
nm_2 <- list.files(path="/Users/jasmijnvanhulsen/Desktop/Classes/Module B/Text Analytics/Harry Potter Survey")

# Bind all the documents together in DF
my_corp <- do.call(rbind, lapply(nm_2, function(x) paste(read_document(file=x), collapse = " ")))

# Create a corpus of original my_txt_text
msg.dfm <- dfm(corpus(my_corp), tolower = TRUE)
msg.dfm <- dfm_trim(msg.dfm, min_termfreq = 2, min_docfreq = 1)
msg.dfm <- dfm_weight(msg.dfm) 

head(msg.dfm)

# Splitting into training and testing data (80/20 split)
msg.dfm.train<-msg.dfm[1:17,]
msg.dfm.test<-msg.dfm[18:21,] 

# Building the Naive Bayes model:
NB_classifier <- textmodel_nb(msg.dfm.train, c(0,1,1,1,0,1,1,1,0,0,0,0,0,1,0,0,1),
                              prior = "docfreq")
NB_classifier
summary(NB_classifier)

# Predicting the testing data
pred <- predict(NB_classifier, msg.dfm.test)
pred

# Predicts 2x No, 2x Yes  (which is 100% correct)


# Turning prediction into table 
my_nb_table <- c("Survey 18", "Survey 19",
                 "Survey 20", "Survey 21")

# Turning table into DF
my_nb_table <- as.data.frame(my_nb_table)

# Adding title ID to surveys 
my_nb_table$Survey_ID <- my_nb_table$my_nb_table

# Creating prediction and actual 
my_prediction <- c("No", "No", "Yes", "Yes")
my_actual <- c("No", "No", "Yes", "Yes")  

# Changing names 
my_nb_table$Actual <- my_actual
my_nb_table$Prediction <- my_prediction

# Creating final Table 
final_table <- subset(my_nb_table, select = -c(my_nb_table))

#-----------------------------------------------------------------------------#
#                                 UI                                          #
#-----------------------------------------------------------------------------#
library(shiny)
library(shinydashboard)

#-----------------------------------------------------------------------------#
#                           Header & Sidebar                                  #
#-----------------------------------------------------------------------------#

# Creating a Header + Title
header <- dashboardHeader(title = "Harry Potter")


# Creating a Sidebar menu
sidebar <- dashboardSidebar(
  sidebarMenu(menuItem("Introduction", tabName = "Introduction"),
              menuItem("TF-IDF", tabName = "TF-IDF"),
              menuItem("Sentiments", tabName = "Sentiments"),
              menuItem("Correlation", tabName = "Correlation"),
              menuItem("Conclusion", tabName = "Conclusion")
              #tags$head(tags$style(hp(n = 8, house = "Gryffindor"))) #color CHANGE!!! 
  )
)

#-----------------------------------------------------------------------------#
#                                 Body                                        #
#-----------------------------------------------------------------------------#

# Creating body 
body <- dashboardBody(
  
  tabItems(
    
#-----------------------------------------------------------------------------#
    
    # Menu Item: Introduction 
    tabItem(tabName = "Introduction",
            tabBox(width = 12, 
              tabPanel("Ministry of Magic Survey",
                       h4(strong("Are you a Witch / Wizzard or are you not?"), align = "center"),
                       p("With the increase of magic in the Muggle media, it is harder and harder to identify 
                       Wizzards and Witches vs Muggles.
                             How can the Ministry of Magic identify Witches and Wizzards vs Muggles?"),
                       br(),
                       h6("Wizzard tracking team 6 - Doston Yunusmatov, Harish Mallavarapu,
                           Jasmijn van Hulsen, Mosiuwa Tshabalala & Srinitha Chowdary Dandamudi"), 
                       
                       imageOutput("team")
                       
                       ), # Closing tabPanel 1.1
              
              tabPanel("Questions", 
                       
                       br(),
                       
                       p("1. What is your Hogwarts House and what are its characteristics?"), 
                       p("2. What is the meaning behind your Patronus Charm?"),
                       p("3. What animal would take to Hogwarts and why?"),
                       p("4. If you could start a new Hogwarts House, what would you name 
                         it and what whould be its colors and characteristics?"),
                       p("5. What does your Boggart show before and after you have cast the Riddikulus charm?"),
                       p("6. Which Deathly hallow would you want and why?"),
                       strong("7. Are you a Witch/Wizzard?"), 
                       br(), 
                       imageOutput("hp") )  # tabPanel 1.2
            ) # tabBox 1
    ), # tabItem 1
    
#-----------------------------------------------------------------------------#
    
    # Menu Item: TF-IDF
    tabItem(tabName = "TF-IDF",
            tabBox(width = 12, 
              tabPanel("Token Frequency",
                       
                       #Input 
                       column(3, 
                              selectInput(inputId = "success1", 
                                          label = "Are you a Witch/Wizzard?",
                                          choices = unique(frequencies_tokens$success))),
                       column(4, offset = 1,        
                              sliderInput(inputId = "frequencies_token", 
                                          label = "Select minimum number of count", 
                                          value = 3,
                                          min = 2,
                                          max = 8)),
                       
                       # Output 
                       plotOutput("my_freq_hist")    
              ), # tabPanel 2.1
              
              tabPanel("TF-IDF", 
                       
                       #Input 
                       column(3,
                              selectInput(inputId = "success2", 
                                          label = "Are you a Witch/Wizzard?",
                                          choices = unique(Harry_words$success))),
                       
                       column(4, offset = 1,
                              sliderInput(inputId = "Harry_word", 
                                          label = "Select max number of words", 
                                          value = 3,
                                          min = 1,
                                          max = 7)),
                       
                       # Output 
                       plotOutput("tf_idf_plot")
              ) # tabPanel 2.2
            ) # tabBox 2
    ), # tabItem 2
    
#-----------------------------------------------------------------------------#
    
    # Menu Item: Sentiment
    tabItem(tabName = "Sentiments",
            tabBox(width = 12, 
              tabPanel("AFINN",
                       
                       #Input 
                       selectInput(inputId = "success3", 
                                   label = "Are you a Witch/Wizzard?",
                                   choices = unique(token_list_all$success)),
                       
                       # Output 
                       plotOutput("hp_sentiment")    
              ) # tabPanel 3.1
            ) # tabBox 3
    ), # tabItem 3
    
#-----------------------------------------------------------------------------#
    
    # Menu Item: Correlations
    tabItem(tabName = "Correlation",
            tabBox(width = 12, 
              tabPanel("Networks",
                       
                       #Input 
                       sliderInput(inputId = "min_corr", 
                                   label = "Select minimum correlation", 
                                   value = 0.3,
                                   min = 0.1,
                                   max = 1.0),
                       
                       # Output 
                       plotOutput("network_corr")    
              ), # tabPanel 4.1
              
              tabPanel("Correlated words", 
                       column(3,
                              #Input 
                              sliderInput(inputId = "max_results", 
                                          label = "Select maximum results", 
                                          value = 5,
                                          min = 1,
                                          max = 10)),
                       
                       column(4, offset = 1, 
                              textInput(inputId = "var1", 
                                        label = "Choose first word to visualize", 
                                        value = "invisibility"),
                              
                              textInput(inputId = "var2", 
                                        label = "Choose second word to visualize", 
                                        value = "cat")),
                       
                       column(4, 
                              textInput(inputId = "var3", 
                                        label = "Choose third word to visualize", 
                                        value = "dog"),
                              
                              textInput(inputId = "var4", 
                                        label = "Choose fourth word to visualize", 
                                        value = "owl")),
                       
                       # Output 
                       plotOutput("compare_corr")
              ) # tabPanel 4.2
            ) # tabBox 4
    ), # tabItem 4
    
#-----------------------------------------------------------------------------#

    # Menu Item: Prediction
    tabItem(tabName = "Conclusion",
            tabBox(width = 12,  
                   tabPanel("Prediction",
                        
                        #Input 
                        h4(strong("Conclusion"), align = "center"),
                        p("We have seen that with the help of the questions, we 
                              can already find trends between Witches / Wizzards and 
                              Muggles. Witches and Wizzards can in a way correctly 
                              answer the questions, whereas Muggles will just give 
                              an random answer that they feel might be correct."),
                        br(),
                        
                        # Output 
                        column(4, 
                               tableOutput("pred_table")),
                        
                        br(),br(),br(),br(),br(),br(),br(),br(),br(),
                        
                        #Input 
                        p("We have taken the last 4 serveys and see if based on
                              the answers we could correctly predict and identify 
                              Witches and Wizzards vs Muggles. The prediction was 100%
                              correct (as can be seen in the table above). This means
                              that the questions asked result in answers that will 
                              correctly identify magical folks from non-magical ones.") 
                        
                        ), # tabPanel 5.1
               
               tabPanel("Questions", 
                        
                        br(), 
                        
                        p("1. What is your Hogwarts House and what are its characteristics?"), 
                        p("2. What is the meaning behind your Patronus Charm?"),
                        p("3. What animal would take to Hogwarts and why?"),
                        p("4. If you could start a new Hogwarts House, what would you name
                              it and what whould be its colors and characteristics?"),
                        p("5. What does your Boggart show before and after you have cast 
                              the Riddikulus charm?"),
                        p("6. Which Deathly hallow would you want and why?"),
                        strong("7. Are you a Witch/Wizzard?") , 
                        br(), 
                        
                        imageOutput("hp2")
                        
                        )  # tabPanel 5.2
                ) # tabBox 5
    ) # tabItem 5
  ) # tabItems 
) # dashboardBody 

#-----------------------------------------------------------------------------#
#                           Calling UI                                        #
#-----------------------------------------------------------------------------#
ui <- dashboardPage(header, sidebar, body)

#-----------------------------------------------------------------------------#
#                               Server                                        #
#-----------------------------------------------------------------------------#

server <- function(input, output, session) ({
  
  # Team Image
  output$team <- renderImage({
    filename <- normalizePath(file.path('/Users/jasmijnvanhulsen/Desktop/Team.png'))
    
    # Return a list containing the filename and alt text
    list(src = filename,
         width = 900,
         height = 180,
         align = "center",
         alt = "This should be a nice picture of the team")
  }, deleteFile = FALSE)
  
  # Harry Potter 
  output$hp <- renderImage({
    filename2 <- normalizePath(file.path('/Users/jasmijnvanhulsen/Desktop/Hp.png'))
    
    # Return a list containing the filename and alt text
    list(src = filename2,
         width = 400,
         hight = 90,
         align = "center",
         alt = "This should be a logo")
  }, deleteFile = FALSE)
  
#-----------------------------------------------------------------------------#
  
  # Token Frequency  
  my_token_freq <- reactive({
    frequencies_tokens %>%
      mutate(word=reorder(word, n)) %>%
      filter(success == input$success1) %>%
      top_n(as.numeric(input$frequencies_token))
  }) #Closing of the Frequency of Tokens
  
  # PLOT the Frequency of Tokens
  output$my_freq_hist <- renderPlot({
    my_token_freq() %>%
      ggplot(aes(word, n, fill = "Harry Potter"))+
      geom_col(show.legend = FALSE)+
      xlab(NULL)+
      coord_flip() +
      scale_fill_hp(discrete = TRUE, option = "HarryPotter")
  }) #Closing of the Freq_Hist Plot
  
#-----------------------------------------------------------------------------#
  
  # TF -IDF 
  name_tdidf <- reactive({
    Harry_words %>%
      arrange(desc(tf_idf)) %>%
      mutate(word=factor(word, levels=rev(unique(word)))) %>%
      group_by(question) %>%
      filter(success == input$success2) %>%
      top_n(as.numeric(input$Harry_word))
  }) #Closing of tf-idf output
  
  #PLOT the result above (tf-idf)
  output$tf_idf_plot <- renderPlot({
    name_tdidf() %>%
      ggplot(aes(word, tf_idf, fill=question))+
      geom_col(show.legend=FALSE)+
      scale_fill_hp_d(option = "Gryffindor")+
      labs(x=NULL, y="tf-idf")+
      facet_wrap(~question, ncol=2, scales="free")+
      coord_flip()
  }) # Closing of TD_IDF Plot
  
#-----------------------------------------------------------------------------#  
  
  # Sentiment
  my_sentiment <- reactive({
    token_list_all %>%
      inner_join(get_sentiments("afinn"), by = "word") %>%
      group_by(question) %>%
      filter(success == input$success3) %>%
      summarize(score = sum(value * n) / sum(n)) %>%
      mutate(question = reorder(question, score))
  }) #Closing of Afinn Output
  
  # PLOT the result above (sentiment)
  output$hp_sentiment <- renderPlot({
    my_sentiment() %>%
      ggplot(aes(question, score, fill = score > 0)) +
      geom_col(show.legend = FALSE) +
      scale_fill_hp_d(option = "Ravenclaw")+
      coord_flip() +
      ylab("Average sentiment score")
  }) #Closing of Sentiment Plot
  
#-----------------------------------------------------------------------------#
  
  # Correlation Network
  name_network_corr <- reactive({
    word_cors %>%
      filter(correlation >= input$min_corr) 
  }) #Closing of Correlation network output
  
  # PLOT the result above (correlation network)
  output$network_corr <- renderPlot({
    name_network_corr() %>%
      graph_from_data_frame() %>%
      ggraph(layout = "fr")+
      geom_edge_link(aes(edge_alpha = correlation), show.legend=F)+
      geom_node_point(color = "darkred", size=2)+
      geom_node_text(aes(label=name), repel=T)+
      theme_void()
  }) # Closing of Correlation Nettwork plot
  
#-----------------------------------------------------------------------------#
  
  # Correlation Plot
  name_compare_corr <- reactive({
    compare_cors %>%
      filter(item1 %in% c(input$var1, input$var2, input$var3, input$var4)) %>%
      group_by(item1) %>%
      top_n(as.numeric(input$max_results)) %>%
      mutate(item2 = reorder(item2, correlation)) 
  }) #Closing of Correlation plot output
  
  # PLOT the result above (correlation plot)
  output$compare_corr <- renderPlot({
    name_compare_corr() %>%
      ggplot(aes(item2, correlation, fill = "Slytherin", show.legend = FALSE)) +
      geom_col(show.legend = FALSE) +
      scale_fill_hp_d(begin = 1, option = "Slytherin")+
      facet_wrap(~ item1, scales = "free") +
      coord_flip()
  }) # Closing of Correlation Plot
  
#-----------------------------------------------------------------------------#
  
  # Naive Bayes
  output$pred_table <- renderTable(final_table)
  
  # Harry Potter 
  output$hp2 <- renderImage({
    filename3 <- normalizePath(file.path('/Users/jasmijnvanhulsen/Desktop/Hp.png'))
    
    # Return a list containing the filename and alt text
    list(src = filename3,
         width = 400,
         hight = 90,
         align = "center",
         alt = "This should be a logo")
  }, deleteFile = FALSE)
  
}) # Closing Server

#-----------------------------------------------------------------------------#
#                               Shiny                                         #
#-----------------------------------------------------------------------------#

shiny::shinyApp(ui, server)
