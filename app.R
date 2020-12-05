library(shiny)
library(tidyverse)
library(elo)
library(shinydashboard)
library(plotly)
library(lubridate)
library(tseries)
library(xts)
library(quantmod)
library(NLP)

create_vote_share_data <- function(){
    #TO-DO
    temp_df <- primary_results %>% filter(party == "Republican") %>%
        group_by(candidate, Date) %>%
        summarise(votes = sum(votes))
    
    return(temp_df)
}
create_lsd_timeseries_data <- function(){
    temp_df <- primaryLSD %>%
        group_by(doc_id,date,debate, state) %>%
        summarise(positive = sum(positive), negative = sum(negative))%>%
        inner_join(totalWordspD, by = c("doc_id" = "speaker", "date" = "date")) %>%
        rename(speaker = doc_id) %>%
        ungroup(date) %>%
        mutate(positive = positive/total * 100,
               negative = negative/total * 100,
               difference = positive - negative) %>%
        filter(speaker == "TRUMP" | speaker == "CRUZ" | speaker == "RUBIO" | speaker == "KASICH"| 
                   speaker == "CARSON" | speaker == "BUSH" | speaker == "PAUL" | speaker == "CHRISTIE" | 
                   speaker == "FIORINA" | speaker == "WALKER")
    return(temp_df)
}
create_scatter_data <- function(df, d){
    temp_df <- df %>%
        filter(debate == d)
    
    return(temp_df)
}
create_lsd_bargraph_data <- function(d){
    temp_df <- primaryLSD %>%
        select(doc_id, positive, negative, debate) %>%
        group_by(doc_id, debate) %>%
        summarise(positive = sum(positive), negative = sum(negative)) %>%
        inner_join(totalWordspD, by = c("doc_id" = "speaker")) %>%
        rename(speaker = doc_id) %>%
        mutate(positive = positive/total * 100,
               negative = negative/total * 100,
               difference = positive - negative) %>%
        gather(sentiment, percent, 3:4) %>%
        filter(speaker == "TRUMP" | speaker == "CRUZ" | speaker == "RUBIO" | speaker == "KASICH"| 
                   speaker == "CARSON" | speaker == "BUSH" | speaker == "PAUL" | speaker == "CHRISTIE" | 
                   speaker == "FIORINA" | speaker == "WALKER") %>%
        filter(debate == d)
    
    return(temp_df)
}
create_complexity_timeseries_data <- function(){
    temp_df1 <- primaryDebates %>% 
        unnest_tokens(sentence, text, token = "sentences") %>%
        mutate(id = row_number()) %>%
        unnest_tokens(word, sentence) %>%
        group_by(date,debate, id, speaker) %>%
        count() %>%
        ungroup(id) %>%
        summarize(temp_df1 = mean(n))
    
    temp_df2 <- primaryDebates %>%
        unnest_tokens(word, text) %>%
        mutate(
            length = nchar(word)
        ) %>%
        group_by(debate,date, speaker) %>%
        summarize(temp_df2 = mean(length))
    
    temp_df3 <- temp_df1 %>%
        inner_join(temp_df2, by = c("speaker" = "speaker", "date" = "date", "debate" = "debate")) %>%
        inner_join(totalWordspD, by = c("speaker" = "speaker", "date" = "date")) %>%
        filter(speaker == "TRUMP" | speaker == "CRUZ" | speaker == "RUBIO" | 
                   speaker == "KASICH"| speaker == "CARSON" | speaker == "BUSH" | 
                   speaker == "PAUL" | speaker == "CHRISTIE" | speaker == "FIORINA" | 
                   speaker == "WALKER")
        
    
    return(temp_df3)
}

lsd_timeseries_data <- create_lsd_timeseries_data()
complexity_timeseries_data <- create_complexity_timeseries_data()


ui <- dashboardPage(
    dashboardHeader(title = "Capstone Project"),
    
    dashboardSidebar(
        sidebarMenu(
            menuItem("Main",
                     tabName = "main_tab",
                     icon = icon("home", lib = "glyphicon")),
            menuItem("Sentiments",
                     tabName = "sentiment_tab",
                     icon = icon("menu-right", lib = "glyphicon")),
            menuItem("Topics",
                     tabName = "topic_tab",
                     icon = icon("menu-right", lib = "glyphicon")),
            menuItem("Complexity",
                     tabName = "complexity_tab",
                     icon = icon("menu-right", lib = "glyphicon")),
            menuItem("Part of Speech",
                     tabName = "part_of_speech_tab",
                     icon = icon("menu-right", lib = "glyphicon"))
        )
    ),
    
    dashboardBody(
        tabItems(
            tabItem(tabName = "main_tab",
                    box(plotlyOutput("vote_share"))
            ),
            tabItem(tabName = "sentiment_tab",
                    box(plotlyOutput("lsd_scatter_difference")),
                    box(plotlyOutput("lsd_bargraph")),
                    box(plotlyOutput("lsd_timeseries_positive")),
                    box(plotlyOutput("lsd_timeseries_negative")),
                    box(sliderInput(inputId = "debate_1",
                                    label = "Debate",
                                    min = 1, 
                                    max = 12,
                                    value = 1))),
            tabItem(tabName = "topic_tab"),
            tabItem(tabName = "complexity_tab",
                    box(plotlyOutput("complexity_scatter")),
                    box(plotlyOutput("word_length_timeseries")),
                    box(plotlyOutput("sentence_length_timeseries")),
                    box(sliderInput(inputId = "debate_2",
                                    label = "Debate",
                                    min = 1, 
                                    max = 12,
                                    value = 1))),
            tabItem(tabName = "part_of_speech_tab")
        )
    )
    
    
)

server <- function(input, output) {
    output$vote_share <- renderPlotly({
        vote_share_df <- create_vote_share_data()
        
        ggplot(data = vote_share_df, mapping = aes(x = Date, y = votes)) + 
            geom_col(mapping = aes(fill = candidate), position = "fill")
    })
    output$lsd_scatter_difference <- renderPlotly({
        lsd_scatter_df <- create_scatter_data(lsd_timeseries_data, input$debate_1)
        
       ggplot(data = lsd_scatter_df, mapping = aes(x = positive, y = negative, size = difference)) +
           geom_point(aes(color = difference, group = speaker)) + 
           scale_color_gradient2(limits = c(-2,4)) + 
           xlim(0, 7.5) + ylim(0,7.5)
    })
    output$lsd_bargraph <- renderPlotly({
        lsd_bargraph_df <- create_lsd_bargraph_data(input$debate_1) 
        
        ggplot(data = lsd_bargraph_df, mapping = aes(x = fct_reorder(speaker, percent), y = percent, fill =sentiment)) + 
                     geom_col(position = "dodge") + coord_flip() + labs(x = "speaker") + scale_fill_manual(values = c(red, blue)) + theme_bw()+
                     theme(
                         panel.grid.major=element_blank(),
                         panel.grid.minor=element_blank(),
                         panel.border=element_blank(),
                         plot.title=element_text(size = 16, face="bold"),
                         plot.title.position = "plot",
                         plot.subtitle=element_text(face="italic", size=12, margin=margin(b=12)),
                         plot.caption=element_text(size=8, margin=margin(t=12), color="#7a7d7e")
                     )
    })
    output$lsd_timeseries_positive <- renderPlotly({
        
        ggplot(data = lsd_timeseries_data) + geom_point(mapping = aes(x = date, y = positive, color = speaker), alpha = .7, size = 3) + 
            geom_line(mapping = aes(x = date, y = positive, color = speaker)) + 
            theme_fivethirtyeight() + labs() + ylim(0,7)
    })
    output$lsd_timeseries_negative <- renderPlotly({

        ggplot(data = lsd_timeseries_data) + geom_point(mapping = aes(x = date, y = negative, color = speaker), alpha = .7, size = 3) + 
            geom_line(mapping = aes(x = date, y = negative, color = speaker)) + 
            theme_fivethirtyeight() + labs() + ylim(0,7)
    })
    output$complexity_scatter <- renderPlotly({
        complexity_scatter_data <- create_scatter_data(complexity_timeseries_data, input$debate_2)
        
        ggplot(data = complexity_scatter_data) +
            geom_point(mapping = aes(x = temp_df1, y = temp_df2, color = speaker, size = total, alpha = .7)) +
            ylim(0,5) + xlim(0,30)
    })
    output$word_length_timeseries <- renderPlotly({
        ggplot(data = complexity_timeseries_data) +
            geom_point(mapping = aes(x = date,y = temp_df2,color = speaker), alpha = .7, size = 3) + 
            geom_line(mapping = aes(x = date, y = temp_df2, color = speaker)) + 
            theme_fivethirtyeight() + labs() + ylim(0,5)
    })
    output$sentence_length_timeseries <- renderPlotly({
        ggplot(data = complexity_timeseries_data) +
            geom_point(mapping = aes(x = date,y = temp_df1,color = speaker), alpha = .7, size = 3) + 
            geom_line(mapping = aes(x = date, y = temp_df1, color = speaker)) + 
            theme_fivethirtyeight() + labs() + ylim(0,30)
    })
    
    
}

shinyApp(ui = ui, server = server)
