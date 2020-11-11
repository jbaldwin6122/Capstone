library(tidyverse)

scrape_transcript <- function(debate, participants, participantsRemove) {
  transcript <- debate %>% 
    html_nodes("div.field-docs-content") %>%
    html_text()
  
  transcript <- transcript %>%
    str_replace_all("[\n]", "")
  
  markers <- str_locate_all(transcript, pattern = participants)
  
  matches <- str_extract_all(transcript, participants)
  
  markers <- markers[[1]]
  matches <- matches[[1]]
  markersIndex <- markers[,1]
  n <- length(markersIndex)
  
  df <- data.frame(line = integer(n),
                   text = character(n),                      
                   speaker = character(n)
  )
  
  for (i in 1:(length(markersIndex)-1)) {
    df$line[i] <- i
    df$text[i] <- substr(transcript,markersIndex[i],markersIndex[i+1]-1)
    df$speaker[i] <- matches[i]
    
  }
  
  df$text <- df$text %>%
    str_remove_all(participantsRemove)
  
  return(df)
}
