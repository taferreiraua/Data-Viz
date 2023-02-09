library(tidyverse)
library(rvest)


get_personality<-function(url){
  html = url%>%read_html()
  
  character = html%>%
    html_elements("h3")%>%
    head(1)%>%
    html_text()
  
  data= html%>%
    html_elements("table.zui-table")%>%
    html_table()%>%
    .[[1]]
  
  names(data)=c("item","avg_rating","rank","rating_sd","number_ratings")
  data$character = str_replace(character," Descriptive Personality Statistics","")
  
  data
}

base_url<-'https://openpsychometrics.org/tests/characters/stats/SW/'

starwars<-data.frame()

for(i in 1:6){
  url<-paste0(base_url, i)
  temp_data<-get_personality(url)
  starwars<-rbind(starwars,temp_data)
}

write.table(starwars, file = "starwars.csv", sep = ",", na = "", quote = T, row.names = F)
