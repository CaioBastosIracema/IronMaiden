#This script creates an wordcloud with the lyrics's terms of iron maiden's
# entire carrier

`%>%`<-magrittr::`%>%`

#importing the songs's dataset
iron_maiden=readr::read_csv('data/iron_maiden.csv')

#excluding some terms
stop_words <-proibir_palavras(c('oh','yow','x4','yeh','chorus','well','can',
                                'just','way','go', 'us','like', 'got', 'come',
                                'away','back','still', 'things','must','let',
                                'turn','gonna','get',"ain't",'s','til','till',
                                'set','put','make','one', 'coming','within',
                                'take','now','right','see','done','look',
                                'another','something','around','ever','upon',
                                'cause','left','goes','gotta'))


#creating the data frame with the terms to be showed in the plot
top_words <- iron_maiden %>%
  tidytext::unnest_tokens(term, Lyrics)%>%
  dplyr::count( term, sort = TRUE)%>%
  dplyr::anti_join(stop_words, by = c("term" = "word"))


#plotting the wordcloud

#jpeg(paste("imagens/wordcloud_all.jpeg",sep=""))

wordcloud::wordcloud(top_words$term, top_words$n,
                     min.freq=10,max.words=50,random.order=F,
                     colors = RColorBrewer::brewer.pal(8, "Dark2"))

#dev.off()

#wordcloud2::wordcloud2(top_words[,-2],backgroundColor = 'black')

top_words%>%head(20)%>%
ggplot2::ggplot(ggplot2::aes(x=reorder(term,dplyr::desc(n)), y= n), data=.)+
  ggplot2::geom_col()+ggplot2::xlab('Termos')+
  ggplot2::ylab('Ocorrência dos Termos')



