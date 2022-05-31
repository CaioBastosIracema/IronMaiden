proibir_palavras<-function(palavras, data=NULL){
  `%>%`<-magrittr::`%>%`
  #This function adds terms on the data frame "stop_words"

  if(is.data.frame(data)){
    stop_words=data%>%rbind(
      list(c(as.character(palavras)),
           c(rep('added', length(palavras)))
           ))%>%unique()
  }else{
    stop_words=tidytext::get_stopwords()%>%rbind(
      list(c(as.character(palavras)),
           c(rep('added', length(palavras)))
      ))%>%unique()
  }
  stop_words
}


