proibir_palavras<-function(palavras, data=NULL){
  `%>%`<-magrittr::`%>%`

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


permitir_palavras<-function(palavras, data=NULL){
  `%>%`<-magrittr::`%>%`
  `%notin%`<-Negate(`%in%`)

  if(is.data.frame(data)){
    stop_words=data%>%dplyr::filter(word%notin%palavras)
  }else{
    stop_words=tidytext::get_stopwords()%>%dplyr::filter(word%notin%palavras)
  }
  stop_words
}
