termos_frequentes<- function(Data, Disco, stop_words=tidytext::get_stopwords()
                             ){

  `%>%`<-magrittr::`%>%`
  top_words <- Data %>%subset(Album==Disco)%>%
    tidytext::unnest_tokens(term, Lyrics)%>%
    dplyr::count( term, sort = TRUE)%>%
    dplyr::anti_join(stop_words, by = c("term" = "word"))


  top_words <- Data %>%subset(Album==Disco)%>%
    dplyr::group_by(Song)%>%
    tidytext::unnest_tokens(term, Lyrics)%>%
    dplyr::count( term, sort = TRUE)%>%
    dplyr::anti_join(stop_words, by = c("term" = "word"))%>%dplyr::ungroup()%>%
    dplyr::group_by(term)%>%
    dplyr::summarise(n_songs=dplyr::n())%>%
    dplyr::inner_join(top_words, by = 'term')%>%
    dplyr::ungroup()%>%dplyr::arrange(desc(n))%>%
    subset(n>quantile(n,0.5) & n_songs>1)%>%.[,-2]

   top_words
}

agrupar_termos<-function(Album, palavras, substituir){
  `%>%`<-magrittr::`%>%`

  Album[Album$term%in%palavras,'term']<-substituir

  x=Album%>%dplyr::group_by(term)%>%dplyr::summarise(n=sum(n))%>%
    dplyr::ungroup()%>%dplyr::arrange(desc(n))
  x
}

agrupa_padrao<-function(Album){
  `%>%`<-magrittr::`%>%`
  agrupamento=Album%>%
    agrupar_termos(c('live','lives','lived','lifes','living','alive'), 'life')%>%
    agrupar_termos('man', 'men')%>%
    agrupar_termos('days', 'day')%>%
    agrupar_termos(c('die','died','dead','dying','dies'), 'death')%>%
    agrupar_termos(c('darkest','darkness'), 'dark')%>%
    agrupar_termos(c('beginning','begins'), 'begin')%>%
    agrupar_termos('humanity', 'human')%>%
    agrupar_termos('goddess', 'god')%>%
    agrupar_termos('tonight', 'night')%>%agrupar_termos('lie', 'lies')%>%
    agrupar_termos('shadows','shadow')%>%
    agrupar_termos('dreams', 'dream')%>%agrupar_termos('feeling', 'feel')%>%
    agrupar_termos('hand', 'hands')%>%agrupar_termos('falling', 'fall')%>%
    agrupar_termos("someone's", 'someone')%>%
    agrupar_termos('burning', 'burn')%>%
    agrupar_termos('running', 'run')%>%
    agrupar_termos('stranger', 'strange')%>%
    agrupar_termos('seventh', 'seven')%>%
    agrupar_termos('true', 'truth')%>%agrupar_termos('eye', 'eyes')%>%
    agrupar_termos('fighting', 'fight')%>%
    agrupar_termos('silent', 'silence')%>%
    agrupar_termos(c('afraid','fright','frighted','horror','horrored','panic','despair'), 'fear')

  agrupamento
}

zero_vezes<-function(termo){
  `%>%`<-magrittr::`%>%`

  zeros=ifelse(as.numeric(termo)==0, 1, 0)%>%sum()
  zeros
}



