`%>%`<-magrittr::`%>%`

discografia=readr::read_csv('data-raw/iron_maiden.csv')
#Versos=readRDS('data-raw/Versos.rds')
stop_words <-proibir_palavras(c('oh','yow','x4','yeh','chorus','well','can',
                                'just','way','go', 'us','like', 'got', 'come',
                                'away','back','still', 'things','must','let',
                                'turn','gonna','get',"ain't",'s','til','till',
                                'set','put','make','one', 'coming','within',
                                'take','now','right','see','done','look',
                                'another','something','around','ever','upon',
                                'cause','left','goes','gotta','t','know','yeah',
                                '*','shall','may'))

discografia$Lyrics[21]=discografia$Lyrics[21]%>%
  stringr::str_replace_all('[Rr]un', '*')


discografia$Lyrics[99]=discografia$Lyrics[99]%>%stringr::str_replace_all('brave new world', 'brave new *')

discografia$Lyrics[77]=discografia$Lyrics[77]%>%stringr::str_replace_all('[Ff]ear of the dark', '* of the dark')

discografia$Lyrics[100]=discografia$Lyrics[100]%>%stringr::str_replace_all('blood brothers', '* brothers')

discografia$Lyrics[101]=discografia$Lyrics[101]%>%stringr::str_replace_all('Show them no fear', 'show them no *')

discografia$Lyrics[110]=discografia$Lyrics[110]%>%stringr::str_replace_all('Blood on the stones of the citadel', '* on the stones of the citadel')


discografia$Lyrics[131]=discografia$Lyrics[131]%>%
  stringr::str_replace_all('the night', 'the *')%>%
  stringr::str_replace_all('been tonight', 'been *')

discografia$Lyrics[133]=discografia$Lyrics[133]%>%
  stringr::str_replace_all('bloodof the dead', 'blood of the dead')%>%
  stringr::str_replace_all('of the dead', 'of the *')

discografia$Lyrics[134]=discografia$Lyrics[134]%>%
  stringr::str_replace_all('of night', 'of *')%>%
  stringr::str_replace_all('sun tonight', 'sun *')%>%
  stringr::str_replace_all('a life to live', '*')

discografia$Lyrics[138]=discografia$Lyrics[138]%>%
  stringr::str_replace_all('edge of the world', 'edge of the *')

discografia$Lyrics[144]=discografia$Lyrics[144]%>%
  stringr::str_replace_all('Death or glory', '* or glory')

discografia$Lyrics[145]=discografia$Lyrics[145]%>%
  stringr::str_replace_all('valley of death', 'valley of *')

#############correçao de texto##############

for(i in 1:length(discografia$Lyrics)){

  discografia$Lyrics[i]=corpus::text_tokens(discografia$Lyrics,
                                            stemmer = "en")%>%.[[i]]%>%
    paste(collapse =" ")
}


############# Termos mais frequentes por Album ########################

#iron maiden

iron_maiden<-termos_frequentes(discografia, 'Iron Maiden', stop_words)


wordcloud::wordcloud(iron_maiden$term, iron_maiden$n,
                     max.words=100,random.order=F,
                     colors = RColorBrewer::brewer.pal(8, "Dark2"))

#iron_maiden=iron_maiden%>%agrupa_padrao()

#killers

killers<-termos_frequentes(discografia, 'Killers', stop_words)


wordcloud::wordcloud(killers$term, killers$n,
                     max.words=100,random.order=F,
                     colors = RColorBrewer::brewer.pal(8, "Dark2"))

#killers=killers%>%agrupa_padrao()

#the number of the beast

the_number<-termos_frequentes(discografia, 'The Number of the Beast', stop_words)


wordcloud::wordcloud(the_number$term, the_number$n,
                     max.words=100,random.order=F,
                     colors = RColorBrewer::brewer.pal(8, "Dark2"))


the_number=the_number%>%agrupa_padrao()


#piece of mind

piece_of_mind<-termos_frequentes(discografia, 'Piece of Mind', stop_words)


wordcloud::wordcloud(piece_of_mind$term, piece_of_mind$n,
                     max.words=100,random.order=F,
                     colors = RColorBrewer::brewer.pal(8, "Dark2"))

piece_of_mind=piece_of_mind%>%agrupa_padrao()

#powerslave

powerslave<-termos_frequentes(discografia, 'Powerslave', stop_words)


wordcloud::wordcloud(powerslave$term, powerslave$n,
                     max.words=100,random.order=F,
                     colors = RColorBrewer::brewer.pal(8, "Dark2"))

powerslave=powerslave%>%agrupa_padrao()

#somewhere in Time

somewhere_in_time<-termos_frequentes(discografia, 'Somewhere in Time', stop_words)


wordcloud::wordcloud(somewhere_in_time$term, somewhere_in_time$n,
                     max.words=100,random.order=F,
                     colors = RColorBrewer::brewer.pal(8, "Dark2"))

somewhere_in_time<-somewhere_in_time%>%agrupa_padrao()

#seventh son of a seventh son

seventh_son<-termos_frequentes(discografia, 'Seventh Son of a Seventh Son', stop_words)


wordcloud::wordcloud(seventh_son$term, seventh_son$n,
                     max.words=100,random.order=F,
                     colors = RColorBrewer::brewer.pal(8, "Dark2"))

seventh_son<-seventh_son%>%agrupa_padrao()

#no prayer for the dying

no_prayer<-termos_frequentes(discografia, 'No Prayer for the Dying', stop_words)


wordcloud::wordcloud(no_prayer$term, no_prayer$n,
                     max.words=100,random.order=F,
                     colors = RColorBrewer::brewer.pal(8, "Dark2"))

no_prayer<-no_prayer%>%agrupa_padrao()

#fear of the dark

fear<-termos_frequentes(discografia, 'Fear of the Dark', stop_words)


wordcloud::wordcloud(fear$term, fear$n,
                     max.words=100,random.order=F,
                     colors = RColorBrewer::brewer.pal(8, "Dark2"))

fear<-fear%>%agrupa_padrao()

#the x factor

the_x_factor<-termos_frequentes(discografia, 'The X Factor', stop_words)


wordcloud::wordcloud(the_x_factor$term, the_x_factor$n,
                     max.words=100,random.order=F,
                     colors = RColorBrewer::brewer.pal(8, "Dark2"))

the_x_factor<-the_x_factor%>%agrupa_padrao()

#virtual xi

virtual_xi<-termos_frequentes(discografia, 'Virtual XI', stop_words)


wordcloud::wordcloud(virtual_xi$term, virtual_xi$n,
                     max.words=100,random.order=F,
                     colors = RColorBrewer::brewer.pal(8, "Dark2"))

virtual_xi<-virtual_xi%>%agrupa_padrao()

#brave new world

brave_new_world<-termos_frequentes(discografia, 'Brave New World', stop_words)


wordcloud::wordcloud(brave_new_world$term, brave_new_world$n,
                     max.words=100,random.order=F,
                     colors = RColorBrewer::brewer.pal(8, "Dark2"))

brave_new_world<-brave_new_world%>%agrupa_padrao()

#dance of the death

dance<-termos_frequentes(discografia, 'Dance of Death', stop_words)


wordcloud::wordcloud(dance$term, dance$n,
                     max.words=100,random.order=F,
                     colors = RColorBrewer::brewer.pal(8, "Dark2"))

dance<-dance%>%agrupa_padrao()

#a matter of life and death

a_matter<-termos_frequentes(discografia, 'A Matter of Life and Death', stop_words)


wordcloud::wordcloud(a_matter$term,  a_matter$n,
                     max.words=100,random.order=F,
                     colors = RColorBrewer::brewer.pal(8, "Dark2"))

a_matter<-a_matter%>%agrupa_padrao()

#the final frontier

the_final<-termos_frequentes(discografia, 'The Final Frontier', stop_words)


wordcloud::wordcloud(the_final$term, the_final$n,
                     max.words=100,random.order=F,
                     colors = RColorBrewer::brewer.pal(8, "Dark2"))

the_final<-the_final%>%agrupa_padrao()

#the book of souls

the_book<-termos_frequentes(discografia, 'The Book of Souls', stop_words)


wordcloud::wordcloud(the_book$term, the_book$n,
                     max.words=100,random.order=F,
                     colors = RColorBrewer::brewer.pal(8, "Dark2"))

the_book<-the_book%>%agrupa_padrao()

#senjutsu

senjutsu<-termos_frequentes(discografia, 'Senjutsu', stop_words)


wordcloud::wordcloud(senjutsu$term, senjutsu$n,
                     max.words=100,random.order=F,
                     colors = RColorBrewer::brewer.pal(8, "Dark2"))

senjutsu<-senjutsu%>%agrupa_padrao()



########################## Unindo as bases #############################


termos<-iron_maiden%>%dplyr::full_join(killers, by='term')%>%
  dplyr::full_join(the_number, by='term')%>%
  dplyr::full_join(piece_of_mind, by='term')%>%
  dplyr::full_join(powerslave, by='term')%>%
  dplyr::full_join(somewhere_in_time, by='term')%>%
  dplyr::full_join(seventh_son, by='term')%>%
  dplyr::full_join(no_prayer, by='term')%>%
  dplyr::full_join(fear, by='term')%>%
  dplyr::full_join(the_x_factor, by='term')%>%
  dplyr::full_join(virtual_xi, by='term')%>%
  dplyr::full_join(brave_new_world, by='term')%>%
  dplyr::full_join(dance, by='term')%>%
  dplyr::full_join(a_matter, by='term')%>%
  dplyr::full_join(the_final, by='term')%>%
  dplyr::full_join(the_book, by='term')%>%
  dplyr::full_join(senjutsu, by='term')

colnames(termos)<-c('termo', unique(discografia$Album))

termos[is.na(termos)] <- 0

termos2=data.table::transpose(termos,keep.names="rn")%>%
  janitor::row_to_names(row_number = 1)

colnames(termos2)[1]<-'Album'


freq_termos<-apply(termos2[,-1], 2,zero_vezes)
freq_termos<-freq_termos[freq_termos>=5]%>%names()


termos=subset(termos, termo%in%freq_termos)

termos2=termos2[ ,c('Album', freq_termos)]
#termos2[,c('call','tell','never','say','every')]<- list(NULL)
termos2[,c("i'm","i'v","you'r","you'v","they'r","they'v","we'r","we'v")]<- list(NULL)

tf_idf=tf_idf(termos2)

readr::write_csv(termos, file = "data-raw/Termos.csv")
readr::write_csv(tf_idf, file = "data-raw/tf_idf.csv")
readr::write_csv(discografia, file = "data-raw/iron_maiden2.csv")
