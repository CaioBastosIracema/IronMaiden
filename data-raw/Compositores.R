#### CLusters ######

`%>%`<-magrittr::`%>%`

discografia=readr::read_csv('data-raw/iron_maiden.csv')


termos=readr::read_csv('data-raw/Termos.csv')

termos[,c('death','life')]<- list(NULL)


distancia=termos[,-1]%>%dist()%>%hclust()

distancia[["labels"]]<-termos$Album

grupos=dendextend::cutree(distancia, k=4)%>%as.data.frame()
grupos$Album<-rownames(grupos)
rownames(grupos)<-NULL
colnames(grupos)[1]='Grupo'

discografia=grupos%>%dplyr::inner_join(discografia, by='Album')
termos=grupos%>%dplyr::inner_join(termos, by='Album')

rm(grupos)

ggdendro::ggdendrogram(data = distancia, rotate = F)+
  ggplot2::labs(title = "Dendrogram IM Lyrics")

n_composers=discografia%>%dplyr::group_by(Album)%>%
  dplyr::summarise(Grupo=mean(Grupo),
                   Musicas=dplyr::n(),
                   tempo_medio=mean(Seconds),
                   Harris=round(100*sum(Harris)/dplyr::n(),2),
                   Dickinson=round(100*sum(Dickinson)/dplyr::n(),2),
                   Smith=round(100*sum(Smith)/dplyr::n(),2),
                   Murray=round(100*sum(Murray)/dplyr::n(),2))%>%
  dplyr::ungroup()%>%dplyr::arrange(Grupo)



readr::write_csv(discografia, 'data-raw/iron_maiden.csv')
readr::write_csv(termos, 'data-raw/Termos.csv')
readr::write_csv(n_composers, 'data-raw/Compositores.csv')
