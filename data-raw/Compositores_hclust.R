#### This script uses hierarchical cluster to find possible groups of albuns
#### based on their terms

`%>%`<-magrittr::`%>%`
`%notin%`<-Negate(`%in%`)

discografia=readr::read_csv('data/iron_maiden2.csv')
termos=readr::read_csv('data/Termos.csv')

tf_idf=readr::read_csv('data/tf_idf.csv')

#Excluding some generic terms

tf_idf[,c('death','life')]<- list(NULL)
termos=subset(termos, termo%notin%c('death','life'))
termos=subset(termos, termo%notin%c("i'm","i'v","you'r","you'v",
                                    "they'r","they'v","we'r","we'v"))



#tf_idf[,c('call','tell','never','say','every')]<- list(NULL)


#normalizing the data
tf_idf[,-1]=gensvm::gensvm.maxabs.scale(tf_idf[,-1])


#termos=termos[,-1:-2]%>%apply(2, var)%>%order(decreasing = T)%>%+2%>%head(20)%>%
 # append(2, 0)%>%termos[,.]

#Applying the ward method/algorithm

distancia=tf_idf[,-1]%>%dist()%>%hclust(method="ward.D")

distancia[["labels"]]<-tf_idf$Album

#Separating the data into 3 groups
grupos=dendextend::cutree(distancia, k=3)%>%as.data.frame()
grupos$Album<-rownames(grupos)
rownames(grupos)<-NULL
colnames(grupos)[1]='Grupo'

discografia=grupos%>%dplyr::inner_join(discografia, by='Album')
tf_idf=grupos%>%dplyr::inner_join(tf_idf, by='Album')

rm(grupos)

#jpeg('imagens/dendograma.jpeg')
ggdendro::ggdendrogram(data = distancia, rotate = F)+
  ggplot2::labs(title = "Dendrogram IM Lyrics")
#dev.off()

grupos=termos$termo%>%as.data.frame()
grupos$grupo1=apply(termos[,2:3],1,sum)
grupos$grupo2=apply(termos[,c(4:6,9)],1,sum)
grupos$grupo3=apply(termos[,c(7:8,10:18)],1,sum)
colnames(grupos)[1]<-'Termo'

#wordcloud by group
wordcloud::wordcloud(grupos$Termo, grupos$grupo1,
                     min.freq=2,max.words=50,random.order=F,
                     colors = RColorBrewer::brewer.pal(8, "Dark2"))

wordcloud::wordcloud(grupos$Termo, grupos$grupo2,
                     min.freq=2,max.words=50,random.order=F,
                     colors = RColorBrewer::brewer.pal(8, "Dark2"))

wordcloud::wordcloud(grupos$Termo, grupos$grupo3,
                     min.freq=2,max.words=50,random.order=F,
                     colors = RColorBrewer::brewer.pal(8, "Dark2"))


"
n_composers=discografia%>%dplyr::group_by(Album)%>%
  dplyr::summarise(Grupo=mean(Grupo),
                   Musicas=dplyr::n(),
                   tempo_medio=mean(Seconds),
                   Harris=round(100*sum(Harris)/dplyr::n(),2),
                   Dickinson=round(100*sum(Dickinson)/dplyr::n(),2),
                   Smith=round(100*sum(Smith)/dplyr::n(),2),
                   Murray=round(100*sum(Murray)/dplyr::n(),2))%>%
  dplyr::ungroup()%>%dplyr::arrange(Grupo)
"


readr::write_csv(discografia, 'data-raw/iron_maiden2.csv')
readr::write_csv(termos, 'data-raw/Termos2.csv')
readr::write_csv(n_composers, 'data-raw/Compositores.csv')
