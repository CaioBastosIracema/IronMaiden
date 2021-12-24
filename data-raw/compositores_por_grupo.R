`%>%`<-magrittr::`%>%`
n_composers=readr::read_csv('data-raw/Compositores.csv')


# Compositores por grupo

ggplot2::ggplot(ggplot2::aes(Harris, reorder(Album, Grupo)), data=n_composers)+
  ggplot2::geom_col(ggplot2::aes(fill=as.factor(Grupo)))

ggplot2::ggplot(ggplot2::aes(Dickinson, reorder(Album, Grupo)), data=n_composers)+
  ggplot2::geom_col(ggplot2::aes(fill=as.factor(Grupo)))

ggplot2::ggplot(ggplot2::aes(Smith, reorder(Album, Grupo)), data=n_composers)+
  ggplot2::geom_col(ggplot2::aes(fill=as.factor(Grupo)))

ggplot2::ggplot(ggplot2::aes(Murray, reorder(Album, Grupo)), data=n_composers)+
  ggplot2::geom_col(ggplot2::aes(fill=as.factor(Grupo)))

#Tempo médio por grupo

ggplot2::ggplot(ggplot2::aes(tempo_medio, reorder(Album, Grupo)), data=n_composers)+
  ggplot2::geom_col(ggplot2::aes(fill=as.factor(Grupo)))

ggplot2::ggplot(ggplot2::aes(Musicas, reorder(Album, Grupo)), data=n_composers)+
  ggplot2::geom_col(ggplot2::aes(fill=as.factor(Grupo)))



