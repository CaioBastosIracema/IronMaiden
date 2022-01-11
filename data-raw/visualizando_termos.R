discografia=readr::read_csv('data-raw/iron_maiden2.csv')
termos=readr::read_csv('data-raw/Termos2.csv')

# TOken do github: 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

ggplot2::ggplot(termos,ggplot2::aes(x=blood, y=reorder(Album, Grupo), fill=as.factor(Grupo)))+
  ggplot2::geom_col()

ggplot2::ggplot(termos,ggplot2::aes(x=run, y=reorder(Album, Grupo), fill=as.factor(Grupo)))+
  ggplot2::geom_col()

ggplot2::ggplot(termos,ggplot2::aes(x=find, y=reorder(Album, Grupo), fill=as.factor(Grupo)))+
  ggplot2::geom_col()

ggplot2::ggplot(termos,ggplot2::aes(x=long, y=reorder(Album, Grupo), fill=as.factor(Grupo)))+
  ggplot2::geom_col()

ggplot2::ggplot(termos,ggplot2::aes(x=eyes, y=reorder(Album, Grupo), fill=as.factor(Grupo)))+
  ggplot2::geom_col()

ggplot2::ggplot(termos,ggplot2::aes(x=hear, y=reorder(Album, Grupo), fill=as.factor(Grupo)))+
  ggplot2::geom_col()

ggplot2::ggplot(termos,ggplot2::aes(x=light, y=reorder(Album, Grupo), fill=as.factor(Grupo)))+
  ggplot2::geom_col()

ggplot2::ggplot(termos,ggplot2::aes(x=feel, y=reorder(Album, Grupo), fill=as.factor(Grupo)))+
  ggplot2::geom_col()

ggplot2::ggplot(termos,ggplot2::aes(x=sky, y=reorder(Album, Grupo), fill=as.factor(Grupo)))+
  ggplot2::geom_col()

ggplot2::ggplot(termos,ggplot2::aes(x=mind, y=reorder(Album, Grupo), fill=as.factor(Grupo)))+
  ggplot2::geom_col()

ggplot2::ggplot(termos,ggplot2::aes(x=day, y=reorder(Album, Grupo), fill=as.factor(Grupo)))+
  ggplot2::geom_col()

ggplot2::ggplot(termos,ggplot2::aes(x=inside, y=reorder(Album, Grupo), fill=as.factor(Grupo)))+
  ggplot2::geom_col()

ggplot2::ggplot(termos,ggplot2::aes(x=night, y=reorder(Album, Grupo), fill=as.factor(Grupo)))+
  ggplot2::geom_col()

ggplot2::ggplot(termos,ggplot2::aes(x=nothing, y=reorder(Album, Grupo), fill=as.factor(Grupo)))+
  ggplot2::geom_col()

