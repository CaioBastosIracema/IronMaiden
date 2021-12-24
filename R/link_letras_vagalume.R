SiteVagalume<-function(artist, setlist){

  `%>%`<-magrittr::`%>%`

  setlist%>%stringr::str_squish()%>%stringr::str_to_lower()%>%
    stringr::str_replace_all(' ','-')%>%
    abjutils::rm_accent()%>%stringr::str_replace_all("[.']",'')->x

  artist%>%stringr::str_squish()%>%stringr::str_to_lower()%>%
    stringr::str_replace_all(' ','-')%>%
    abjutils::rm_accent()%>%stringr::str_replace_all("[.']",'')->y

    paste('https://www.vagalume.com.br/',y,'/',x,'.html')%>%
      stringr::str_replace_all(' ','')->Links

    Versos=list()
    for(i in 1:length(Links)){
      Versos[[setlist[i]]]=xml2::read_html(Links[i])%>%rvest::html_nodes('#lyrics')%>%
        rvest::html_text2()%>%stringr::str_split('\n')%>%.[[1]]
    }
    Versos
}

ModoTexto<-function(Versos){
  `%>%`<-magrittr::`%>%`

  Texto=vector()
  for( i in 1:length(Versos)){
    Texto[i]=''
    for( j in 1:length(Versos[[i]])){
      Texto[i]=stringr::str_c(Texto[i],' ',Versos[[i]][j])
  }
  }
  Texto
}


