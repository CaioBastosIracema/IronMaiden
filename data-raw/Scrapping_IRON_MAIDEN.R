#This script does web scraping on www.ironmaiden.com and www.vagalume.com.br
# to extract the information needed about the band and the song's lyrics.

`%>%`<-magrittr::`%>%`

#scraping on ironmaiden.com and putting all data into the data frame "iron_maiden"

LinkTitles<-xml2::read_html('https://www.ironmaiden.com/discography/studio-albums')%>%
  rvest::html_nodes('#middle a')%>%rvest::html_attrs()

iron_maiden=tibble::tibble()
for(i in 1:length(LinkTitles)){
  xml2::read_html(LinkTitles[[i]]['href'])%>%
    rvest::html_nodes('.dur , .by , .ttl')%>%rvest::html_text()%>%
    matrix(ncol=3, byrow = TRUE)%>%tibble::as_tibble()%>%dplyr::rename(Song=V1,
                                                                       Composers=V2,
                                                                       Time=V3)%>%
    dplyr::mutate(Album=LinkTitles[[i]]['title'])%>%rbind(iron_maiden)%>%
    subset(stringr::str_sub(Song, start = -15)!=' (Instrumental)')->iron_maiden
}

#corrects a song's name
iron_maiden$Song=iron_maiden$Song%>%stringr::str_replace('Love and Hate', 'Love Hate')

#scraping on vagalume.com.br
Lyrics=SiteVagalume('iron maiden',iron_maiden$Song)

#new column with the song's lyrics well structured
iron_maiden$Lyrics=ModoTexto(Lyrics)

#These 2 functions (ModoTexto and SiteVagalume) have been created in
#the "link_letras_vagalume.R" script

#creating dummys to work better with the composers information
iron_maiden$Harris<-iron_maiden%>%.$Composers%>%
  stringr::str_detect('Harris')
iron_maiden$Dickinson<-iron_maiden%>%.$Composers%>%
  stringr::str_detect('Dickinson')
iron_maiden$Murray<-iron_maiden%>%.$Composers%>%
  stringr::str_detect('Murray')
iron_maiden$Smith<-iron_maiden%>%.$Composers%>%
  stringr::str_detect('Smith')
iron_maiden$Gers<-iron_maiden%>%.$Composers%>%
  stringr::str_detect('Gers')
iron_maiden$`Di'Anno`<-iron_maiden%>%.$Composers%>%
  stringr::str_detect("Di'Anno")
iron_maiden$Bayley<-iron_maiden%>%.$Composers%>%
  stringr::str_detect("Bayley")
iron_maiden$McBrain<-iron_maiden%>%.$Composers%>%
  stringr::str_detect("McBrain")
iron_maiden$Burr<-iron_maiden%>%.$Composers%>%
  stringr::str_detect("Burr")

#Creating a new feature to separate the band's albums by phases (1 to 5)

iron_maiden$Fase= ifelse(iron_maiden$Album%in%c('The X Factor', 'Virtual XI'), 4, 5)%>%
  ifelse(iron_maiden$Album%in%c('No Prayer for the Dying', 'Fear of the Dark'), 3,.)%>%
  ifelse(iron_maiden$Album%in%c('The Number of the Beast', 'Piece of Mind',
                                'Powerslave', 'Somewhere in Time',
                                'Seventh Son of a Seventh Son'), 2,.)%>%
  ifelse(iron_maiden$Album%in%c('Iron Maiden', 'Killers'), 1,.)

#Some adjustments on the information about the song's duration

iron_maiden$Time[55]<-'4:40'
iron_maiden$Seconds=iron_maiden$Time%>%lubridate::ms()%>%
  lubridate::as.period(unit='sec')%>%as.numeric()

iron_maiden=iron_maiden%>%dplyr::relocate(Fase, .after=Album)%>%
  dplyr::relocate(Seconds, .after=Time)


#Exporting
readr::write_csv(iron_maiden,'data/iron_maiden.csv')




