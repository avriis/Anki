getLarousse <- function(words = c("faire","mouvoir")){
  for(w in 1:length(words)){
    word <- words[w]
    # Make url
    url <- paste("http://www.larousse.fr/dictionnaires/francais",
                 word,
                 sep = "/")
    # Get content from page
    html <- read_html(url)
    # Get all definitions
    DefAll <- html %>%
      html_nodes(".DivisionDefinition") %>%
      html_text()
    # Sorterer fra, hvis manglende ord
    if(length(DefAll)==0){
      stop(paste0("Ordet ",word," findes ikke i Larousse!")) 
    }
    # Behandler de individuelle definitioner
    for(i in 1:length(DefAll)){
      Def <- DefAll[i]
      # Trækker eksempler ud
      if(grepl("\\:",Def)){
        # Søger efter kolon
        colon <- gregexpr("\\:",Def)[[1]][1]-2
        Ex <- substr(Def,colon+4,nchar(Def))
        Def <- paste0(substr(Def,1,colon),".")
      } else { # ... eller lægger blot def. ind, hvis ingen eks.
        Def <- Def
        Ex <- ""
      }
      # Fjerner ord fra visse definitioner
      # Fx "bredouille" og crépuscule
      if(grepl(word,Def)){
        if(grepl("\\(",Def)){ # Anvendes til crépuscule
          #           parentes <- gregexpr("\\(",Def)[[1]][1]-2
          #           x <- substr(Def,1,parentes)
          #           x <- paste0(x,".")
          Def <- gsub(word,"[...]",Def)
          Ex <- ""
        } else { # Anvendes fx til bredouille
          start <- as.numeric(gregexpr(word,Def)[[1]][1])+as.numeric(attributes(gregexpr(word,Def)[[1]]))
          Ex <- paste0(substr(Def,1,start-1),".")
          Def <- substr(Def,start+2,nchar(Def))
          Def <- paste0(toupper(substr(Def,1,1)),
                        substr(Def,2,nchar(Def)))
        }
      }
      # Gemmer eksempel
      assign(paste0("Ex",i),
             Ex
             # ,envir = .GlobalEnv
      )
      # Gemmer definition
      assign(paste0("Def",i),
             Def)
    }
    ## Get synonyms
    synlink <- html_nodes(html, ".section .sel+ li a")
    # Tjekker, at der er syn.
    if(length(synlink)==0){ # Fx catalepsie, hvor der ikke er
      Syn <- ""
    } else {
      # Tjekker, at det er synonymer og ikke andet
      if(html_text(synlink)=="Synonymes"){
        urlSyn <- html_attr(synlink, name = "href")
        urlSyn <- paste0("http://www.larousse.fr",
                         urlSyn)
        Syn <- read_html(urlSyn)
        Syn <- html_nodes(Syn, ".Synonymes:nth-child(1)")
        Syn <- html_text(Syn)
        Syn <- substr(Syn,as.numeric(gregexpr("\\:",Syn))+1,nchar(Syn))
        # print(Syn)
        Syn <- sub("\r\n\t\t\t","",Syn)
        Syn <- gsub("\r\n\t\t\t",", ",Syn)
        Syn <- sub("\r\n\t","",Syn)
      } else {
        synlink <- html_nodes(html, ".section li:nth-child(3) a")
        urlSyn <- html_attr(synlink, name = "href")
        urlSyn <- paste0("http://www.larousse.fr",
                         urlSyn)
        Syn <- read_html(urlSyn)
        Syn <- html_nodes(Syn, ".Synonymes:nth-child(1)")
        Syn <- html_text(Syn)
        Syn <- substr(Syn,as.numeric(gregexpr("\\:",Syn))+1,nchar(Syn))
        # print(Syn)
        Syn <- sub("\r\n\t\t\t","",Syn)
        Syn <- gsub("\r\n\t\t\t",", ",Syn)
        Syn <- sub("\r\n\t","",Syn)
        # Syn <- ""
      }
    }
    
    # Hvis der er mange synonymer, samles de
    if(length(Syn)>1){
      Syn0 <- numeric(0)
      for(i in 1:length(Syn)){
        Syn0 <- paste(Syn0, Syn[i], sep = "; ")
        assign("Syn1",Syn0)
      }
      Syn <- substr(Syn1,3,nchar(Syn1))
    }
    ## Sound
    soundLink <- html_nodes(html, ".lienson")
    soundLink <- html_attr(soundLink, name = "href")
    # Download sound
    start <- max(gregexpr("\\/",soundLink)[[1]])+1
    # Trækker lyd-nummer ud
    soundLink <- substr(soundLink,start,nchar(soundLink))
    url <- paste0("http://voix.larousse.fr/francais/",
                  soundLink,
                  ".mp3")
    destfile <- paste0("~/Documents/Anki/User 1/collection.media/",
                       soundLink,".mp3")
    if(!file.exists(destfile)){
      download.file(
        url,
        destfile = destfile,
        quiet = FALSE)
    }
    # Lægger lyd på ordet i Anki
    word <- paste0(word,
                   "[sound:",soundLink,".mp3]")
    # Samler data
    ord <- data.frame(
      Ord = word,
      Def1 = Def1,
      Ex1 = Ex1,
      Def2 = if("Def2" %in% ls()){Def2}else{""},
      Ex2 = if("Ex2" %in% ls()){Ex2}else{""},
      Def3 = if("Def3" %in% ls()){Def3}else{""},
      Ex3 = if("Ex3" %in% ls()){Ex3}else{""},
      Def4 = if("Def4" %in% ls()){Def4}else{""},
      Ex4 = if("Ex4" %in% ls()){Ex4}else{""},
      Def5 = if("Def5" %in% ls()){Def5}else{""},
      Ex5 = if("Ex5" %in% ls()){Ex5}else{""},
      Def6 = if("Def6" %in% ls()){Def6}else{""},
      Ex6 = if("Ex6" %in% ls()){Ex6}else{""},
      Def7 = if("Def7" %in% ls()){Def7}else{""},
      Ex7 = if("Ex7" %in% ls()){Ex7}else{""},
      Syn = if(length(Syn)==0){""} else {Syn},
      stringsAsFactors = FALSE
    )
    assign(paste0("ord",w),ord)
    # Sletter objekter for at tomme pladser ikke overskrives
    # med tidligere indhold
    to.remove <- ls()
    to.remove <- c(to.remove[grepl("Def", to.remove)],
                   to.remove[grepl("Ex", to.remove)],
                   to.remove[grepl("Syn", to.remove)])
    rm(list=to.remove)
  }
  # Samler alle ord
  AlleOrd <- ord1[-1,]
  for(i in 1:length(words)){
    AlleOrd <- rbind(AlleOrd,get(paste0("ord",i)))
    assign("ord",AlleOrd)
  }
  return(ord)
}