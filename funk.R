hentAlt <- function(words){
  LarousseOrd <- getLarousse(words = words)
  assign("LarousseOrd",LarousseOrd,envir = .GlobalEnv)
  # print(!("LarousseOrd" %in% ls()))
  ord <- getGyldendal(words = words)
  return(ord)
}


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

getGyldendal <- function(words = c("mouvoir","bredouille"),
                         retning = "frda"){
  for(w in 1:length(words)){
  word <- words[w]
  getUrlGyldendal <- function(word = word,direction = retning) {
    root <- "https://ordbog.gyldendal.dk/api/dimension?lemmaIds=&bookId=&targetNodeId=&primaryQueryText="
    root2 <- "&secondaryQueryText=&type=Primary&searchDirection="
    root3 <- "&lemmaBoundOnly=false&pageNumber=1&languageKey=da&showSuggestions=true&selectedLevelId=expert"
    u <- paste0(root, word, root2, direction, root3)
    return(URLencode(u))
  }
  # Laver url
  target <- getUrlGyldendal(word = word, direction = retning)
  # Henter data
  dat <- fromJSON(target)
  # Laver fejl, hvis ordet ikke findes
  if(length(dat$entryListResult$items)==0){
    stop(paste("Ordet",word,"findes ikke i Gyldendal!"))
  }
  # Link til lydfil
  LydLink <- dat$entryListResult$items[[1]]$pronunciationSoundFileRef
  # Finder antal betydninger
#   betydninger <- dat$entryListResult$items[[1]]$senses[[1]]$subSenses[[1]]$annotatedTargets
#   antal.betydninger <- length(betydninger)
#   sprogA <- ""
#   for(i in 1:antal.betydninger){
#     assign(
#       paste0("SprogA",i),
#       betydninger[[i]]$translation
#     )
#     # sprogA <- cbind(sprogA,betydninger[[i]]$translation)
#   }
#   sprogA <- sprogA[-1]
#   x <- as.data.frame(t(sprogA))
#   names(x) <- capture.output(cat(for(i in 1:antal.betydninger){print(paste0("sprogA",i))}))
  
  # Gemmer overbetydninger
  senses <- dat$entryListResult$items[[1]]$senses
  
  # Hvor mange underdefinitioner er der?
#   no.senses <- 0
#   for(i in 1:length(senses)){
#     no.senses <- 
#       no.senses + length(senses[[i]]$subSenses[[1]]$annotatedTargets)
#   }
  
  # Ser alle betydninger
  counter <- 0
  for(i in 1:length(senses)){
    for(ii in 1:length(senses[[i]]$subSenses[[1]]$annotatedTargets)){
      counter <- counter + 1
      assign(paste0("SprogA",counter),
             if(retning=="dafr"){ as.character(senses[[i]]$subSenses[[1]]$targetGroups[[1]]$annotatedTargets[[ii]]["translationGramLemmaRef"]) } else {
               senses[[i]]$subSenses[[1]]$targetGroups[[1]]$annotatedTargets[[ii]]$translation
             }
      )
    }}
  
  gyldendalOrd <- data.frame(
    SprogA1 = SprogA1,
    SprogA2 = if("SprogA2" %in% ls()){SprogA2}else{""},
    SprogA3 = if("SprogA3" %in% ls()){SprogA3}else{""},
    SprogA4 = if("SprogA4" %in% ls()){SprogA4}else{""},
    SprogA5 = if("SprogA5" %in% ls()){SprogA5}else{""},
    SprogA6 = if("SprogA6" %in% ls()){SprogA6}else{""},
    SprogA7 = if("SprogA7" %in% ls()){SprogA7}else{""},
    SprogA8 = if("SprogA8" %in% ls()){SprogA8}else{""},
    SprogA9 = if("SprogA9" %in% ls()){SprogA9}else{""},
    SprogA10 = if("SprogA10" %in% ls()){SprogA10}else{""},
    stringsAsFactors = FALSE
  )
  # Hvis jeg bare ønsker at slå gyldendalop
  if(!("LarousseOrd" %in% ls())){
    x <- gyldendalOrd
  } else {
    x <- cbind(LarousseOrd[w,],gyldendalOrd,
               stringsAsFactors = FALSE)
  }
  assign(paste0("ord",w),x)
  # Sletter objekter for at tomme pladser ikke overskrives
  # med tidligere indhold
  to.remove <- ls()
  to.remove <- c(to.remove[grepl("SprogA", to.remove)])
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


startSelenium <- function(appURL,
                          silentOpen = FALSE,
                          browserName = "firefox",
                          PageSource = "PageSource",
                          word = "mouvoir"){
  require(RSelenium)
  if(file.exists("selenium-server-standalone.jar")){
    checkForServer()
  } else {
    checkForServer(update = TRUE,dir = getwd()) # Sæt til TRUE
  }
  system("open selenium-server-standalone.jar")
  RSelenium::startServer()
  # remDr <- remoteDriver$new()
  remDr <- remoteDriver(browserName = browserName)
  # remDr$setImplicitWaitTimeout(milliseconds = 10000)
  remDr$open(silent = silentOpen)
  remDr$navigate(appURL)
  assign(PageSource,remDr$getPageSource(header = TRUE),
         envir = .GlobalEnv)
  #   start <- gregexpr(word,PageSource)[[1]][1]
  #   slut <- start+1000
  #   return(substr(PageSource,start,slut))
}
getMW <- function(words = c("peremptory")){
  for(w in 1:length(words)){
    word <- words[w]
    getUrlMW <- function(word = word,dictionary = "thesaurus") {
      root <- "http://www.merriam-webster.com"
      dictionary <- dictionary
      u <- paste(root, dictionary, word,sep = "/")
      return(URLencode(u))
    }
    url <- getUrlMW(word = word)
    # Get content from page
    html <- read_html(url)
    # Get all definitions
    DefAll <- html %>%
      html_nodes(".ssens") %>%
      html_text()
    for(i in 1:length(DefAll)){
      def <- DefAll[i]
      # Henter definition
      assign(
        paste0("Def",i),
        substr(def,3,regexec("<",def)[[1]][1]-3)
      )
      # Henter eksempel
      assign(
        paste0("Ex",i),
        substr(def,
               regexec("<",def)[[1]][1]+1,
               regexec(">",def)[[1]][1]-1)
      )
      # Henter synonymer
      assign(
        paste0("Syn",i),
        substr(def,
               regexec("Synonyms",def)[[1]][1]+9,
               regexec("Related Words",def)[[1]][1]-1)
      )
      # Henter relaterede ord
      assign(
        paste0("Rel",i),
        substr(def,
               regexec("Related Words",def)[[1]][1]+14,
               regexec("Near Antonyms",def)[[1]][1]-1)
      )
    }
    # Samler data
    ord <- data.frame(
      Ord = word,
      Def1 = Def1,
      Ex1 = Ex1,
      Syn1 = Syn1,
      Rel1 = Rel1,
      Def2 = if("Def2" %in% ls()){Def2}else{""},
      Ex2 = if("Ex2" %in% ls()){Ex2}else{""},
      Syn2 = if("Syn2" %in% ls()){Syn2}else{""},
      Rel2 = if("Rel2" %in% ls()){Rel2}else{""},
      Def3 = if("Def3" %in% ls()){Def3}else{""},
      Ex3 = if("Ex3" %in% ls()){Ex3}else{""},
      Syn3 = if("Syn3" %in% ls()){Syn3}else{""},
      Rel3 = if("Rel3" %in% ls()){Rel3}else{""},
      Def4 = if("Def4" %in% ls()){Def4}else{""},
      Ex4 = if("Ex4" %in% ls()){Ex4}else{""},
      Syn4 = if("Syn4" %in% ls()){Syn4}else{""},
      Rel4 = if("Rel4" %in% ls()){Rel4}else{""},
      Def5 = if("Def5" %in% ls()){Def5}else{""},
      Ex5 = if("Ex5" %in% ls()){Ex5}else{""},
      Syn5 = if("Syn5" %in% ls()){Syn5}else{""},
      Rel5 = if("Rel5" %in% ls()){Rel5}else{""},
      Def6 = if("Def6" %in% ls()){Def6}else{""},
      Ex6 = if("Ex6" %in% ls()){Ex6}else{""},
      Syn6 = if("Syn6" %in% ls()){Syn6}else{""},
      Rel6 = if("Rel6" %in% ls()){Rel6}else{""},
      Def7 = if("Def7" %in% ls()){Def7}else{""},
      Ex7 = if("Ex7" %in% ls()){Ex7}else{""},
      Syn7 = if("Syn7" %in% ls()){Syn7}else{""},
      Rel7 = if("Rel7" %in% ls()){Rel7}else{""},
      Syn = paste(Syn1,
                  if("Syn2" %in% ls()){Syn2}else{""},
                  if("Syn3" %in% ls()){Syn3}else{""},
                  if("Syn4" %in% ls()){Syn4}else{""},
                  if("Syn5" %in% ls()){Syn5}else{""},
                  if("Syn6" %in% ls()){Syn6}else{""},
                  if("Syn7" %in% ls()){Syn7}else{""}),
      stringsAsFactors = FALSE
    )
    assign(paste0("ord",w),ord)
    # Sletter objekter for at tomme pladser ikke overskrives
    # med tidligere indhold
    to.remove <- ls()
    to.remove <- c(to.remove[grepl("Def", to.remove)],
                   to.remove[grepl("Ex", to.remove)],
                   to.remove[grepl("Syn", to.remove)],
                   to.remove[grepl("Rel", to.remove)])
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
getDuden <- function(words){
  for(w in 1:length(words)){
    word <- words[w]
    url <- paste0("http://www.duden.de/rechtschreibung/",
                  word)
    # Get content from page
    html <- read_html(url)
    # Get all definitions
    DefAll <- ""
    for(i in 1:4){
      DefAll <- c(DefAll,html %>%
        html_nodes(paste0("#block-duden-tiles-6 .entry > li:nth-child(",
                          i,
                          ")")) %>%
        html_text()
      )
    }
    DefAll <- DefAll[-1]
    
    # #block-duden-tiles-6 .entry > li:nth-child(1)
    for(i in 1:length(DefAll)){
      def <- DefAll[i]
      # Henter definition
      assign(
        paste0("Def",i),
        substr(def,3,regexec("<",def)[[1]][1]-3)
      )
      # Henter eksempel
      assign(
        paste0("Ex",i),
        substr(def,
               regexec("<",def)[[1]][1]+1,
               regexec(">",def)[[1]][1]-1)
      )
      # Henter synonymer
      assign(
        paste0("Syn",i),
        substr(def,
               regexec("Synonyms",def)[[1]][1]+9,
               regexec("Related Words",def)[[1]][1]-1)
      )
      # Henter relaterede ord
      assign(
        paste0("Rel",i),
        substr(def,
               regexec("Related Words",def)[[1]][1]+14,
               regexec("Near Antonyms",def)[[1]][1]-1)
      )
    }
    # Samler data
    ord <- data.frame(
      Ord = word,
      Def1 = Def1,
      Ex1 = Ex1,
      Syn1 = Syn1,
      Rel1 = Rel1,
      Def2 = if("Def2" %in% ls()){Def2}else{""},
      Ex2 = if("Ex2" %in% ls()){Ex2}else{""},
      Syn2 = if("Syn2" %in% ls()){Syn2}else{""},
      Rel2 = if("Rel2" %in% ls()){Rel2}else{""},
      Def3 = if("Def3" %in% ls()){Def3}else{""},
      Ex3 = if("Ex3" %in% ls()){Ex3}else{""},
      Syn3 = if("Syn3" %in% ls()){Syn3}else{""},
      Rel3 = if("Rel3" %in% ls()){Rel3}else{""},
      Def4 = if("Def4" %in% ls()){Def4}else{""},
      Ex4 = if("Ex4" %in% ls()){Ex4}else{""},
      Syn4 = if("Syn4" %in% ls()){Syn4}else{""},
      Rel4 = if("Rel4" %in% ls()){Rel4}else{""},
      Def5 = if("Def5" %in% ls()){Def5}else{""},
      Ex5 = if("Ex5" %in% ls()){Ex5}else{""},
      Syn5 = if("Syn5" %in% ls()){Syn5}else{""},
      Rel5 = if("Rel5" %in% ls()){Rel5}else{""},
      Def6 = if("Def6" %in% ls()){Def6}else{""},
      Ex6 = if("Ex6" %in% ls()){Ex6}else{""},
      Syn6 = if("Syn6" %in% ls()){Syn6}else{""},
      Rel6 = if("Rel6" %in% ls()){Rel6}else{""},
      Def7 = if("Def7" %in% ls()){Def7}else{""},
      Ex7 = if("Ex7" %in% ls()){Ex7}else{""},
      Syn7 = if("Syn7" %in% ls()){Syn7}else{""},
      Rel7 = if("Rel7" %in% ls()){Rel7}else{""},
      Syn = paste(Syn1,
                  if("Syn2" %in% ls()){Syn2}else{""},
                  if("Syn3" %in% ls()){Syn3}else{""},
                  if("Syn4" %in% ls()){Syn4}else{""},
                  if("Syn5" %in% ls()){Syn5}else{""},
                  if("Syn6" %in% ls()){Syn6}else{""},
                  if("Syn7" %in% ls()){Syn7}else{""}),
      stringsAsFactors = FALSE
    )
    assign(paste0("ord",w),ord)
    # Sletter objekter for at tomme pladser ikke overskrives
    # med tidligere indhold
    to.remove <- ls()
    to.remove <- c(to.remove[grepl("Def", to.remove)],
                   to.remove[grepl("Ex", to.remove)],
                   to.remove[grepl("Syn", to.remove)],
                   to.remove[grepl("Rel", to.remove)])
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



