getWikipedia <- function(countries,
                         skrivUd = FALSE,
                         language = "es",
                         checkForWholeCountry = FALSE){
  require(rvest)
  source("getForvo.R")
  for(i in 1:length(countries)){
    x <- countries[i]
    country <- x
    # x <- country
    print(paste0("Ser nu på: ", country))
    # print(paste("Mette er",sample(c("sød.","skrap."),1)))
    warnings()
    # Make url
    url <- paste0("https://en.wikipedia.org/wiki/",x)
    # Get content from page
    html <- read_html(url)
    wholeBodyPage <- as.character(html_children(html)[2])
    wholeHead <- as.character(html_children(html)[1])
    # Tjekker, om det er hele landet
    if(checkForWholeCountry){
      if(grepl("not the country itself",wholeBodyPage)){ # Fx Holland/Netherlands
        bid1 <- substr(wholeBodyPage,
               gregexpr("see",
                 wholeBodyPage)[[1]][1],
               gregexpr("see",
                        wholeBodyPage)[[1]][1]+100)
        bid1 <- substr(bid1,
                       gregexpr("href",bid1)[[1]][1]+12,
                       gregexpr("title",bid1)[[1]][1]-3)
        # Make url
        url <- paste0("https://en.wikipedia.org/wiki/",bid1)
        # Get content from page
        html <- read_html(url)
        # Omdøber
        print(paste0(x," omdøbes til ",bid1))
        x <- bid1
        country <- x
      }
    }
    # Tager hele boksen til højre
    facts <- html_nodes(html, ".vcard")
    
    #### Official language ####
    if(grepl("Overseas region",
             as.character(html_children(facts)[2]))){ # For Martinique
      posOfOffLang <- gregexpr("official language",
                               as.character(html_children(html)[2]))[[1]][1]
      offLang <- substr(as.character(html_children(html)[2]),
             posOfOffLang,
             posOfOffLang+100)
    } else {
      offLang <- html_text(html_children(facts)[grep("Official language|National language",
                                                     html_children(facts))])
    }
    if(length(offLang)==0){ # For Saint martin
      posOfOffLang <- gregexpr("official language",
                               as.character(html_children(html)[2]))[[1]][1]
      offLang <- substr(as.character(html_children(html)[2]),
                        posOfOffLang,
                        posOfOffLang+100)
    }
    languages <- c("English","French","Spanish")
    for(lang in languages){
      if(grepl(lang,offLang)){
        switch (lang,
                "English" = assign("offLang","en"),
                "French" = assign("offLang","fr"),
                "Spanish" = assign("offLang","es")
        )
      }
    }
    
    #### Capital ####
    capital <- html_text(html_children(facts)[grep("Capital",
                                                   html_children(facts))])
    if(length(capital)==0){ # For French Guiana
      capital <- html_text(html_children(facts)[grep("Prefecture",
                                                     html_children(facts))])[1]
    }
    if(is.na(capital)){ # For Holland, i.e. not Netherlands
      capital <- html_text(html_children(facts)[grep("Largest settlement",
                                                     html_children(facts))])
    }
    capital <- substr(capital[1],
                      gregexpr("[A-Z]",capital)[[1]][2],
                      nchar(capital))
    capital <- substr(capital,1,
                      regexec("\\n",capital)[[1]][1]-1)
    if(grepl("\\[",capital)){ # E.g. Sucre
      capital <- substr(capital,1,gregexpr("\\[",capital)[[1]][1]-1)
    }
    if(grepl("\\(",capital)){ # For Marigot i Saint Martin
      capital <- substr(capital,
             1,
             gregexpr("\\(",capital)[[1]][1]-2)
    }
    tekstBid <- substr(wholeBodyPage,
                       gregexpr("Capital",wholeBodyPage)[[1]][1],
                       gregexpr("Capital",wholeBodyPage)[[1]][1]+200)
    if(!grepl(capital,tekstBid)){ # For Belgium
      capital <- substr(capital,
                        1,
                        nchar(capital)-1)
    }
    
    # Lægger lyd på, hvis den findes
    t <- try(
      capital <- getForvo(word = capital, language = offLang),
      silent = TRUE)
    if("try-error" %in% class(t)){
      capital <- capital
    } else {
      capital <- getForvo(word = capital, language = offLang)
    }
    #### Map ####
    # as.character(html_children(facts)[3])
    map <- as.character(html_children(facts)[grep("Location|orthographic_projection",
                                                  html_children(facts))])
    if(length(map)>1){ # For Uruguay
      map <- as.character(html_children(facts)[grep("orthographic_projection",
                                                    html_children(facts))])
    }
    if(length(map)==0){ # For Argentina
      map <- as.character(html_children(facts)[grep("orthographic",
                                                    html_children(facts))])
    }
    if(length(map)==0){ # French Guiana
      map <- as.character(html_children(facts)[grep("svg",
                                                    html_children(facts))])
      map <- map[-grep("Flag",map)]
    }
    if(length(map)>1){ # For Belgium
      map <- as.character(html_children(facts)[grep("in the European Union",
                                                    html_children(facts))])
    }
    if(!grepl("svg",map)){ # For Saint Martin
      map <- as.character(html_children(facts)[grep("in_its_region",
                                                    html_children(facts))])
    }
    map <- substr(map,
                  gregexpr("commons/thumb",map)[[1]][1]+14,
                  nchar(map))
    map <- substr(map,1,
                  gregexpr(".svg",map)[[1]][1]+3)
    specificUrlToMap <- map
    UrlToMap <- paste0("https://upload.wikimedia.org/wikipedia/commons/",
                       specificUrlToMap)
    fileName <- substr(specificUrlToMap,
                       gregexpr("/",map)[[1]][2]+1,
                       nchar(specificUrlToMap))
    destfile <- paste0("~/Documents/Anki/User 1/collection.media/",
                       fileName)
    if(!file.exists(destfile)){
      download.file(UrlToMap,
                    destfile = destfile)
    }
    map <- paste0('<img src="',
                  fileName,'">')
    #### Flag ####
    as.character(html_children(facts)[7])
    flag <- as.character(html_children(facts)[grep("Flag",
                                                   html_children(facts))])
    
    if(length(flag)>1){ # For Brazil
      flag <- flag[-grep("Anthem",flag)]
      if(length(flag)==0){ # For French Guiana
        flag <- as.character(html_children(facts)[grep("Flag",
                                                       html_children(facts))])[1]
      }
    }
    if(length(flag)==0){ # For Saint Martin
      flag = ""
    } else {
      flag <- substr(flag,
                     gregexpr("commons/thumb",flag)[[1]][1]+14,
                     nchar(flag))
      if(grepl("Coat_of_arms",substr(flag,1,20))){ # For Brazil
        flag <- as.character(html_children(facts)[grep("Flag",
                                                       html_children(facts))])
        flag <- flag[-grep("Anthem",flag)]
        flag <- substr(flag,
                       gregexpr("thumb",flag)[[1]][1]+6,
                       nchar(flag))
      }
      flag <- substr(flag,1,
                     gregexpr(".svg",flag)[[1]][1]+3)
      specificUrlToflag <- flag
      UrlToflag <- paste0("https://upload.wikimedia.org/wikipedia/commons/",
                          specificUrlToflag)
      fileName <- substr(specificUrlToflag,
                         gregexpr("/",flag)[[1]][2]+1,
                         nchar(specificUrlToflag))
      destfile <- paste0("~/Documents/Anki/User 1/collection.media/",
                         fileName)
      if(!file.exists(destfile)){
        download.file(UrlToflag,
                      destfile = destfile)
      }
      flag <- paste0('<img src="',
                     fileName,'">')
    }
    #### Name ####
    # Lægger lyd på, hvis den findes
    t <- try(
        name <- getForvo(word = country, language = offLang),
        silent = TRUE)
    if("try-error" %in% class(t)){
      name <- x
    } else {
      name <- getForvo(word = country, language = offLang)
    }
    #### Samler data ####
    df <- data.frame(
      name = name,
      capital = capital,
      map = map,
      flag = flag,
      stringsAsFactors = FALSE
    )
    assign(paste0("enhed",i),df)
    # closeAllConnections()
  }
  # Samler alle enheder
  # print(ls())
  AlleEnheder <- enhed1[-1,]
  for(i in 1:length(countries)){
    AlleEnheder <- rbind(AlleEnheder,get(paste0("enhed",i)))
    assign("output",AlleEnheder)
  }
  
  if(skrivUd){
    file <- paste0("~/Documents/Anki/User 1/",
                   "getWikipedia",
                   Sys.time(),
                   ".csv")
    write.csv(output,file,row.names = FALSE)
  }
  return(output)
}
# getWikipedia(countries = c("England","Belgium","Holland"),
#              checkForWholeCountry = TRUE)
# 

# getWikipedia(countries = c("Colombia","Paraguay","Panama","Costa Rica",
#                            "Bolivia","Peru","Guyana","Nicaragua",
#                            "Venezuela","Ecuador","Suriname","French Guiana",
#                            "Uruguay","Argentina","Chile","Brazil",
#                            "Honduras","El Salvador","Cuba",
#                            "Dominican Republic","Guadeloupe",
#                            "Guatemala", "Haiti","Martinique",
#                            "Mexico", "Puerto Rico", "Saint Barthélemy",
#                            "Saint Martin"),skrivUd = FALSE)
# getWikipedia(countries = "Mexico")
# x = "Bolivia"
# Hent også udtale fra Forvo
