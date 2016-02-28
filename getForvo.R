getForvo <- function(word, language="en"){
  # Tjekker, om filen allerede ligger
  # Der kan kun hentes 500 filer om dagen 
  fileName <- paste0("pronunciation_",
                     language,"_",
                     word,".mp3")
  destfile <- paste0("~/Documents/Anki/User 1/collection.media/",
                     fileName)
  if(file.exists(destfile)){
    return(word)
  }
  require(RJSONIO)
  APIkey <- "256094f122938b583b25f944db8cc393"
  url <- paste0(
    "http://apifree.forvo.com/action/word-pronunciations/format/json/word/",
    word,
    "/language/",
    language,
    "/key/",APIkey
  )
  # Henter indhold
  content <- fromJSON(url)
  # Tjekker, om ord findes
  if(length(content$items)==0){ # Hvis det ikke findes, returneres blot ord
    return(word)
  } else { # Hvis det findes, downloades ordet
    # Finder sti til download
    downloadPath <- content[2]$items[[1]]$pathmp3
    langCode <- content[2]$items[[1]]$code
    # Downloader fil
    fileName <- paste0("pronunciation_",
                       langCode,"_",
                       word,".mp3")
    destfile <- paste0("~/Documents/Anki/User 1/collection.media/",
                       fileName)
    if(!file.exists(destfile)){
      download.file(downloadPath,
                    destfile = destfile)
    }
    makeFieldName <- paste0(word,"[sound:",
                            fileName,"]")
    return(makeFieldName)
  }
}

# getForvo(word = "Martinique",language = "fr")

# getForvo <- function(word, language="en"){
#   require(rvest)
#   APIkey <- "256094f122938b583b25f944db8cc393"
#     # Make url
#     getForvoUrl <- function(word,language){
#       root <- "http://forvo.com/word/"
#       url <- paste0(root,word,"/#",language)
#       return(url)
#     }
#     url <- getForvoUrl(word = word,language = language)
#     # Get content from page
#     t <- try(
#       html <- read_html(url),
#       silent = TRUE
#     )
#     if("try-error" %in% class(t)){
#       return(word)
#     } else {
#       # Finder download-link
#       downloadLink <- as.character(html_nodes(html, ".download")[1])
#       downloadLink <- substr(downloadLink,
#                              gregexpr("href",downloadLink)[[1]][1]+6,
#                              gregexpr("title",downloadLink)[[1]][1]-3)
#       # Downloader fil
#       fileName <- paste0("pronunciation_",
#                          language,"_",
#                          word,".mp3")
#       destfile <- paste0("~/Documents/Anki/User 1/collection.media/",
#                          fileName)
#       if(!file.exists(destfile)){
#         download.file(downloadLink,
#                       destfile = destfile)
#       }
#       makeFieldName <- paste0(word,"[sound:",
#                               fileName,"]")
#       return(makeFieldName)
#     }
# }
# getForvo(word = "Bogotá",language = "es")
