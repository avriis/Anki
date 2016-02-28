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
getMW(words = "peremptory")
