getGyldendal <- function(words = c("mouvoir","bredouille"),
                         retning = "frda",
                         synOpslag = TRUE){
  require(rvest)
  require(RJSONIO)
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
    # Gemmer overbetydninger
    senses <- dat$entryListResult$items[[1]]$senses
    # Ser alle betydninger
    counter <- 0
    for(i in 1:length(senses)){
      # for(ii in 1:length(senses[[i]]$subSenses[[1]]$annotatedTargets)){
      for(ii in 1:length(senses[[i]]$subSenses[[1]]$targetGroups[[1]]$annotatedTargets)){
        counter <- counter + 1
        assign(paste0("SprogA",counter),
               if(!is.list(senses[[i]]$subSenses[[1]]$targetGroups[[1]]$annotatedTargets[[ii]])){ 
                 as.character(senses[[i]]$subSenses[[1]]$targetGroups[[1]]$annotatedTargets[[ii]]["translationGramLemmaRef"]) 
                 } else {
                 senses[[i]]$subSenses[[1]]$targetGroups[[1]]$annotatedTargets[[ii]]$translation
               }
        )
      }}
    # Slår synonymer op
    if(synOpslag){
    
      SynOpslag <- getGyldendal(SprogA1,
                   synOpslag = FALSE, # Sikrer mod uendeligt loop
                   retning = paste0(substr(retning,3,4),
                                    substr(retning,1,2)))
      for(j in 2:counter){
        SynOpslag <- cbind(SynOpslag, getGyldendal(
          get(paste0("SprogA",j)),
          synOpslag = FALSE, # Sikrer mod uendeligt loop
          retning = paste0(substr(retning,3,4),
                           substr(retning,1,2))
        ))
        assign("SynOpslag",SynOpslag)
      }
      # Fjerner tomme og sorterer
      SynOpslag <- sort(SynOpslag[SynOpslag != ""])
      # Lægger sammen i én tekststreng
      SynOpslag <- paste(unique(SynOpslag),sep = "",collapse = ", ")
    }
    
    # Samler delelementer for ord
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
      SynOpslag = if("SynOpslag" %in% ls()){SynOpslag}else{""},
      stringsAsFactors = FALSE
    )
    # Hvis jeg bare ønsker at slå gyldendalop
    # print(("LarousseOrd" %in% ls()))
    # print(gyldendalOrd)
#     if(synOpslag){
#       if(!("LarousseOrd" %in% ls())){
#         x <- gyldendalOrd
#       } else {
#         x <- cbind(LarousseOrd[w,],gyldendalOrd,
#                    stringsAsFactors = FALSE)
#       }
#     } else {
#       x <- gyldendalOrd
#     }
    x <- gyldendalOrd
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
# getGyldendal(words = c("mouvoir"))

