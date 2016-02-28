hentOrd <- function(words,lang = "fr"){
  if(lang == "fr"){
    source("getGyldendal.R")
    source("getLarousse.R")
    LarousseOrd <- getLarousse(words = words)
    gyldOrd <- getGyldendal(words = words)
    # Lægger ord sammen
    FlereOrd <- cbind(LarousseOrd,gyldOrd)
    # Lægger synonymer sammen
      # columns to paste together
      cols <- c("Syn","SynOpslag")
      # create a new column `x` with the three columns collapsed together
      FlereOrd$SynAll <-
        apply(FlereOrd[, cols] , 1 , paste , collapse = ", ")
      # remove the unnecessary rows
      FlereOrd <- FlereOrd[ , !( names( FlereOrd ) %in% cols ) ]
      # Tager kun unikke og sorterer
      for(i in 1:length(words)){
        out <- unlist(strsplit(as.character(FlereOrd$SynAll[i]),', '))
        # Lægger sammen i én tekststreng
        FlereOrd$SynAll[i] <- sort(paste(unique(out),
                                         sep = "",
                                         collapse = ", "))
        # Fjerner komma i begyndelsen
        if(grepl(",",substr(FlereOrd$SynAll[i],1,1))){
          FlereOrd$SynAll[i] <- substr(FlereOrd$SynAll[i],2,
                                       nchar(FlereOrd$SynAll[i]))
        }
      }
    return(FlereOrd)
  }
  if(lang == "en"){
    source("hentMW.R")
    MWOrd <- getMW(words = words)
    return(MWOrd)
  }
}
# hentOrd(words = c("bredouille","mouvoir"))
