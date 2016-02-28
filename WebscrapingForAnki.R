library(rvest)
library(xml2)
library(magrittr)
library(RSelenium)
rm(list = ls())
source("funk.R")


# getWikipedia(countries = c("Martinique"))
# # getLarousse(words = "casser", retn)
# # getGyldendal(words = c("flytte","røre"),retning = "dafr",synOpslag = FALSE)
# 
# getGyldendal(words = "fendre",retning = "frda")
# getMW(words = c("fluffy"))

hentAlt(words = c("mouvoir","bredouille"))

ordTilAnki
#### Skriv fil ud ####
write.csv(ord,"ord.csv",row.names = FALSE)

# To do
# Get word type
# Få eksempler fra Gyldendal (plus lyd)?
# Hent fra andre Larousse-sider
# Få synonymer fra andre Larousse-sider
# fænge virker ikke i dafr i Gyldendal
# Hent danske lydfiler
## Engelske ord: http://dictionary.cambridge.org/
## Duden
# Lav cloze
# Slå andre sprog op i Gyldendal
# Fluffy in MW cutter et bogstav af i Def1
# Kan ikke finde SprogA3 ved bevæge i frda (mettre en mouvement)
# Lav github konto, hvor funktioner kan smides op

# SynOpslag: Søg efter synonym i Syn fra Larousse og tilføj, hvis den ikke er der

# Hent alt virker ikke!

