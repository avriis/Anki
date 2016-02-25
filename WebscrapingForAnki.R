library(rvest)
library(xml2)
library(magrittr)
library(RSelenium)
rm(list = ls())
setwd("~/OneDrive/R-sandkasse")
source("funk.r")

getLarousse(words = "traîner")
getGyldendal(words = "spalte",retning = "dafr")
getGyldendal(words = "teal",retning = "enda")
getMW(words = c("fluffy"))

hentAlt("gaspiller")

ordTilAnki
#### Skriv fil ud ####
write.csv(ord,"ord.csv",row.names = FALSE)

# To do
# Get word type
# Få eksempler fra Gyldendal (plus lyd)?
# Hent fra andre Larousse-sider
## Få synonymer fra gyldendal (1. slå ord op baglæns, og tjek om de ord er i synonymlisten) og synonymer fra andre Larousse-sider
# fænge virker ikke i dafr i Gyldendal
# Hent danske lydfiler
# Hent lydfiler fra Forvo
## Engelske ord
## Duden
# Lav cloze
# Slå andre sprog op i Gyldendal
# Fluffy in MW cutter et bogstav af i Def1

# Lav github konto, hvor funktioner kan smides op

