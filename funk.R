source("getDuden.R")
source("getGyldendal.R")
source("getLarousse.R")
source("getMW.R")
source("getDuden.R")
source("getWikipedia.R")
source("getForvo.R")
source("hentOrd.R")


# startSelenium <- function(appURL,
#                           silentOpen = FALSE,
#                           browserName = "firefox",
#                           PageSource = "PageSource",
#                           word = "mouvoir"){
#   require(RSelenium)
#   if(file.exists("selenium-server-standalone.jar")){
#     checkForServer()
#   } else {
#     checkForServer(update = TRUE,dir = getwd()) # Sæt til TRUE
#   }
#   system("open selenium-server-standalone.jar")
#   RSelenium::startServer()
#   # remDr <- remoteDriver$new()
#   remDr <- remoteDriver(browserName = browserName)
#   # remDr$setImplicitWaitTimeout(milliseconds = 10000)
#   remDr$open(silent = silentOpen)
#   remDr$navigate(appURL)
#   assign(PageSource,remDr$getPageSource(header = TRUE),
#          envir = .GlobalEnv)
#   #   start <- gregexpr(word,PageSource)[[1]][1]
#   #   slut <- start+1000
#   #   return(substr(PageSource,start,slut))
# }





