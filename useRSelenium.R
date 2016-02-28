useRSelenium <- function(word){
  require(RSelenium)
  if(!file.exists("selenium-server-standalone.jar")){
    checkForServer(update = TRUE, dir = getwd())
  }
  system("open selenium-server-standalone.jar")
  startServer()
  remDr <- remoteDriver$new()
  remDr$open()
  url <- paste0("http://forvo.com/word/",word,"/#es")
  remDr$navigate(url)
  webElem1 <- remDr$findElement(using = 'css',
                                value = '.download a')
  remDr$mouseMoveToLocation(webElement = webElem1)
  remDr$buttondown(buttonId = 0)
  remDr$buttonup(buttonId = 0)
  # remDr$sendKeysToActiveElement(list('RSelenium was here'))
  webElem2 <- remDr$findElement(using = 'css',
                                value = '#login')
  webElem1 <- remDr$findElement(using = 'name', value = 'q')
#   remDr$sendKeysToElement(list('RSelenium was here'))
  webElem2 <- remDr$findElement(using = 'xpath',
                                value = '//*[(@id = "login")]')
#   webElem1$sendKeysToElement(list('RSelenium was here'))
  remDr$closeWindow()
  remDr$close()
  remDr$quit()
}

# useRSelenium(word = "Colombia")

# webElem1 <- remDr$findElement(using = 'css',
#                               value = '.download a')
# remDr$mouseMoveToLocation(webElement = webElem1)
# remDr$buttondown(buttonId = 0)
# remDr$buttonup(buttonId = 0)
# close
# 
# 
# webElem2 <- remDr$findElement(using = 'xpath',
#                               value = '//*[(@id = "word_search_header")]')
# webElem1$sendKeysToElement(list('RSelenium was here'))
# 
# remDr$getActiveElement()