getCambridgeDic <- function(words){
  require(rvest)
  for(w in 1:length(words)){
    word <- words[w]
    url <- paste0("http://dictionary.cambridge.org/dictionary/english/",
                  word)
    # Get content from page
    html <- read_html(url)
    # Get all definitions
    DefAll <- html %>%
      html_nodes("#british-1-1-1 .def-head") %>%
      html_text()
    # Get all examples
    ExAll <- html %>%
      html_nodes("#british-1-1-1 .def-body") %>%
      html_text()
    
  }
  
}