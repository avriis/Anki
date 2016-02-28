




vars <- c("this", "is", "is", "text")
str(vars)
paste(unique(x), collapse = ', ')
paste(vars,sep = "",collapse = ", ")

str(SynOpslag)

na.omit(SynOpslag)


x <- SynOpslag[SynOpslag != ""]

str(x)

str <- "How do I best try and try and try and find a way to to improve this code?"

# For flytte
word <- "flytte"
retning <- "dafr"
# > length(senses)
# [1] 2
# > i <- 1
# > length(senses[[i]]$subSenses[[1]]$annotatedTargets)
# [1] 4
# > i <- 2
# > length(senses[[i]]$subSenses[[1]]$annotatedTargets)
# [1] 2
i <- 1; ii <- 1 # OK
i <- 1; ii <- 2 # OK
i <- 1; ii <- 3 # Fejl!
i <- 1; ii <- 4 # 
i <- 2; ii <- 1 # 
i <- 2; ii <- 2 # 

# This test is wrong!
!is.list(senses[[i]]$subSenses[[1]]$targetGroups[[1]]$annotatedTargets[[ii]])

length(senses[[i]]$subSenses[[1]]$targetGroups[[1]]$annotatedTargets)
length(senses[[i]]$subSenses[[1]]$targetGroups[[1]]$annotatedTargets)
str(senses[[i]]$subSenses[[1]]$annotatedTargets)


str(senses[[i]]$subSenses[[1]]$targetGroups)



is.list(
  senses[[i]]$subSenses[[1]]$targetGroups[[1]]$annotatedTargets[[ii]]
)

senses[[i]]$subSenses[[1]]$targetGroups[[1]]$annotatedTargets[[ii]]$translation