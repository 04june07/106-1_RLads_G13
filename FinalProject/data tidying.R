library(rjson)
library(tidyverse)
library(stringr)

filename = '' 
test <- fromJSON(file = paste(filename, '.json', sep = ""))
titles <-  c()
dates <- c()
contents <- c()
push <- c()
id <- c()

for (i in (1:length(test$articles))){
  if (is.null(test$articles[[i]]$article_title) == F){
  a <- test$articles[[i]]$article_title
  titles <- c(titles, a)
  b <- test$articles[[i]]$date
  dates <- c(dates, b)
  c <- test$articles[[i]]$content
  contents <- c(contents, c)
  d <- test$articles[[i]]$message_conut$push
  push <- c(push, d)
  e <- test$articles[[i]]$article_id
  id <- c(id, e)
  } else next
}

test_df <- cbind.data.frame(dates, titles, contents, push, id) 
write.csv(test_df,paste(filename,".csv",sep = ""))

test_df_boom <- filter(test_df, push > 99 )
write.csv(test_df_boom,paste(filename, "-Boom.csv", sep = ""))


