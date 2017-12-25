library(rjson)
library(tidyverse)
library(stringr)
lct <- Sys.getlocale("LC_TIME"); Sys.setlocale("LC_TIME", "C")
Sys.setlocale("LC_TIME", lct)

filename = '' 
test <- fromJSON(file = paste(filename, '.json', sep = ""))
titles <-  c()
dates <- c()
push <- c()
boo <- c()
id <- c()

for (i in (1:length(test$articles))){
  if (is.null(test$articles[[i]]$article_title) == F){
  a <- test$articles[[i]]$article_title
  titles <- c(titles, a)
  b <- test$articles[[i]]$date
  dates <- c(dates, b)
  d <- test$articles[[i]]$message_conut$push
  push <- c(push, d)
  e <- test$articles[[i]]$message_conut$boo
  boo <- c(boo, e)
  f <- test$articles[[i]]$article_id
  id <- c(id, f)
  } else next
}

dates <- as.Date(str_sub(dates, 5, -15), "%b%d")

test_df <- cbind.data.frame(dates, titles, push = (push-boo), id) 
write.csv(test_df,paste(filename,".csv",sep = ""))


test_df_boom <- filter(test_df, push > 99 )
write.csv(test_df_boom,paste(filename, "-Boom.csv", sep = ""))
