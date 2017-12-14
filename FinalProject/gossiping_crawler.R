
# environment
library(RCurl)
library(httr)
library(XML)
library(dplyr)

# under 18 cookie
curl <- getCurlHandle()
curlSetOpt(cookie="over18=1", followlocation = TRUE, curl=curl)

# 開始爬蟲
start.no = 28883  #開始的頁面編號
end.no = 28683    #結束的頁面編號，建議一次爬200頁左右
subpath = 'https://www.ptt.cc/bbs/Gossiping/index'
alldata = data.frame()

for (id in (start.no:end.no)){
  url <- paste(subpath,id,".html",sep = "")
  html <- getURL(url, curl = curl, encoding = "big5") # mac的encoding可能要改成UTF-8
  xmldoc <- htmlParse(html)
  title <- xpathSApply(xmldoc, "//div[@class=\"title\"]", xmlValue)
  date <- xpathSApply(xmldoc, "//div[@class=\"date\"]", xmlValue)
  push <- xpathSApply(xmldoc, "//div[@class=\"nrec\"]", xmlValue)
  subdata <- data.frame(title, date, push)
  alldata <- rbind(alldata, subdata)
}

#到這裡跑完會出現一個alldata的data frame，內容有標題、日期、推文數


#篩選爆文&存成csv
boom <- alldata %>%
  filter(push == "爆") 

filename <- paste("Gossiping_", end.no, "-", start.no, ".csv", sep = "")
write.csv(boom,filename)

#到這裡為止在working directory會出現一個叫做"Gossiping_(start-end).csv"的檔案

#用下面這行試試看能不能成功讀取，如果出現和剛剛的boom一樣的data frame 就成功了！
test <- read.csv("檔名")
test
