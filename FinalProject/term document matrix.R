library(tm)
library(readr)
library(stringr)
library(jiebaR)
library(tidyverse)
library(wordcloud2)
options(stringsAsFactors = F)

data <- lapply(DirSource(".\\all")$file, read.csv)
alldata <- data.frame()

for (i in 1:length(data)){
  subdata <- data[[i]][,2:5]
  alldata <- rbind(alldata, subdata)
}

alldata <- alldata %>%
  filter(is.na(dates) == F) 
alldata <- arrange(alldata, dates)[987:418374,]

titles <- alldata$titles
eng <- worker(stop_word = 'stop_words.txt', user = 'user.utf8')
segment <- eng[titles]
filter <- c('問卦', 'Re', '新聞', '八卦', '爆卦', '嗎', '什麼', '有沒有', '是不是', '為', '會', '怎麼', '怎麼辦', 'U', '沒', '該', '到底', '誰', '說', '怎樣', '不會', '為何', '真的', '還是', '讓', '沒有', '一個', '哪個', '這麼', '後', '與', '這', '幹嘛', '這樣', '卻', '一直', '當', '那麼', '時候', '還', '請問', '不能', '覺得', '感覺', '很多', '前', '個', '爆', '甚麼', '欸', '哪裡', '太', '不要', '一堆' ,'怎辦', '超', '一樣', '其實', '不到', '這個', '竟', '應該', '一定', '這次', '多久', '惹', '更', '最大', '怎麼樣', '哪種', '最後', '因為', '著', '先', '已經', '一次', '以前', '次', '你們', '這種', '根本', '了嗎', '多少錢', '的卦', '搞', '逾', '剩', '連', '還會', '別', '裡', '只能')
freq <- filter_segment(segment, filter)
freq <- freq(freq) %>%
  arrange(desc(freq)) %>%
  filter(freq >= 300)
freq <- freq[-1,]

write.csv(freq, 'termfrequency_all.csv')

boomdata <- filter(alldata, push > 99)
boomtitles <- boomdata$titles
boomsegment <- eng[boomtitles]
filter <- c('問卦', 'Re', '新聞', '八卦', '爆卦', '嗎', '什麼', '有沒有', '是不是', '為', '會', '怎麼', '怎麼辦', 'U', '沒', '該', '到底', '誰', '說', '怎樣', '不會', '為何', '真的', '還是', '讓', '沒有', '一個', '哪個', '這麼', '後', '與', '這', '幹嘛', '這樣', '卻', '一直', '當', '那麼', '時候', '還', '請問', '不能', '覺得', '感覺', '很多', '前', '個', '爆', '甚麼', '欸', '哪裡', '太', '不要', '一堆' ,'怎辦', '超', '一樣', '其實', '不到', '這個', '竟', '應該', '一定', '這次', '多久', '惹', '更', '最大', '怎麼樣', '哪種', '最後', '因為', '著', '先', '已經', '一次', '以前', '次', '你們', '這種', '根本', '了嗎', '多少錢', '的卦', '搞', '逾', '剩', '連', '還會', '別', '裡', '只能')
boomfreq <- filter_segment(boomsegment, filter)
boomfreq <- freq(boomfreq) %>%
  arrange(desc(freq)) %>%
  filter(freq >= 10)
boomfreq <- boomfreq[-1,]

wordcloud2(freq)
wordcloud2(boomfreq, size = .8)
