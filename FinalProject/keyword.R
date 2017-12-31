library(tm)
library(readr)
library(tidyverse)
library(wordcloud2)
library(plotly)

#data <- lapply(DirSource(".\\all")$file, read.csv)
#alldata <- data.frame()
#for (i in 1:length(data)){
#    subdata <- data[[i]][,2:5]
#    alldata <- rbind(alldata, subdata)
#} 
#write.csv(alldata, 'Gossiping-0701-1224.csv')

alldata <- read.csv('Gossiping-0701-1224.csv')[,2:5] 
alldata <- arrange(alldata, dates)[987:418374,]
alldata$titles = as.character(alldata$titles)
alldata$dates = as.Date(alldata$dates, "%Y-%m-%d")

mask <- function(a){sapply(lapply(alldata$titles, a), 
                           function(i){i[[1]][1] != -1})}

# 勞基法

greg_1 <- function(x){
  result <- gregexpr('勞基法', x)
  return(result)
}
greg_2 <- function(x){
  result <- gregexpr('七休一', x)
  return(result)
}
greg_3 <- function(x){
  result <- gregexpr('勞動部', x)
  return(result)
}
greg_4 <- function(x){
  result <- gregexpr('勞資',x)
  return(result)
}
greg_5 <- function(x){
  result <- gregexpr('一例一休',x)
  return(result)
}
greg_6 <- function(x){
  result <- gregexpr('修法',x)
  return(result)
}

labor_1 <- mask(greg_1)
labor_2 <- mask(greg_2)
labor_3 <- mask(greg_3)
labor_4 <- mask(greg_4)
labor_5 <- mask(greg_5)
labor_6 <- mask(greg_6)

score <- function(x){cut(x$push, c(-300, 0 , 50, 100, 200, 300, 400, 500, 1060), 
             labels = c(0.1, 0.5, 1, 2, 3, 4, 5, 6))}
score2 <- function(x){cut(x$push, c(-300, 0 , 50, 100, 200, 300, 400, 500, 1060), 
                         labels = c(0.1, 0.5, 1, 1.5, 2, 2.5, 3, 3.5))}
fun <- function(x){x %>% 
  mutate(score = as.numeric(as.character(score(x)))) %>%
  mutate(score2 = as.numeric(as.character(score2(x)))) %>%
  group_by(dates) %>%
  summarise(score = sum(score), score2 = sum(score2)) %>%
  ungroup()}


labor_data <- subset(alldata, (labor_1|labor_2|labor_3|labor_4|(labor_5&labor_6))) %>%
  mutate(topic = as.factor('勞基法修法')) %>%
  mutate(theme = as.factor('政治'))
labor_data2 <- fun(labor_data) %>%
  mutate(topic = as.factor('勞基法修法')) %>%
  mutate(theme = as.factor('政治'))

  
labor_plot <- ggplot(labor_data2, aes(x = dates, y = score)) + 
  geom_line(aes(group = 1), color = 'red') +
  scale_x_date(date_breaks = '1 month', date_labels = '%y/%m') +
  ylim(0, 110) +
  ggtitle('2017下半年「勞基法修法」相關議題熱門度變化') 


#labor_plot2 <- ggplot(labor_data2, aes(x = dates, y = score2)) +
#  geom_line(aes(group = 1), color = 'red') +
#  scale_x_date(date_breaks = '1 month', date_labels = '%y/%m') +
#  ylim(0, 110) +
#  ggtitle('2017下半年「勞基法修法」相關議題熱門度變化') 

png('labor.png', width = 1024, height = 768)
labor_plot
dev.off()


# 中國新歌聲

greg_7 <- function(x){
  result <- gregexpr('新歌聲',x)
  return(result)
}
greg_8 <- function(x){
  result <- gregexpr('台大',x)
  return(result)
}
greg_9 <- function(x){
  result <- gregexpr('操場',x)
  return(result)
}
greg_10 <- function(x){
  result <- gregexpr('甩棍阿伯',x)
  return(result)
}
greg_11 <- function(x){
  result <- gregexpr('愛國同心會',x)
  return(result)
}

ntu_1 <- mask(greg_7)
ntu_2 <- mask(greg_8)
ntu_3 <- mask(greg_9)
ntu_4 <- mask(greg_10)
ntu_5 <- mask(greg_11)

ntu_data <- subset(alldata, ntu_1|(ntu_2&ntu_3)|ntu_4|ntu_5) %>%
  mutate(topic = as.factor('中國新歌聲')) %>%
  mutate(theme = as.factor('政治'))
ntu_data2 <- fun(ntu_data) %>%
  mutate(topic = as.factor('中國新歌聲')) %>%
  mutate(theme = as.factor('政治'))


ntu_plot <- ggplot(ntu_data2, aes(x = dates, y = score))+
  geom_line(aes(group = 1), color = 'red') +
  scale_x_date(date_breaks = '1 month', date_labels = '%y/%m') +
  ylim(0,110) +
  ggtitle('2017下半年「《中國新歌聲》台大衝突事件」相關議題熱門度變化') 

png('ntu.png', width = 1024, height = 768)
ntu_plot
dev.off()

# 黃國昌罷免案

greg_12 <- function(x){
  result <- gregexpr('罷免',x)
  return(result)
}
greg_13 <- function(x){
  result <- gregexpr('黃國昌',x)
  return(result)
}
greg_14 <- function(x){
  result <- gregexpr('罷昌',x)
  return(result)
}
greg_15 <- function(x){
  result <- gregexpr('汐止',x)
  return(result)
}
greg_16 <- function(x){
  result <- gregexpr('智',x)
  return(result)
}

huang_1 <- mask(greg_12)
huang_2 <- mask(greg_13)
huang_3 <- mask(greg_14)
huang_4 <- mask(greg_15)
huang_5 <- mask(greg_16)

huang_data <- subset(alldata, ((huang_1&huang_2)|huang_3|(huang_4&huang_5))) %>%
  mutate(topic = as.factor('罷昌案')) %>%
  mutate(theme = as.factor('政治'))
huang_data2 <- fun(huang_data) %>%
  mutate(topic = as.factor('罷昌案')) %>%
  mutate(theme = as.factor('政治'))


huang_plot <- ggplot(huang_data2, aes(x = dates, y = score))+
  geom_line(aes(group = 1), color = 'red') +
  scale_x_date(date_breaks = '1 month', date_labels = '%y/%m') +
  ylim(0,110) +
  ggtitle('2017下半年「黃國昌罷免案」相關議題熱門度變化')

png('huang.png', width = 1024, height = 768)
huang_plot
dev.off()


# 前瞻計畫

greg_17 <- function(x){
  result <- gregexpr('前瞻', x)
  return(result)
}
greg_18 <- function(x){
  result <- gregexpr('8800億', x)
  return(result)
}

forward_1 <- mask(greg_17)
forward_2 <- mask(greg_18)

forward_data <- subset(alldata, forward_1|forward_2) %>%
  mutate(topic = as.factor('前瞻計畫')) %>%
  mutate(theme = as.factor('政治'))
forward_data2 <- fun(forward_data) %>%
  mutate(topic = as.factor('前瞻計畫')) %>%
  mutate(theme = as.factor('政治'))


forward_plot <- ggplot(forward_data2, aes(x = dates, y = score))+
  geom_line(aes(group = 1), color = 'red') +
  scale_x_date(date_breaks = '1 month', date_labels = '%y/%m') +
  ylim(0,110) +
  ggtitle('2017下半年「前瞻計畫」相關議題熱門度變化')

png('forward.png', width = 1024, height = 768)
forward_plot
dev.off()


# 墾丁觀光

greg_19 <- function(x){
  result <- gregexpr('墾丁', x)
  return(result)
}
greg_20 <- function(x){
  result <- gregexpr('觀光', x)
  return(result)
}
greg_21 <- function(x){
  result <- gregexpr('陸客',x) 
  return(result)
}
greg_22 <- function(x){
  result <- gregexpr('遊客',x)
  return(result)
}
greg_23 <- function(x){
  result <- gregexpr('沖繩', x)
  return(result)
}
greg_24 <- function(x){
  result <- gregexpr('滷味',x)
  return(result)
}

kt_1 <- mask(greg_19)
kt_2 <- mask(greg_20)
kt_3 <- mask(greg_21)
kt_4 <- mask(greg_22)
kt_5 <- mask(greg_23)
kt_6 <- mask(greg_24)

kt_data <- subset(alldata, !(kt_1 & kt_6) & ((kt_1&kt_2)|(kt_1&kt_3)|(kt_1&kt_4)|(kt_1&kt_5))) %>%
  mutate(topic = as.factor('墾丁觀光')) %>%
  mutate(theme = as.factor('社會'))
kt_data2 <- fun(kt_data) %>%
  mutate(topic = as.factor('墾丁觀光')) %>%
  mutate(theme = as.factor('社會'))


kt_plot <- ggplot(kt_data2, aes(x = dates, y = score))+
  geom_line(aes(group = 1), color = 'red') +
  scale_x_date(date_breaks = '1 month', date_labels = '%y/%m') +
  ylim(0,110) +
  ggtitle('2017下半年「墾丁觀光人數銳減」相關議題熱門度變化')

png('kt.png', width = 1024, height = 768)
kt_plot
dev.off()


# 815大停電

greg_25 <- function(x){
  result <- gregexpr('停電', x)
  return(result)
}
greg_26 <- function(x){
  result <- gregexpr('815', x)
  return(result)
}
greg_27 <- function(x){
  result <- gregexpr('跳電',x) 
  return(result)
}
greg_28 <- function(x){
  result <- gregexpr('蔡總統',x)
  return(result)
}
greg_29 <- function(x){
  result <- gregexpr('林全', x)
  return(result)
}

power_1 <- mask(greg_25)
power_2 <- mask(greg_26)
power_3 <- mask(greg_27)
power_4 <- mask(greg_28)
power_5 <- mask(greg_29)

power_data <- subset(alldata, (power_1&power_2)|(power_2&power_3)|(power_1&power_4)|(power_1&power_5)) %>%
  mutate(topic = as.factor('815大停電')) %>%
  mutate(theme = as.factor('社會'))
power_data2 <- fun(power_data) %>%
  mutate(topic = as.factor('815大停電')) %>%
  mutate(theme = as.factor('社會'))


power_plot <- ggplot(power_data2, aes(x = dates, y = score))+
  geom_line(aes(group = 1), color = 'red') +
  scale_x_date(date_breaks = '1 month', date_labels = '%y/%m') +
  ylim(0,110) +
  ggtitle('2017下半年「815大停電」相關議題熱門度變化')

png('power815.png', width = 1024, height = 768)
power_plot
dev.off()


# 乃哥

greg_30 <- function(x){
  result <- gregexpr('徐乃麟', x)
  return(result)
}
greg_31 <- function(x){
  result <- gregexpr('乃哥', x)
  return(result)
}
greg_32 <- function(x){
  result <- gregexpr('唐從聖',x)
  return(result)
}
greg_33 <- function(x){
  result <- gregexpr('從從',x)
  return(result)
}
greg_34 <- function(x){
  result <- gregexpr('罵',x)
  return(result)
}

hsu_1 <- mask(greg_30)
hsu_2 <- mask(greg_31)
hsu_3 <- mask(greg_32)
hsu_4 <- mask(greg_33)
hsu_5 <- mask(greg_34) 

hsu_data <- subset(alldata, ((hsu_1|hsu_2)&hsu_3)|((hsu_1|hsu_2)&hsu_4)|((hsu_1|hsu_2)&hsu_5)) %>%
  mutate(topic = as.factor('徐乃麟')) %>%
  mutate(theme = as.factor('娛樂'))
hsu_data2 <- fun(hsu_data) %>%
  mutate(topic = as.factor('徐乃麟')) %>%
  mutate(theme = as.factor('娛樂'))


hsu_plot <- ggplot(hsu_data2, aes(x = dates, y = score))+
  geom_line(aes(group = 1), color = 'red') +
  scale_x_date(date_breaks = '1 month', date_labels = '%y/%m', limits = as.Date(c('2017/07','2017/12'), '%Y/%m')) +
  ylim(0,110) +
  ggtitle('2017下半年「徐乃麟國罵事件」相關議題熱門度變化')

png('hsu.png', width = 1024, height = 768)
hsu_plot
dev.off()


# 妙禪

greg_35 <- function(x){
  result <- gregexpr('妙禪', x)
  return(result)
}
greg_36 <- function(x){
  result <- gregexpr('seafood', x)
  return(result)
}
greg_37 <- function(x){
  result <- gregexpr('如來宗',x)
  return(result)
}


seafood_1 <- mask(greg_35)
seafood_2 <- mask(greg_36)
seafood_3 <- mask(greg_37)

seafood_data <- subset(alldata, seafood_1|seafood_3) %>%
  mutate(topic = as.factor('妙禪')) %>%
  mutate(theme = as.factor('娛樂'))
seafood_data2 <- fun(seafood_data) %>%
  mutate(topic = as.factor('妙禪')) %>%
  mutate(theme = as.factor('娛樂'))


seafood_plot <- ggplot(seafood_data2, aes(x = dates, y = score))+
  geom_line(aes(group = 1), color = 'red') +
  scale_x_date(date_breaks = '1 month', date_labels = '%y/%m', limits = as.Date(c('2017/07','2017/12'), '%Y/%m')) +
  ylim(0,110) +
  ggtitle('2017下半年「如來宗妙禪」相關議題熱門度變化')

png('seafood.png', width = 1024, height = 768)
seafood_plot
dev.off()


# 世大運

greg_38 <- function(x){
  result <- gregexpr('世大運', x)
  return(result)
}
greg_39 <- function(x){
  result <- gregexpr('女排')
  return(result)
}
greg_40 <- function(x){
  result <- gregexpr('男排',x)
  return(result)
}
greg_41 <- function(x){
  result <- gregexpr('奪牌',x)
  return(result)
}
greg_42 <- function(x){
  result <- gregexpr()
}

uni_1 <- mask(greg_38)
uni_2 <- mask(greg_41)

uni_data <- subset(alldata, uni_1|uni_2) %>%
  mutate(topic = as.factor('世大運')) %>%
  mutate(theme = as.factor('重大活動'))
uni_data2 <- fun(uni_data) %>%
  mutate(topic = as.factor('世大運')) %>%
  mutate(theme = as.factor('重大活動'))


uni_plot <- ggplot(uni_data2, aes(x = dates, y = score))+
  geom_line(aes(group = 1), color = 'red') +
  scale_x_date(date_breaks = '1 month', date_labels = '%y/%m', limits = as.Date(c('2017/07','2017/12'), '%Y/%m')) +
  ylim(0,110) +
  ggtitle('2017下半年「世大運」相關議題熱門度變化')

png('uni.png', width = 1024, height = 768)
uni_plot
dev.off()


# 加泰隆尼亞獨立公投

greg_43 <- function(x){
  result <- gregexpr('加泰隆尼亞', x)
  return(result)
}

cata_1 <- mask(greg_43)

cata_data <- subset(alldata, cata_1) %>%
  mutate(topic = as.factor('加泰隆尼亞')) %>%
  mutate(theme = as.factor('國際'))
cata_data2 <- fun(cata_data) %>%
  mutate(topic = as.factor('加泰隆尼亞')) %>%
  mutate(theme = as.factor('國際'))


cata_plot <- ggplot(cata_data2, aes(x = dates, y = score))+
  geom_line(aes(group = 1), color = 'red') +
  scale_x_date(date_breaks = '1 month', date_labels = '%y/%m', limits = as.Date(c('2017/07','2017/12'), '%Y/%m')) +
  ylim(0,110) +
  ggtitle('2017下半年「加泰隆尼亞獨立」相關議題熱門度變化')

png('cata.png', width = 1024, height = 768)
cata_plot
dev.off()


# 分類統計
bigdata <- rbind(labor_data, ntu_data, huang_data, forward_data, kt_data, power_data, hsu_data, seafood_data, uni_data, cata_data)
bigdata$score = as.numeric(as.character(score(bigdata)))
bigdata$score2 = as.numeric(as.character(score2(bigdata)))
write.csv(bigdata, "top ten issues.csv")

bigdata2 <- rbind(labor_data2, ntu_data2, huang_data2, forward_data2, kt_data2, power_data2, hsu_data2, seafood_data2, uni_data2, cata_data2)


politics <- filter(bigdata2, theme == '政治') 
society <- filter(bigdata2, theme == '社會')
entertainment <- filter(bigdata2, theme == '娛樂')
event <- filter(bigdata2, theme == '重大活動')
international <- filter(bigdata2, theme == '國際')

politics_plot <- ggplot(politics, aes(x = dates, y = score, group = topic))+
  geom_line(aes(color = topic)) +
  scale_x_date(date_breaks = '1 month', date_labels = '%y/%m', limits = as.Date(c('2017/07','2017/12'), '%Y/%m')) +
  ylim(0,110) +
  ggtitle('2017下半年政治類議題熱門度變化')
png('politics.png', width = 1024, height = 768)
politics_plot
dev.off()

society_plot <- ggplot(society, aes(x = dates, y = score, group = topic))+
  geom_line(aes(color = topic)) +
  scale_x_date(date_breaks = '1 month', date_labels = '%y/%m', limits = as.Date(c('2017/07','2017/12'), '%Y/%m')) +
  ylim(0,110) +
  ggtitle('2017下半年社會類議題熱門度變化')
png('society.png', width = 1024, height = 768)
society_plot
dev.off()

entertainment_plot <- ggplot(entertainment, aes(x = dates, y = score, group = topic))+
  geom_line(aes(color = topic)) +
  scale_x_date(date_breaks = '1 month', date_labels = '%y/%m', limits = as.Date(c('2017/07','2017/12'), '%Y/%m')) +
  ylim(0,110) +
  ggtitle('2017下半年娛樂類議題熱門度變化')
png('entertainment.png', width = 1024, height = 768)
entertainment_plot
dev.off()

