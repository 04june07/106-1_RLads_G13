##########################################
# Data input and cleansing
##########################################
top10 = read.csv("d:/data/top10.csv")
top10 = top10[,-1]
top10$dates = as.character(top10$dates)
top10$titles = as.character(top10$titles)
top10$id = as.character(top10$id)


##########################################
# 各事件的文章數目

table(top10$topic)

# 815大停電 中國新歌聲     世大運 加泰隆尼亞       妙禪   前瞻計畫 
#       148        561       4668        444       1315        954 
#    徐乃麟 勞基法修法     罷昌案   墾丁觀光 
#       410       3252        864        248 


##########################################################
# 把同一天的文章分別做 mean 與 sum 的彙整
##########################################################
data2 = list()
data3 = list()
for (i in 1:nlevels(top10$topic))
{
   tmp.data = top10[top10$topic == levels(top10$topic)[i],]
   # mean 
   data2[[i]] = tapply(tmp.data$score,tmp.data$dates,mean,na.rm=T)
   # sum 
   data3[[i]] = tapply(tmp.data$score,tmp.data$dates,sum,na.rm=T)
}
	   
	   
#data2[[1]]
#2017-08-15 2017-08-16 2017-08-17 2017-08-18 2017-08-19 2017-08-20 
# 0.4058824  0.3428571  0.3860465  0.4058824  0.5000000  0.5000000 
#2017-08-21 2017-08-22 2017-08-23 2017-08-24 2017-08-25 2017-08-29 
# 0.3545455  0.4428571  0.1000000  0.5000000  0.5000000  0.5000000 
#.........................
#2017-11-11 
# 0.5000000 

# 各事件的不同日期數
ndays = sapply(data2,length)
#> ndays
# [1]  25  31 122  53 116  87  22 145  83  28

 

 
# 2017/07/01 ~ 2017/12/31
timeRange =seq(as.Date("2017-07-01"), as.Date("2017-12-31"), by="1 day")
#length(timeRange)
#[1] 184
#head(timeRange)
#[1] "2017-07-01" "2017-07-02" "2017-07-03" "2017-07-04" "2017-07-05"
#[6] "2017-07-06"

# 把上面已經彙整的 data2, data3 資料紀錄到 2017/07/01 ~ 2017/12/31 
# 的 score 矩陣 M : 共有 184 rows(代表184天), 10 columns(代表10事件)
M = matrix(0,length(timeRange),10)
#> dim(M)
#[1] 184  10

colnames(M) = levels(top10$topic)
xdata2 = xdata3 = data.frame(M)
rownames(xdata2)= rownames(xdata3) = timeRange

for (i in 1:nlevels(top10$topic))
{
   tmpDates = names(data2[[i]])
   xdata2[tmpDates,i] = data2[[i]]  
   xdata3[tmpDates,i] = data3[[i]]
}

######################################################
# 畫出各事件平均分數(mean of scores) 的趨勢圖
######################################################
jpeg("d:/steve/diane/mean_trend.jpg",width=2000,height=1400,res=300)
oldpar = par()
par(mai=c(0.3,0.4,0.2,0.4),mfrow=c(5,2))
for (i in 1:length(data2))
{
	tmp.time = as.Date(rownames(xdata2))
	tmp.score = xdata2[,i]
	plot(tmp.time,tmp.score,type="l",xaxt="n",
	     ylim=c(0,1.4),
		 xlim=c(as.Date("2017/07/01"), as.Date("2017/12/31")),
	     main=levels(top10$topic)[i],xlab="",ylab="",col=2,lwd=1.5)
	axis.Date(1, at = seq(as.Date("2017/07/01"), as.Date("2017/12/31"), "1 month"),
	   format="%m/%d")
}
par(oldpar)
dev.off()

######################################################
# 畫出各事件總和分數(sum of scores)的趨勢圖
######################################################
jpeg("d:/steve/diane/data3_trend.jpg",width=2000,height=1400,res=300)
oldpar = par()
par(mai=c(0.3,0.4,0.2,0.4),mfrow=c(5,2))
xdata.max = max(xdata3)
for (i in 1:length(data3))
{
	tmp.time = as.Date(rownames(xdata))
	tmp.score = xdata[,i]
	plot(tmp.time,tmp.score,type="l",xaxt="n",
		 ylim=c(0,xdata.max),
		 xlim=c(as.Date("2017/07/01"), as.Date("2017/12/31")),
	     main=levels(top10$topic)[i],xlab="",ylab="",col=2,lwd=1.5)
	axis.Date(1, at = seq(as.Date("2017/07/01"), as.Date("2017/12/31"), "1 month"),
	   format="%m/%d")
}
par(oldpar)
dev.off()

######################################################
# 畫出各事件平均分數的直方圖
######################################################

xdata2 = t(as.matrix(xdata2))

breaks = seq(0,1.0,0.1)
oldpar = par()
par(mai=rep(0.51,4),mfrow=c(5,2))
for (i in 1:nrow(xdata2))
{ 
  tmp = xdata2[i,]
  tmp = tmp[tmp <= 1]
  hist(tmp,breaks=breaks,main=levels(top10$topic)[i],
  xlab="score",ylab="percentage",freq=F,ylim=c(0,10),col=rainbow(20))
}
par(oldpar)

##########################################################
# 使用 kml 套件做縱貫資料的集群分析: 
##########################################################
library(kml)

tmp = clusterLongData(xdata2)
kml(tmp,2:4,toPlot="traj") # 测试 2 群到 4 群,畫出趨勢線圖形
kml(tmp,2:4,toPlot="criterion") # 测试 2 群到 4 群,畫出篩選指標圖形

# 根據不同的指標，動態選擇最佳分群數: 4
# 執行 choice(tmp) 時，可按 d 查看不同的分群數指標圖形
choice(tmp)


#====================================
# 分成 4 群 (best number of clusters)

clusters4=getClusters(tmp,4)
clusters4 = as.character(clusters4)
names(clusters4)=levels(top10$topic)

clusters4

# 815大停電 中國新歌聲     世大運 加泰隆尼亞       妙禪   前瞻計畫 
#       "A"        "A"        "B"        "A"        "C"        "B" 
#    徐乃麟 勞基法修法     罷昌案   墾丁觀光 
#       "A"        "C"        "D"        "A" 

#######################################################
# 使用 dtwclust 套件做縱貫資料的集群分析 
#######################################################
library(dtwclust)

# 使用 dtwclust 套件做階層式集群分析，

resultH = tsclust(xdata2,type = "hierarchical",k = 10)
plot(resultH,xlab="topic",sub="")


