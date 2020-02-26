setwd("D:/付琳/毛坦厂/R纵向分析_高一")
getwd()#查看工作空间
T1CLASS1 <- read.csv("T1-1班.csv")#导入数据
T1CLASS2 <- read.csv("T1-2班.csv")#导入数据
T1CLASS3 <- read.csv("T1-3班.csv")#导入数据
T1CLASS4 <- read.csv("T1-4班.csv")#导入数据
T1CLASS5 <- read.csv("T1-5班.csv")#导入数据
T1CLASS6 <- read.csv("T1-6班.csv")#导入数据
T1CLASS7 <- read.csv("T1-7班.csv")#导入数据
T1CLASS13 <- read.csv("T1-13班.csv")#导入数据
T2CLASS1 <- read.csv("T2-1班.csv")#导入数据
T2CLASS2 <- read.csv("T2-2班.csv")#导入数据
T2CLASS3 <- read.csv("T2-3班.csv")#导入数据
T2CLASS4 <- read.csv("T2-4班.csv")#导入数据
T2CLASS5 <- read.csv("T2-5班.csv")#导入数据
T2CLASS6 <- read.csv("T2-6班.csv")#导入数据
T2CLASS13 <- read.csv("T2-13班.csv")#导入数据
T2CLASS15 <- read.csv("T2-15班.csv")#导入数据
T3CLASS1 <- read.csv("T3-1班.csv")#导入数据
T3CLASS2 <- read.csv("T3-2班.csv")#导入数据
T3CLASS3 <- read.csv("T3-3班.csv")#导入数据
T3CLASS4 <- read.csv("T3-4班.csv")#导入数据
T3CLASS5 <- read.csv("T3-5班.csv")#导入数据
T3CLASS6 <- read.csv("T3-6班.csv")#导入数据
T3CLASS13 <- read.csv("T3-13班.csv")#导入数据
T3CLASS15 <- read.csv("T3-15班.csv")#导入数据

install.packages("dplyr")  #安装包
library(dplyr)#加载包

T1CLASS13 <- filter(T1CLASS13,Z03!="") #删去空值的行

#提取某几列变量作为对象（如果一开始不加序号的话，提取列的序号要重新数）
提名1 <- T1CLASS13[,c(4,76,84,92,100,108,116,124)]
提名2 <- T1CLASS13[,c(4,77,85,93,101,109,117,125)]
提名3 <- T1CLASS13[,c(4,78,86,94,102,110,118,126)]
提名4 <- T1CLASS13[,c(4,79,87,95,103,111,119,127)]
提名5 <- T1CLASS13[,c(4,80,88,96,104,112,120,128)]

#将不同数据框改成相同的名字以便合并（若不加序号，则把第一个变量名称from_order删去）
colnames(提名1)<-c("from","to","gender","friends","similar","study","talk","hangout")
colnames(提名2)<-c("from","to","gender","friends","similar","study","talk","hangout")
colnames(提名3)<-c("from","to","gender","friends","similar","study","talk","hangout")
colnames(提名4)<-c("from","to","gender","friends","similar","study","talk","hangout")
colnames(提名5)<-c("from","to","gender","friends","similar","study","talk","hangout")

#合并
提名_all<-rbind(提名1,提名2,提名3,提名4,提名5)

#筛选to值不等于空值
library("dplyr")
提名_all <- filter(提名_all,to!="")#抽取朋友网络
emotion1 <- filter(提名_all,talk=="1")#抽取情感网络1
emotion2 <- filter(提名_all,hangout=="1")#抽取情感网络2
instrument <- filter(提名_all,study=="1")#抽取工具网络

#匹配顺序 排序名字
T1CLASS13<-mutate(T1CLASS13,Name_order=factor(姓名,order=T,levels=姓名)) #固定名字顺序
T1CLASS13$Name_order<-as.numeric(T1CLASS13$Name_order) #将顺序转换成数字


A<-T1CLASS13$姓名 #起名

#朋友网络邻接矩阵
提名_all<-mutate(提名_all,from_order=as.numeric(factor(from,levels=A,order=T))) #根据固定顺序匹配序号
提名_all<-mutate(提名_all,to_order=as.numeric(factor(to,levels=A,order=T))) #根据固定顺序匹配序号
提名_all<-mutate(提名_all,tie=1)
提名_all.copy <- 提名_all[, 9:11]
提名_all.copy <- filter(提名_all.copy,is.na(to_order)==FALSE)#删除异常值
提名_all.copy <- as.matrix(提名_all.copy)
write.table(adj,file = 'T1CLASS13friend.txt',sep = ' ',row.names = FALSE,col.names = FALSE)

#情感网络1邻接矩阵
emotion1<-mutate(emotion1,from_order=as.numeric(factor(from,levels=A,order=T))) #根据固定顺序匹配序号
emotion1<-mutate(emotion1,to_order=as.numeric(factor(to,levels=A,order=T))) #根据固定顺序匹配序号
emotion1<-mutate(emotion1,tie=1)
emotion1.copy <- emotion1[, 9:11]
emotion1.copy <- filter(emotion1.copy,is.na(to_order)==FALSE)#删除异常值
emotion1.copy <- as.matrix(emotion1.copy)
adj <- matrix(0, length(A), length(A))
adj[emotion1.copy[,1:2]] <- emotion1.copy[, 3]
write.table(adj,file = 'T1CLASS13emotion1.txt',sep = ' ',row.names = FALSE,col.names = FALSE)

#情感网络2邻接矩阵
emotion2<-mutate(emotion2,from_order=as.numeric(factor(from,levels=A,order=T))) #根据固定顺序匹配序号
emotion2<-mutate(emotion2,to_order=as.numeric(factor(to,levels=A,order=T))) #根据固定顺序匹配序号
emotion2<-mutate(emotion2,tie=1)
emotion2.copy <- emotion2[, 9:11]
emotion2.copy <- filter(emotion2.copy,is.na(to_order)==FALSE)#删除异常值
emotion2.copy <- as.matrix(emotion2.copy)
adj <- matrix(0, length(A), length(A))
adj[emotion2.copy[,1:2]] <- emotion2.copy[, 3]
write.table(adj,file = 'T1CLASS13emotion2.txt',sep = ' ',row.names = FALSE,col.names = FALSE)

#工具网络邻接矩阵
instrument<-mutate(instrument,from_order=as.numeric(factor(from,levels=A,order=T))) #根据固定顺序匹配序号
instrument<-mutate(instrument,to_order=as.numeric(factor(to,levels=A,order=T))) #根据固定顺序匹配序号
instrument<-mutate(instrument,tie=1)
instrument.copy <- instrument[, 9:11]
instrument.copy <- filter(instrument.copy,is.na(to_order)==FALSE)#删除异常值
instrument.copy <- as.matrix(instrument.copy)
adj <- matrix(0, length(A), length(A))
adj[instrument.copy[,1:2]] <- instrument.copy[, 3]
write.table(adj,file = 'T1CLASS13instrument.txt',sep = ' ',row.names = FALSE,col.names = FALSE)
