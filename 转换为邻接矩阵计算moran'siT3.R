setwd("D:/付琳/毛坦厂/R纵向分析_复读/R输入数据")
getwd()#查看工作空间
理04T3 <- read.csv("理04T3.csv")#导入数据
理11T3 <- read.csv("理11T3.csv")#导入数据
理27T3 <- read.csv("理27T3.csv")#导入数据
理35T3 <- read.csv("理35T3.csv")#导入数据
文07T3 <- read.csv("文07T3.csv")#导入数据
文08T3 <- read.csv("文08T3.csv")#导入数据
文17T3 <- read.csv("文17T3.csv")#导入数据
#install.packages("dplyr")  #安装包
library(dplyr)#加载包

文17T3 <- filter(文17T3,Z03!="") #删去空值的行

#提取某几列变量作为对象（如果一开始不加序号的话，提取列的序号要重新数）
classesT3 <- rbind(理04T3,理11T3,理27T3,理35T3,文07T3,文08T3,文17T3)
提名1 <- classesT3[,c(5,72,80,88,96,104,112,120)]
提名2 <- classesT3[,c(5,73,81,89,97,105,113,121)]
提名3 <- classesT3[,c(5,74,82,90,98,106,114,122)]
提名4 <- classesT3[,c(5,75,83,91,99,107,115,123)]
提名5 <- classesT3[,c(5,76,84,92,100,108,116,124)]

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
提名_all <- filter(提名_all,to!="")
emotion1 <- filter(提名_all,talk=="1")#抽取情感网络1
emotion2 <- filter(提名_all,hangout=="1")#抽取情感网络2
instrument <- filter(提名_all,study=="1")#抽取工具网络

#匹配顺序 排序名字
文17T3<-mutate(文17T3,Name_order=factor(Z03,order=T,levels=Z03)) #固定名字顺序
文17T3$Name_order<-as.numeric(文17T3$Name_order) #将顺序转换成数字


A<-文17T3$Z03 #起名

#朋友网络邻接矩阵
提名_all<-mutate(提名_all,from_order=as.numeric(factor(from,levels=A,order=T))) #根据固定顺序匹配序号
提名_all<-mutate(提名_all,to_order=as.numeric(factor(to,levels=A,order=T))) #根据固定顺序匹配序号
提名_all<-mutate(提名_all,tie=1)
提名_all.copy <- 提名_all[, 9:11]
提名_all.copy <- as.matrix(提名_all.copy)
adj <- matrix(0, 143, 143)
adj[提名_all.copy[,1:2]] <- 提名_all.copy[, 3]
write.table(adj,file = '文17T3friend.txt',sep = ' ',row.names = FALSE,col.names = FALSE)

#情感网络1邻接矩阵
emotion1<-mutate(emotion1,from_order=as.numeric(factor(from,levels=A,order=T))) #根据固定顺序匹配序号
emotion1<-mutate(emotion1,to_order=as.numeric(factor(to,levels=A,order=T))) #根据固定顺序匹配序号
emotion1<-mutate(emotion1,tie=1)
emotion1.copy <- emotion1[, 9:11]
emotion1.copy <- as.matrix(emotion1.copy)
adj <- matrix(0, 148, 148)
adj[emotion1.copy[,1:2]] <- emotion1.copy[, 3]
write.table(adj,file = '文17T3emotion1.txt',sep = ' ',row.names = FALSE,col.names = FALSE)

#情感网络2邻接矩阵
emotion2<-mutate(emotion2,from_order=as.numeric(factor(from,levels=A,order=T))) #根据固定顺序匹配序号
emotion2<-mutate(emotion2,to_order=as.numeric(factor(to,levels=A,order=T))) #根据固定顺序匹配序号
emotion2<-mutate(emotion2,tie=1)
emotion2.copy <- emotion2[, 9:11]
emotion2.copy <- as.matrix(emotion2.copy)
adj <- matrix(0, 148, 148)
adj[emotion2.copy[,1:2]] <- emotion2.copy[, 3]
write.table(adj,file = '文17T3emotion2.txt',sep = ' ',row.names = FALSE,col.names = FALSE)

#工具网络邻接矩阵
instrument<-mutate(instrument,from_order=as.numeric(factor(from,levels=A,order=T))) #根据固定顺序匹配序号
instrument<-mutate(instrument,to_order=as.numeric(factor(to,levels=A,order=T))) #根据固定顺序匹配序号
instrument<-mutate(instrument,tie=1)
instrument.copy <- instrument[, 9:11]
instrument.copy <- as.matrix(instrument.copy)
adj <- matrix(0, 148, 148)
adj[instrument.copy[,1:2]] <- instrument.copy[, 3]
write.table(adj,file = '文17T3instrument.txt',sep = ' ',row.names = FALSE,col.names = FALSE)
