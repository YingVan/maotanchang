setwd("D:/付琳/毛坦厂/R纵向分析")
getwd()#查看工作空间
文17T4 <- read.csv("文17T4.csv")#导入数据
#install.packages("dplyr")  #安装包
library(dplyr)#加载包

文17T4 <- filter(文17T4,Z03!="") #删去空值的行

#提取某几列变量作为对象（如果一开始不加序号的话，提取列的序号要重新数）
提名1 <- 文17T4[,c(5,32,40,48,56,64,72,80)]
提名2 <- 文17T4[,c(5,33,41,49,57,65,73,81)]
提名3 <- 文17T4[,c(5,34,42,50,58,66,74,82)]
提名4 <- 文17T4[,c(5,35,43,51,59,67,75,83)]
提名5 <- 文17T4[,c(5,36,44,52,60,68,76,84)]

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
文17T4<-mutate(文17T4,Name_order=factor(Z03,order=T,levels=Z03)) #固定名字顺序
文17T4$Name_order<-as.numeric(文17T4$Name_order) #将顺序转换成数字


A<-文17T4$Z03 #起名

#朋友网络邻接矩阵
提名_all<-mutate(提名_all,from_order=as.numeric(factor(from,levels=A,order=T))) #根据固定顺序匹配序号
提名_all<-mutate(提名_all,to_order=as.numeric(factor(to,levels=A,order=T))) #根据固定顺序匹配序号
提名_all<-mutate(提名_all,tie=1)
提名_all.copy <- 提名_all[, 9:11]
提名_all.copy <- as.matrix(提名_all.copy)
adj <- matrix(0, 143, 143)
adj[提名_all.copy[,1:2]] <- 提名_all.copy[, 3]
write.table(adj,file = '文17T4friend.txt',sep = ' ',row.names = FALSE,col.names = FALSE)

#情感网络1邻接矩阵
emotion1<-mutate(emotion1,from_order=as.numeric(factor(from,levels=A,order=T))) #根据固定顺序匹配序号
emotion1<-mutate(emotion1,to_order=as.numeric(factor(to,levels=A,order=T))) #根据固定顺序匹配序号
emotion1<-mutate(emotion1,tie=1)
emotion1.copy <- emotion1[, 9:11]
emotion1.copy <- as.matrix(emotion1.copy)
adj <- matrix(0, 148, 148)
adj[emotion1.copy[,1:2]] <- emotion1.copy[, 3]
write.table(adj,file = '文17T4emotion1.txt',sep = ' ',row.names = FALSE,col.names = FALSE)

#情感网络2邻接矩阵
emotion2<-mutate(emotion2,from_order=as.numeric(factor(from,levels=A,order=T))) #根据固定顺序匹配序号
emotion2<-mutate(emotion2,to_order=as.numeric(factor(to,levels=A,order=T))) #根据固定顺序匹配序号
emotion2<-mutate(emotion2,tie=1)
emotion2.copy <- emotion2[, 9:11]
emotion2.copy <- as.matrix(emotion2.copy)
adj <- matrix(0, 148, 148)
adj[emotion2.copy[,1:2]] <- emotion2.copy[, 3]
write.table(adj,file = '文17T4emotion2.txt',sep = ' ',row.names = FALSE,col.names = FALSE)

#工具网络邻接矩阵
instrument<-mutate(instrument,from_order=as.numeric(factor(from,levels=A,order=T))) #根据固定顺序匹配序号
instrument<-mutate(instrument,to_order=as.numeric(factor(to,levels=A,order=T))) #根据固定顺序匹配序号
instrument<-mutate(instrument,tie=1)
instrument.copy <- instrument[, 9:11]
instrument.copy <- as.matrix(instrument.copy)
adj <- matrix(0, 148, 148)
adj[instrument.copy[,1:2]] <- instrument.copy[, 3]
write.table(adj,file = '文17T4instrument.txt',sep = ' ',row.names = FALSE,col.names = FALSE)
