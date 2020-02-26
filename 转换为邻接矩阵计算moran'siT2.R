setwd("D:/付琳/毛坦厂/R纵向分析_复读/R输入数据")
getwd()#查看工作空间
文17T2 <- read.csv("文17T2.csv")#导入数据
#install.packages("dplyr")  #安装包
library(dplyr)#加载包

文17T2 <- filter(文17T2,Z03!="") #删去空值的行
文17T2 <-mutate(文17T2,depression=F01+F02+F03+F04+5-F05+F06+F07+5-F08+F09+F10)#计算抑郁总分

#提取某几列变量作为对象（如果一开始不加序号的话，提取列的序号要重新数）
提名1 <- 文17T2[,c(5,69,77,85,93,101,109,117,127)]
提名2 <- 文17T2[,c(5,70,78,86,94,102,110,118,127)]
提名3 <- 文17T2[,c(5,71,79,87,95,103,111,119,127)]
提名4 <- 文17T2[,c(5,72,80,88,96,104,112,120,127)]
提名5 <- 文17T2[,c(5,73,81,89,97,105,113,121,127)]

#将不同数据框改成相同的名字以便合并（若不加序号，则把第一个变量名称from_order删去）
colnames(提名1)<-c("from","to","gender","friends","similar","study","talk","hangout","depression")
colnames(提名2)<-c("from","to","gender","friends","similar","study","talk","hangout","depression")
colnames(提名3)<-c("from","to","gender","friends","similar","study","talk","hangout","depression")
colnames(提名4)<-c("from","to","gender","friends","similar","study","talk","hangout","depression")
colnames(提名5)<-c("from","to","gender","friends","similar","study","talk","hangout","depression")

#合并
提名_all<-rbind(提名1,提名2,提名3,提名4,提名5)

#筛选to值不等于空值
library("dplyr")
提名_all <- filter(提名_all,to!="")
emotion1 <- filter(提名_all,talk=="1")#抽取情感网络1
emotion2 <- filter(提名_all,hangout=="1")#抽取情感网络2
instrument <- filter(提名_all,study=="1")#抽取工具网络

#匹配顺序 排序名字
文17T2<-mutate(文17T2,Name_order=factor(Z03,order=T,levels=Z03)) #固定名字顺序
文17T2$Name_order<-as.numeric(文17T2$Name_order) #将顺序转换成数字


A<-文17T2$Z03 #起名

#朋友网络邻接矩阵
提名_all<-mutate(提名_all,from_order=as.numeric(factor(from,levels=A,order=T))) #根据固定顺序匹配序号
提名_all<-mutate(提名_all,to_order=as.numeric(factor(to,levels=A,order=T))) #根据固定顺序匹配序号
提名_all<-mutate(提名_all,tie=1)
提名_all.copy <- 提名_all[, 10:12]
提名_all.copy <- as.matrix(提名_all.copy)
adj <- matrix(0, 145, 145)
adj[提名_all.copy[,1:2]] <- 提名_all.copy[, 3]
write.table(adj,file = 'A07T2friend.txt',sep = ' ',row.names = FALSE,col.names = FALSE)

#friendweighted朋友网络邻接矩阵
提名_all<-mutate(提名_all,from_order=as.numeric(factor(from,levels=A,order=T))) #根据固定顺序匹配序号
提名_all<-mutate(提名_all,to_order=as.numeric(factor(to,levels=A,order=T))) #根据固定顺序匹配序号
提名_all.copy <- 提名_all[, c(10,11,4)]
提名_all.copy <- as.matrix(提名_all.copy)
adj <- matrix(0, 148, 148)
adj[提名_all.copy[,1:2]] <- 提名_all.copy[, 3]
write.table(adj,file = 'A17T2friendweightednet.txt',sep = ' ',row.names = FALSE,col.names = FALSE)

#情感网络1邻接矩阵
emotion1<-mutate(emotion1,from_order=as.numeric(factor(from,levels=A,order=T))) #根据固定顺序匹配序号
emotion1<-mutate(emotion1,to_order=as.numeric(factor(to,levels=A,order=T))) #根据固定顺序匹配序号
emotion1<-mutate(emotion1,tie=1)
emotion1.copy <- emotion1[, 9:27]
emotion1.copy <- as.matrix(emotion1.copy)
adj <- matrix(0, 163, 163)
adj[emotion1.copy[,1:2]] <- emotion1.copy[, 3]
write.table(adj,file = '文17T2emotion1.txt',sep = ' ',row.names = FALSE,col.names = FALSE)

#情感网络2邻接矩阵
emotion2<-mutate(emotion2,from_order=as.numeric(factor(from,levels=A,order=T))) #根据固定顺序匹配序号
emotion2<-mutate(emotion2,to_order=as.numeric(factor(to,levels=A,order=T))) #根据固定顺序匹配序号
emotion2<-mutate(emotion2,tie=1)
emotion2.copy <- emotion2[, 9:27]
emotion2.copy <- as.matrix(emotion2.copy)
adj <- matrix(0, 163, 163)
adj[emotion2.copy[,1:2]] <- emotion2.copy[, 3]
write.table(adj,file = '文17T2emotion2.txt',sep = ' ',row.names = FALSE,col.names = FALSE)

#工具网络邻接矩阵
instrument<-mutate(instrument,from_order=as.numeric(factor(from,levels=A,order=T))) #根据固定顺序匹配序号
instrument<-mutate(instrument,to_order=as.numeric(factor(to,levels=A,order=T))) #根据固定顺序匹配序号
instrument<-mutate(instrument,tie=1)
instrument.copy <- instrument[, 9:27]
instrument.copy <- as.matrix(instrument.copy)
adj <- matrix(0, 163, 163)
adj[instrument.copy[,1:2]] <- instrument.copy[, 3]
write.table(adj,file = '文17T2instrument.txt',sep = ' ',row.names = FALSE,col.names = FALSE)

library("spdep")
coordinates(提名_all) <- ~ from_order + to_order
grph <- relativeneigh(提名_all)
neib <- graph2nb(grph)
mylistw <- nb2listw(neib,zero.policy=TRUE)
B <- 提名_all$depression
moran.test(B,mylistw,zero.policy = TRUE,na.action = na.omit)
