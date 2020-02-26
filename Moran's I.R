library(dplyr)#加载包
library("spdep")

#读入数据
理04T1 <- read.csv("理04T1.csv")#导入数据
理11T1 <- read.csv("理11T1.csv")#导入数据
理27T1 <- read.csv("理27T1.csv")#导入数据
理35T1 <- read.csv("理35T1.csv")#导入数据
文07T1 <- read.csv("文07T1.csv")#导入数据
文08T1 <- read.csv("文08T1.csv")#导入数据
文17T1 <- read.csv("文17T1.csv")#导入数据
理04T2 <- read.csv("理04T2.csv")#导入数据
理11T2 <- read.csv("理11T2.csv")#导入数据
理27T2 <- read.csv("理27T2.csv")#导入数据
理35T2 <- read.csv("理35T2.csv")#导入数据
文07T2 <- read.csv("文07T2.csv")#导入数据
文08T2 <- read.csv("文08T2.csv")#导入数据
文17T2 <- read.csv("文17T2.csv")#导入数据
理04T3 <- read.csv("理04T3.csv")#导入数据
理11T3 <- read.csv("理11T3.csv")#导入数据
理27T3 <- read.csv("理27T3.csv")#导入数据
理35T3 <- read.csv("理35T3.csv")#导入数据
文07T3 <- read.csv("文07T3.csv")#导入数据
文08T3 <- read.csv("文08T3.csv")#导入数据
文17T3 <- read.csv("文17T3.csv")#导入数据
理04T4 <- read.csv("理04T4.csv")#导入数据
理11T4 <- read.csv("理11T4.csv")#导入数据
理27T4 <- read.csv("理27T4.csv")#导入数据
理35T4 <- read.csv("理35T4.csv")#导入数据
文07T4 <- read.csv("文07T4.csv")#导入数据
文08T4 <- read.csv("文08T4.csv")#导入数据
文17T4 <- read.csv("文17T4.csv")#导入数据
T1_all <- rbind(理04T1,理11T1,理27T1,理35T1,文07T1,文08T1,文17T1)
T2_all <- rbind(理04T2,理11T2,理27T2,理35T2,文07T2,文08T2,文17T2)
T3_all <- rbind(理04T3,理11T3,理27T3,理35T3,文07T3,文08T3,文17T3)
T4_all <- rbind(理04T4,理11T4,理27T4,理35T4,文07T4,文08T4,文17T4)

MoranT1 <- function(data)
{
  data <-mutate(data,TAI=(5-data[,172]+rowSums(data[,173:191])))#计算TAI总分
  
  #提取某几列变量作为对象（如果一开始不加序号的话，提取列的序号要重新数）
  Arclist1 <- data[,c(4,76,241)]
  Arclist2 <- data[,c(4,77,241)]
  Arclist3 <- data[,c(4,78,241)]
  Arclist4 <- data[,c(4,79,241)]
  Arclist5 <- data[,c(4,80,241)]
  
  #将不同数据框改成相同的名字以便合并（若不加序号，则把第一个变量名称from_order删去）
  colnames(Arclist1)<-c("from","to","TAI")
  colnames(Arclist2)<-c("from","to","TAI")
  colnames(Arclist3)<-c("from","to","TAI")
  colnames(Arclist4)<-c("from","to","TAI")
  colnames(Arclist5)<-c("from","to","TAI")
  
  #合并
  Arclist_all<-rbind(Arclist1,Arclist2,Arclist3,Arclist4,Arclist5)
  
  #筛选to值不等于空值
  Arclist_all <- filter(Arclist_all,to!="")
  
  A<-data$姓名 #起名
  Arclist_all<-mutate(Arclist_all,from_order=as.numeric(factor(from,levels=A,order=T))) #根据固定顺序匹配序号
  Arclist_all<-mutate(Arclist_all,to_order=as.numeric(factor(to,levels=A,order=T))) #根据固定顺序匹配序号
  #转化数据格式计算莫兰指数
  coordinates(Arclist_all) <- ~ from_order + to_order
  grph <- relativeneigh(Arclist_all)
  neib <- graph2nb(grph)
  mylistw <- nb2listw(neib,zero.policy=TRUE)
  B <- Arclist_all$TAI
  moranIndex <- moran.test(B,mylistw,zero.policy = TRUE,na.action = na.omit)
  
  return(moranIndex)
}

MoranT2 <- function(data)
{
  data <-mutate(data,TAI=(5-data[,49]+rowSums(data[,50:68])))#计算TAI总分
  
  #提取某几列变量作为对象（如果一开始不加序号的话，提取列的序号要重新数）
  Arclist1 <- data[,c(5,69,127)]
  Arclist2 <- data[,c(5,70,127)]
  Arclist3 <- data[,c(5,71,127)]
  Arclist4 <- data[,c(5,72,127)]
  Arclist5 <- data[,c(5,73,127)]
  
  #将不同数据框改成相同的名字以便合并（若不加序号，则把第一个变量名称from_order删去）
  colnames(Arclist1)<-c("from","to","TAI")
  colnames(Arclist2)<-c("from","to","TAI")
  colnames(Arclist3)<-c("from","to","TAI")
  colnames(Arclist4)<-c("from","to","TAI")
  colnames(Arclist5)<-c("from","to","TAI")
  
  #合并
  Arclist_all<-rbind(Arclist1,Arclist2,Arclist3,Arclist4,Arclist5)
  
  #筛选to值不等于空值
  Arclist_all <- filter(Arclist_all,to!="")
  
  A<-data$Z03 #起名
  Arclist_all<-mutate(Arclist_all,from_order=as.numeric(factor(from,levels=A,order=T))) #根据固定顺序匹配序号
  Arclist_all<-mutate(Arclist_all,to_order=as.numeric(factor(to,levels=A,order=T))) #根据固定顺序匹配序号
  #转化数据格式计算莫兰指数
  coordinates(Arclist_all) <- ~ from_order + to_order
  grph <- relativeneigh(Arclist_all)
  neib <- graph2nb(grph)
  mylistw <- nb2listw(neib,zero.policy=TRUE)
  B <- Arclist_all$TAI
  moranIndex <- moran.test(B,mylistw,zero.policy = TRUE,na.action = na.omit)
  
  return(moranIndex)
}

MoranT3 <- function(data)
{
  data <-mutate(data,TAI=(5-data[,24]+rowSums(data[,25:43])))#计算TAI总分
  
  #提取某几列变量作为对象（如果一开始不加序号的话，提取列的序号要重新数）
  Arclist1 <- data[,c(5,72,133)]
  Arclist2 <- data[,c(5,73,133)]
  Arclist3 <- data[,c(5,74,133)]
  Arclist4 <- data[,c(5,75,133)]
  Arclist5 <- data[,c(5,76,133)]
  
  #将不同数据框改成相同的名字以便合并（若不加序号，则把第一个变量名称from_order删去）
  colnames(Arclist1)<-c("from","to","TAI")
  colnames(Arclist2)<-c("from","to","TAI")
  colnames(Arclist3)<-c("from","to","TAI")
  colnames(Arclist4)<-c("from","to","TAI")
  colnames(Arclist5)<-c("from","to","TAI")
  
  #合并
  Arclist_all<-rbind(Arclist1,Arclist2,Arclist3,Arclist4,Arclist5)
  
  #筛选to值不等于空值
  Arclist_all <- filter(Arclist_all,to!="")
  
  A<-data$Z03 #起名
  Arclist_all<-mutate(Arclist_all,from_order=as.numeric(factor(from,levels=A,order=T))) #根据固定顺序匹配序号
  Arclist_all<-mutate(Arclist_all,to_order=as.numeric(factor(to,levels=A,order=T))) #根据固定顺序匹配序号
  #转化数据格式计算莫兰指数
  coordinates(Arclist_all) <- ~ from_order + to_order
  grph <- relativeneigh(Arclist_all)
  neib <- graph2nb(grph)
  mylistw <- nb2listw(neib,zero.policy=TRUE)
  B <- Arclist_all$TAI
  moranIndex <- moran.test(B,mylistw,zero.policy = TRUE,na.action = na.omit)
  
  return(moranIndex)
}

MoranT4 <- function(data)
{
  data <-mutate(data,TAI=(5-data[,146]+rowSums(data[,147:165])))#计算TAI总分
  
  #提取某几列变量作为对象（如果一开始不加序号的话，提取列的序号要重新数）
  Arclist1 <- data[,c(5,32,209)]
  Arclist2 <- data[,c(5,33,209)]
  Arclist3 <- data[,c(5,34,209)]
  Arclist4 <- data[,c(5,35,209)]
  Arclist5 <- data[,c(5,36,209)]
  
  #将不同数据框改成相同的名字以便合并（若不加序号，则把第一个变量名称from_order删去）
  colnames(Arclist1)<-c("from","to","TAI")
  colnames(Arclist2)<-c("from","to","TAI")
  colnames(Arclist3)<-c("from","to","TAI")
  colnames(Arclist4)<-c("from","to","TAI")
  colnames(Arclist5)<-c("from","to","TAI")
  
  #合并
  Arclist_all<-rbind(Arclist1,Arclist2,Arclist3,Arclist4,Arclist5)
  
  #筛选to值不等于空值
  Arclist_all <- filter(Arclist_all,to!="")
  
  A<-data$Z03 #起名
  Arclist_all<-mutate(Arclist_all,from_order=as.numeric(factor(from,levels=A,order=T))) #根据固定顺序匹配序号
  Arclist_all<-mutate(Arclist_all,to_order=as.numeric(factor(to,levels=A,order=T))) #根据固定顺序匹配序号
  #转化数据格式计算莫兰指数
  coordinates(Arclist_all) <- ~ from_order + to_order
  grph <- relativeneigh(Arclist_all)
  neib <- graph2nb(grph)
  mylistw <- nb2listw(neib,zero.policy=TRUE)
  B <- Arclist_all$TAI
  moranIndex <- moran.test(B,mylistw,zero.policy = TRUE,na.action = na.omit)
  
  return(moranIndex)
}

moran_all <- matrix(0,28,2)

moran_all[1,] <- c(MoranT1(理04T1)[[2]], MoranT1(理04T1)[[3]][1])
moran_all[2,] <- c(MoranT1(理11T1)[[2]], MoranT1(理11T1)[[3]][1])
moran_all[3,] <- c(MoranT1(理27T1)[[2]], MoranT1(理27T1)[[3]][1])
moran_all[4,] <- c(MoranT1(理35T1)[[2]], MoranT1(理35T1)[[3]][1])
moran_all[5,] <- c(MoranT1(文07T1)[[2]], MoranT1(文07T1)[[3]][1])
moran_all[6,] <- c(MoranT1(文08T1)[[2]], MoranT1(文08T1)[[3]][1])
moran_all[7,] <- c(MoranT1(文17T1)[[2]], MoranT1(文17T1)[[3]][1])
moran_all[8,] <- c(MoranT2(理04T2)[[2]], MoranT2(理04T2)[[3]][1])
moran_all[9,] <- c(MoranT2(理11T2)[[2]], MoranT2(理11T2)[[3]][1])
moran_all[10,] <- c(MoranT2(理27T2)[[2]], MoranT2(理27T2)[[3]][1])
moran_all[11,] <- c(MoranT2(理35T2)[[2]], MoranT2(理35T2)[[3]][1])
moran_all[12,] <- c(MoranT2(文07T2)[[2]], MoranT2(文07T2)[[3]][1])
moran_all[13,] <- c(MoranT2(文08T2)[[2]], MoranT2(文08T2)[[3]][1])
moran_all[14,] <- c(MoranT2(文17T2)[[2]], MoranT2(文17T2)[[3]][1])
moran_all[15,] <- c(MoranT3(理04T3)[[2]], MoranT3(理04T3)[[3]][1])
moran_all[16,] <- c(MoranT3(理11T3)[[2]], MoranT3(理11T3)[[3]][1])
moran_all[17,] <- c(MoranT3(理27T3)[[2]], MoranT3(理27T3)[[3]][1])
moran_all[18,] <- c(MoranT3(理35T3)[[2]], MoranT3(理35T3)[[3]][1])
moran_all[19,] <- c(MoranT3(文07T3)[[2]], MoranT3(文07T3)[[3]][1])
moran_all[20,] <- c(MoranT3(文08T3)[[2]], MoranT3(文08T3)[[3]][1])
moran_all[21,] <- c(MoranT3(文17T3)[[2]], MoranT3(文17T3)[[3]][1])
moran_all[22,] <- c(MoranT4(理04T4)[[2]], MoranT4(理04T4)[[3]][1])
moran_all[23,] <- c(MoranT4(理11T4)[[2]], MoranT4(理11T4)[[3]][1])
moran_all[24,] <- c(MoranT4(理27T4)[[2]], MoranT4(理27T4)[[3]][1])
moran_all[25,] <- c(MoranT4(理35T4)[[2]], MoranT4(理35T4)[[3]][1])
moran_all[26,] <- c(MoranT4(文07T4)[[2]], MoranT4(文07T4)[[3]][1])
moran_all[27,] <- c(MoranT4(文08T4)[[2]], MoranT4(文08T4)[[3]][1])
moran_all[28,] <- c(MoranT4(文17T4)[[2]], MoranT4(文17T4)[[3]][1])
MoranT1(T1_all)
MoranT2(T2_all)
MoranT3(T3_all)
MoranT4(T4_all)
