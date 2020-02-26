library(dplyr)
library(networkD3)

getLinkT1 <- function(x){
  #提取某几列变量作为对象（如果一开始不加序号的话，提取列的序号要重新数）
  提名1 <- x[,c(4,76,84,92,100,108,116,124)]
  提名2 <- x[,c(4,77,85,93,101,109,117,125)]
  提名3 <- x[,c(4,78,86,94,102,110,118,126)]
  提名4 <- x[,c(4,79,87,95,103,111,119,127)]
  提名5 <- x[,c(4,80,88,96,104,112,120,128)]
  #将不同数据框改成相同的名字以便合并（若不加序号，则把第一个变量名称from_order删去）
  colnames(提名1)<-c("from","to","gender","friends","similar","study","talk","hangout")
  colnames(提名2)<-c("from","to","gender","friends","similar","study","talk","hangout")
  colnames(提名3)<-c("from","to","gender","friends","similar","study","talk","hangout")
  colnames(提名4)<-c("from","to","gender","friends","similar","study","talk","hangout")
  colnames(提名5)<-c("from","to","gender","friends","similar","study","talk","hangout")
  提名_all<-rbind(提名1,提名2,提名3,提名4,提名5)#合并
  提名_all <- filter(提名_all,to!="")#筛选to值不等于空值
  #匹配顺序 排序名字 动态图
  x<-mutate(x,Name_order=factor(name,order=T,levels=name)) #固定名字顺序
  x$Name_order<-as.numeric(x$Name_order) #将顺序转换成数字
  A<-x$name #起名
  提名_all<-mutate(提名_all,from_order=as.numeric(factor(from,levels=A,order=T))) #根据固定顺序匹配序号
  提名_all<-mutate(提名_all,to_order=as.numeric(factor(to,levels=A,order=T))) #根据固定顺序匹配序号
  #动态图的要求从0开始，我们的编号是1开始
  x$Name_order<-x$Name_order-1 
  提名_all$from_order<-提名_all$from_order-1  
  提名_all$to_order<-提名_all$to_order-1 
  return(提名_all)
}

getLinkT2 <- function(x){
  #提取某几列变量作为对象（如果一开始不加序号的话，提取列的序号要重新数）
  提名1 <- x[,c(5,69,77,85,93,101,109,117)]
  提名2 <- x[,c(5,70,78,86,94,102,110,118)]
  提名3 <- x[,c(5,71,79,87,95,103,111,119)]
  提名4 <- x[,c(5,72,80,88,96,104,112,120)]
  提名5 <- x[,c(5,73,81,89,97,105,113,121)]
  #将不同数据框改成相同的名字以便合并（若不加序号，则把第一个变量名称from_order删去）
  colnames(提名1)<-c("from","to","gender","friends","similar","study","talk","hangout")
  colnames(提名2)<-c("from","to","gender","friends","similar","study","talk","hangout")
  colnames(提名3)<-c("from","to","gender","friends","similar","study","talk","hangout")
  colnames(提名4)<-c("from","to","gender","friends","similar","study","talk","hangout")
  colnames(提名5)<-c("from","to","gender","friends","similar","study","talk","hangout")
  提名_all<-rbind(提名1,提名2,提名3,提名4,提名5)#合并
  提名_all <- filter(提名_all,to!="")#筛选to值不等于空值
  x<-mutate(x,Name_order=factor(name,order=T,levels=name)) #固定名字顺序
  x$Name_order<-as.numeric(x$Name_order) #将顺序转换成数字
  A<-x$name #起名
  提名_all<-mutate(提名_all,from_order=as.numeric(factor(from,levels=A,order=T))) #根据固定顺序匹配序号
  提名_all<-mutate(提名_all,to_order=as.numeric(factor(to,levels=A,order=T))) #根据固定顺序匹配序号
  #动态图的要求从0开始，我们的编号是1开始
  x$Name_order<-x$Name_order-1 
  提名_all$from_order<-提名_all$from_order-1  
  提名_all$to_order<-提名_all$to_order-1  
  return(提名_all)
}

getLinkT3 <- function(x){
  #提取某几列变量作为对象（如果一开始不加序号的话，提取列的序号要重新数）
  提名1 <- x[,c(5,89,97,105,113,121,129,137)]
  提名2 <- x[,c(5,90,98,106,114,122,130,138)]
  提名3 <- x[,c(5,91,99,107,115,123,131,139)]
  提名4 <- x[,c(5,92,100,108,116,124,132,140)]
  提名5 <- x[,c(5,93,101,109,117,125,133,141)]
  #将不同数据框改成相同的名字以便合并（若不加序号，则把第一个变量名称from_order删去）
  colnames(提名1)<-c("from","to","gender","friends","similar","study","talk","hangout")
  colnames(提名2)<-c("from","to","gender","friends","similar","study","talk","hangout")
  colnames(提名3)<-c("from","to","gender","friends","similar","study","talk","hangout")
  colnames(提名4)<-c("from","to","gender","friends","similar","study","talk","hangout")
  colnames(提名5)<-c("from","to","gender","friends","similar","study","talk","hangout")
  提名_all<-rbind(提名1,提名2,提名3,提名4,提名5)#合并
  提名_all <- filter(提名_all,to!="")#筛选to值不等于空值
  x<-mutate(x,Name_order=factor(Z03,order=T,levels=Z03)) #固定名字顺序
  x$Name_order<-as.numeric(x$Name_order) #将顺序转换成数字
  A<-x$Z03 #起名
  提名_all<-mutate(提名_all,from_order=as.numeric(factor(from,levels=A,order=T))) #根据固定顺序匹配序号
  提名_all<-mutate(提名_all,to_order=as.numeric(factor(to,levels=A,order=T))) #根据固定顺序匹配序号
  #动态图的要求从0开始，我们的编号是1开始
  x$Name_order<-x$Name_order-1 
  提名_all$from_order<-提名_all$from_order-1  
  提名_all$to_order<-提名_all$to_order-1  
  return(提名_all)
}

getLinkT4 <- function(x){
  #提取某几列变量作为对象（如果一开始不加序号的话，提取列的序号要重新数）
  提名1 <- x[,c(5,29,37,45,53,61,69,77)]
  提名2 <- x[,c(5,30,38,46,54,62,70,78)]
  提名3 <- x[,c(5,31,39,47,55,63,71,79)]
  提名4 <- x[,c(5,32,40,48,56,64,72,80)]
  提名5 <- x[,c(5,33,41,49,57,65,73,81)]
  #将不同数据框改成相同的名字以便合并（若不加序号，则把第一个变量名称from_order删去）
  colnames(提名1)<-c("from","to","gender","friends","similar","study","talk","hangout")
  colnames(提名2)<-c("from","to","gender","friends","similar","study","talk","hangout")
  colnames(提名3)<-c("from","to","gender","friends","similar","study","talk","hangout")
  colnames(提名4)<-c("from","to","gender","friends","similar","study","talk","hangout")
  colnames(提名5)<-c("from","to","gender","friends","similar","study","talk","hangout")
  提名_all<-rbind(提名1,提名2,提名3,提名4,提名5)#合并
  提名_all <- filter(提名_all,to!="")#筛选to值不等于空值
  x<-mutate(x,Name_order=factor(name,order=T,levels=name)) #固定名字顺序
  x$Name_order<-as.numeric(x$Name_order) #将顺序转换成数字
  A<-x$name #起名
  提名_all<-mutate(提名_all,from_order=as.numeric(factor(from,levels=A,order=T))) #根据固定顺序匹配序号
  提名_all<-mutate(提名_all,to_order=as.numeric(factor(to,levels=A,order=T))) #根据固定顺序匹配序号
  #动态图的要求从0开始，我们的编号是1开始
  x$Name_order<-x$Name_order-1 
  提名_all$from_order<-提名_all$from_order-1  
  提名_all$to_order<-提名_all$to_order-1  
  return(提名_all)
}

graph <- function(node,link){
  forceNetwork(Links = link,#线性质数据框
               Nodes = node,#节点性质数据框
               
               Source = "from_order",#连线的源变量
               Target = "to_order",#连线的目标变量
               Value = "friends",#连线的粗细值
               NodeID = "name",#节点名称
               Group = "sex",#节点的分组
               
               ###美化部分
               Nodesize = "Z01" ,#节点大小，节点数据框中
               fontFamily="宋体",#字体设置如"华文行楷" 等
               fontSize = 20, #节点文本标签的数字字体大小（以像素为单位）。
               linkColour="grey",#连线颜色,black,red,blue,  
               colourScale =JS("d3.scaleOrdinal(d3.schemeCategory10);"),#节点颜色
               #linkWidth=0.6,
               charge = -10,#数值表示节点排斥强度（负值）或吸引力（正值）  
               opacity = 1.0,# 所有节点初始透明度
               legend=T,#显示节点分组的颜色标签
               arrows=T,#是否带方向
               bounded=F,#是否启用限制图像的边框
               opacityNoHover=0.2,#当鼠标未悬停其上时，节点标的不透明度
               zoom = T)#允许放缩，双击放大
}


setwd("D:/付琳/毛坦厂/R纵向分析_复读/9班4次数据文件")
理03T1 <- read.csv("理03T1.csv")#导入数据
colnames(理03T1)[c(4,11)] <- c("name","sex")
sex_S03 <- 理03T1[,"sex"]
理03T2 <- read.csv("理03T2.csv")#导入数据
理03T2 <- mutate(理03T2,sex = sex_S03)
理03T2[,"Z01"] = 4
colnames(理03T2)[5] <- "name"
理03T3 <- read.csv("理03T3.csv")#导入数据
理03T3 <- mutate(理03T3,sex = sex_S03)
colnames(理03T3)[5] <- "name"
理03T4 <- read.csv("理03T4.csv")#导入数据
理03T4 <- mutate(理03T4,sex = sex_S03)
理03T4[,"Z01"] = 4
colnames(理03T4)[5] <- "name"
文06T1 <- read.csv("文06T1.csv")#导入数据
colnames(文06T1)[c(3,4,11)] <- c("Z01","name","sex")
文06T1[,"sex"][文06T1[,"sex"] == 1] <- "男"
文06T1[,"sex"][文06T1[,"sex"] == 2] <- "女"
sex_A06 <- 文06T1[,"sex"]
文06T2 <- read.csv("文06T2.csv")#导入数据
文06T2 <- mutate(文06T2,sex = sex_A06)
文06T2[,"Z01"] = 4
colnames(文06T2)[5] <- "name"
文06T3 <- read.csv("文06T3.csv")#导入数据
文06T3 <- mutate(文06T3,sex = sex_A06)
文06T3[,"Z01"] = 4
colnames(文06T3)[5] <- "name"
文06T4 <- read.csv("文06T4.csv")#导入数据
文06T4 <- mutate(文06T4,sex = sex_A06)
文06T4[,"Z01"] = 4
colnames(文06T4)[5] <- "name"

#删除joiner和leaver
文06T1 <- filter(文06T1,name != "何自圆" & name != "陶韦延")
文06T2 <- filter(文06T2,name != "何自圆" & name != "陶韦延")
文06T4 <- filter(文06T4,name != "丁千里",name != "李仁娟")

S03T1Link <- getLinkT1(理03T1)
S03T2Link <- getLinkT2(理03T2)
S03T3Link <- getLinkT3(理03T3)
S03T4Link <- getLinkT4(理03T4)
A06T1Link <- getLinkT1(文06T1)
A06T2Link <- getLinkT2(文06T2)
A06T3Link <- getLinkT3(文06T3)
A06T4Link <- getLinkT4(文06T4)

S03T1 <- graph(理03T1,S03T1Link)
S03T2 <- graph(理03T2,S03T2Link)
S03T3 <- graph(理03T3,S03T3Link)
S03T4 <- graph(理03T4,S03T4Link)
saveNetwork(S03T2,"S03T2.html",selfcontained=TRUE)
saveNetwork(S03T3,"S03T3.html",selfcontained=TRUE)
saveNetwork(S03T4,"S03T4.html",selfcontained=TRUE)

A06T1 <- graph(文06T1,A06T1Link)
A06T2 <- graph(文06T2,A06T2Link)
A06T3 <- graph(文06T3,A06T3Link)
A06T4 <- graph(文06T4,A06T4Link)
saveNetwork(A06T1,"A06T1.html",selfcontained=TRUE)
saveNetwork(A06T2,"A06T2.html",selfcontained=TRUE)
saveNetwork(A06T3,"A06T3.html",selfcontained=TRUE)
saveNetwork(A06T4,"A06T4.html",selfcontained=TRUE)

