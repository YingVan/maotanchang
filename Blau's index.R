#定义blau's index计算函数
Blau <- function(x){
  x <- as.vector(table(x))
  if (sum(is.na(x)) == length(x)){Blau <- NA}
  else {
    Blau <- 1
    for (i in 1:length(x)){
      Blau = Blau - (x[i]/sum(x))^length(x)
    }
  }
  return(Blau)
}

Net_Blau_Vertex <- function(adjmatrix,attri){
  Blaus <- 1:length(adjmatrix[1,])
  for (i in 1:length(adjmatrix[1,])){
    if (is.na(sum(adjmatrix[i,]))){Blaus[i] <- NA}
    else {
      if (sum(adjmatrix[i,]) == 0){Blaus[i] <- NA}
      else {
        send <- as.numeric(which(adjmatrix[i,] == 1))
        receive <- as.numeric(which(adjmatrix[,i] == 1))
        all <- union(send,receive)
        Blaus[i] = Blau(attri[all])
        #print(Blau(attri[alt]))
      }
    }
  }
  return(Blaus)
}

Net_Blau_Edge <- function(adjmatrix,instrument,emotion){
  Blaus <- 1:length(adjmatrix[1,])
  for (i in 1:length(adjmatrix[1,])){
    if (is.na(sum(adjmatrix[i,]))){Blaus[i] <- NA}
    else {
      if (sum(adjmatrix[i,]) == 0){Blaus[i] <- NA}
      else {
        send <- as.numeric(which(adjmatrix[i,] == 1))
        mix <- instrument + emotion*2#宸ュ叿缃戠粶涓?1锛屾儏鎰熺綉缁滀负2
        Blaus[i] = Blau(mix[i,send])
        #print(Blau(attri[alt]))
      }
    }
  }
  return(Blaus)
}

#户口异质性
Blau_City_S04 <- cbind(Net_Blau_Vertex(S04T1friend,S04City),Net_Blau_Vertex(S04T2friend,S04City),
                       Net_Blau_Vertex(S04T3friend,S04City),Net_Blau_Vertex(S04T4friend,S04City))
Blau_City_S11 <- cbind(Net_Blau_Vertex(S11T1friend,S11City),Net_Blau_Vertex(S11T2friend,S11City),
                       Net_Blau_Vertex(S11T3friend,S11City),Net_Blau_Vertex(S11T4friend,S11City))
Blau_City_S27 <- cbind(Net_Blau_Vertex(S27T1friend,S27City),Net_Blau_Vertex(S27T2friend,S27City),
                       Net_Blau_Vertex(S27T3friend,S27City),Net_Blau_Vertex(S27T4friend,S27City))
Blau_City_S35 <- cbind(Net_Blau_Vertex(S35T1friend,S35City),Net_Blau_Vertex(S35T2friend,S35City),
                       Net_Blau_Vertex(S35T3friend,S35City),Net_Blau_Vertex(S35T4friend,S35City))
Blau_City_A07 <- cbind(Net_Blau_Vertex(A07T1friend,A07City),Net_Blau_Vertex(A07T2friend,A07City),
                       Net_Blau_Vertex(A07T3friend,A07City),Net_Blau_Vertex(A07T4friend,A07City))
Blau_City_A08 <- cbind(Net_Blau_Vertex(A08T1friend,A08City),Net_Blau_Vertex(A08T2friend,A08City),
                       Net_Blau_Vertex(A08T3friend,A08City),Net_Blau_Vertex(A08T4friend,A08City))
Blau_City_A17 <- cbind(Net_Blau_Vertex(A17T1friend,A17City),Net_Blau_Vertex(A17T2friend,A17City),
                       Net_Blau_Vertex(A17T3friend,A17City),Net_Blau_Vertex(A17T4friend,A17City))
Blau_City_total <- rbind(Blau_City_A07,Blau_City_A08,Blau_City_A17,Blau_City_S04,Blau_City_S11,
                         Blau_City_S27,Blau_City_S35)

#性别异质性
Blau_gender_S04 <- cbind(Net_Blau_Vertex(S04T1friend,S04gender),Net_Blau_Vertex(S04T2friend,S04gender),
                         Net_Blau_Vertex(S04T3friend,S04gender),Net_Blau_Vertex(S04T4friend,S04gender))
Blau_gender_S11 <- cbind(Net_Blau_Vertex(S11T1friend,S11gender),Net_Blau_Vertex(S11T2friend,S11gender),
                         Net_Blau_Vertex(S11T3friend,S11gender),Net_Blau_Vertex(S11T4friend,S11gender))
Blau_gender_S27 <- cbind(Net_Blau_Vertex(S27T1friend,S27gender),Net_Blau_Vertex(S27T2friend,S27gender),
                         Net_Blau_Vertex(S27T3friend,S27gender),Net_Blau_Vertex(S27T4friend,S27gender))
Blau_gender_S35 <- cbind(Net_Blau_Vertex(S35T1friend,S35gender),Net_Blau_Vertex(S35T2friend,S35gender),
                         Net_Blau_Vertex(S35T3friend,S35gender),Net_Blau_Vertex(S35T4friend,S35gender))
Blau_gender_A07 <- cbind(Net_Blau_Vertex(A07T1friend,A07gender),Net_Blau_Vertex(A07T2friend,A07gender),
                         Net_Blau_Vertex(A07T3friend,A07gender),Net_Blau_Vertex(A07T4friend,A07gender))
Blau_gender_A08 <- cbind(Net_Blau_Vertex(A08T1friend,A08gender),Net_Blau_Vertex(A08T2friend,A08gender),
                         Net_Blau_Vertex(A08T3friend,A08gender),Net_Blau_Vertex(A08T4friend,A08gender))
Blau_gender_A17 <- cbind(Net_Blau_Vertex(A17T1friend,A17gender),Net_Blau_Vertex(A17T2friend,A17gender),
                         Net_Blau_Vertex(A17T3friend,A17gender),Net_Blau_Vertex(A17T4friend,A17gender))
Blau_gender_total <- rbind(Blau_gender_A07,Blau_gender_A08,Blau_gender_A17,Blau_gender_S04,
                           Blau_gender_S11,Blau_gender_S27,Blau_gender_S35)
#读入数据
S04T1instrument <- as.matrix(read.table("理04T1instrument.txt"))
S04T2instrument <- as.matrix(read.table("理04T2instrument.txt"))
S04T3instrument <- as.matrix(read.table("理04T3instrument.txt"))
S04T4instrument <- as.matrix(read.table("理04T4instrument.txt"))
S04T1emotion1 <- as.matrix(read.table("理04T1emotion1.txt"))
S04T2emotion1 <- as.matrix(read.table("理04T2emotion1.txt"))
S04T3emotion1 <- as.matrix(read.table("理04T3emotion1.txt"))
S04T4emotion1 <- as.matrix(read.table("理04T4emotion1.txt"))
S04T1emotion2 <- as.matrix(read.table("理04T1emotion2.txt"))
S04T2emotion2 <- as.matrix(read.table("理04T2emotion2.txt"))
S04T3emotion2 <- as.matrix(read.table("理04T3emotion2.txt"))
S04T4emotion2 <- as.matrix(read.table("理04T4emotion2.txt"))

S11T1instrument <- as.matrix(read.table("理11T1instrument.txt"))
S11T2instrument <- as.matrix(read.table("理11T2instrument.txt"))
S11T3instrument <- as.matrix(read.table("理11T3instrument.txt"))
S11T4instrument <- as.matrix(read.table("理11T4instrument.txt"))
S11T1emotion1 <- as.matrix(read.table("理11T1emotion1.txt"))
S11T2emotion1 <- as.matrix(read.table("理11T2emotion1.txt"))
S11T3emotion1 <- as.matrix(read.table("理11T3emotion1.txt"))
S11T4emotion1 <- as.matrix(read.table("理11T4emotion1.txt"))
S11T1emotion2 <- as.matrix(read.table("理11T1emotion2.txt"))
S11T2emotion2 <- as.matrix(read.table("理11T2emotion2.txt"))
S11T3emotion2 <- as.matrix(read.table("理11T3emotion2.txt"))
S11T4emotion2 <- as.matrix(read.table("理11T4emotion2.txt"))

S27T1instrument <- as.matrix(read.table("理27T1instrument.txt"))
S27T2instrument <- as.matrix(read.table("理27T2instrument.txt"))
S27T3instrument <- as.matrix(read.table("理27T3instrument.txt"))
S27T4instrument <- as.matrix(read.table("理27T4instrument.txt"))
S27T1emotion1 <- as.matrix(read.table("理27T1emotion1.txt"))
S27T2emotion1 <- as.matrix(read.table("理27T2emotion1.txt"))
S27T3emotion1 <- as.matrix(read.table("理27T3emotion1.txt"))
S27T4emotion1 <- as.matrix(read.table("理27T4emotion1.txt"))
S27T1emotion2 <- as.matrix(read.table("理27T1emotion2.txt"))
S27T2emotion2 <- as.matrix(read.table("理27T2emotion2.txt"))
S27T3emotion2 <- as.matrix(read.table("理27T3emotion2.txt"))
S27T4emotion2 <- as.matrix(read.table("理27T4emotion2.txt"))

S35T1instrument <- as.matrix(read.table("理35T1instrument.txt"))
S35T2instrument <- as.matrix(read.table("理35T2instrument.txt"))
S35T3instrument <- as.matrix(read.table("理35T3instrument.txt"))
S35T4instrument <- as.matrix(read.table("理35T4instrument.txt"))
S35T1emotion1 <- as.matrix(read.table("理35T1emotion1.txt"))
S35T2emotion1 <- as.matrix(read.table("理35T2emotion1.txt"))
S35T3emotion1 <- as.matrix(read.table("理35T3emotion1.txt"))
S35T4emotion1 <- as.matrix(read.table("理35T4emotion1.txt"))
S35T1emotion2 <- as.matrix(read.table("理35T1emotion2.txt"))
S35T2emotion2 <- as.matrix(read.table("理35T2emotion2.txt"))
S35T3emotion2 <- as.matrix(read.table("理35T3emotion2.txt"))
S35T4emotion2 <- as.matrix(read.table("理35T4emotion2.txt"))

A07T1instrument <- as.matrix(read.table("文07T1instrument.txt"))
A07T2instrument <- as.matrix(read.table("文07T2instrument.txt"))
A07T3instrument <- as.matrix(read.table("文07T3instrument.txt"))
A07T4instrument <- as.matrix(read.table("文07T4instrument.txt"))
A07T1emotion1 <- as.matrix(read.table("文07T1emotion1.txt"))
A07T2emotion1 <- as.matrix(read.table("文07T2emotion1.txt"))
A07T3emotion1 <- as.matrix(read.table("文07T3emotion1.txt"))
A07T4emotion1 <- as.matrix(read.table("文07T4emotion1.txt"))
A07T1emotion2 <- as.matrix(read.table("文07T1emotion2.txt"))
A07T2emotion2 <- as.matrix(read.table("文07T2emotion2.txt"))
A07T3emotion2 <- as.matrix(read.table("文07T3emotion2.txt"))
A07T4emotion2 <- as.matrix(read.table("文07T4emotion2.txt"))

A08T1instrument <- as.matrix(read.table("文08T1instrument.txt"))
A08T2instrument <- as.matrix(read.table("文08T2instrument.txt"))
A08T3instrument <- as.matrix(read.table("文08T3instrument.txt"))
A08T4instrument <- as.matrix(read.table("文08T4instrument.txt"))
A08T1emotion1 <- as.matrix(read.table("文08T1emotion1.txt"))
A08T2emotion1 <- as.matrix(read.table("文08T2emotion1.txt"))
A08T3emotion1 <- as.matrix(read.table("文08T3emotion1.txt"))
A08T4emotion1 <- as.matrix(read.table("文08T4emotion1.txt"))
A08T1emotion2 <- as.matrix(read.table("文08T1emotion2.txt"))
A08T2emotion2 <- as.matrix(read.table("文08T2emotion2.txt"))
A08T3emotion2 <- as.matrix(read.table("文08T3emotion2.txt"))
A08T4emotion2 <- as.matrix(read.table("文08T4emotion2.txt"))

A17T1instrument <- as.matrix(read.table("文17T1instrument.txt"))
A17T2instrument <- as.matrix(read.table("文17T2instrument.txt"))
A17T3instrument <- as.matrix(read.table("文17T3instrument.txt"))
A17T4instrument <- as.matrix(read.table("文17T4instrument.txt"))
A17T1emotion1 <- as.matrix(read.table("文17T1emotion1.txt"))
A17T2emotion1 <- as.matrix(read.table("文17T2emotion1.txt"))
A17T3emotion1 <- as.matrix(read.table("文17T3emotion1.txt"))
A17T4emotion1 <- as.matrix(read.table("文17T4emotion1.txt"))
A17T1emotion2 <- as.matrix(read.table("文17T1emotion2.txt"))
A17T2emotion2 <- as.matrix(read.table("文17T2emotion2.txt"))
A17T3emotion2 <- as.matrix(read.table("文17T3emotion2.txt"))
A17T4emotion2 <- as.matrix(read.table("文17T4emotion2.txt"))

#工具支持情感支持异质性1
Blau_support1_S04 <- cbind(Net_Blau_Edge(S04T1friend,S04T1instrument,S04T1emotion1),
                           Net_Blau_Edge(S04T2friend,S04T2instrument,S04T2emotion1),
                           Net_Blau_Edge(S04T3friend,S04T3instrument,S04T3emotion1),
                           Net_Blau_Edge(S04T4friend,S04T4instrument,S04T4emotion1))
Blau_support1_S11 <- cbind(Net_Blau_Edge(S11T1friend,S11T1instrument,S11T1emotion1),
                           Net_Blau_Edge(S11T2friend,S11T2instrument,S11T2emotion1),
                           Net_Blau_Edge(S11T3friend,S11T3instrument,S11T3emotion1),
                           Net_Blau_Edge(S11T4friend,S11T4instrument,S11T4emotion1))
Blau_support1_S27 <- cbind(Net_Blau_Edge(S27T1friend,S27T1instrument,S27T1emotion1),
                           Net_Blau_Edge(S27T2friend,S27T2instrument,S27T2emotion1),
                           Net_Blau_Edge(S27T3friend,S27T3instrument,S27T3emotion1),
                           Net_Blau_Edge(S27T4friend,S27T4instrument,S27T4emotion1))
Blau_support1_S35 <- cbind(Net_Blau_Edge(S35T1friend,S35T1instrument,S35T1emotion1),
                           Net_Blau_Edge(S35T2friend,S35T2instrument,S35T2emotion1),
                           Net_Blau_Edge(S35T3friend,S35T3instrument,S35T3emotion1),
                           Net_Blau_Edge(S35T4friend,S35T4instrument,S35T4emotion1))
Blau_support1_A07 <- cbind(Net_Blau_Edge(A07T1friend,A07T1instrument,A07T1emotion1),
                           Net_Blau_Edge(A07T2friend,A07T2instrument,A07T2emotion1),
                           Net_Blau_Edge(A07T3friend,A07T3instrument,A07T3emotion1),
                           Net_Blau_Edge(A07T4friend,A07T4instrument,A07T4emotion1))
Blau_support1_A08 <- cbind(Net_Blau_Edge(A08T1friend,A08T1instrument,A08T1emotion1),
                           Net_Blau_Edge(A08T2friend,A08T2instrument,A08T2emotion1),
                           Net_Blau_Edge(A08T3friend,A08T3instrument,A08T3emotion1),
                           Net_Blau_Edge(A08T4friend,A08T4instrument,A08T4emotion1))
Blau_support1_A17 <- cbind(Net_Blau_Edge(A17T1friend,A17T1instrument,A17T1emotion1),
                           Net_Blau_Edge(A17T2friend,A17T2instrument,A17T2emotion1),
                           Net_Blau_Edge(A17T3friend,A17T3instrument,A17T3emotion1),
                           Net_Blau_Edge(A17T4friend,A17T4instrument,A17T4emotion1))
Blau_support1_total <- rbind(Blau_support1_A07,Blau_support1_A08,Blau_support1_A17,Blau_support1_S04,
                             Blau_support1_S11,Blau_support1_S27,Blau_support1_S35)

#工具支持情感支持异质性2
Blau_support2_S04 <- cbind(Net_Blau_Edge(S04T1friend,S04T1instrument,S04T1emotion2),
                           Net_Blau_Edge(S04T2friend,S04T2instrument,S04T2emotion2),
                           Net_Blau_Edge(S04T3friend,S04T3instrument,S04T3emotion2),
                           Net_Blau_Edge(S04T4friend,S04T4instrument,S04T4emotion2))
Blau_support2_S11 <- cbind(Net_Blau_Edge(S11T1friend,S11T1instrument,S11T1emotion2),
                           Net_Blau_Edge(S11T2friend,S11T2instrument,S11T2emotion2),
                           Net_Blau_Edge(S11T3friend,S11T3instrument,S11T3emotion2),
                           Net_Blau_Edge(S11T4friend,S11T4instrument,S11T4emotion2))
Blau_support2_S27 <- cbind(Net_Blau_Edge(S27T1friend,S27T1instrument,S27T1emotion2),
                           Net_Blau_Edge(S27T2friend,S27T2instrument,S27T2emotion2),
                           Net_Blau_Edge(S27T3friend,S27T3instrument,S27T3emotion2),
                           Net_Blau_Edge(S27T4friend,S27T4instrument,S27T4emotion2))
Blau_support2_S35 <- cbind(Net_Blau_Edge(S35T1friend,S35T1instrument,S35T1emotion2),
                           Net_Blau_Edge(S35T2friend,S35T2instrument,S35T2emotion2),
                           Net_Blau_Edge(S35T3friend,S35T3instrument,S35T3emotion2),
                           Net_Blau_Edge(S35T4friend,S35T4instrument,S35T4emotion2))
Blau_support2_A07 <- cbind(Net_Blau_Edge(A07T1friend,A07T1instrument,A07T1emotion2),
                           Net_Blau_Edge(A07T2friend,A07T2instrument,A07T2emotion2),
                           Net_Blau_Edge(A07T3friend,A07T3instrument,A07T3emotion2),
                           Net_Blau_Edge(A07T4friend,A07T4instrument,A07T4emotion2))
Blau_support2_A08 <- cbind(Net_Blau_Edge(A08T1friend,A08T1instrument,A08T1emotion2),
                           Net_Blau_Edge(A08T2friend,A08T2instrument,A08T2emotion2),
                           Net_Blau_Edge(A08T3friend,A08T3instrument,A08T3emotion2),
                           Net_Blau_Edge(A08T4friend,A08T4instrument,A08T4emotion2))
Blau_support2_A17 <- cbind(Net_Blau_Edge(A17T1friend,A17T1instrument,A17T1emotion2),
                           Net_Blau_Edge(A17T2friend,A17T2instrument,A17T2emotion2),
                           Net_Blau_Edge(A17T3friend,A17T3instrument,A17T3emotion2),
                           Net_Blau_Edge(A17T4friend,A17T4instrument,A17T4emotion2))
Blau_support2_total <- rbind(Blau_support2_A07,Blau_support2_A08,Blau_support2_A17,Blau_support2_S04,
                             Blau_support2_S11,Blau_support2_S27,Blau_support2_S35)

All_var <- read.csv("All.csv")
final <- matrix(nrow = 80,ncol = 4)
for (i in 1:4){
  x <- All_var[,c(55+i,59+i,63+i,67+i,71+i)]
  x[x==99] <- NA
  y <- cbind(Blau_gender_total[,i],Blau_City_total[,i],Blau_support1_total[,i],Blau_support2_total[,i])
  colnames(y) <- c("gender","City","support1","support2")
  for (j in 1:5){
    for (k in 1:4){
      correlate <- cor.test(x[,j],y[,k])
      #rowname <- paste(colnames(x)[j],colnames(y)[k])
      row <- c(colnames(x)[j],colnames(y)[k],as.numeric(correlate[c("p.value","estimate")]))
      final[(i-1)*20+(j-1)*4+k,] <- row
    }
  }
  #Cor <- cor(x,y,use = "pairwise.complete.obs")
  #colnames(Cor) <- c("Blau_gender","Blau_City","Blau_support1","Blau_support2")
  #print(Cor)
  #write.table(Cor,file = paste("CorT",i,".txt",sep =""),sep = ' ',row.names = T,col.names = T)
}
write.table(final,file = "相关及显著性检验.txt",sep = ' ',row.names = F,col.names = F)

