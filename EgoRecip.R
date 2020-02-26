#定义个体网互惠率计算函数
EgoRecip <- function(adjmatrix){
  egoreciprocity <- 1:length(adjmatrix[1,])
  for (i in 1:length(adjmatrix[1,])){
    if (is.na(sum(adjmatrix[i,]))){egoreciprocity[i] <- NA}
    else {
      if (sum(adjmatrix[i,]) == 0){egoreciprocity[i] <- 0}
      else {
        g <- adjmatrix
        g[is.na(g)] = 0
        Egoout <- as.numeric(which(g[i,] == 1))
        Egoin <- as.numeric(which(g[,i] == 1))
        egoreciprocity[i] <- length(intersect(Egoout,Egoin))/length(Egoout)
      }
    }
  }
  return(egoreciprocity)
}

EgoRecip(S04T1friend)
table(EgoRecip(S04T4friend),useNA = "ifany")
table(rowSums(S04T4friend),useNA = "ifany")

EgoRecip_S04<-cbind(EgoRecip(S04T1friend),EgoRecip(S04T2friend),
                    EgoRecip(S04T3friend),EgoRecip(S04T4friend))
colnames(EgoRecip_S04) <- c("T1","T2","T3","T4")
write.table(EgoRecip_S04,file = 'EgoRecip_S04.txt',sep = ' ',row.names = F,col.names = T)

EgoRecip_S11<-cbind(EgoRecip(S11T1friend),EgoRecip(S11T2friend),
                    EgoRecip(S11T3friend),EgoRecip(S11T4friend))
colnames(EgoRecip_S11) <- c("T1","T2","T3","T4")
write.table(EgoRecip_S11,file = 'EgoRecip_S11.txt',sep = ' ',row.names = F,col.names = T)

EgoRecip_S27<-cbind(EgoRecip(S27T1friend),EgoRecip(S27T2friend),
                    EgoRecip(S27T3friend),EgoRecip(S27T4friend))
colnames(EgoRecip_S27) <- c("T1","T2","T3","T4")
write.table(EgoRecip_S27,file = 'EgoRecip_S27.txt',sep = ' ',row.names = F,col.names = T)

EgoRecip_S35<-cbind(EgoRecip(S35T1friend),EgoRecip(S35T2friend),
                    EgoRecip(S35T3friend),EgoRecip(S35T4friend))
colnames(EgoRecip_S35) <- c("T1","T2","T3","T4")
write.table(EgoRecip_S35,file = 'EgoRecip_S35.txt',sep = ' ',row.names = F,col.names = T)

EgoRecip_A07<-cbind(EgoRecip(A07T1friend),EgoRecip(A07T2friend),
                    EgoRecip(A07T3friend),EgoRecip(A07T4friend))
colnames(EgoRecip_A07) <- c("T1","T2","T3","T4")
write.table(EgoRecip_A07,file = 'EgoRecip_A07.txt',sep = ' ',row.names = F,col.names = T)

EgoRecip_A08<-cbind(EgoRecip(A08T1friend),EgoRecip(A08T2friend),
                    EgoRecip(A08T3friend),EgoRecip(A08T4friend))
colnames(EgoRecip_A08) <- c("T1","T2","T3","T4")
write.table(EgoRecip_A08,file = 'EgoRecip_A08.txt',sep = ' ',row.names = F,col.names = T)

EgoRecip_A17<-cbind(EgoRecip(A17T1friend),EgoRecip(A17T2friend),
                    EgoRecip(A17T3friend),EgoRecip(A17T4friend))
colnames(EgoRecip_A17) <- c("T1","T2","T3","T4")
write.table(EgoRecip_A17,file = 'EgoRecip_A17.txt',sep = ' ',row.names = F,col.names = T)

EgoRecip_total <- rbind(EgoRecip_A07,EgoRecip_A08,EgoRecip_A17,EgoRecip_S04,EgoRecip_S11,
                        EgoRecip_S27,EgoRecip_S35)
write.table(EgoRecip_total,file = 'EgoRecip_total.txt',sep = ' ',row.names = F,col.names = T)
