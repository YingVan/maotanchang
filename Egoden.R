#定义个体网密度计算函数(只考虑提名不考虑被提名)
Egoden <- function(adjmatrix){
  egodensity <- 1:length(adjmatrix[1,])
  for (i in 1:length(adjmatrix[1,])){
    if (is.na(sum(adjmatrix[i,]))){egodensity[i] <- NA}
    else {
      if (sum(adjmatrix[i,]) == 0){egodensity[i] <- 0}
      else {
        g <- adjmatrix
        g[is.na(g)] = 0
        Egonet <- append(as.numeric(which(g[i,] == 1)),i)
        coordinate <- t(cbind(combn(Egonet,2)[order(combn(Egonet,2)[,1],decreasing=F),],
                              combn(Egonet,2)[order(combn(Egonet,2)[,1],decreasing=T),]))
        den <- sum(g[coordinate])/(length(Egonet)*(length(Egonet)-1))
        egodensity[i] <- den
      }
    }
  }
  return(egodensity)
}

Egoden(S04T1friend)
table(Egoden(S04T4friend),useNA = "ifany")
table(rowSums(S04T4friend),useNA = "ifany")

Egoden_S04<-cbind(Egoden(S04T1friend),Egoden(S04T2friend),
                Egoden(S04T3friend),Egoden(S04T4friend))
colnames(Egoden_S04) <- c("T1","T2","T3","T4")
write.table(Egoden_S04,file = 'Egoden_S04.txt',sep = ' ',row.names = F,col.names = T)

Egoden_S11<-cbind(Egoden(S11T1friend),Egoden(S11T2friend),
                  Egoden(S11T3friend),Egoden(S11T4friend))
colnames(Egoden_S11) <- c("T1","T2","T3","T4")
write.table(Egoden_S11,file = 'Egoden_S11.txt',sep = ' ',row.names = F,col.names = T)

Egoden_S27<-cbind(Egoden(S27T1friend),Egoden(S27T2friend),
                  Egoden(S27T3friend),Egoden(S27T4friend))
colnames(Egoden_S27) <- c("T1","T2","T3","T4")
write.table(Egoden_S27,file = 'Egoden_S27.txt',sep = ' ',row.names = F,col.names = T)

Egoden_S35<-cbind(Egoden(S35T1friend),Egoden(S35T2friend),
                  Egoden(S35T3friend),Egoden(S35T4friend))
colnames(Egoden_S35) <- c("T1","T2","T3","T4")
write.table(Egoden_S35,file = 'Egoden_S35.txt',sep = ' ',row.names = F,col.names = T)

Egoden_A07<-cbind(Egoden(A07T1friend),Egoden(A07T2friend),
                  Egoden(A07T3friend),Egoden(A07T4friend))
colnames(Egoden_A07) <- c("T1","T2","T3","T4")
write.table(Egoden_A07,file = 'Egoden_A07.txt',sep = ' ',row.names = F,col.names = T)

Egoden_A08<-cbind(Egoden(A08T1friend),Egoden(A08T2friend),
                  Egoden(A08T3friend),Egoden(A08T4friend))
colnames(Egoden_A08) <- c("T1","T2","T3","T4")
write.table(Egoden_A08,file = 'Egoden_A08.txt',sep = ' ',row.names = F,col.names = T)

Egoden_A17<-cbind(Egoden(A17T1friend),Egoden(A17T2friend),
                  Egoden(A17T3friend),Egoden(A17T4friend))
colnames(Egoden_A17) <- c("T1","T2","T3","T4")
write.table(Egoden_A17,file = 'Egoden_A17.txt',sep = ' ',row.names = F,col.names = T)
