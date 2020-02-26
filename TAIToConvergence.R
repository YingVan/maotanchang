##################################################################
# R script for assignment on multi-group options & meta-analysis #
##################################################################

# load RSiena commands:
library("RSiena")
library(sna)
library(lattice)  # for plotting
source("SelectionTables.r")

# set working directory to where the data are:
setwd("D:/付琳/毛坦厂/R纵向分析_复读/R输入数据")
# Create a series of functions
siena07ToConvergence <- function(alg, dat, eff, ans0=NULL, ...){
  numr <- 0
  ans <- siena07(alg, data=dat, effects=eff, prevAns=ans0, ...) # the first run
  repeat {
    save(ans, file=paste("ans",numr,".RData",sep="")) # to be safe
    numr <- numr+1 # count number of repeated runs
    tm <- ans$tconv.max # convergence indicator
    cat(numr, tm,"\n") # report how far we are
    if (tm < 0.25) {break} # success
    if (tm > 10) {break} # divergence without much hope
    # of returning to good parameter values
    if (numr > 30) {break} # now it has lasted too long
    ans <- siena07(alg, data=dat, effects=eff, prevAns=ans, ...)
  }
  if (tm > 0.25)
  {
    cat("Warning: convergence inadequate.\n")
  }
  ans
}

igraphNetworkExtraction <- function(i, data, sims, period, groupName, varName){
  require(igraph)
  dimsOfDepVar<- attr(data[[groupName]]$depvars[[varName]], "netdims")
  missings <- is.na(data[[groupName]]$depvars[[varName]][,,period]) |
    is.na(data[[groupName]]$depvars[[varName]][,,period+1])
  if (is.null(i)) {
    # sienaGOF wants the observation:
    original <- data[[groupName]]$depvars[[varName]][,,period+1]
    original[missings] <- 0
    returnValue <- graph.adjacency(original)
  }
  else
  {
    missings <- graph.adjacency(missings)
    #sienaGOF wants the i-th simulation:
    returnValue <- graph.difference(
      graph.empty(dimsOfDepVar) +
        edges(t(sims[[i]][[groupName]][[varName]][[period]][,1:2])),
      missings)
  }
  returnValue
}

GeodesicDistribution <- function (i, data, sims, period, groupName,
                                  varName, levls=c(1:5,Inf), cumulative=TRUE, ...) {
  x <- networkExtraction(i, data, sims, period, groupName, varName)
  require(sna)
  a <- sna::geodist(symmetrize(x))$gdist
  if (cumulative)
  {
    gdi <- sapply(levls, function(i){ sum(a<=i) })
  }
  else
  {
    gdi <- sapply(levls, function(i){ sum(a==i) })
  }
  names(gdi) <- as.character(levls)
  gdi
}

# Holland and Leinhardt Triad Census; see ?sna::triad.census.
TriadCensus <- function(i, data, sims, wave, groupName, varName, levls=1:16){
  unloadNamespace("igraph") # to avoid package clashes
  require(sna)
  require(network)
  x <- networkExtraction(i, data, sims, wave, groupName, varName)
  if (network.edgecount(x) <= 0){x <- symmetrize(x)}
  # because else triad.census(x) will lead to an error
  tc <- sna::triad.census(x)[1,levls]
  # names are transferred automatically
  tc
}

# Distribution of Bonacich eigenvalue centrality; see ?igraph::evcent.
EigenvalueDistribution <- function (i, data, sims, period, groupName, varName,
                                    levls=c(seq(0,1,by=0.125)), cumulative=TRUE){
  require(igraph)
  x <- igraphNetworkExtraction(i, data, sims, period, groupName, varName)
  a <- igraph::evcent(x)$vector
  a[is.na(a)] <- Inf
  lel <- length(levls)
  if (cumulative)
  {
    cdi <- sapply(2:lel, function(i){sum(a<=levls[i])})
  }
  else
  {
    cdi <- sapply(2:lel, function(i){
      sum(a<=levls[i]) - sum(a <= levls[i-1])})
  }
  names(cdi) <- as.character(levls[2:lel])
  cdi
}

MoranGeary <- function(i, data, sims, wave, groupName, varName, levls=1:2){
  #unloadNamespace("igraph") # to avoid package clashes
  require(sna)
  require(network)
  x <- as.sociomatrix(networkExtraction(i, data, sims, wave, groupName, varName[1]))
  z <- behaviorExtraction(i,data,sims,wave,groupName,varName[2])
  n <- length(z)
  z.ave <- mean(z,na.rm=TRUE)
  numerator <- n*sum(x*outer(z-z.ave,z-z.ave),na.rm=TRUE)
  denominator <- sum(x,na.rm=TRUE)*sum((z-z.ave)^2,na.rm=TRUE)
  res <- numerator/denominator
  numerator <- (n-1)*sum(x*(outer(z,z,FUN='-')^2),na.rm=TRUE)
  denominator <- 2*sum(x,na.rm=TRUE)*sum((z-z.ave)^2,na.rm=TRUE)
  res[2] <- numerator/denominator
  names(res) <- c("Moran","Geary")
  return(res)
}

Moran123 <- function(i, data, sims, wave, groupName, varName, levls=1){
  #unloadNamespace("igraph") # to avoid package clashes
  require(sna)
  require(network)
  x <- as.sociomatrix(networkExtraction(i, data, sims, wave, groupName, varName[1]))
  z <- behaviorExtraction(i,data,sims,wave,groupName,varName[2])
  # handle missing data [not checked if this makes sense]:
  x[is.na(x)] <- 0
  z[is.na(z)] <- mean(z,na.rm=TRUE)
  res <- nacf(x,z,lag.max=3,typ="moran")[2:4]
  names(res) <- c("d=1","d=2","d=3")
  return(res)
}

Geary123 <- function(i, data, sims, wave, groupName, varName, levls=1){
  #unloadNamespace("igraph") # to avoid package clashes
  require(sna)
  require(network)
  x <- as.sociomatrix(networkExtraction(i, data, sims, wave, groupName, varName[1]))
  z <- behaviorExtraction(i,data,sims,wave,groupName,varName[2])
  # handle missing data [not checked if this makes sense]:
  x[is.na(x)] <- 0
  z[is.na(z)] <- mean(z,na.rm=TRUE)
  res <- nacf(x,z,lag.max=5,typ="geary")[2:4]
  names(res) <- c("d=1","d=2","d=3")
  return(res)
}

EgoAlterTable <- function(i, data, sims, wave, groupName, varName, levls=1){
  #unloadNamespace("igraph") # to avoid package clashes
  #require(sna)
  #require(network)
  x <- as.sociomatrix(networkExtraction(i, data, sims, wave, groupName, varName[1]))
  z <- behaviorExtraction(i,data,sims,wave,groupName,varName[2])
  res <- matrix(0,nr=5,nc=5)
  for (ego in 1:5) {
    for (alt in 1:5) {
      thesum <- sum(x[z==ego,z==alt],na.rm=TRUE)
      if (thesum>0) {
        res[ego,alt] <- thesum
      }
    }}
  thenames <- paste('e',col(res),'a',row(res),sep='')
  res <- c(t(res))
  names(res) <- thenames
  return(res)
}

outTable <- function(x) {
  coef <- abs(x$theta)
  coefPretty <- sprintf("%.3f", round(coef,3))
  se <- diag(x$covtheta)**.5
  sePretty <- sprintf("%.3f", round(se,3))
  pval <- 2*pnorm(-abs(coef/se))
  symp <- symnum(pval, corr = FALSE,
                 cutpoints = c(0,  .001,.01,.05, .1, 1),
                 symbols = c("***","**","*","."," "))
  convPretty <- sprintf("%.3f", round(abs(x$tconv),3))
  out1 <- noquote(cbind(
    Function = x$effects[[1]], 
    Effect = x$effects[[2]], 
    Coef = coefPretty, 
    StEr = sePretty, 
    Sig = symp, 
    Conv = convPretty))
  out2 <- paste("Maximum Convergence Ratio:", round(x$tconv.max,3))
  return(list(out1,out2))
}



# Please also copy the data from classroom 12b into this directory.
# They can be found on the SIENA website.
#
# ---------------- READING & MANIPULTING THE DATA -------------
# 
# read the data sets for the assignment:
S04T1friend <- as.matrix(read.table("理04T1friend.txt"))
S04T2friend <- as.matrix(read.table("理04T2friend.txt"))
S04T3friend <- as.matrix(read.table("理04T3friend.txt"))
S04T4friend <- as.matrix(read.table("理04T4friend.txt"))
S04TestAnxiety <- as.matrix(read.table("S04TAI.txt"))
S04Depression <- as.matrix(read.table("S04CESD.txt"))
S04gender <- as.matrix(read.table("理04gender.txt"))
S04SocialSupport <- as.matrix(read.table("S04PSSS.txt"))
S04GSES <- as.matrix(read.table("S04GSES.txt"))
S04City <- as.matrix(read.table("理04City.txt"))
S04OnlyKid <- as.matrix(read.table("理04onlykid.txt"))
S04EduMom <- as.matrix(read.table("理04EduMom.txt"))
S04grade <- as.matrix(read.table("S04score.txt"))

S11T1friend <- as.matrix(read.table("理11T1friend.txt"))
S11T2friend <- as.matrix(read.table("理11T2friend.txt"))
S11T3friend <- as.matrix(read.table("理11T3friend.txt"))
S11T4friend <- as.matrix(read.table("理11T4friend.txt"))
S11TestAnxiety <- as.matrix(read.table("S11TAI.txt"))
S11Depression <- as.matrix(read.table("S11CESD.txt"))
S11gender <- as.matrix(read.table("理11gender.txt"))
S11SocialSupport <- as.matrix(read.table("S11PSSS.txt"))
S11GSES <- as.matrix(read.table("S11GSES.txt"))
S11City <- as.matrix(read.table("理11City.txt"))
S11OnlyKid <- as.matrix(read.table("理11onlykid.txt"))
S11EduMom <- as.matrix(read.table("理11EduMom.txt"))
S11grade <- as.matrix(read.table("S11score.txt"))

S27T1friend <- as.matrix(read.table("理27T1friend.txt"))
S27T2friend <- as.matrix(read.table("理27T2friend.txt"))
S27T3friend <- as.matrix(read.table("理27T3friend.txt"))
S27T4friend <- as.matrix(read.table("理27T4friend.txt"))
S27TestAnxiety <- as.matrix(read.table("S27TAI.txt"))
S27Depression <- as.matrix(read.table("S27CESD.txt"))
S27gender <- as.matrix(read.table("理27gender.txt"))
S27SocialSupport <- as.matrix(read.table("S27PSSS.txt"))
S27GSES <- as.matrix(read.table("S27GSES.txt"))
S27City <- as.matrix(read.table("理27City.txt"))
S27OnlyKid <- as.matrix(read.table("理27onlykid.txt"))
S27EduMom <- as.matrix(read.table("理27EduMom.txt"))
S27grade <- as.matrix(read.table("S27score.txt"))

S35T1friend <- as.matrix(read.table("理35T1friend.txt"))
S35T2friend <- as.matrix(read.table("理35T2friend.txt"))
S35T3friend <- as.matrix(read.table("理35T3friend.txt"))
S35T4friend <- as.matrix(read.table("理35T4friend.txt"))
S35TestAnxiety <- as.matrix(read.table("S35TAI.txt"))
S35Depression <- as.matrix(read.table("S35CESD.txt"))
S35gender <- as.matrix(read.table("理35gender.txt"))
S35SocialSupport <- as.matrix(read.table("S35PSSS.txt"))
S35GSES <- as.matrix(read.table("S35GSES.txt"))
S35City <- as.matrix(read.table("理35City.txt"))
S35OnlyKid <- as.matrix(read.table("理35onlykid.txt"))
S35EduMom <- as.matrix(read.table("理35EduMom.txt"))
S35grade <- as.matrix(read.table("S35score.txt"))

A07T1friend <- as.matrix(read.table("文07T1friend.txt"))
A07T2friend <- as.matrix(read.table("文07T2friend.txt"))
A07T3friend <- as.matrix(read.table("文07T3friend.txt"))
A07T4friend <- as.matrix(read.table("文07T4friend.txt"))
A07TestAnxiety <- as.matrix(read.table("A07TAI.txt"))
A07Depression <- as.matrix(read.table("A07CESD.txt"))
A07gender <- as.matrix(read.table("文07gender.txt"))
A07SocialSupport <- as.matrix(read.table("A07PSSS.txt"))
A07GSES <- as.matrix(read.table("A07GSES.txt"))
A07City <- as.matrix(read.table("文07City.txt"))
A07OnlyKid <- as.matrix(read.table("文07onlykid.txt"))
A07EduMom <- as.matrix(read.table("文07EduMom.txt"))
A07grade <- as.matrix(read.table("A07score.txt"))

A08T1friend <- as.matrix(read.table("文08T1friend.txt"))
A08T2friend <- as.matrix(read.table("文08T2friend.txt"))
A08T3friend <- as.matrix(read.table("文08T3friend.txt"))
A08T4friend <- as.matrix(read.table("文08T4friend.txt"))
A08TestAnxiety <- as.matrix(read.table("A08TAI.txt"))
A08Depression <- as.matrix(read.table("A08CESD.txt"))
A08gender <- as.matrix(read.table("文08gender.txt"))
A08SocialSupport <- as.matrix(read.table("A08PSSS.txt"))
A08GSES <- as.matrix(read.table("A08GSES.txt"))
A08City <- as.matrix(read.table("文08City.txt"))
A08OnlyKid <- as.matrix(read.table("文08onlykid.txt"))
A08EduMom <- as.matrix(read.table("文08EduMom.txt"))
A08grade <- as.matrix(read.table("A08score.txt"))

A17T1friend <- as.matrix(read.table("文17T1friend.txt"))
A17T2friend <- as.matrix(read.table("文17T2friend.txt"))
A17T3friend <- as.matrix(read.table("文17T3friend.txt"))
A17T4friend <- as.matrix(read.table("文17T4friend.txt"))
A17TestAnxiety <- as.matrix(read.table("A17TAI.txt"))
A17Depression <- as.matrix(read.table("A17CESD.txt"))
A17gender <- as.matrix(read.table("文17gender.txt"))
A17SocialSupport <- as.matrix(read.table("A17PSSS.txt"))
A17GSES <- as.matrix(read.table("A17GSES.txt"))
A17City <- as.matrix(read.table("文17City.txt"))
A17OnlyKid <- as.matrix(read.table("文17onlykid.txt"))
A17EduMom <- as.matrix(read.table("文17EduMom.txt"))
A17grade <- as.matrix(read.table("A17score.txt"))

# ----------------identify missing values (see 'klasdata-readme.txt')----------------
S04T1friend[c(6,9,14,16,21,28,30,33,39,48,52,71,79,87,107,110),] <- 9
S04T2friend[c(5,14,16,33,85,106,108,142),] <- 9
S04T3friend[c(7,16,33,76,78,85,108,121,142),] <- 9
S04T4friend[c(21,33,34,58,61,74,76,77,78,85,95,107,108,115,139,142),] <- 9
S11T1friend[c(6,14,60,103,105,127),] <- 9
S11T2friend[c(14,30,60,157),] <- 9
S11T3friend[c(2,3,4,8,11,16,30,47,52,53,59,66,70,74,78,96,97,109,118,124,125,127,128,130,143,148,157,158),] <- 9
S11T4friend[c(1,4,14,15,17,20,30,44,51,58,64,67,73,80,113,117,118,126,127,140,148,155,157,158),] <- 9
S27T1friend[c(32,33,43,47,50,127),] <- 9
S27T2friend[c(2,13,33,50,69,70,88,89,103,128),] <- 9
S27T3friend[c(2,14,33,43,50,52,58,59,70,79,88,89,91,102,107,118,127,128,135,136,142,154,157),] <- 9
S27T4friend[c(1,3,6,9,10,13,17,20,26,29,41,43,44,46,49,56,58,59,63,65,68,69,70,71,78:81,84,89,90,94,
              96,98,101:103,105:107,110,112,115,118:121,124,126,128,142,145,146,149,151,153,154,157:159,162),] <- 9
S35T1friend[c(23),] <- 9
S35T2friend[c(20,55),] <- 9
S35T3friend[c(20,54,60,98,107,122,146),] <- 9
S35T4friend[c(4,13,16,20,21,24,34,47,50,56,62,69,88,98),] <- 9
A07T1friend[c(11,51,69,87,142),] <- 9
A07T2friend[c(24,94,124),] <- 9
A07T3friend[c(27,34,36,94,124,140:142),] <- 9
A07T4friend[c(8,18,26,30,36,39,50,62,63,79,80,82,85,87,89,94,116:118,124,125,134,135,137,142,144),] <- 9
A08T1friend[c(1,3,5,10,18,26,35,40,50,54,57,69,71,78,84,91,124,128,135,136,139),] <- 9
A08T2friend[c(1,8,26,29,36,37,39,46,57,67,69,76,78,92,99,100,108,133),] <- 9
A08T3friend[c(1,4,12,14,22,26,32,35,43,48,57,67,69,78,89,92,100,131,138,146),] <- 9
A08T4friend[c(1,14,26,35,38,41,43,48,52,64,73,78,80,83,89,97,98,100:102,104,114,117,122,133,136,137,140,143),] <- 9
A17T1friend[c(43,100,106,110),] <- 9
A17T2friend[c(13,36,66,94,107),] <- 9
A17T3friend[c(36,63,100,102,106,108,145),] <- 9
A17T4friend[c(2,4,9,26,29,30,36,40,45,46,49,51,60,61,74,85,87,100,103,104,106,108,123,136,138,141),] <- 9

S04T1friend[S04T1friend==9] <- NA
S04T2friend[S04T2friend==9] <- NA
S04T3friend[S04T3friend==9] <- NA
S04T4friend[S04T4friend==9] <- NA
S04TestAnxiety[S04TestAnxiety==99] <- NA
S04SocialSupport[S04SocialSupport==99] <- NA
S04Depression[S04Depression==99] <- NA
S04GSES[S04GSES==99] <- NA
S04City[S04City==99] <- NA
S04OnlyKid[S04OnlyKid==99] <- NA
S04EduMom[S04EduMom==99] <- NA

S11T1friend[S11T1friend==9] <- NA
S11T2friend[S11T2friend==9] <- NA
S11T3friend[S11T3friend==9] <- NA
S11T4friend[S11T4friend==9] <- NA
S11TestAnxiety[S11TestAnxiety==99] <- NA
S11SocialSupport[S11SocialSupport==99] <- NA
S11Depression[S11Depression==99] <- NA
S11GSES[S11GSES==99] <- NA
S11City[S11City==99] <- NA
S11OnlyKid[S11OnlyKid==99] <- NA
S11EduMom[S11EduMom==99] <- NA

S27T1friend[S27T1friend==9] <- NA
S27T2friend[S27T2friend==9] <- NA
S27T3friend[S27T3friend==9] <- NA
S27T4friend[S27T4friend==9] <- NA
S27TestAnxiety[S27TestAnxiety==99] <- NA
S27SocialSupport[S27SocialSupport==99] <- NA
S27Depression[S27Depression==99] <- NA
S27GSES[S27GSES==99] <- NA
S27City[S27City==99] <- NA
S27OnlyKid[S27OnlyKid==99] <- NA
S27EduMom[S27EduMom==99] <- NA

S35T1friend[S35T1friend==9] <- NA
S35T2friend[S35T2friend==9] <- NA
S35T3friend[S35T3friend==9] <- NA
S35T4friend[S35T4friend==9] <- NA
S35TestAnxiety[S35TestAnxiety==99] <- NA
S35SocialSupport[S35SocialSupport==99] <- NA
S35Depression[S35Depression==99] <- NA
S35GSES[S35GSES==99] <- NA
S35City[S35City==99] <- NA
S35OnlyKid[S35OnlyKid==99] <- NA
S35EduMom[S35EduMom==99] <- NA

A07T1friend[A07T1friend==9] <- NA
A07T2friend[A07T2friend==9] <- NA
A07T3friend[A07T3friend==9] <- NA
A07T4friend[A07T4friend==9] <- NA
A07TestAnxiety[A07TestAnxiety==99] <- NA
A07SocialSupport[A07SocialSupport==99] <- NA
A07Depression[A07Depression==99] <- NA
A07GSES[A07GSES==99] <- NA
A07City[A07City==99] <- NA
A07OnlyKid[A07OnlyKid==99] <- NA
A07EduMom[A07EduMom==99] <- NA

A08T1friend[A08T1friend==9] <- NA
A08T2friend[A08T2friend==9] <- NA
A08T3friend[A08T3friend==9] <- NA
A08T4friend[A08T4friend==9] <- NA
A08TestAnxiety[A08TestAnxiety==99] <- NA
A08SocialSupport[A08SocialSupport==99] <- NA
A08Depression[A08Depression==99] <- NA
A08GSES[A08GSES==99] <- NA
A07City[A07City==99] <- NA
A07OnlyKid[A07OnlyKid==99] <- NA
A07EduMom[A07EduMom==99] <- NA

A17T1friend[A17T1friend==9] <- NA
A17T2friend[A17T2friend==9] <- NA
A17T3friend[A17T3friend==9] <- NA
A17T4friend[A17T4friend==9] <- NA
A17TestAnxiety[A17TestAnxiety==99] <- NA
A17SocialSupport[A17SocialSupport==99] <- NA
A17Depression[A17Depression==99] <- NA
A17City[A17City==99] <- NA
A17OnlyKid[A17OnlyKid==99] <- NA
A17EduMom[A17EduMom==99] <- NA

S04grade[S04grade==0] <- NA
S11grade[S11grade==0] <- NA
S27grade[S27grade==0] <- NA
S35grade[S35grade==0] <- NA
A07grade[A07grade==0] <- NA
A08grade[A08grade==0] <- NA
A17grade[A17grade==0] <- NA

# ----------------find out number of actors in data sets----------------
S04numberActors <- dim(S04TestAnxiety)[1]
S11numberActors <- dim(S11TestAnxiety)[1]
S27numberActors <- dim(S27TestAnxiety)[1]
S35numberActors <- dim(S35TestAnxiety)[1]
A07numberActors <- dim(A07TestAnxiety)[1]
A08numberActors <- dim(A08TestAnxiety)[1]
A17numberActors <- dim(A17TestAnxiety)[1]

# ----------------identify joiners and leavers----------------

S04Comp <- rep(list(c(1,4)),S04numberActors)
S11Comp <- rep(list(c(1,4)),S11numberActors)
S27Comp <- rep(list(c(1,4)),S27numberActors)
S35Comp <- rep(list(c(1,4)),S35numberActors)
A07Comp <- rep(list(c(1,4)),A07numberActors)
A08Comp <- rep(list(c(1,4)),A08numberActors)
A17Comp <- rep(list(c(1,4)),A17numberActors)
S04Comp[[9]] <- c(1.99,4)
S04Comp[[16]] <- c(3.99,4)
S04Comp[[39]] <- c(1.99,4)
S04Comp[[48]] <- c(1.99,4)
S04Comp[[58]] <- c(1,3.99)
S04Comp[[76]] <- c(1,3.99)
S04Comp[[78]] <- c(1,2.99)
S04Comp[[95]] <- c(1,3.99)
S04Comp[[107]] <- c(1.99,3.99)
S04Comp[[115]] <- c(1,3.99)
S04Comp[[142]] <- c(1,1.99)
S11Comp[[1]] <- c(1,3.99)
S11Comp[[14]] <- c(2.99,3.99)
S11Comp[[17]] <- c(1,3.99)
S11Comp[[20]] <- c(1,3.99)
S11Comp[[30]] <- c(1,1.99)
S11Comp[[51]] <- c(1,3.99)
S11Comp[[60]] <- c(2.99,4)
S11Comp[[67]] <- c(1,3.99)
S11Comp[[105]] <- c(1.99,4)
S11Comp[[117]] <- c(1,3.99)
S11Comp[[118]] <- c(1,3.99)
S11Comp[[126]] <- c(1,3.99)
S11Comp[[157]] <- c(1,1.99)
S11Comp[[158]] <- c(1,2.99)
S27Comp[[3]] <- c(1,3.99)
S27Comp[[6]] <- c(1,3.99)
S27Comp[[9]] <- c(1,3.99)
S27Comp[[10]] <- c(1,3.99)
S27Comp[[17]] <- c(1,3.99)
S27Comp[[33]] <- c(3.99,4)
S27Comp[[43]] <- c(1.99,2.99)
S27Comp[[50]] <- c(3.99,4)
S27Comp[[58]] <- c(1,3.99)
S27Comp[[68]] <- c(1,3.99)
S27Comp[[70]] <- c(1,2.99)
S27Comp[[71]] <- c(1,3.99)
S27Comp[[78]] <- c(1,3.99)
S27Comp[[79]] <- c(1,2.99)
S27Comp[[84]] <- c(1,3.99)
S27Comp[[101]] <- c(1,3.99)
S27Comp[[102]] <- c(1,3.99)
S27Comp[[103]] <- c(1,3.99)
S27Comp[[115]] <- c(1,3.99)
S27Comp[[124]] <- c(1,3.99)
S27Comp[[127]] <- c(1.99,4)
S27Comp[[128]] <- c(1,1.99)
S35Comp[[13]] <- c(1,3.99)
S35Comp[[20]] <- c(1,1.99)
S35Comp[[34]] <- c(1,3.99)
S35Comp[[50]] <- c(1,3.99)
S35Comp[[98]] <- c(1,2.99)
A07Comp[[11]] <- c(1.99,4)
A07Comp[[36]] <- c(1,3.99)
A07Comp[[50]] <- c(1,3.99)
A07Comp[[62]] <- c(1,3.99)
A07Comp[[87]] <- c(1.99,3.99)
A07Comp[[117]] <- c(1,3.99)
A07Comp[[124]] <- c(1,3.99)
A07Comp[[125]] <- c(1,3.99)
A07Comp[[142]] <- c(1,3.99)
A08Comp[[1]] <- c(3.99,4)
A08Comp[[57]] <- c(3.99,4)
A08Comp[[69]] <- c(3.99,4)
A08Comp[[78]] <- c(1.99,2.99)
A08Comp[[83]] <- c(1,3.99)
A08Comp[[89]] <- c(1,2.99)
A08Comp[[97]] <- c(1,3.99)
A08Comp[[100]] <- c(1,1.99)
A08Comp[[101]] <- c(1,3.99)
A08Comp[[102]] <- c(1,3.99)
A17Comp[[26]] <- c(1,3.99)
A17Comp[[43]] <- c(1.99,4)
A17Comp[[46]] <- c(1,3.99)
A17Comp[[100]] <- c(1.99,2.99)
A17Comp[[106]] <- c(1,3.99)
A17Comp[[123]] <- c(1,3.99)

# ----------------identify dependent variable for the analysis----------------
S04friends <- sienaNet(array(c(S04T1friend,S04T2friend,S04T3friend,S04T4friend),
                             dim=c(S04numberActors,S04numberActors,4)))
S11friends <- sienaNet(array(c(S11T1friend,S11T2friend,S11T3friend,S11T4friend),
                             dim=c(S11numberActors,S11numberActors,4)))
S27friends <- sienaNet(array(c(S27T1friend,S27T2friend,S27T3friend,S27T4friend),
                             dim=c(S27numberActors,S27numberActors,4)))
S35friends <- sienaNet(array(c(S35T1friend,S35T2friend,S35T3friend,S35T4friend),
                             dim=c(S35numberActors,S35numberActors,4)))
A07friends <- sienaNet(array(c(A07T1friend,A07T2friend,A07T3friend,A07T4friend),
                             dim=c(A07numberActors,A07numberActors,4)))
A08friends <- sienaNet(array(c(A08T1friend,A08T2friend,A08T3friend,A08T4friend),
                             dim=c(A08numberActors,A08numberActors,4)))
A17friends <- sienaNet(array(c(A17T1friend,A17T2friend,A17T3friend,A17T4friend),
                             dim=c(A17numberActors,A17numberActors,4)))

S04Depbeh <- sienaDependent( S04Depression, type = "behavior" )
S11Depbeh <- sienaDependent( S11Depression, type = "behavior" )
S27Depbeh <- sienaDependent( S27Depression, type = "behavior" )
S35Depbeh <- sienaDependent( S35Depression, type = "behavior" )
A07Depbeh <- sienaDependent( A07Depression, type = "behavior" )
A08Depbeh <- sienaDependent( A08Depression, type = "behavior" )
A17Depbeh <- sienaDependent( A17Depression, type = "behavior" )

S04SocSupbeh <- sienaDependent( S04SocialSupport, type = "behavior" )
S11SocSupbeh <- sienaDependent( S11SocialSupport, type = "behavior" )
S27SocSupbeh <- sienaDependent( S27SocialSupport, type = "behavior" )
S35SocSupbeh <- sienaDependent( S35SocialSupport, type = "behavior" )
A07SocSupbeh <- sienaDependent( A07SocialSupport, type = "behavior" )
A08SocSupbeh <- sienaDependent( A08SocialSupport, type = "behavior" )
A17SocSupbeh <- sienaDependent( A17SocialSupport, type = "behavior" )

S04GSESbeh <- sienaDependent( S04GSES, type = "behavior" )
S11GSESbeh <- sienaDependent( S11GSES, type = "behavior" )
S27GSESbeh <- sienaDependent( S27GSES, type = "behavior" )
S35GSESbeh <- sienaDependent( S35GSES, type = "behavior" )
A07GSESbeh <- sienaDependent( A07GSES, type = "behavior" )
A08GSESbeh <- sienaDependent( A08GSES, type = "behavior" )
A17GSESbeh <- sienaDependent( A17GSES, type = "behavior" )

S04TAIbeh <- sienaDependent( S04TestAnxiety, type = "behavior" )
S11TAIbeh <- sienaDependent( S11TestAnxiety, type = "behavior" )
S27TAIbeh <- sienaDependent( S27TestAnxiety, type = "behavior" )
S35TAIbeh <- sienaDependent( S35TestAnxiety, type = "behavior" )
A07TAIbeh <- sienaDependent( A07TestAnxiety, type = "behavior" )
A08TAIbeh <- sienaDependent( A08TestAnxiety, type = "behavior" )
A17TAIbeh <- sienaDependent( A17TestAnxiety, type = "behavior" )

# ----------------identify covariates for the analysis----------------
S04genderco <- coCovar( S04gender[,1] )
S11genderco <- coCovar( S11gender[,1] )
S27genderco <- coCovar( S27gender[,1] )
S35genderco <- coCovar( S35gender[,1] )
A07genderco <- coCovar( A07gender[,1] )
A08genderco <- coCovar( A08gender[,1] )
A17genderco <- coCovar( A17gender[,1] )

S04Cityco <- coCovar( S04City[,1] )
S11Cityco <- coCovar( S11City[,1] )
S27Cityco <- coCovar( S27City[,1] )
S35Cityco <- coCovar( S35City[,1] )
A07Cityco <- coCovar( A07City[,1] )
A08Cityco <- coCovar( A08City[,1] )
A17Cityco <- coCovar( A17City[,1] )

S04OnlyKidco <- coCovar( S04OnlyKid[,1] )
S11OnlyKidco <- coCovar( S11OnlyKid[,1] )
S27OnlyKidco <- coCovar( S27OnlyKid[,1] )
S35OnlyKidco <- coCovar( S35OnlyKid[,1] )
A07OnlyKidco <- coCovar( A07OnlyKid[,1] )
A08OnlyKidco <- coCovar( A08OnlyKid[,1] )
A17OnlyKidco <- coCovar( A17OnlyKid[,1] )

S04EduMomco <- coCovar( S04EduMom[,1] )
S11EduMomco <- coCovar( S11EduMom[,1] )
S27EduMomco <- coCovar( S27EduMom[,1] )
S35EduMomco <- coCovar( S35EduMom[,1] )
A07EduMomco <- coCovar( A07EduMom[,1] )
A08EduMomco <- coCovar( A08EduMom[,1] )
A17EduMomco <- coCovar( A17EduMom[,1] )

S04gradeco <- coCovar( S04grade[,1] )
S11gradeco <- coCovar( S11grade[,1] )
S27gradeco <- coCovar( S27grade[,1] )
S35gradeco <- coCovar( S35grade[,1] )
A07gradeco <- coCovar( A07grade[,1] )
A08gradeco <- coCovar( A08grade[,1] )
A17gradeco <- coCovar( A17grade[,1] )

S04SESco <- coCovar( S04SES[,1] )
S11SESco <- coCovar( S11SES[,1] )
S27SESco <- coCovar( S27SES[,1] )
S35SESco <- coCovar( S35SES[,1] )
A07SESco <- coCovar( A07SES[,1] )
A08SESco <- coCovar( A08SES[,1] )
A17SESco <- coCovar( A17SES[,1] )

# identify composition change for the analysis:
S04changes <- sienaCompositionChange(S04Comp)
S11changes <- sienaCompositionChange(S11Comp)
S27changes <- sienaCompositionChange(S27Comp)
S35changes <- sienaCompositionChange(S35Comp)
A07changes <- sienaCompositionChange(A07Comp)
A08changes <- sienaCompositionChange(A08Comp)
A17changes <- sienaCompositionChange(A17Comp)

# combine data for the analysis (& make sure variable names
# are identical in the different project data objects!):
S04data <- sienaDataCreate(friends=S04friends,gender=S04genderco,
                           TAI=S04TAIbeh,CopmChan=S04changes)
S11data <- sienaDataCreate(friends=S11friends,gender=S11genderco,
                           TAI=S11TAIbeh,CopmChan=S11changes)
S27data <- sienaDataCreate(friends=S27friends,gender=S27genderco,
                           TAI=S27TAIbeh,CopmChan=S27changes)
S35data <- sienaDataCreate(friends=S35friends,gender=S35genderco,
                           TAI=S35TAIbeh,CopmChan=S35changes)
A07data <- sienaDataCreate(friends=A07friends,gender=A07genderco,
                           TAI=A07TAIbeh,CopmChan=A07changes)
A08data <- sienaDataCreate(friends=A08friends,gender=A08genderco,
                           TAI=A08TAIbeh,CopmChan=A08changes)
A17data <- sienaDataCreate(friends=A17friends,gender=A17genderco,
                           TAI=A17TAIbeh,CopmChan=A17changes)


# ---------------- PLOT -----------
require(graphics)
heat.colors(7, alpha = 1)
color_sex <- c('red','blue')  # identify sex by color 
color7 <- rev(heat.colors(7, alpha = 1))
g.layout <- gplot(apply(S04friends, c(1,2), max), usearrows=F)
par(mfrow=c(1,1))
for (i in 1:4) {
  gplot(S04friends[,,i], vertex.col=color7[S04Depression[,i]+1], arrowhead.cex=.75, edge.col='gray70', coord=g.layout, 	
        vertex.cex=1.5, main=paste('Test anxiety t',i,sep="") )
}

# ---------------- STARTING MULTI-GROUP ANALYSIS -----------

# and join into multi-group project:
groupdata <- sienaGroupCreate(list(S04data,S11data,S27data,S35data,A07data,A08data,A17data))

# get effects object:
groupEffects <- getEffects(groupdata)

# etc. (the rest follows the same pattern as 'normal' analyses).
# More specifically:

groupEffects <- includeEffects(groupEffects,density,recip,transTrip,cycle3,nbrDist2,inPopSqrt,test = F,fix = F)

groupEffects <- includeEffects(groupEffects,outActSqrt,test = T,fix = T)

groupEffects <- setEffect(groupEffects,outActSqrt,include = F)

groupEffects <- includeEffects(groupEffects,altX,sameX,interaction1="gender")

groupEffects <- includeEffects(groupEffects,altX,diffX,interaction1="TAI")

groupEffects <- includeEffects(groupEffects,sameX,interaction1="TAI",test = T,fix = T)

groupEffects <- includeEffects(groupEffects,name = "TAI",totSim,interaction1 = "friends",test = T,fix = T)

groupEffects <- includeEffects( groupEffects,name = "TAI", effFrom,
                                interaction1 = "genderX")
groupEffects <- includeEffects( groupEffects,name = "TAI", effFrom,
                                interaction1 = "SocSup")
groupEffects <- includeEffects( groupEffects,name = "TAI", effFrom,
                                interaction1 = "Dep")

groupModel <- sienaModelCreate(useStdInits=FALSE, 
                               projname='TAI_0113',seed = 949413)
ConvergenceResults <- siena07ToConvergence(groupModel, groupdata, groupEffects, batch=F,verbose=FALSE, 
                                           nbrNodes = 4,useCluster = TRUE,initC = TRUE, returnDeps = T)

# ---------------- GOODNESS OF FIT -----------
# goodness of fit on indegree
( gof.id <- sienaGOF(ConvergenceResults, verbose=TRUE, varName="friends", 
                     IndegreeDistribution,join=T, cumulative=F) )
plot(gof.id)
# goodness of fit on outdegree
( gof.od <- sienaGOF(ConvergenceResults, verbose=TRUE, varName="friends", 
                     OutdegreeDistribution,join=T, cumulative=F, levls=0:5) )
plot(gof.od)
# goodness of fit on reciprocity
( gof.rd <- sienaGOF(ConvergenceResults, verbose=TRUE, varName="friends", 
                     ReciprocityDistribution,join=T, cumulative=F) )
plot(gof.rd)
# goodness of fit on behaviour
( gof.bd <- sienaGOF(ConvergenceResults, verbose=TRUE, varName="TAI", 
                     BehaviorDistribution,join=T, cumulative=F) )
plot(gof.bd)
# goodness of fit on TriadCensus
( gof.TriadCensus <- sienaGOF(ConvergenceResults, verbose=TRUE, varName="friends", 
                     TriadCensus,join=T) )
plot(gof.TriadCensus)

# ---------------- SELECTION TABLE -----------
sm.TAI <- selectionMatrix(ConvergenceResults, groupdata, "friends", "TAI", 0:5)
# It can be displayed
sm.TAI
# and if package xtable is loaded, also be written
# to a latex or html file. For example,
tab.TAI <- xtable(sm.TAI)
print(tab.TAI,file="tab_TAI.htm", type="html",
      html.table.attributes = "rules = none")
# The html.table.attributes option gives the <table> tag
# used in the html file.
# and optional
xtable(sm.TAI) # for a LaTeX table
