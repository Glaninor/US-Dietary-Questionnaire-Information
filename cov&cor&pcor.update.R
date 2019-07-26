library(Hmisc)
library(dplyr)
library(DMwR)
library(corpcor)

filename <- read.csv("PILIANG.csv")
filename$XPT = as.character(filename$XPT)
filename$NAME = as.character(filename$NAME)

file_name <- filename$NAME[1]
file_df <- sasxport.get(filename$XPT[1])
assign( x = file_name, value = file_df, envir = .GlobalEnv)
file_name <- filename$NAME[2]
file_df <- sasxport.get(filename$XPT[2])
assign( x = file_name, value = file_df, envir = .GlobalEnv)
all = left_join(ACQ_I,ALQ_I,by="seqn")

for (i in 1:38)
{file_name <- filename$NAME[i]
  file_df <- sasxport.get(filename$XPT[i])
  assign( x = file_name, value = file_df, envir = .GlobalEnv)
  all = left_join(all,file_df,by="seqn")}

DR2IFF = matrix(nrow=9971,ncol=84)
DR2IFF[,1]=83732:93702
colnames(DR2IFF) = colnames(DR2IFF_I)

for (i in 83732:93702)
{
  DR2IFF[which(DR2IFF_I$seqn==i),2:84] <- sapply(DR2IFF_I[which(DR2IFF_I$seqn==i),2:84],mean)
}
DR2IFF <- as.data.frame(DR2IFF)
all = left_join(all,DR2IFF,by="seqn")

DR1IFF = matrix(nrow=9971,ncol=84)
DR1IFF[,1]=83732:93702
colnames(DR1IFF) = colnames(DR1IFF_I)
for (i in 83732:93702)
{
  DR1IFF[which(DR1IFF_I$seqn==i),2:84] <- sapply(DR1IFF_I[which(DR1IFF_I$seqn==i),2:84],mean)
}
DR1IFF <- as.data.frame(DR1IFF)
all = left_join(all,DR1IFF,by="seqn")

RXQ_RX = matrix(nrow=9971,ncol=13)
RXQ_RX[,1]=83732:93702
colnames(RXQ_RX) = colnames(RXQ_RX_I)
for (i in 83732:93702)
{
  RXQ_RX[which(RXQ_RX_I$seqn==i),2:13] <- sapply(RXQ_RX_I[which(RXQ_RX_I$seqn==i),2:13],mean)
}
RXQ_RX <- as.data.frame(RXQ_RX)
all = left_join(all,RXQ_RX,by="seqn")

C = as.data.frame(matrix(ncol=1337,nrow = 2))
colnames(C) = colnames(all)
for (i in 1:1337)
{
  C[,i] <- as.character(class(all[,i]))
}
summary(C)
all <- all[,c(-709,-710,-727,-728)]

ALL <- all[,-1]
ALL <- centralImputation(ALL)
cov.ALL = cov(ALL,method = "spearman")
cor.ALL = cor(ALL,method = "spearman")
cor.ALL <- centralImputation(cor.ALL)
pcor.ALL = cor2pcor(cor.ALL)
colnames(pcor.ALL) = colnames(cor.ALL)
rownames(pcor.ALL) = rownames(cor.ALL)

