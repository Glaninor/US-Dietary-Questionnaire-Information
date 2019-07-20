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
all = left_join(ACQ_I,ALQ_I,by="seqn")

for (i in 1:nrow(filename))
{file_name <- filename$NAME[i]
  file_df <- sasxport.get(filename$XPT[i])
  assign( x = file_name, value = file_df, envir = .GlobalEnv)
  all = left_join(all,file_df,by="seqn")}

All <- all[,-1]
ALL <- centralImputation(All)
cov.ALL = cov(ALL,method = spearman)
cor.ALL = cor(ALL,method = spearman)
pcor.ALL = cor2pcor(cov.ALL)
