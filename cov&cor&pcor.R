library(Hmisc)
library(dplyr)
library(DMwR)
library(corpcor)

filename <- read.csv("PILIANG.csv")
filename$XPT = as.character(filename$XPT)
filename$NAME = as.character(filename$NAME)

for (i in 1:nrow(filename$XPT))
{file_name <- filename$NAME[i]
  file_df <- sasxport.get(filename$XPT[i])
  assign( x = file_name, value = file_df, envir = .GlobalEnv)}

all = left_join(ACQ_I,ALQ_I,by="seqn")

for (i in nrow(filename$XPT))
{all = left_join(all,file_df,by="seqn")}

ALL <- centralImputation(all)
cov.ALL = cov(ALL,method = spearman)
cor.ALL = cor(ALL,method = spearman)
pcor.ALL = cor2pcor(cov.ALL)
