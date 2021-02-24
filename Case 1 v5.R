#########################Case 1-Computational data analysis#####################################
## Loading packages ####
library(markdown)
library(caret)
library(dplyr)
library(doFuture)
library(doParallel)
library(earth)
library(gbm)
library(gam)
library(ggplot2)
library(glmnet)
library(grid)
library(gridExtra)
library(hexbin)
library(ipred)
library(labeling)
library(MASS)
library(pdp)
library(plotmo)
library(randomForest)
library(ranger)
library(reshape2)
library(rlang)
library(rpart.plot)
library(rsample)
library(shape)
library(splines)
library(pROC)
library(caTools)
library(VIM)
library(adimpro)
library(lars)
library(corrr)
## Setting working directory####
setwd("~/Desktop/Computational Data Analysis (02582)/Case 1")
##Loading data####
data = data.frame(read.csv("case1Data.txt",sep=",",header=TRUE))
new = data.frame(read.csv("new.txt",sep=",",header=TRUE))
str(data);str(new) #95 numeric, 5 factor (with NA's)
tail(data);tail(new)
#############Explatoratory data analysis################
##Descriptive summaries####
summary(data)



cor.print <- function(x,y){panel.text(mean(range(x)),
                                      mean(range(y)),paste('$',round(cor(x,y),digits=2),'$',sep=''))}
scat.plot.matrix <- function(data,contVar){splom(na.omit(data)[,contVar],xlab=""
                                               ,upper.panel=panel.hexbinplot,pscales=0,xbins=20
                                               ,varnames=c(contVar),lower.panel=cor.print)}



## Missing values ####
levels(data$C_.1)[6]<-NA;levels(new$C_.1)[6]<-NA
levels(data$C_.2)[3]<-NA;levels(new$C_.2)[3]<-NA
levels(data$C_.3)[6]<-NA;levels(new$C_.3)[6]<-NA
levels(data$C_.4)[6]<-NA;levels(new$C_.4)[6]<-NA
levels(data$C_.5)[6]<-NA;levels(new$C_.5)[6]<-NA

sum(is.na(data[,1:96])) ##Only NA's for the 5 factor variables
sum(is.na(data[,97:101]))/(nrow(data[,97:101])*ncol(data[,97:101])) #13 pct. of factor obs. are missing in train data
sum(is.na(new[,96:100]))/(nrow(new[,96:100])*ncol(new[,96:100])) #16 pct. of factor obs. are missing in test data

## Visualization of missing values##
navis=VIM::aggr(data[,97:101],numbers=T)
navis=VIM::aggr(new[,96:100],numbers=T) #Not much trend in missing values (MCAR)

##Boxplots of the categorical variables ####
box<-melt(data[,c("y","C_.1","C_.2","C_.3","C_.4","C_.5")]
          ,id.vars="y",factorsAsStrings=F)
ggplot(box,
       aes(x=factor(value),y=y))+
  geom_boxplot(fill=I(gray(0.8)))+xlab("")+
  stat_summary(fun.y="mean",col="red")+
facet_wrap( ~ variable,scale="free_x",ncol=3,nrow=2)
#C_4 and C_5 have similar trend in mean response
#C_1 and C_3 have similar trend in mean response
#Couple of outliers-Could affect the OLS estimate in regularization


##Distribution of the categorical variables####
tmp<-lapply(names(data),function(x)
  ggplot(data=data[,x,drop=F])+
              aes_string(x)+xlab(x)+ylab(""))
gb<-geom_bar()
grid.arrange(
  tmp[[97]]+gb,
  tmp[[98]]+gb,
  tmp[[99]]+gb,
  tmp[[100]]+gb,
  tmp[[101]]+gb
  ,ncol=5
)

## Corrolation with response####
cor_data<-data.frame(t(correlate(data[,1:96],
                   use="pairwise.complete.obs",
                   diagonal=NA,method="spearman")[1,]))
cor_data<-cor_data[-c(1:2),]
cor_data<-data.frame(as.numeric(levels(cor_data))[cor_data])
names(cor_data)="Correlation with response,y"
strongcor=subset(cor_data,abs(cor_data$`Correlation with response,y`)>0.4)
#9 strongest correlated variables with y
ggplot(data,aes(x_42,y))+geom_point() #strongest correlated variable scatterplot
