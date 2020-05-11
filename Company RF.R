cd <- read.csv(file.choose())
cd1 <- cd
View(cd)
cd1$ShelveLoc <- as.numeric(cd$ShelveLoc)
cd1$Urban <- as.numeric(cd$Urban)
cd1$US <- as.numeric(cd$US)
View(cd1)
summary(cd1$Sales)
library(tidyverse)
ggplot(cd1,aes(cd1$Sales))+geom_boxplot(fill="red")+coord_flip()
hist(cd1$Sales)
cd1$Sales <- ifelse(cd1$Sales>=10,"high",ifelse(cd1$Sales>=5,"medium","low"))
#Converting the Sales variable in factors 
cd1$Sales <- as.factor(cd1$Sales)
str(cd1)
table(cd$Sales)
#Conidering 0-5 as low sales,5-10 as medium sales,10-17 as high sales
library(randomForest)
mod <- randomForest(cd1$Sales~.,data = cd1)
mod
plot(mod)
legend("topright",colnames(mod$err.rate),col=1:3,cex=0.8,fill=1:3)
prd <- predict(mod,cd1)
table(prd)
str(prd)
View(prd)
library(gmodels)
library(arules)
crossTable(prd,cd1$Sales)
mean(prd==cd1$Sales)
#we are gettting an accuracy of 100% in our model

