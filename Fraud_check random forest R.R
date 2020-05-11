fraud <- read.csv(file.choose())
View(fraud)
fraud1 <- fraud
summary(fraud)
str(fraud)
fraud1$Taxable.Income <- ifelse(fraud1$Taxable.Income<=30000,"Risky","Good")
table(fraud1$Taxable.Income)
fraud1$Taxable.Income <- as.factor(fraud1$Taxable.Income)
str(fraud1)
library(randomForest)
mod1 <- randomForest(fraud1$Taxable.Income~.,data=fraud1)
mod1
plot(mod1)
legend("topright",colnames(mod1$err.rate),col=1:3,cex=0.8,fill=1:3)
prd <- predict(mod1,fraud1)
prd
table(prd)
crossTable(prd,fraud1$Taxable.Income)
mean(prd==fraud1$Taxable.Income)
#we are getting 87.5% accuracy.