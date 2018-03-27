library(readxl)
AccidentsDF<- read_excel("Desktop/Data Mining /KNN/Accidents(1).xlsx",
sheet = "Data")
AccidentsDF$INJURY<-ifelse(AccidentsDF$MAX_SEV_IR>0,1,0)
summary(AccidentsDF$INJURY)
table(AccidentsDF$INJURY)
write.table(AccidentsDF, file = "Injury.csv", na="", sep = ",",col.names = TRUE)
AccidentsDF$ALIGN_I <- factor(AccidentsDF$ALIGN_I)
AccidentsDF$WRK_ZONE <- factor(AccidentsDF$WRK_ZONE)
AccidentsDF$WKDY_I_R <- factor(AccidentsDF$WKDY_I_R)
AccidentsDF$INT_HWY <- factor(AccidentsDF$INT_HWY)
AccidentsDF$TRAF_WAY <- factor(AccidentsDF$TRAF_WAY)
AccidentsDF$WEATHER_R <- factor(AccidentsDF$WEATHER_R)
AccidentsDF$TRAF_CON_R <- factor(AccidentsDF$TRAF_CON_R)
AccidentsDF$INJURY <- factor(AccidentsDF$INJURY)
# create training and validation set now
library(e1071)
selectedvar <- c(3,5,6,7,17,19,25)
set.seed(1)
train.index = sample(c(1:dim(AccidentsDF)[1]), dim(AccidentsDF)[1]*0.6)
custom <- AccidentsDF[1:12,c(16,19)]
traindfnb <- AccidentsDF[train.index,c(16,19,25)]
traindf <- AccidentsDF[train.index,selectedvar]
validdf <- AccidentsDF[-train.index,selectedvar]
## Run naive bayes
AccidentsNB <- naiveBayes(INJURY~.,data = traindf)
AccidentsNB
## Pred Class
library(caret)
pred.class <- predict(AccidentsNB,newdata = traindf)
confusionMatrix(pred.class,traindf$INJURY)
## Validation Stats
pred.class <- predict(AccidentsNB,newdata = validdf)
confusionMatrix(pred.class,validdf$INJURY)
### NB predictor stats
Accidents2nb <- naiveBayes(INJURY~.,data = traindfnb)
Accidents2nb
