library(readxl)
BankDF <- read_excel("Desktop/Data Mining /KNN/UniversalBank(1).xlsx",
sheet = "Data")
#Transforming categorical variable Education into dummy variables.
BankDF$Education2 <- ifelse(BankDF$Education==2,1,0)
BankDF$Education3 <- ifelse(BankDF$Education==3,1,0)
BankManiDF <- BankDF[,-c(1,5,8,10)]
trainsize = 0.6
splitpoint <- nrow(BankManiDF)*trainsize
#Break data into training and validation data
traindata <- BankManiDF[1:splitpoint,]
vaild_data <- BankManiDF[(splitpoint+1):nrow(BankManiDF),]
response <- BankDF$`Personal Loan`
trainresponse <- response[1:splitpoint]
valid_response <- response[(splitpoint+1):nrow(BankManiDF)]
#Now lets run knn
library(class)
knn.1 <- knn(traindata, vaild_data, trainresponse, k=23, prob = TRUE)
knn.1


100 * sum(valid_response == knn.1)/2000
#Lets make a table
t1 <- table(knn.1,valid_response)
t1
err=(t1[1,2]+t1[2,1])/2000
#To find the k value with the least error, let's automate it with a simple script
getknnerr <-function(n) {
  ncount=0
  looper=0
  for(i in 1:n){
    looper=looper+1
    knnsamp <- knn(traindata, vaild_data, trainresponse, k=i)
    tablesamp <- table(knnsamp, valid_response)
    errcheck=(tablesamp[1,2]+tablesamp[2,1])/2000
    print(i)
    if(errcheck<err){
      ncount=i
      print(i)
      print(errcheck)
      err=errcheck
      if(looper==n){
        return(ncount)
      }
    }
  }
}
#Let's run our function for up to 70-nearest neighbors
iter<-getknnerr(70)
#We get 23 as k value with least error with error margin of 0.088

# Verify against custom example
CustomSearch <- colnames(BankManiDF)
CustomSearch <- c(35,9,84,2,2,0,0,0,1,1,1,0)

Customresponse <- c(1)
#add new column in vaild_data
Vaild_data_new <- rbind(vaild_data,CustomSearch)
knntest_new <- knn(traindata,CustomSearch,trainresponse,k=1, prob = TRUE)
knntest_new
attr(knntest_new,"prob")


"
library(xgboost)
dtrain=xgb.DMatrix(data = data.matrix(traindata), label=data.matrix(trainresponse))
xgmodel=xgboost(data = dtrain, nround=10, objective="binary:logistic")
cv=xgb.cv(data = dtrain,nrounds = 10, nfold = 5, objective="binary:logistic")
preds=predict(xgmodel,data.matrix(vaild_data))
prediction=as.numeric(preds>0.5)
prediction"

part5train<-BankManiDF[1:2500,]
part5trainrs<-response[1:2500]
part5valid<-BankManiDF[2501:4000,]
part5validrs<- response[2501:4000]
part5test<-BankManiDF[4001:5000,]
part5testrs<-response[4001:5000]

model1<-knn(part5train,part5valid,part5trainrs,k=23, prob = TRUE)
part5table <- table(model1, part5validrs)
part5table
100 * sum(part5validrs == model1)/1500
model2<-knn(part5train,part5test,part5trainrs,k=23, prob = TRUE)
part5tabletest <- table(model2, part5testrs)
part5tabletest
100 * sum(part5testrs == model2)/1000

