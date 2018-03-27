library(readxl)
housingdf <- read_excel("Desktop/Data Mining /KNN/BostonHousing(1).xlsx",
sheet = "Data")
normfunc <- function(x){
(x-min(x))/(max(x)-min(x))
}
housingdfmain<- as.data.frame(lapply(housingdf[,-c(13,14)], normfunc))

summary(housingdfmain)


traindata = housingdfmain[1:303,]
nrow(traindata)
trainresponse = housingdf[1:303,14]
nrow(trainresponse)
testdata = housingdfmain[304:506,]
testresponse = housingdf[304:506,14]
knn.1 <- knn(traindata, testdata, trainresponse$`CAT. MEDV`, k=1)
tablesamp <- table(knn.1, testresponse$`CAT. MEDV`)
tablesamp
err=(tablesamp[1,2]+tablesamp[2,1])/203


library(class)
getknnerr <-function(n) {
  ncount=0
  looper=0
  for(i in 1:n){
    looper=looper+1
    knnsamp <- knn(traindata, testdata, trainresponse$`CAT. MEDV`, k=i)
    tablesamp <- table(knnsamp, testresponse$`CAT. MEDV`)
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
foundn <- getknnerr(5)

# k is 2
testcase<-c(0.2,0,7,0,0.538,6,62,4.7,4,307,21,10)
tempdf <- rbind(housingdf[,-c(13,14)],testcase)
tempdf <- as.data.frame(lapply(tempdf, normfunc))
testknn <- knn(traindata,tempdf[507,],trainresponse$`CAT. MEDV`,k=2, prob = TRUE)
testknn
attr(testknn,"prob")
