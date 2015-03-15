library("caret")
library("ggplot2")
library("reshape")
library("corrplot")
library("randomForest")

train<-read.csv(("pml-training.csv"),stringsAsFactor=FALSE,skip = 0,fill=NA,comment.char="#")
test<-read.csv("pml-testing.csv")

length(names(train))

nrow(train)   

var<-names(train)[apply(train,2,function(x) table(is.na(x))[1]==19622)]   
train2<-train[,var]
test2<-test[,var[-length(var)]] 

var2<-melt(apply(train2,2,function(x) sum(ifelse(x=="",1,0)))==0)
select.var<-rownames(var2)[var2$value==TRUE]
train3<-train2[,select.var]
test3<-test2[,select.var[-length(select.var)]]
train4<-train3[,names(train3[-c(1:7,length(train3))])]      # only considering numeric variable from HAR sensor
test4<-test3[,names(test3[-c(1:7)])]

correlations <- cor(train4) 
corrplot(correlations,order = "hclust",tl.cex = .5) 

highCorr <- findCorrelation(correlations, cutoff = .75)
predictor <- train4[, -highCorr] 
filtered.test4 <- test4[, -highCorr] 
classe<-train3$classe 
trainData<-cbind(classe,predictor)  

rfModel <- randomForest(classe ~ .,data = trainData,importance = TRUE,ntrees = 10)

print(rfModel)    

varImpPlot(rfModel,cex=.5)   

confusion<-rfModel$confusion
sensitivity<-(confusion[2,2]/(confusion[2,2]+confusion[2,1]))*100
specificity<-(confusion[1,1]/(confusion[1,1]+confusion[1,2]))*100
overall_error<-rfModel$err.rate[length(rfModel$err.rate[,1]),1]*100
overall_accuracy<-1-overall_error

out.test<-predict(rfModel,filtered.test4)

