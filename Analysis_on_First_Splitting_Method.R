

### This script covers mainly analysis using first splitting method (Xingzhi Wang)

#Loading data
setwd('C:/Users/tcwan/Desktop/Classes/Spring2019/STAT154/project2') ## set this path to your folder containing given image data

varnames<-c('y','x','label','NDAI','SD','CORR','Rad_DF','Rad_CF','Rad_BF','Rad_AF','Rad_AN')
image1<-read.csv('image1.csv', header=FALSE, stringsAsFactor = FALSE)
colnames(image1)<-varnames
image2<-read.csv('image2.csv', header=FALSE, stringsAsFactor = FALSE)
colnames(image2)<-varnames
image3<-read.csv('image3.csv', header=FALSE, stringsAsFactor = FALSE)
colnames(image3)<-varnames

# 1.c Visualizing Relationship between features using correlation plot
library(corrplot)
data_all<-rbind(image1,image2,image3)
corrplot(cor(data_all), method = "number")

clouds<-data_all[data_all$label==1,]
notclouds<-data_all[data_all$label==-1,]

data<-data_all[data_all$label==1|data_all$label==-1,]

corrplot(cor(clouds), method = "number")
corrplot(cor(notclouds), method = "number")
#Makes Figure 2


library(ggplot2)
ggplot(data=clouds)+geom_histogram(aes(NDAI))
ggplot(data=notclouds)+geom_histogram(aes(NDAI))

ggplot(data=clouds)+geom_histogram(aes(log(SD)))
ggplot(data=notclouds)+geom_histogram(aes(log(SD)))

ggplot(data=clouds)+geom_histogram(aes(CORR))
ggplot(data=notclouds)+geom_histogram(aes(CORR))

ggplot(data=clouds)+geom_histogram(aes(Rad_DF))
ggplot(data=notclouds)+geom_histogram(aes(Rad_DF))

ggplot(data=clouds)+geom_histogram(aes(Rad_CF))
ggplot(data=notclouds)+geom_histogram(aes(Rad_CF))

ggplot(data=clouds)+geom_histogram(aes(Rad_BF))
ggplot(data=notclouds)+geom_histogram(aes(Rad_BF))

ggplot(data=clouds)+geom_histogram(aes(Rad_AF))
ggplot(data=notclouds)+geom_histogram(aes(Rad_AF))

ggplot(data=clouds)+geom_histogram(aes(Rad_AN))
ggplot(data=notclouds)+geom_histogram(aes(Rad_AN))

#EDA not included in the report


# 1. b.
cloud_percent_1<-sum(image1$label==1)/nrow(image1)
noncloud_percent_1<-sum(image1$label==-1)/nrow(image1)

cloud_percent_2<-sum(image2$label==1)/nrow(image2)
noncloud_percent_2<-sum(image2$label==-1)/nrow(image2)

cloud_percent_3<-sum(image3$label==1)/nrow(image3)
noncloud_percent_3<-sum(image3$label==-1)/nrow(image3)

# Calculates percentage of each label

#1. c.
ggplot() + geom_histogram(aes(x= notclouds$NDAI, col="green")) + geom_histogram(aes(x= clouds$NDAI, col="red")) + xlab("NDAI") 
ggplot() + geom_histogram(aes(x= notclouds$SD, col="green")) + geom_histogram(aes(x= clouds$SD, col="red")) + xlab("SD") 
ggplot() + geom_histogram(aes(x= notclouds$CORR, col="green")) + geom_histogram(aes(x= clouds$CORR, col="red")) + xlab("CORR") 

#Makes figure 3


# Training/test split by rad
image1_labelled<-image1[image1$label==1|image1$label==-1,]
image2_labelled<-image2[image2$label==1|image2$label==-1,]
image3_labelled<-image3[image3$label==1|image3$label==-1,]

data_labelled<-rbind(image1_labelled,image2_labelled,image3_labelled)
corrplot(cor(data_labelled), method = "number")

clouds<-data_all[data_all$label==1,]
notclouds<-data_all[data_all$label==-1,]

data<-data_all[data_all$label==1|data_all$label==-1,]

corrplot(cor(clouds), method = "number")
corrplot(cor(notclouds), method = "number")

seg_data<-list()
seg_size<-15000

for (i in 1:floor(dim(image1_labelled)[1]/seg_size)) {
  if (seg_size*(i+1)<dim(image1_labelled)[1]) {
    seg_data<-c(seg_data,list(image1_labelled[(1+seg_size*(i-1)):(seg_size*i),]))
  }
  else {
    seg_data<-c(seg_data,list(image1_labelled[(1+seg_size*(i-1)):nrow(image1_labelled),]))
  }
}

for (i in 1:floor(dim(image2_labelled)[1]/seg_size)) {
  if (seg_size*(i+1)<dim(image2_labelled)[1]) {
    seg_data<-c(seg_data,list(image2_labelled[(1+seg_size*(i-1)):(seg_size*i),]))
  }
  else {
    seg_data<-c(seg_data,list(image2_labelled[(1+seg_size*(i-1)):nrow(image2_labelled),]))
  }
}

for (i in 1:floor(dim(image3_labelled)[1]/seg_size)) {
  if (seg_size*(i+1)<dim(image3_labelled)[1]) {
    seg_data<-c(seg_data,list(image3_labelled[(1+seg_size*(i-1)):(seg_size*i),]))
  }
  else {
    seg_data<-c(seg_data,list(image3_labelled[(1+seg_size*(i-1)):nrow(image3_labelled),]))
  }
}



# 2.a
set.seed(42)
data_range<-1:length(seg_data)
train_sample<-sample(data_range,7)
val_sample<-sample(data_range[-train_sample],3)
test_sample<-data_range[-c(train_sample,val_sample)]

data_train<-seg_data[train_sample]
data_val<-seg_data[val_sample]
data_test<-seg_data[test_sample]

data_train_num<-c()
data_val_num<-c()
data_test_num<-c()

for (j in 1:length(data_train)) {
  data_train_num<-rbind(data_train_num,data_train[[j]])
}

for (k in 1:length(data_val)) {
  data_val_num<-rbind(data_val_num,data_val[[k]])
}

for (l in 1:length(data_test)) {
  data_test_num<-rbind(data_test_num,data_test[[l]])
}


#Training/test/validation split


# 2.b
percent_error<-function(truth,predictions) {
  return(sum(truth!=predictions)/length(truth))
}

val_baseline<-percent_error(data_val_num[,3],rep(-1,nrow(data_val_num)))
test_baseline<-percent_error(data_test_num[,3],rep(-1,nrow(data_test_num)))

#Calculates baseline error



# 2.d
CVgeneric<-function(K,classifier,features,labels,loss=percent_error) {
  
  n<-nrow(features)
  fold_size<-floor(n/K)
  k_fold_loss<-c()
  
  for (i in 1:K) {
    features_val<-c()
    features_train<-c()
    labels_val<-c()
    labels_train<-c()
    
  if (i!=K) {
    features_val<-features[(1+fold_size*(i-1)):(i*fold_size),]
    features_train<-features[-((1+fold_size*(i-1)):(i*fold_size)),]
    labels_val<-labels[(1+fold_size*(i-1)):(i*fold_size)]
    labels_train<-labels[-((1+fold_size*(i-1)):(i*fold_size))]
  } else {
    features_val<-features[(1+fold_size*(i-1)):n,]
    features_train<-features[-((1+fold_size*(i-1)):n),]
    labels_val<-labels[(1+fold_size*(i-1)):n]
    labels_train<-labels[-((1+fold_size*(i-1)):n)]
  }
    train<-cbind(features_train,labels_train)
    colnames(train)[ncol(train)]<-'label'
    predictions<-classifier(train,features_val)
    curr_lost<-loss(labels_val,predictions)
    k_fold_loss<-c(k_fold_loss,curr_lost)
    
  }
  return(k_fold_loss)
}

#CVgeneric function

# 3.a

cv_data<-data.frame(rbind(data_train_num,data_val_num))
form<-as.formula(paste("as.factor(label)", "~", "NDAI+SD+Rad_AF", sep = ""))


### LDA
require(MASS)
lda_classifier<-function(data_train, data_val) {
  model_lda <- lda(formula = form, data=data_train, prior = c(1,1)/2)
  predictions<-predict(model_lda,data_val)
  return(as.numeric(as.character(predictions$class)))
}

cv_loss_lda<-CVgeneric(5,lda_classifier,subset(cv_data,select=c(-label)),cv_data$label)



### QDA 
qda_classifier<-function(data_train, data_val) {
  model_qda <- qda(formula = form, data=data_train, prior = c(1,1)/2)
  predictions<-predict(model_qda,data_val)
  return(as.numeric(as.character(predictions$class)))
}

cv_loss_qda<-CVgeneric(5,qda_classifier,subset(cv_data,select=c(-label)),cv_data$label)



### LR
require(nnet)
lr_classifier<-function(data_train, data_val) {
  model_lr <- multinom(formula = form, data=data_train, prior = c(1,1)/2)
  predictions<-predict(model_lr,data_val)
  return(as.numeric(as.character(predictions)))
}
cv_loss_lr<-CVgeneric(5,lr_classifier,subset(cv_data,select=c(-label)),cv_data$label)



### RF
require(ranger)
rf_classifier<-function(data_train, data_val) {
  model_rf <- ranger(formula = form, data=data_train)
  #model_rf <- randomForest(x=subset(data_train,select=c(-label)),y=as.factor(data_train$label))
  predictions<-predict(model_rf,data_val)
  return(as.numeric(as.character(predictions$predictions)))
}
cv_loss_rf<-CVgeneric(5,rf_classifier,subset(cv_data,select=c(-label)),cv_data$label)

#Generates Table 1

test_loss_lda<-percent_error(data_test_num$label,lda_classifier(cv_data,data_test_num))
test_loss_qda<-percent_error(data_test_num$label,qda_classifier(cv_data,data_test_num))
test_loss_lr<-percent_error(data_test_num$label,lr_classifier(cv_data,data_test_num))
test_loss_rf<-percent_error(data_test_num$label,rf_classifier(cv_data,data_test_num))

#Generates Table 2


# 3.b
library(ROCR)
library(magrittr) # need to run every time you start R and want to use %>%
library(dplyr)    # alternative, this also loads %>%

opt.cut = function(perf, pred){
  cut.ind = mapply(FUN=function(x, y, p){
    d = (x - 0)^2 + (y-1)^2
    ind = which(d == min(d))
    c(sensitivity = y[[ind]], specificity = 1-x[[ind]], 
      cutoff = p[[ind]])
  }, perf@x.values, perf@y.values, pred@cutoffs)
}


model_lr <- multinom(formula = form, data=data_train_num)
predict_lr<-predict(model_lr,data_val_num,type = 'probs')
p1 <- prediction(data.frame(predict_lr), data_val_num$label) %>%
  performance(measure = "tpr", x.measure = "fpr")

pred1 <- prediction(data.frame(predict_lr), data_val_num$label)
perf1 <- performance(pred1, measure = "tpr", x.measure = "fpr")
print(lr_cutoff<- opt.cut(perf1, pred1))

model_lda <- lda(formula = form, data=data_train_num, prior = c(1,1)/2)
predict_lda<-predict(model_lda,data_val_num)
p2 <- prediction(data.frame(predict_lda$posterior[,2]), data_val_num$label) %>%
  performance(measure = "tpr", x.measure = "fpr")

pred2 <- prediction(data.frame(predict_lda$posterior[,2]), data_val_num$label)
perf2 <- performance(pred2, measure = "tpr", x.measure = "fpr")
print(lda_cutoff<- opt.cut(perf2, pred2))

model_qda <- qda(formula = form, data=data_train_num, prior = c(1,1)/2)
predict_qda<-predict(model_qda,data_val_num)
p3 <- prediction(data.frame(predict_qda$posterior[,2]), data_val_num$label) %>%
  performance(measure = "tpr", x.measure = "fpr")

pred3 <- prediction(data.frame(predict_qda$posterior[,2]), data_val_num$label)
perf3 <- performance(pred3, measure = "tpr", x.measure = "fpr")
print(qda_cutoff <- opt.cut(perf3, pred3))


p4 <- prediction(data.frame(rf_classifier(data_train_num,data_val_num)), data_val_num$label) %>%
  performance(measure = "tpr", x.measure = "fpr")

pred4 <- prediction(data.frame(rf_classifier(data_train_num,data_val_num)), data_val_num$label)
perf4 <- performance(pred4, measure = "tpr", x.measure = "fpr")
print(rf_cutoff <- opt.cut(perf4, pred4))



library(magrittr) # need to run every time you start R and want to use %>%
library(dplyr)    # alternative, this also loads %>%
library(ROCR)

require(MASS)

### random forrest 
rf_fit <- ranger(formula = label ~ NDAI+SD+Rad_AF, data=data_train_num)
rf_predict<-predict(rf_fit, data=data_val_num)
rf_p <- ifelse(rf_predict$predictions>0, 1, -1)


pred4 <- prediction(rf_predict$predictions, data_val_num$label)
perf4 <- performance(pred4, measure = "tpr", x.measure = "fpr")
print(rf_cutoff<- opt.cut(perf4, pred4))


p4 <- prediction(rf_predict$predictions, data_val_num$label) %>%
  performance(measure = "tpr", x.measure = "fpr")



plot(p1, col = "red")
plot(p2, add = TRUE, col = "blue")
plot(p3, add = TRUE, col = "green")
plot(p4, add = TRUE, col = "black")
legend(0.8,0.3,c('lr','lda','qda','rf'),col=c("red","blue","green","black"), lty = 1)
points(x=1-lr_cutoff[2],y=lr_cutoff[1],col = "red")
points(x=1-lda_cutoff[2],y=lda_cutoff[1],col = "blue")
points(x=1-qda_cutoff[2],y=qda_cutoff[1],col = "green")
points(x=1-rf_cutoff[2],y=rf_cutoff[1],col = "black")

#Generates Figure 9 (left)

# 4.a
num_points<-floor(nrow(data_train_num)/12)
errors_lda<-c()
training_size<-c()
for (i in 1:12) {
  if (i<12) {
    data_train_temp<-data_train_num[1:(i*num_points),]
  }
  else {
    data_train_temp<-data_train_num
  }
  predictions<-lda_classifier(data_train_temp, data_val_num)
  errors_lda<-c(errors_lda,percent_error(data_val_num$label,predictions))
  training_size<-c(training_size,nrow(data_train_temp))
}


df<-data.frame(cbind(training_size,errors_lda))
ggplot(df,aes(x=training_size))+geom_point(aes(y=errors_lda)) + xlab("Training set size") + ylab('Validation error')

#Generates Figure 10 (left)


# 4. b.
predictions<-lda_classifier(data_train_num,data_val_num)
misclass<-data_val_num[data_val_num$label!=predictions,]

library(ggplot2)

summary(misclass)
ggplot() + geom_histogram(aes(x= data_val_num$NDAI, col="red")) + geom_histogram(aes(x= misclass$NDAI, col="green")) + xlab('NDAI')
ggplot() + geom_histogram(aes(x= data_val_num$SD, col="red")) + geom_histogram(aes(x= misclass$SD, col="green"))  + xlab('SD')
ggplot() + geom_histogram(aes(x= data_val_num$Rad_AF, col="red")) + geom_histogram(aes(x= misclass$Rad_AF, col="green")) + xlab('Radiance angle AF')
 
#Generates Figure 11


ggplot(data=data_val_num[1:15000,]) + geom_point(aes(x= x, y= y, col= NDAI)) + xlim(c(50,160))
ggplot(data=misclass) + geom_point(aes(x= x, y= y, col= NDAI)) + xlim(c(50,160))
ggplot() + geom_point(aes(x= data_val_num$x[1:15017], y= data_val_num$y[1:15017], col= data_val_num$label[1:15017])) + labs(x = "x", y = "y", col="label") + xlim(c(50,160))

#Generates Figure 12


