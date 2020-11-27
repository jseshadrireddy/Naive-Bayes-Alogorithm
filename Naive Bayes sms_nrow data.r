sms_nrow=read.csv(file.choose())
View(sms_nrow)
table(sms_nrow$type)
plot(sms_nrow)
plot(sms_nrow$type)
summary(sms_nrow)
###visuvalization
library(ggplot2)
ggplot(data = sms_nrow,aes(x=sms_nrow$type,fill=sms_nrow$type))+
  geom_density(alpha=0.9,color='black')+
  theme(panel.background = element_rect(fill = 'peachpuff'))

##
ggplot(data = sms_nrow,aes(x=sms_nrow$text,fill=sms_nrow$type))+
  geom_density(alpha=0.9,color='black')+
  theme(panel.background = element_rect(fill = 'peachpuff'))
###splitting data in to test and train
set.seed(1234)
id=sample(2,nrow(sms_nrow),prob = c(0.8,0.2),replace = T)
train_sms=sms_nrow[id==1,]
test_sms=sms_nrow[id==2,]
library(e1071)
library(caret)
sms=naiveBayes(type~.,data=train_sms)
sms
pred=predict(sms,test_sms)
pred
table(pred)
con=confusionMatrix(table(pred,test_sms$type))
con
pred1=predict(sms,test_sms,type = 'raw')
pred1
table(pred1)
plot(pred)
plot(pred1)
