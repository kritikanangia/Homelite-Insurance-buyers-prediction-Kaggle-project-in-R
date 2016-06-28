options(java.parameters="-Xmx3500m")
train=read.csv("C:/Users/Kritika/Downloads/traindata.csv",header=T,na.strings=c(""," "))
str(train,list.len=299)
set.seed(100)
library(caTools)
split=sample.split(train$QuoteConversion_Flag,SplitRatio = 0.65)
train2=subset(train,split==TRUE)
cross=subset(train,split==FALSE)
index=apply(train2,2,function(x) sum(is.na(x)))
names(train2)[index>0]
percent=(169489-index)/169489
exclude=percent[percent<0.5]
exclude
which(colnames(train2)=="PropertyField29")
train3=subset(train2,select=c(3:161,163:299))
train3$PersonalField84[is.na(train3$PersonalField84)] =mean(train3$PersonalField84,na.rm=T)
train3$PersonalField7[is.na(train3$PersonalField7)] ='N'
train3$PropertyField3[is.na(train3$PropertyField3)] ='N'
train3$PropertyField4[is.na(train3$PropertyField4)] ='N'
train3$PropertyField32[is.na(train3$PropertyField32)] ='Y'
train3$PropertyField34[is.na(train3$PropertyField34)] ='Y'
train3$PropertyField36[is.na(train3$PropertyField36)] ='N'
train3$PropertyField38[is.na(train3$PropertyField38)] ='N'
summary(train3)
library(FSelector)
weights=oneR(QuoteConversion_Flag~.,data=train3)
subset <- cutoff.k(weights, 150)
f=as.simple.formula(subset,"QuoteConversion_Flag")
model=model=glm(f,data=train3,family=binomial)

summary(model)
model2=glm(QuoteConversion_Flag~SalesField13+SalesField14+SalesField15+PersonalField34+PersonalField38+PersonalField67+PropertyField10+PropertyField11A+PropertyField17+PropertyField22 +GeographicField18A+GeographicField56A+GeographicField60B+GeographicField61A+PersonalField13+PersonalField41+PersonalField39+PersonalField11+PersonalField25+CoverageField5A+PersonalField53+PersonalField49+PersonalField43+PropertyField34+PropertyField37+PropertyField32+PropertyField11B+PropertyField13+PersonalField15+GeographicField14A+PropertyField8+SalesField3+GeographicField49B+PersonalField51+GeographicField23A+PersonalField1+GeographicField50B+GeographicField5A+GeographicField56B+SalesField10+PersonalField56+Field12+PersonalField7+SalesField9+PropertyField30+SalesField11+GeographicField33A+SalesField12+GeographicField55A+GeographicField51A+PropertyField2A+GeographicField51B+GeographicField61B+PersonalField2+PersonalField55+GeographicField4B+GeographicField22A+CoverageField3A+GeographicField37B+PropertyField38+PropertyField3+PropertyField4+ PersonalField6+ GeographicField28A+GeographicField28B+PersonalField82+PropertyField36+PersonalField81+ PersonalField58+PersonalField29+ PersonalField84+GeographicField63+CoverageField6A+PersonalField9+PersonalField77+PersonalField78+GeographicField20A+GeographicField20B+CoverageField5B+PropertyField18+PropertyField35+PersonalField4B,data=train3,family=binomial)
summary(model2)
model3=glm(QuoteConversion_Flag~SalesField13+SalesField14+SalesField15+PersonalField34+PersonalField38+PersonalField67+PropertyField10+PropertyField11A+PropertyField17+PropertyField22 +GeographicField18A+GeographicField56A+GeographicField60B+GeographicField61A+PersonalField13+PersonalField41+PersonalField39+PersonalField11+PersonalField25+CoverageField5A+PersonalField53+PersonalField49+PersonalField43+PropertyField34+PropertyField37+PropertyField32+PropertyField11B+PropertyField13+PersonalField15+GeographicField14A+PropertyField8+SalesField3+GeographicField49B+PersonalField51+GeographicField23A+PersonalField1+GeographicField50B+GeographicField5A+GeographicField56B+SalesField10+PersonalField56+Field12+PersonalField7+SalesField9+PropertyField30+SalesField11+GeographicField33A+SalesField12+GeographicField55A+GeographicField51A+PropertyField2A+GeographicField51B+GeographicField61B+PersonalField2+PersonalField55+GeographicField4B+GeographicField22A+CoverageField3A+GeographicField37B+PropertyField38+PropertyField3+PropertyField4+ PersonalField6+ GeographicField28A+GeographicField28B+PersonalField82+PropertyField36+PersonalField81+ PersonalField58+PersonalField29+ PersonalField84+CoverageField6A+PersonalField9+PersonalField77+PersonalField78+GeographicField20A+GeographicField20B+CoverageField5B+PropertyField18+PropertyField35+PersonalField4B,data=train3,family=binomial)
summary(model3)
model4=glm(QuoteConversion_Flag~SalesField13+SalesField14+SalesField15+PersonalField38+PersonalField67+PropertyField10+PropertyField11A+PropertyField17+PropertyField22 +GeographicField18A+GeographicField56A+GeographicField60B+GeographicField61A+PersonalField13+PersonalField41+PersonalField39+PersonalField11+PersonalField25+CoverageField5A+PersonalField53+PersonalField49+PersonalField43+PropertyField34+PropertyField37+PropertyField32+PropertyField11B+PropertyField13+PersonalField15+GeographicField14A+PropertyField8+SalesField3+GeographicField49B+PersonalField51+GeographicField23A+PersonalField1+GeographicField50B+GeographicField5A+GeographicField56B+SalesField10+PersonalField56+Field12+PersonalField7+SalesField9+PropertyField30+SalesField11+GeographicField33A+SalesField12+GeographicField55A+GeographicField51A+PropertyField2A+GeographicField51B+GeographicField61B+PersonalField2+PersonalField55+GeographicField4B+GeographicField22A+CoverageField3A+GeographicField37B+PropertyField38+PropertyField3+PropertyField4+ PersonalField6+ GeographicField28A+GeographicField28B+PersonalField82+PropertyField36+ PersonalField58+PersonalField29+ PersonalField84+CoverageField6A+PersonalField9+PersonalField77+GeographicField20A+GeographicField20B+CoverageField5B+PropertyField18+PropertyField35+PersonalField4B,data=train3,family=binomial)
summary(model4)
predictlog=predict(model4,newdata=cross,type="response")
table(cross$QuoteConversion_Flag,predictlog>0.5)
(37962+5294)/(37692+1147+3354+5296)
test=read.csv("C:/Users/Kritika/Downloads/testdata.csv",header=T,na.strings=c(""," "))
index=apply(test,2,function(x) sum(is.na(x)))
names(test)[index>0]
test$PropertyField3[is.na(test$PropertyField3)] ='N'
 test$PropertyField4[is.na(test$PropertyField4)] ='N'
test$PropertyField32[is.na(test$PropertyField32)] ='Y'
test$PropertyField34[is.na(test$PropertyField34)] ='Y'
test$PropertyField36[is.na(test$PropertyField36)] ='N'
  test$PropertyField38[is.na(test$PropertyField38)] ='N'
  test$PersonalField7[is.na(test$PersonalField7)] ='N'
 test$PropertyField5[is.na(test$PropertyField5)] ='Y'
 test$PropertyField30[is.na(test$PropertyField30)] ='N'
  test$PropertyField29[is.na(test$PropertyField29)] =mean(test$PropertyField29,na.rm=T)
 test$PersonalField84[is.na(test$PersonalField84)] =mean(test$PersonalField84,na.rm=T)
 test$PropertyField37[is.na(test$PropertyField37)] ='N'
 test$GeographicField63[is.na(test$GeographicField63)] ='N'
 
