library(class) 
library(e1071)
library(caret) 
#Reading data
data=read.csv("police.csv",header=TRUE,na.strings = c("","NA"))

#preprocessing
dateTime<-as.character(data$INC.DATETIME)
data$INC.DATETIME<-as.POSIXlt(dateTime, format="%m/%d/%Y %H:%M")
data$time<-strftime(data$INC.DATETIME,"%H:%M:%S")
data$year<-lapply(strsplit(as.character(data$INC.DATETIME), "-"), "[", 1)
data$month<-lapply(strsplit(as.character(data$INC.DATETIME), "-"), "[", 2)
data$year<-as.numeric(data$year)
data$month<-as.numeric(data$month)
data$crime<-ifelse(grepl("lar",data$LCR.DESC,ignore.case=T),"Larceny", ifelse(grepl("assault",data$LCR.DESC,ignore.case=T),"Assault",ifelse(grepl("vehicle",data$LCR.DESC,ignore.case=T),"MV related",ifelse(grepl("burg",data$LCR.DESC,ignore.case=T),"Burglary",ifelse(grepl("rob",data$LCR.DESC,ignore.case=T),"Robbery",ifelse(grepl("drug",data$LCR.DESC,ignore.case=T),"Drugs",ifelse(grepl("weapon",data$LCR.DESC,ignore.case=T),"Weapons",ifelse(grepl("alcoho",data$LCR.DESC,ignore.case=T),"Alcohol",ifelse(grepl("prostit",data$LCR.DESC,ignore.case=T),"Prostitution",ifelse(grepl("dwi",data$LCR.DESC,ignore.case=T),"Driving While Impaired","Other"))))))))))
data$duration<-ifelse(data$time >= "06:00:00" & data$time<"12:00:00","Morning",ifelse(data$time >= "12:00:00" & data$time<"18:00:00","Afternoon",ifelse(data$time >= "18:00:00" & data$time<"24:00:00","Night",ifelse(data$time >="00:00:00" & data$time<"06:00:00","Late Night","NA"))))
data$duration<-as.factor(data$duration)
data$crime<-as.factor(data$crime)
#deleting INC NO
data<-data[,-4] 
#deleting Location
data<-data[,-4] 
#Now omitting NAs .
data<-na.omit(data) 
data<-data[data$crime!="Other",]

#Using 2012 data as the training data
training <-data[data$year[]=="2012" ,]
#Using 2013 data as the test data
testing<-data[data$year[]=="2013" ,]

#Naive Bayes

#Larceny
nb_larceny = naiveBayes(crime ~ DISTRICT+duration+month, data=training)

#CONFUSION MATRIX
pred_larceny <- predict(nb_larceny, testing)
cmat_naive = list()
cmat_naive = confusionMatrix(pred_larceny, testing$crime)
accuracy_larceny_naive = cmat_naive[[3]][1]
precision_larceny_naive= cmat_naive[[4]][25]
recall_larceny_naive = cmat_naive[[4]][5]

#Naive Bayes for Assault. Remove the data for Larceny. Assault becomes the dominant class now.
training<-data[data$year[]=="2012" & data$crime != "Larceny",  ]
testing<-data[data$year[]=="2013" & data$crime != "Larceny", ]
nb_assault = naiveBayes(crime ~ DISTRICT+duration+month, data=training)

#CONFUSION MATRIX
pred_assault <- predict(nb_assault, testing)
cmat_naive = list()
cmat_naive = confusionMatrix(pred_assault, testing$crime)
accuracy_assault_naive = cmat_naive[[3]][1]
precision_assault_naive= cmat_naive[[4]][22]
recall_assault_naive = cmat_naive[[4]][2]

#Naive Bayes for Drugs. Remove the data for Assault & Larceny. Drugs becomes the dominant class now.
training<-data[data$year[]=="2012" & data$crime != "Larceny" & data$crime != "Assault" ,  ]
testing<-data[data$year[]=="2013" & data$crime != "Larceny" & data$crime != "Assault" , ]
nb_drugs = naiveBayes(crime ~ DISTRICT+duration+month, data=training)

#CONFUSION MATRIX
pred_drugs <- predict(nb_drugs, testing)
cmat_naive = list()
cmat_naive = confusionMatrix(pred_drugs, testing$crime)
accuracy_drugs_naive = cmat_naive[[3]][1]
precision_drugs_naive= cmat_naive[[4]][24]
recall_drugs_naive = cmat_naive[[4]][4]
#Naive Bayes for MV Related. Remove the data for Assault, Larceny & Drugs. MV Related becomes the dominant class now.
training<-data[data$year[]=="2012" & data$crime != "Larceny" & data$crime != "Assault" & data$crime != "Drugs",  ]
testing<-data[data$year[]=="2013" & data$crime != "Larceny" & data$crime != "Assault" & data$crime != "Drugs",  ]
nb_mv = naiveBayes(crime ~ DISTRICT+duration+month, data=training)

#CONFUSION MATRIX
pred_mv <- predict(nb_mv, testing)
cmat_naive = list()
cmat_naive = confusionMatrix(pred_mv, testing$crime)
accuracy_mv_naive = cmat_naive[[3]][1]
precision_mv_naive= cmat_naive[[4]][26]
recall_mv_naive = cmat_naive[[4]][6]

f_measure_larceny_naive = (2 * recall_larceny_naive * precision_larceny_naive )/ (recall_larceny_naive + precision_larceny_naive)
f_measure_assault_naive = (2 * recall_assault_naive * precision_assault_naive )/ (recall_assault_naive + precision_assault_naive)
f_measure_drugs_naive = (2 * recall_drugs_naive * precision_drugs_naive )/ (recall_drugs_naive + precision_drugs_naive)
f_measure_mv_naive = (2 * recall_mv_naive * precision_mv_naive) / (recall_mv_naive + precision_mv_naive)

# rounding to 4 decimal places

baseline_larceny_naive = round(baseline_larceny,4)
baseline_assault_naive = round(baseline_assault,4)
baseline_drugs_naive = round(baseline_drugs,4)
baseline_mv_naive = round(baseline_mv,4)

accuracy_larceny_naive = round(accuracy_larceny_naive,4)
accuracy_assault_naive = round(accuracy_assault_naive,4)
accuracy_drugs_naive = round(accuracy_drugs_naive,4)
accuracy_mv_naive = round(accuracy_mv_naive,4)

precision_larceny_naive = round(precision_larceny_naive,4)
precision_assault_naive = round(precision_assault_naive,4)
precision_drugs_naive = round(precision_drugs_naive,4)
precision_mv_naive = round(precision_mv_naive,4)

recall_larceny_naive = round(recall_larceny_naive,4)
recall_assault_naive = round(recall_assault_naive,4)
recall_drugs_naive = round(recall_drugs_naive,4)
recall_mv_naive = round(recall_mv_naive,4)

f_measure_larceny_naive = round(f_measure_larceny_naive,4)
f_measure_assault_naive = round(f_measure_assault_naive,4)
f_measure_drugs_naive = round(f_measure_drugs_naive,4)
f_measure_mv_naive = round(f_measure_mv_naive,4)
performance_table_naive = t(matrix(c(baseline_larceny,baseline_assault,baseline_drugs,baseline_mv,accuracy_larceny_naive,accuracy_assault_naive,accuracy_drugs_naive,accuracy_mv_naive,precision_larceny_naive,precision_assault_naive,precision_drugs_naive,precision_mv_naive,recall_larceny_naive,recall_assault_naive,recall_drugs_naive,recall_mv_naive,f_measure_larceny_naive,f_measure_assault_naive,f_measure_drugs_naive,f_measure_mv_naive),nrow = 5,ncol = 4,byrow = TRUE,dimnames = list(c("Baseline","Accuracy","Precision","Recall","F-measure"),c("Larceny","Assault","Drugs","MV Related"))))
performance_table_naive
