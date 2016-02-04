library(RWeka)
library(caret)
data=read.csv("police.csv",header=TRUE,na.strings = c("","NA"))
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
data$DISTRICT<-as.factor(data$DISTRICT)
data$crime<-as.factor(data$crime)
data<-data[,-4]
data<-data[,-4]
data<-na.omit(data)
data<-data[data$crime!="Other",]

#Decision Trees
train<-data[data$year[]=="2012"  ,  ] #Extracting training data
test<-data[data$year[]=="2013" , ]  #Extracting testing data

larceny_tree<-J48(crime ~ duration + DISTRICT ,data=train)
larceny_pred<-predict(larceny_tree,test)
cmat_tree = list()
cmat_tree = confusionMatrix(larceny_pred,test$crime)
accuracy_larceny_tree = cmat_tree[[3]][1]
precision_larceny_tree= cmat_tree[[4]][25]
recall_larceny_tree = cmat_tree[[4]][5]

#Removing larceny to  build a model for assault 
train<-data[data$year[]=="2012" & data$crime != "Larceny" ,  ] 
test<-data[data$year[]=="2013" & data$crime != "Larceny", ]

assault_tree<-J48(crime ~ duration + DISTRICT ,data=train)
assault_pred<-predict(assault_tree,test)
cmat_tree = list()
cmat_tree = confusionMatrix(assault_pred,test$crime)
accuracy_assault_tree = cmat_tree[[3]][1]
precision_assault_tree= cmat_tree[[4]][22]
recall_assault_tree = cmat_tree[[4]][2]


#Removing larceny and assault to  build a model for Drugs 
train<-data[data$year[]=="2012" & data$crime != "Larceny" &  data$crime != "Assault",  ]
test<-data[data$year[]=="2013" & data$crime != "Larceny" &  data$crime != "Assault", ]

drugs_tree<-J48(crime ~ duration + DISTRICT ,data=train)
drugs_pred<-predict(drugs_tree,test)

cmat_tree = list()
cmat_tree = confusionMatrix(drugs_pred,test$crime)
accuracy_drugs_tree = cmat_tree[[3]][1]
precision_drugs_tree= cmat_tree[[4]][24]
recall_drugs_tree = cmat_tree[[4]][4]


#Removing larceny,  assault and drugs to  build a model for MV related
train<-data[data$year[]=="2012" & data$crime != "Larceny" &  data$crime != "Assault" &  data$crime != "Drugs",  ]
test<-data[data$year[]=="2013" & data$crime != "Larceny" &  data$crime != "Assault" &  data$crime != "Drugs", ]

mvrelated_tree<-J48(crime ~ duration + DISTRICT ,data=train)
mvrelated_pred<-predict(mvrelated_tree,test)
cmat_tree = list()
cmat_tree = confusionMatrix(mvrelated_pred,test$crime)
accuracy_mv_tree = cmat_tree[[3]][1]
precision_mv_tree= cmat_tree[[4]][26]
recall_mv_tree = cmat_tree[[4]][6]

f_measure_larceny_tree = (2 * recall_larceny_tree * precision_larceny_tree )/ (recall_larceny_tree + precision_larceny_tree)
f_measure_assault_tree = (2 * recall_assault_tree * precision_assault_tree )/ (recall_assault_tree + precision_assault_tree)
f_measure_drugs_tree = (2 * recall_drugs_tree * precision_drugs_tree )/ (recall_drugs_tree + precision_drugs_tree)
f_measure_mv_tree = (2 * recall_mv_tree * precision_mv_tree) / (recall_mv + precision_mv)

# rounding to 4 decimal places

baseline_larceny_tree = round(baseline_larceny,4)
baseline_assault_tree = round(baseline_assault,4)
baseline_drugs_tree = round(baseline_drugs,4)
baseline_mv_tree = round(baseline_mv,4)

accuracy_larceny_tree = round(accuracy_larceny_tree,4)
accuracy_assault_tree = round(accuracy_assault_tree,4)
accuracy_drugs_tree = round(accuracy_drugs_tree,4)
accuracy_mv_tree = round(accuracy_mv_tree,4)

precision_larceny_tree = round(precision_larceny_tree,4)
precision_assault_tree = round(precision_assault_tree,4)
precision_drugs_tree = round(precision_drugs_tree,4)
precision_mv_tree = round(precision_mv_tree,4)

recall_larceny_tree = round(recall_larceny_tree,4)
recall_assault_tree = round(recall_assault_tree,4)
recall_drugs_tree = round(recall_drugs_tree,4)
recall_mv_tree = round(recall_mv_tree,4)

f_measure_larceny_tree = round(f_measure_larceny_tree,4)
f_measure_assault_tree = round(f_measure_assault_tree,4)
f_measure_drugs_tree = round(f_measure_drugs_tree,4)
f_measure_mv_tree = round(f_measure_mv_tree,4)
performance_table_tree = t(matrix(c(baseline_larceny,baseline_assault,baseline_drugs,baseline_mv,accuracy_larceny_tree,accuracy_assault_tree,accuracy_drugs_tree,accuracy_mv_tree,precision_larceny_tree,precision_assault_tree,precision_drugs_tree,precision_mv_tree,recall_larceny_tree,recall_assault_tree,recall_drugs_tree,recall_mv_tree,f_measure_larceny_tree,f_measure_assault_tree,f_measure_drugs_tree,f_measure_mv_tree),nrow = 5,ncol = 4,byrow = TRUE,dimnames = list(c("Baseline","Accuracy","Precision","Recall","F-measure"),c("Larceny","Assault","Drugs","MV Related"))))
performance_table_tree






