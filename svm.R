# SVM
library(e1071)
library(caret)
# read data
data=read.csv("police.csv",header=TRUE,na.strings = c("","NA"))

# convert datetime to accessible format 
data$DATETIME<-strptime(data$INC.DATETIME,format="%m/%d/%Y %H:%M")

# extract time part from DATETIME
data$TIME<-strftime(data$DATETIME,"%H:%M")
# extract month part from DATETIME
data$MONTH<-lapply(strsplit(as.character(data$DATETIME), "-"), "[", 2)
#extract year part from DATETIME
data$YEAR<-lapply(strsplit(as.character(data$DATETIME), "-"), "[", 1)
# make year and month numeric values
data$MONTH<-as.numeric(data$MONTH)
data$YEAR<-as.numeric(data$YEAR)

# converting time to a discrete category
data$TIME_CATEGORY<-ifelse(data$TIME >= "06:00" & data$TIME<"12:00","Morning",
                           ifelse(data$TIME >= "12:00" & data$TIME<"18:00","Afternoon",
                                  ifelse(data$TIME >= "18:00" & data$TIME<"24:00","Night",
                                         ifelse(data$TIME >="00:00" & data$TIME<"06:00","Late Night","NA"))))
data$TIME_CATEGORY<-as.factor(data$TIME_CATEGORY)

# converting time to a numerical attribute
# this is done specifically for SVM
data$TIME_NUM_CAT<-ifelse(data$TIME >= "06:00" & data$TIME<"12:00",1,
                          ifelse(data$TIME >= "12:00" & data$TIME<"18:00",2,
                                 ifelse(data$TIME >= "18:00" & data$TIME<"24:00",3,
                                        ifelse(data$TIME >="00:00" & data$TIME<"06:00",4,5))))


# converting district to a numerical attribute
# this is done specifically for SVM
data$DISTRICT_NUM_CAT<-ifelse(grepl("NORTHEAST",data$DISTRICT,ignore.case=T),1, 
                              ifelse(grepl("NORTHWEST",data$DISTRICT,ignore.case=T),2,
                                     ifelse(grepl("NORTH",data$DISTRICT,ignore.case=T),3,
                                            ifelse(grepl("SOUTHEAST",data$DISTRICT,ignore.case=T),4,
                                                   ifelse(grepl("DOWNTOWN",data$DISTRICT,ignore.case=T),5,
                                                          ifelse(grepl("SOUTWEST",data$DISTRICT,ignore.case=T),6,7))))))


# converting crime types to categories
data$CRIME_CATEGORY<-ifelse(grepl("lar",data$LCR.DESC,ignore.case=T),"Larceny", 
                            ifelse(grepl("assault",data$LCR.DESC,ignore.case=T),"Assault",
                                   ifelse(grepl("vehicle",data$LCR.DESC,ignore.case=T),"MV related",
                                          ifelse(grepl("burg",data$LCR.DESC,ignore.case=T),"Burglary",
                                                 ifelse(grepl("rob",data$LCR.DESC,ignore.case=T),"Robbery",
                                                        ifelse(grepl("drug",data$LCR.DESC,ignore.case=T),"Drugs",
                                                               ifelse(grepl("weapon",data$LCR.DESC,ignore.case=T),"Weapons",
                                                                      ifelse(grepl("alcoho",data$LCR.DESC,ignore.case=T),"Alcohol",
                                                                             ifelse(grepl("prostit",data$LCR.DESC,ignore.case=T),"Prostitution",
                                                                                    ifelse(grepl("dwi",data$LCR.DESC,ignore.case=T),"Driving While Impaired","Other"))))))))))


# converting district to a numerical attribute
# this is done specifically for SVM
# not used now, but let it stay
data$CRIME_NUM_CAT<-ifelse(grepl("lar",data$LCR.DESC,ignore.case=T),1, 
                           ifelse(grepl("assault",data$LCR.DESC,ignore.case=T),2,
                                  ifelse(grepl("vehicle",data$LCR.DESC,ignore.case=T),3,
                                         ifelse(grepl("burg",data$LCR.DESC,ignore.case=T),4,
                                                ifelse(grepl("rob",data$LCR.DESC,ignore.case=T),5,
                                                       ifelse(grepl("drug",data$LCR.DESC,ignore.case=T),6,
                                                              ifelse(grepl("weapon",data$LCR.DESC,ignore.case=T),7,
                                                                     ifelse(grepl("alcoho",data$LCR.DESC,ignore.case=T),8,
                                                                            ifelse(grepl("prostit",data$LCR.DESC,ignore.case=T),9,
                                                                                   ifelse(grepl("dwi",data$LCR.DESC,ignore.case=T),10,11))))))))))


# deleting INC NO
data<-data[,-4]
#deleting Location
data<-data[,-4]

# remove records with 'NA' data
data<-na.omit(data)

# remove records with Other Crime category
data<-data[data$CRIME_CATEGORY!="Other",]

# CRIME PREDICTION
# 2012 -train, 2013 - test
# given month,time,district - predict crime

# select data for 2012,2013
data_for_2012<-data[data$YEAR[]=="2012", c(8,9,4,7,10,11,12,13)]
data_for_2013<-data[data$YEAR[]=="2013", c(8,9,4,7,10,11,12,13)]

# Larceny prediction

# get train and test data
x_train1 = data_for_2012[,c(4,5,6)]
x_test1 = data_for_2013[,c(4,5,6)]

x_train1 = scale(x_train1)
b1 = c(6.564789,2.335342,3.505641)
b2 = c(3.392622,1.005821,1.623893)
x_test1 = scale(x_test1,b1,b2)

y_train1 = data_for_2012[,c(7)]
y_test1 = data_for_2013[,c(7)]

# Modelling and Predicting SVM

print("Radial kernel for Larceny")

svm1<-svm(x_train1,y_train1,type="C-classification",kernel="radial",cost = 10,gamma =5,scale=FALSE)
larc_pred = predict(svm1,x_test1)
print("Prediction")
print(xtabs(~larc_pred))
print("Actual")
print(xtabs(~y_test1))
baseline_larceny = (xtabs(~y_test1)[4]) / length(y_test1)

cmat = list()
cmat = confusionMatrix(larc_pred,y_test1)
accuracy_larceny = cmat[[3]][1]
precision_larceny = cmat[[4]][20]
recall_larceny = cmat[[4]][4]

# Remove Larceny
data<-data[data$CRIME_CATEGORY!="Larceny",]

# Assault prediction

# select data for 2012,2013
data_for_2012<-data[data$YEAR[]=="2012", c(8,9,4,7,10,11,12,13)]
data_for_2013<-data[data$YEAR[]=="2013", c(8,9,4,7,10,11,12,13)]

# get train and test data
x_train2 = data_for_2012[,c(4,5,6)]
x_test2= data_for_2013[,c(4,5,6)]

x_train2 = scale(x_train2)
b1 = c(6.591909,2.617028,3.662728)
b2 = c(3.391693,1.037403,1.564754)
x_test2 = scale(x_test2,b1,b2)

y_train2 = data_for_2012[,c(7)]
y_test2 = data_for_2013[,c(7)]

# Modelling and Predicting SVM

print("Radial kernel for Assault")

svm2<-svm(x_train2,y_train2,type="C-classification",kernel="radial",cost = 10,gamma =5,scale=FALSE)
assault_pred = predict(svm2,x_test2)
print("Prediction")
print(xtabs(~assault_pred))
print("Actual")
print(xtabs(~y_test2))
baseline_assault = (xtabs(~y_test2)[2]) / length(y_test2)

cmat = list()
cmat = confusionMatrix(assault_pred,y_test2)
accuracy_assault = cmat[[3]][1]
precision_assault = cmat[[4]][16]
recall_assault = cmat[[4]][2]


# Remove Assault
data<-data[data$CRIME_CATEGORY!="Assault",]

# Drugs prediction

# select data for 2012,2013
data_for_2012<-data[data$YEAR[]=="2012", c(8,9,4,7,10,11,12,13)]
data_for_2013<-data[data$YEAR[]=="2013", c(8,9,4,7,10,11,12,13)]

# get train and test data
x_train3 = data_for_2012[,c(4,5,6)]
x_test3 = data_for_2013[,c(4,5,6)]

x_train3 = scale(x_train3)
b1 = c(6.598845,2.562170,3.715901)
b2 = c(3.418341,1.046928,1.558045)
x_test3 = scale(x_test3,b1,b2)

y_train3 = data_for_2012[,c(7)]
y_test3 = data_for_2013[,c(7)]

# Modelling and Predicting SVM


# Loop

print("Radial kernel for Drugs")

svm3<-svm(x_train3,y_train3,type="C-classification",kernel="radial",cost = 10,gamma =5,scale=FALSE)
drugs_pred = predict(svm3,x_test3)
print("Prediction")
print(xtabs(~drugs_pred))
print("Actual")
print(xtabs(~y_test3))
baseline_drugs = (xtabs(~y_test3)[2]) / length(y_test3)

cmat = list()
cmat = confusionMatrix(drugs_pred,y_test3)
accuracy_drugs = cmat[[3]][1]
precision_drugs = cmat[[4]][14]
recall_drugs = cmat[[4]][2]


# Remove Drugs
data<-data[data$CRIME_CATEGORY!="Drugs",]

# MV prediction

# select data for 2012,2013
data_for_2012<-data[data$YEAR[]=="2012", c(8,9,4,7,10,11,12,13)]
data_for_2013<-data[data$YEAR[]=="2013", c(8,9,4,7,10,11,12,13)]

# get train and test data
x_train4 = data_for_2012[,c(4,5,6)]
x_test4 = data_for_2013[,c(4,5,6)]

x_train4 = scale(x_train4)
b1 = c(6.694128,2.344912,3.627421)
b2 = c(3.369453,1.065362,1.618470)
x_test4 = scale(x_test4,b1,b2)

y_train4 = data_for_2012[,c(7)]
y_test4 = data_for_2013[,c(7)]

# Modelling and Predicting SVM


# Loop

print("Radial kernel for MV")

svm4<-svm(x_train4,y_train4,type="C-classification",kernel="radial",cost = 10,gamma =5,scale=FALSE)
mv_pred = predict(svm4,x_test4)
print("Prediction")
print(xtabs(~mv_pred))
print("Actual")
print(xtabs(~y_test4))
baseline_mv = (xtabs(~y_test4)[2]) / length(y_test4)

cmat = list()
cmat = confusionMatrix(mv_pred,y_test4)
accuracy_mv = cmat[[3]][1]
precision_mv = cmat[[4]][12]
recall_mv = cmat[[4]][2]

f_measure_larceny = 2 * recall_larceny * precision_larceny / (recall_larceny + precision_larceny)
f_measure_assault = 2 * recall_assault * precision_assault / (recall_assault + precision_assault)
f_measure_drugs = 2 * recall_drugs * precision_drugs / (recall_drugs + precision_drugs)
f_measure_mv = 2 * recall_mv * precision_mv / (recall_mv + precision_mv)

# rounding to 4 decimal places

baseline_larceny = round(baseline_larceny,4)
baseline_assault = round(baseline_assault,4)
baseline_drugs = round(baseline_drugs,4)
baseline_mv = round(baseline_mv,4)

accuracy_larceny = round(accuracy_larceny,4)
accuracy_assault = round(accuracy_assault,4)
accuracy_drugs = round(accuracy_drugs,4)
accuracy_mv = round(accuracy_mv,4)

precision_larceny = round(precision_larceny,4)
precision_assault = round(precision_assault,4)
precision_drugs = round(precision_drugs,4)
precision_mv = round(precision_mv,4)

recall_larceny = round(recall_larceny,4)
recall_assault = round(recall_assault,4)
recall_drugs = round(recall_drugs,4)
recall_mv = round(recall_mv,4)

f_measure_larceny = round(f_measure_larceny,4)
f_measure_assault = round(f_measure_assault,4)
f_measure_drugs = round(f_measure_drugs,4)
f_measure_mv = round(f_measure_mv,4)

performance_table_svm = t(matrix(c(baseline_larceny,baseline_assault,baseline_drugs,baseline_mv,accuracy_larceny,accuracy_assault,accuracy_drugs,accuracy_mv,precision_larceny,precision_assault,precision_drugs,precision_mv,recall_larceny,recall_assault,recall_drugs,recall_mv,f_measure_larceny,f_measure_assault,f_measure_drugs,f_measure_mv),nrow = 5,ncol = 4,byrow = TRUE,dimnames = list(c("Baseline","Accuracy","Precision","Recall","F-measure"),c("Larceny","Assault","Drugs","MV Related"))))

performance_table_svm