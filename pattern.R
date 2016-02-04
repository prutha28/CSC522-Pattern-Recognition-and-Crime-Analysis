data=read.csv("police.csv",header=TRUE,na.strings = c("","NA")) #Reading the csv file and replacing the missing values with NA.
dateTime<-as.character(data$INC.DATETIME) 
data$INC.DATETIME<-as.POSIXlt(dateTime, format="%m/%d/%Y %H:%M")  # convert datetime to accessible format 

data$time<-strftime(data$INC.DATETIME,"%H:%M:%S") # extract time part from DATETIME

data$year<-lapply(strsplit(as.character(data$INC.DATETIME), "-"), "[", 1) #Extracting year.
data$month<-lapply(strsplit(as.character(data$INC.DATETIME), "-"), "[", 2) # extract month part from DATETIME

data$year<-as.numeric(data$year)
data$month<-as.numeric(data$month)
#Discretizing time attribute.
data$duration<-ifelse(data$time >= "06:00:00" & data$time<"12:00:00","Morning",ifelse(data$time >= "12:00:00" & data$time<"18:00:00","Afternoon",ifelse(data$time >= "18:00:00" & data$time<"24:00:00","Night",ifelse(data$time >="00:00:00" & data$time<"06:00:00","Late Night","NA"))))
data$duration<-as.factor(data$duration)
data<-data[,-4] #Removing INC.NO Attribute
data<-data[,-4] #Removing  LOCATION attribute
data<-na.omit(data) #Eliminating the missing values.

# To find crime patterns over the years in different areas. This gives a grpah which portrays Years vs Sum of all crimes.
#The graph is exported as districtwise.png files to the working directory.

D<-data[data$DISTRICT=="DOWNTOWN" & data$year != "2015"  , ]
N<-data[data$DISTRICT=="NORTH" & data$year != "2015" , ]
NE<-data[data$DISTRICT=="NORTHEAST" & data$year != "2015" ,]
NW<-data[data$DISTRICT=="NORTHWEST" & data$year != "2015" ,]
SE<-data[data$DISTRICT=="SOUTHEAST"& data$year != "2015" ,]
SW<-data[data$DISTRICT=="SOUTWEST" & data$year != "2015",]
count_mat<-cbind(table(D$year),table(N$year),table(NE$year),table(NW$year),table(SE$year),table(SW$year))
colnames(count_mat)<-c("DOWNTOWN","NORTH","NORTHEAST","NORTHWEST","SOUTHEAST","SOUTHWEST")
graph<-ts(count_mat,start=2005,end=2014)
png(filename="districtwise.png")
par(fg="#066efb")
plot(graph,main="Crime Pattern in Different Areas" ,xlab="Years", type="o", col="black")
dev.off()

# To find crime patterns over the years. This gives a graph which portrays Years vs Number of incidents of each crime.
#The graph is exported as crimewise.png files to the working directory.
data$crime<-ifelse(grepl("lar",data$LCR.DESC,ignore.case=T),"Larceny", ifelse(grepl("assault",data$LCR.DESC,ignore.case=T),"Assault",ifelse(grepl("vehicle",data$LCR.DESC,ignore.case=T),"MV related",ifelse(grepl("burg",data$LCR.DESC,ignore.case=T),"Burglary",ifelse(grepl("rob",data$LCR.DESC,ignore.case=T),"Robbery",ifelse(grepl("drug",data$LCR.DESC,ignore.case=T),"Drugs",ifelse(grepl("weapon",data$LCR.DESC,ignore.case=T),"Weapons",ifelse(grepl("alcoho",data$LCR.DESC,ignore.case=T),"Alcohol",ifelse(grepl("prostit",data$LCR.DESC,ignore.case=T),"Prostitution",ifelse(grepl("dwi",data$LCR.DESC,ignore.case=T),"Driving While Impaired","Other"))))))))))
alco<-data[data$crime=="Alcohol" & data$year != "2015",]
lar<-data[data$crime=="Larceny" & data$year != "2015",]
weapon<-data[data$crime=="Weapons" & data$year != "2015",]
rob<-data[data$crime=="Robbery" & data$year != "2015",]
prost<-data[data$crime=="Prostitution" & data$year != "2015",]
assua<-data[data$crime=="Assault" & data$year != "2015",]
drug<-data[data$crime=="Drugs" & data$year != "2015",]
mv<-data[data$crime=="MV related" & data$year != "2015",]
count_matrix<-cbind(table(alco$year),table(lar$year),table(weapon$year),table(rob$year),table(prost$year),table(assua$year),table(drug$year),table(mv$year))
colnames(count_matrix)<-c("Alcohol","Larceny","Weapon","Robbery","Prostitution","Assualt","Drugs","Motor Vehicle")
graph<-ts(count_matrix,start=2005,end=2014)
png(filename="crimewise.png")
plot(graph,main="Pattern of Different Crimes Over the Years" ,xlab="Years")
dev.off()
#Finding patterns in each area. Ths outputs 6 graphs one for each DISTRICT which shows the patterns of different categorized crimes in that area.
#To view these graphs add a ".png" extension to the created files.The files are created on the name of the DISTRICT. So the filenames has to be changed to <DISTRICT>.png  in order to view it.
count_matrix<-count_matrix[1:10,]

locations = c("DOWNTOWN", "NORTH", "NORTHEAST", "NORTHWEST" ,"SOUTHEAST", "SOUTWEST")

for (i in 1:6)
{
  loc_temp = locations[i]
  alco<-data[data$crime=="Alcohol" & data$DISTRICT==loc_temp ,]
  lar<-data[data$crime=="Larceny" & data$DISTRICT==loc_temp,]
  weapon<-data[data$crime=="Weapons" & data$DISTRICT==loc_temp,]
  rob<-data[data$crime=="Robbery" & data$DISTRICT==loc_temp,]
  prost<-data[data$crime=="Prostitution" & data$DISTRICT==loc_temp,]
  assua<-data[data$crime=="Assault" & data$DISTRICT==loc_temp,]
  drug<-data[data$crime=="Drugs" & data$DISTRICT==loc_temp,]
  mv<-data[data$crime=="MV related" & data$DISTRICT==loc_temp,]
  count_matrix<-cbind(table(alco$year),table(lar$year),table(weapon$year),table(rob$year),table(prost$year),table(assua$year),table(drug$year),table(mv$year))
  colnames(count_matrix)<-c("Alcohol","Larceny","Weapon","Robbery","Prostitution","Assualt","Drugs","Motor Vehicle")
  
  gname = paste("Crime Pattern in",loc_temp,sep = " ")
  graph<-ts(count_matrix,start=2005,end=2014)
  
  png(filename=append(loc_temp,".png"))
  plot(graph,main = gname,xlab = "YEARS")
  
  dev.off()
}
