#Extracting data for SPR from processors FSP
#@silvia Rodriguez Climent
# 09-09-2019; last updated: 09-09-2022
#---------------------------------------------------------------------##

# ===================================================--
# 00. Set directories ----
# ===================================================--

rm(list=ls())
# set input, output directories
inp_dir <- file.path(getwd(), "Data/Processors/SPR/")
plot_dir <- file.path(getwd(), "Data/plots/SPR/")
out_dir <- file.path(getwd(), "Data/Output/")
list.files(inp_dir,recursive=TRUE) # recursive TRUE to see inside the different folders


# load libraries
packages <- c("ggplot2", "data.table","xlsx","openxlsx","dplyr","readxl","stringr",
              "plyr","tidyr","reshape2","maps","mapdata","mapproj","mapplots","lubridate",
              "rgdal","raster","GISTools","ggspatial","XLConnect","sjmisc","janitor")

lib <- function(packages){ 
  installed_packages <- packages %in% rownames(installed.packages())
  if (any(installed_packages == FALSE)) {
    install.packages(packages[!installed_packages])
  }
  # Packages loading
  invisible(lapply(packages, library, character.only = TRUE))
  
}

lib(packages) #install libraries through own function


# ===================================================--
# 1. SPR COOMBEFISHERIES NEW format ----
# ===================================================--
##reading directly the files in the format they've send them
inp_dir_cf <- file.path(getwd(),"Data/Processors/SPR/CoombeFisheries/")
list.files(inp_dir_cf)

# read excel files
myfiles <- list.files(path=inp_dir_cf, pattern="*.xlsx", recursive = FALSE, full.names = FALSE, include.dirs=F)

### Extract Length-Weight Data from the processors files
mydata <- list()
for(i in 1:length(myfiles)){
  # temporary object with sheetnames
  temp <- unlist(getSheetNames(paste(inp_dir_cf,myfiles[i], sep="/")))
  # if less than 3 sheets, issue a warning and move on to next iteration
  if(length(temp)>2) {
    mydata[[myfiles[i]]] <- xlsx::read.xlsx(paste(inp_dir_cf,myfiles[i], sep="/"), sheetName=temp[1],header=F)} else {
      warning('Check ', myfiles[i], ": sheet LW might be missing")}
}

#mydata <- mapply(cbind, mydata, "SampleID"=myfiles, SIMPLIFY=F)
#mydata <- data.table(do.call(rbind.data.frame, mydata))

# Extract date (should be in dd/mm/yyyy format in the original file)
dates <- list()
for(i in 1:length(myfiles)){
  dates[[myfiles[i]]] <- xlsx::read.xlsx(paste(inp_dir_cf,myfiles[i], sep="/"), sheetIndex=1, startRow=1, rowIndex = 1, header=F, colIndex = 2)
  if(is.na(dates[[myfiles[i]]])) dates[[myfiles[i]]] <- xlsx::read.xlsx(paste(inp_dir_cf,myfiles[i], sep="/"), sheetIndex=1, startRow=1, rowIndex = 1, header=F, colIndex = 2) 
} 

dates <- mapply(cbind, dates, "SampleID"=myfiles, SIMPLIFY=F) 
dates <- data.table(do.call(rbind.data.frame, dates))
dates <- as.data.frame(dates)

# Extract vessel
vessel <- list()
for(i in 1:length(myfiles)){
  vessel[[myfiles[i]]] <- xlsx::read.xlsx(paste(inp_dir_cf,myfiles[i], sep="/"), sheetIndex=1, startRow=1, rowIndex = 1, header=F, colIndex = 25)
  if(is.na(vessel[[myfiles[i]]])) vessel[[myfiles[i]]] <- xlsx::read.xlsx(paste(inp_dir_cf,myfiles[i], sep="/"), sheetIndex=1, startRow=1, rowIndex = 1, header=F, colIndex = 25) 
} 
vessel <- mapply(cbind, vessel, "SampleID"=myfiles, SIMPLIFY=F)
vessel <- data.table(do.call(rbind.data.frame, vessel))
vessel <- as.data.frame(vessel)

mydata2 <-mydata 
#mydata2$`21210819.xlsx`

llista <- list()
for(i in 1:length(mydata2)){
  llista[[i]] <- mydata2[[i]][rowSums(is.na(mydata2[[i]][,0:ncol(mydata2[[i]])]))<ncol(mydata2[[i]]),1:17]
} 

#Select only the cells you are interested on
llista <- lapply(llista,function(x)return(x[3:48,-2]))  # counts the count of odd numbers in the argument
#format it
for(i in 1:length(llista)){
  llista[[i]] <- remove_empty(llista[[i]])
  
}
#add the missing column to the list
llista[5] <- lapply(llista[5], transform, X17 ="NA") #add the missing column to the list
#and copy the empty cells 
for (j in 1:length(llista)){
  for (i in 1:length(llista)){
    if(is.na(llista[[j]][i,1])) llista[[j]][i,1] <- llista[[j]][i-1,1]
  }} 


head(llista)
llista2 <- llista
##to data frame
llista2<- mapply(cbind, llista, "SampleID"=myfiles, SIMPLIFY=F)
#llista2[[2]]$X17 <- NA #create the column that is missing

for(i in 1:length(llista2)){
  llista2[[i]]$SampleID <- as.character(as.factor(llista2[[i]]$SampleID))
}

str(llista2)

llista2 <- data.table(do.call(rbind.data.frame, llista2))
summary(llista2)
llista2[is.na(llista2)] <- 0  

#subset(llista2,SampleID=="01010919.xlsx")# all good
mdata <- melt(llista2, id=c("SampleID","X1")) 

subset(mdata,SampleID=="01-30.08.21.xlsx")# all good

#1.1 need to write it down and read it again (otherwise format is wrong) ----
#write.csv(mdata, file=paste(out_dir,"/mdata2.csv",sep="/"),quote=F,row.names = F)
#mdata <- read.table(paste(out_dir,'/mdata2.csv',sep=''),sep=',',header=TRUE,stringsAsFactors = FALSE)
str(mdata)
mdata <- subset(mdata,mdata$value>0)
plot(X1~value,mdata) #check
table(mdata$SampleID)

rm(mydata,mydata2,llista2,llista)
colnames(mdata)<- c("SampleID","weight","variable","length")

#removing outliers
#mdata <- subset(mdata,!(length>10&weight<5))
plot(weight ~length,mdata) #check

dim(mdata)


#merge dates and vessel
mdata3 <- mdata

head(mdata3);head(dates);head(vessel)
df <- join_all(list(mdata3,dates,vessel), by = 'SampleID')

df <- df[,c("X2","X25","length","weight","SampleID")]
colnames(df) <- c("date","vessel","length","weight","SampleID")
df$species <- "SPR"

#order
#df2= df[order(df[,"length"], df[,"weight"]), ]
plot(weight~length,df) #check visually


#1.2 sample weight----
#they need the weight of the sample across the length and weights
head(df)
str(df)
df$weight <- as.numeric(as.character(df$weight))

sample <- with (df,aggregate(weight,list(date=date,vessel=vessel,species=species),sum))
colnames(sample)[4] <- "Wsample_g"
sample$vessel <- toupper(sample$vessel)

head(sample);dim(sample)#6 4
head(df);dim(df) #533 6

#oceansample.2 <- oceansample[,c( "date","vessel","Wsample_g")]

df <- merge(df,sample,all.x=T)
dim(df)

# format it
head(df);str(df)
df$month <- month(df$date)
df$year <- year(df$date)
df$source <- "processor"
df$processor <- "COOMBEFISHERIES"
df$totalcatch_kg <- ""

head(df)
names(df) <- c("date","vessel","species","totalcatch_kg","length_cm","weight_g","processor","sampleID","month","year","source","processor","samplewt_g")

df <- df[,c("date","vessel","totalcatch_kg","length_cm","weight_g","samplewt_g","sampleID","month","year","source","species","processor")]

#save the aggregated data and ready to start the analysis
#write.csv(df, file=paste(out_dir,"SPR_CoombeFisheriesLW_vs1.csv",sep="/"),quote=F,row.names = F)
######################################################################################################################################--          

# ===================================================--
# 2. SPR FALFISH ----
# ===================================================--

inp_dir_Falfish <- file.path(getwd(),"Data/Processors/SPR/Falfish/")
list.files(inp_dir_Falfish)

# read excel files
myfiles <- list.files(path=inp_dir_Falfish, pattern="CFR.*.xlsx", recursive = FALSE, full.names = FALSE, include.dirs=F)

### Length-Weight Data
mydata <- list()
for(i in 1:length(myfiles)){
  # temporary object with sheetnames
  temp <- unlist(getSheetNames(paste(inp_dir_Falfish,myfiles[i], sep="/")))
  # if less than 3 sheets, issue a warning and move on to next iteration
  if(length(temp)>2) {
    mydata[[myfiles[i]]] <- xlsx::read.xlsx(paste(inp_dir_Falfish,myfiles[i], sep="/"), sheetName=temp[3])} else {
      warning('Check ', myfiles[i], ": sheet LW might be missing")}
  # create list with 3rd sheet
}

# Check which list item have more than 3 columns
check <- which(do.call(rbind.data.frame, (lapply(mydata,dim)))[,2]>4)

#add the damage colum
mydata$`CFR20356 OR.xlsx`$Damage <- "NA"
mydata$`CFR20382 OR.xlsx`$Damage <- "NA"
mydata$`CFR20402 OR.xlsx`$Damage <- "NA"
mydata$`CFR20421 OR.xlsx`$Damage <- "NA"

# # Add column with file name to each list element
mydata <- mapply(cbind, mydata, "SampleID"=myfiles, SIMPLIFY=F)
mydata <- data.table(do.call(rbind.data.frame, mydata))

# Extract date
dates <- list()
for(i in 1:length(myfiles)){
  dates[[myfiles[i]]] <- xlsx::read.xlsx(paste(inp_dir_Falfish,myfiles[i], sep="/"), sheetIndex=1, startRow=1, rowIndex = 1, header=F, colIndex = 29)
  if(is.na(dates[[myfiles[i]]])) dates[[myfiles[i]]] <- xlsx::read.xlsx(paste(inp_dir_Falfish,myfiles[i], sep="/"), sheetIndex=2, startRow=1, rowIndex = 1, header=F, colIndex = 29)
}
dates <- mapply(cbind, dates, "SampleID"=myfiles, SIMPLIFY=F)
dates <- data.table(do.call(rbind.data.frame, dates))

# # Extract species
sp <- list()
for(i in 1:length(myfiles)){
  sp[[myfiles[i]]] <- xlsx::read.xlsx(paste(inp_dir_Falfish,myfiles[i], sep="/"), sheetIndex=1, startRow=1, rowIndex = 1, header=F, colIndex = 16)
  if(sp[[myfiles[[i]]]]!="Sprat") warning('Check ', myfiles[i], ": wrong species?")
  #if(is.na(dates[[myfiles[i]]])) dates[[myfiles[i]]] <- xlsx::read.xlsx(paste(inp_dir_Falfish,myfiles[i], sep="/"), sheetIndex=2, startRow=1, rowIndex = 1, header=F, colIndex = 29)
}

# # Extract weight
sampleWT <- list()
for(i in 1:length(myfiles)){
  sampleWT[[myfiles[i]]] <- xlsx::read.xlsx(paste(inp_dir_Falfish,myfiles[i], sep="/"), sheetIndex=1, rowIndex = 39, header=F, colIndex = 18)}
sampleWT <- mapply(cbind, sampleWT, "SampleID"=myfiles, SIMPLIFY=F)
sampleWT <- data.table(do.call(rbind.data.frame, sampleWT))


# Round TL (some are at mm instead of half cm)
#mydata[,TLcm := (round(Length/5)*5)/10]
mydata <- mydata[,c("Weight","Length","Damage","SampleID")]
#
# Merge everything in a data.table
FALFISH <- merge(mydata, dates, by="SampleID", all=T)
FALFISH <- merge(FALFISH, sampleWT, by="SampleID", all=T)
names(FALFISH) <- c("ID", "weight", "TL", "Damage", "Date", "SampleWt")

# convert date in date format
FALFISH[,Date:=as.Date(Date, format = "%d/%m/%y")]
# column for month
FALFISH[,month:=month(Date)]
# Fit non linear regression
nls_fit_w11 <- nls(weight/1000 ~ a*(TL/10)^b, FALFISH, start = list(a = 0.0005, b = 3))
FALFISH[,LW.w11:=predict(nls_fit_w11)] #does not work with NA in the database

# Plot length weight
SP_LWrel <- ggplot(FALFISH, aes(TL, weight/1000, group=factor(month), col=factor(month))) + geom_point() +
  geom_line(aes(TL, LW.w11), col="red") + theme_bw(25) +
  theme(legend.position="bottom") + ylab("Weight (kg)") + xlab("TL (mm)") +
  scale_colour_discrete(name = "Month") +
  annotate("text", label=paste("a = ", round(coefficients(nls_fit_w11)[["a"]],digits=5),
                               " b =", round(coefficients(nls_fit_w11)[["b"]],digits=5)), x=75.0,y=0.024) +
  NULL #geom_text(aes(size=18))


# ggsave(filename = paste(plot_dir,"SPR_Falfish_LWrel.png", sep="/"), plot = SP_LWrel, width = 25,
#        height = 20, units = "cm", dpi = 300, type = "cairo-png")


#format it
head(FALFISH);str(FALFISH)

FALFISH$year <-year(FALFISH$Date)
FALFISH$source <- "processor"
FALFISH$species <- "SPR"
FALFISH$totalcatch_kg <- ""
FALFISH$processor <- "FALFISH"
FALFISH$Vessel <- "CONSTANT FRIEND"

names(FALFISH) <-c("sampleID","weight_g","length_cm","todel","date","samplewt_g","month", "toldel2","year","source","species","totalcatch_kg","processor","vessel")
FALFISH <-FALFISH[,c("date","vessel","totalcatch_kg","length_cm","weight_g","samplewt_g","sampleID","month","year","source","species","processor")]
FALFISH$length_cm <- FALFISH$length_cm/10

#save the databse
#write.csv(FALFISH, file=paste(out_dir, "SPR_FALFISH_2021_vs1.csv", sep="/"), quote=F, row.names = F)

# ===================================================--
# 3. SPR INTERFISH ----
# ===================================================--

inp_dir_inter <- file.path(getwd(),"Data/Processors/SPR/Interfish/")
list.files(inp_dir_inter)

# read excel files
myfiles <- list.files(path=inp_dir_inter, pattern=".xlsx", recursive = FALSE, full.names = FALSE, include.dirs=F)

#a <- data.table(xlsx::read.xlsx(paste(inp_dirPIL, "/201908_Interfish_PIL.xlsx", sep=""), sheetName="RACHEL ANNE"))
#summary(a)
filename <- "C:/Users/SRC01/OneDrive - CEFAS/SC/Rscripts/FSP2021/Data/Processors/SPR/Interfish/202009_Interfish_SPR.xlsx" #SEPT 2020
filename2 <- "C:/Users/SRC01/OneDrive - CEFAS/SC/Rscripts/FSP2021/Data/Processors/SPR/Interfish/202010_Interfish_SPR.xlsx" #OCT 2020
filename3 <- "C:/Users/SRC01/OneDrive - CEFAS/SC/Rscripts/FSP2021/Data/Processors/SPR/Interfish/202011_Interfish_SPR.xlsx" #NOV 2020

library(readxl) # to keep the format of the dates, if you use openxlsx the data gets unformatted
#names <- excel_sheets(filename)
sprinterfish0920 <- lapply(excel_sheets(filename), read_excel, path = filename)
sprinterfish1020 <- lapply(excel_sheets(filename2), read_excel, path = filename2)
sprinterfish1120 <- lapply(excel_sheets(filename3), read_excel, path = filename3)

#to a dataframe
library(plyr)
sprinterfish0920 <- ldply(sprinterfish0920, data.frame)
dim(sprinterfish0920);head(sprinterfish0920) #100 10
ggplot(sprinterfish0920,aes(x=length.cm.,y=weight.g.))+geom_point(aes(color=vessel))
#geom_line(aes(group = vessel, linetype = vessel))
#save the aggregated data and ready to start the analysis
  #write.csv(sprinterfish0920, file=paste(out_dir,"SPR_Interfish_092020.csv",sep="/"),quote=F,row.names = F)

#10-2020
sprinterfish1020 <- ldply(sprinterfish1020, data.frame)
dim(sprinterfish1020);head(sprinterfish1020) #350 10
ggplot(sprinterfish1020,aes(x=length.cm.,y=weight.g.))+geom_point(aes(color=vessel))
table(sprinterfish1020$length.cm.)

# Round TL (some are at mm instead of half cm)
plot(weight.g.~length.cm.,sprinterfish1020)
table(sprinterfish1020$length.cm.)

sprinterfish1020$TLcm <- (round(sprinterfish1020$length.cm./0.5)*0.5/1) #already in cm here

#sprinterfish1020[,TLcm:=(round(length.cm./5)*5)/10]
table(sprinterfish1020$TLcm)
plot(weight.g.~TLcm, data=sprinterfish1020)

head(sprinterfish1020)

table(sprinterfish1020$TLcm)
table(sprinterfish1020$length.cm.)

sprinterfish1020 <- sprinterfish1020[,c("processor","species","total.catch..kg.", "vessel","Intake.number","date","sample","TLcm","weight.g.", "bins")]
colnames(sprinterfish1020) <- c("processor","species","total.catch..kg.", "vessel","Intake.number","date","sample","length.cm.","weight.g.", "bins")

#geom_line(aes(group = vessel, linetype = vessel))
#save the aggregated data and ready to start the analysis
  #write.csv(sprinterfish1020, file=paste(out_dir,"SPR_Interfish_102020.csv",sep="/"),quote=F,row.names = F)

#11-2020
sprinterfish1120 <- ldply(sprinterfish1120, data.frame)
dim(sprinterfish1120);head(sprinterfish1120) #300 10
ggplot(sprinterfish1120,aes(x=length.cm.,y=weight.g.))+geom_point(aes(color=vessel))
#geom_line(aes(group = vessel, linetype = vessel))
#save the aggregated data and ready to start the analysis
  #write.csv(sprinterfish1120, file=paste(out_dir,"SPR_Interfish_112020.csv",sep="/"),quote=F,row.names = F)

#merge the files
#and then you just merge it month by month to create the final database
head(sprinterfish0920);sprinterfish0920 <- sprinterfish0920[,c("processor","species","total.catch..kg.","vessel", "Intake.number","date","sample","length.cm.","weight.g.")]
head(sprinterfish1020);sprinterfish1020 <- sprinterfish1020[,c("processor","species","total.catch..kg.","vessel", "Intake.number","date","sample","length.cm.","weight.g.")]
head(sprinterfish1120);sprinterfish1120 <- sprinterfish1120[,c("processor","species","total.catch..kg.","vessel", "Intake.number","date","sample","length.cm.","weight.g.")]

sprdataall <- rbind(sprinterfish0920,sprinterfish1020,sprinterfish1120)
dim(sprdataall) #749 9
head(sprdataall)
summary(sprdataall)

#some exploratory graphs to see if the data looks correct
plot(length.cm.~weight.g.,sprdataall)
#
ggplot(sprdataall,aes(x=length.cm.,y=weight.g.))+geom_jitter(aes(color=as.factor(month(sprdataall$date))))
ggplot(sprdataall,aes(x=length.cm.,y=weight.g.))+geom_jitter(aes(color=vessel))
#
ggplot(sprdataall,aes(x=length.cm.))+geom_density(aes(color=vessel))
ggplot(sprdataall,aes(x=length.cm.))+geom_density(aes(color=as.factor(month(sprdataall$date))))
#
ggplot(sprdataall,aes(x=length.cm.,fill=vessel))+geom_density(alpha=0.4)
ggplot(sprdataall,aes(x=length.cm.,fill=as.factor(month(sprdataall$date))))+geom_density(alpha=0.4)


## Histogram with density plot
ggplot(sprdataall, aes(x=length.cm.)) +
   geom_histogram(aes(y=..density..), colour="black", fill="white")+
   geom_density(alpha=.2, fill="#FF6666")
table(sprdataall$length.cm.)


#3.1 add sample weight----
head(sprdataall)

intersample <- with(sprdataall,aggregate(weight.g.,list(date=date,vessel=vessel, total.catch..kg.=total.catch..kg.,species=species),sum)) #15
colnames(intersample)[5] <- "samplewt_g"

#intersample$month <- month(intersample$date)
#intersample$year <- year(intersample$date)

head(intersample);dim(intersample) #15 5
head(sprdataall);dim(sprdataall) #749 9

sprdataall <- merge(sprdataall,intersample,all.x=T)
dim(sprdataall)
summary(sprdataall)

#format it
head(sprdataall)
str(sprdataall)

sprdataall$month <- month(sprdataall$date)
sprdataall$year <-year(sprdataall$date)
sprdataall$source <- "processor"
sprdataall$totalcatch_kg <- ""
sprdataall$processor <- "INTERFISH"
sprdataall$species <- "SPR"

names(sprdataall) <-c("species","totalcatch_kg","vessel","date","processor","sampleID","todel", "length_cm","weight_g","samplewt_g","month","year","source","species")
sprdataall <-sprdataall[,c("date","vessel","totalcatch_kg","length_cm","weight_g","samplewt_g","sampleID","month","year","source","species","processor")]

#save it
#write.csv(sprdataall, file=paste(out_dir,"SPR_Interfish_sepoctnov20.csv",sep="/"),quote=F,row.names = F)


# ===================================================--
# 4. ALL PROCESSORS ----
# ===================================================--

out_dir <- file.path(getwd(), "Data/Output/")
list.files(out_dir)
#get all the final plots from all the processors
list.files(out_dir, pattern="SPR*")

combe <- read.csv(paste(out_dir,"/SPR_CoombeFisheriesLW_vs1.csv",sep=""),sep=",",header=TRUE,stringsAsFactors = F)
head(combe);dim(combe)
str(combe)
combe$date <- as.Date(as.character(combe$date,format="%Y-%m-%d"))
combe$sampleID <- paste(combe$processor,combe$vessel,sep="_")


# fal <- read.csv(paste(out_dir,"/SPR_FALFISH_2021_vs1.csv",sep=""),sep=",",header=TRUE,stringsAsFactors = F)
# head(fal);dim(fal)#802 12
# str(fal)
# fal$date <- as.Date(as.character(fal$date,format="%Y-%m-%d"))
# fal$sampleID <- paste(fal$processor,fal$sampleID,sep="_")
# 
# 
# inter <- read.csv(paste(out_dir,"/SPR_Interfish_sepoctnov20.csv",sep=""),sep=",",header=TRUE,stringsAsFactors = F)
# head(inter);dim(inter) #749 12
# str(inter)
# inter$date <- as.Date(as.character(inter$date,format="%Y-%m-%d"))
# inter$sampleID <- paste(inter$processor,inter$sampleID,sep="_")


#head(combe);head(fal);head(inter)
#proc <- rbind(combe,fal,inter)
#dim(proc)

proc <- combe

#save it
#write.csv(proc,file=paste(out_dir,"/SPR_processors_2022.csv",sep=""),row.names = F)

###########################################################################################################################--

#next script: B2.SPR_Plots_Processors2021 #-----

########################################### END ##############################################################################-----
