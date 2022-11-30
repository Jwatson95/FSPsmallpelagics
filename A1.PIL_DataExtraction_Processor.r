#Extracting data for PIL from processors FSP
#@silvia Rodriguez Climent
# 01-09-2020; last modified 09-09-2022
#---------------------------------------------------------------------##
##check
#C:/Users/JW30/OneDrive - CEFAS/Documents/Data
#setwd("C:/Users/JW30/OneDrive - CEFAS/Documents")
setwd("C:/Users/JW30/OneDrive - CEFAS/Documents/MD007B_2122/Working_area")

# ===================================================--
# 0. Set directories----
# ===================================================--
rm(list=ls())
# set input, output directories
inp_dir <- file.path(getwd(), "Data/Processors/PIL/")
plot_dir <- file.path(getwd(), "Data/plots/PIL/")
out_dir <- file.path(getwd(), "Data/Output/")
list.files(inp_dir,recursive=TRUE) # recursive TRUE to see inside the different folders

# load libraries # Package names
packages <- c("ggplot2", "data.table","xlsx","openxlsx","dplyr","readxl","stringr","plyr","tidyr","reshape2",
              "maps","mapdata","mapproj","mapplots","lubridate","rgdal","raster","GISTools","ggspatial", "tidyverse",
              "XLConnect")

lib <- function(packages){ 
  installed_packages <- packages %in% rownames(installed.packages())
  if (any(installed_packages == FALSE)) {
    install.packages(packages[!installed_packages])
  }
  # Packages loading
  invisible(lapply(packages, library, character.only = TRUE))
  
}

lib(packages) #install libraries through own function

install.packages("GISTools") ##might need newer version 

# ===================================================--
# 1. PIL INTERFISH----
# ===================================================--

#this year interfish is giving us the data in electronic format, so need to change the script here

inp_dir_inter <- file.path(getwd(),"Data/Processors/PIL/Interfish/")
list.files(inp_dir_inter)

#data from interfish is different as each excel tab is one date, need to extract and aggregate all together
#myfiles <- list.files(path=inp_dir_inter, pattern="*.xlsx", recursive = FALSE, full.names = FALSE, include.dirs=F)

filename <- paste(inp_dir_inter,"IFL PELAGIC LANDINGS 2021-2022_2.xlsx",sep="/")

#library(readxl) # to keep the format of the dates, if you use openxlsx the data gets unformatted
#names <- excel_sheets(filename)

fname <- filename

#extract data from excel file-----
multiplesheets <- function(fname) {
  
  # getting info about all excel sheets
  sheets <- readxl::excel_sheets(fname)
  tibble <- lapply(sheets, function(x) readxl::read_excel(fname, sheet = x))
  data_frame <- lapply(tibble, as.data.frame)
  
  # assigning names to data frames
  names(data_frame) <- sheets
  
  # print data frame
  print(data_frame)
}

pilinter <- multiplesheets(filename)
pilinter

pil <- list()
for (i in 1:length(pilinter)){
  pilinter[i]
  
  intake <- pilinter[[i]][1,2]#intake
  vessel <- pilinter[[i]][2,2]#vessel
  landed <- pilinter[[i]][3,2]#landed
  logbook <- pilinter[[i]][1,6]#logbook quantity
  bins <- pilinter[[i]][2,6]#total bins
  division <- pilinter[[i]][3,6]#subdivision
  tab_names <- excel_sheets(path = fname)
  date <- tab_names[i]
  fat <- pilinter[[i]][38,2]
  
  pilinter2 <- pilinter[[i]][6:30,1:3] #length 1-25
  pilinter3 <- pilinter[[i]][6:30,5:7]#length 26-50
  
  colnames(pilinter2) <- c("sample","length.cm","weight.g")
  colnames(pilinter3) <- c("sample","length.cm","weight.g")
  
  pilinter4 <- rbind(pilinter2,pilinter3)
  
  pil5<- merge(pilinter4,intake)
  colnames(pil5) <- c("sample","length.cm", "weight.g", "intake")
  pil6 <- merge(pil5,vessel)
  colnames(pil6) <- c("sample","length.cm", "weight.g", "intake","vessel")
  pil7 <- merge(pil6,landed)
  colnames(pil7) <- c("sample","length.cm", "weight.g", "intake","vessel","landed")
  pil8 <- merge(pil7,logbook)
  colnames(pil8) <- c("sample","length.cm", "weight.g", "intake","vessel","landed","logbook")
  pil9 <- merge(pil8,bins)
  colnames(pil9) <- c("sample","length.cm", "weight.g", "intake","vessel","landed","logbook","bins")
  pil10 <- merge(pil9,division)
  colnames(pil10) <- c("sample","length.cm", "weight.g", "intake","vessel","landed","logbook","bins","division")
  pil11 <- merge(pil10,date)
  colnames(pil11) <- c("sample","length.cm", "weight.g", "intake","vessel","landed","logbook","bins","division","date")
  pil12 <- merge(pil11,fat)
  colnames(pil12) <- c("sample","length.cm", "weight.g", "intake","vessel","landed","logbook","bins","division","date","fat")
  #print(pil12)
  pil <-rbind(pil,pil12)
}

#unify the table
head(pil)

table(pil$length.cm)
#subset(pil,length.cm=="6.5")
#subset(pil,length.cm=="19.16")

table(pil$weight.g)
#subset(pil,weight.g=="0")
table(pil$intake)
table(pil$vessel)

pil$vessel[pil$vessel=="CC"] <- "CHARLOTTE CLARE"
pil$vessel[pil$vessel=="SD"] <- "SERENE DAWN"
pil$vessel <- toupper(pil$vessel)
table(pil$landed)
pil$landed <- toupper(pil$landed)

table(pil$logbook)
table(pil$bins)

pil$catchlanded.kg <- as.numeric(pil$bins)*300
table(pil$catchlanded.kg)

table(pil$division)
pil$division <- toupper(pil$division)

head(pil)
table(pil$date)

pil$date2 <- gsub('[CC,SD,RA]', '', pil$date)
pil$date <- as.Date(as.character(pil$date2),origin="1899-12-30",format="%d-%m-%y")
table(pil$date)

table(pil$fat)
pil$fat <- round(as.numeric(pil$fat),2)

head(pil)
pil <- pil[,c("date","intake","logbook","division","length.cm","weight.g","fat","vessel","bins","landed","catchlanded.kg")]
pil$species <- "PIL"

plot(weight.g~length.cm,data=pil)
hist(as.numeric(pil$length.cm))
#subset(pil,length.cm=="0")
#subset(pil,length.cm<"12")
plot(pil$fat~pil$date,type="b") #need better visualization


# Histogram with density plot
a <- ggplot(pil, aes(x=as.numeric(length.cm))) + 
  geom_histogram(aes(y=..density..), colour="black", fill="white",binwidth = 0.5)+
  geom_density(alpha=.2, fill="#FF6666") 


#save the output
#write.csv(pil,file=paste(out_dir,"PIL_Interfish_untilnov.csv",sep="/"),row.names=F)

#if you need to round them by 0.5 cm
#pildataall$TLcm <- round((pildataall$length.cm./5)*5) #round to 1 cm
# Round TL (half cm)----
#pildataall$TLcm <- round_any(pildataall$length.cm.,0.5,f=floor)#with plyr library

table(pil$length.cm)
colnames(pil)[which(names(pil) == "length.cm")] <- "TLcm"

# Histogram with density plot
a <- ggplot(pil, aes(x=as.numeric(TLcm))) + 
  geom_histogram(aes(y=..density..), colour="black", fill="white",binwidth = 0.5)+
  geom_density(alpha=.2, fill="#FF6666") 

#save plots
#ggsave(file=paste0(plot_dir, "/PIL_0.5.png"), a, width=24, height=16, units="cm", dpi=200)

# convert date in date format
#pildataall$date<-strftime(strptime(pildataall$date,"%Y-%m-%d"),"%d/%m/%Y")  #changing from one format to another one

str(pil$date)
str(pil)
pil$weight.g <- as.numeric(pil$weight.g)
pil$TLcm <- as.numeric(pil$TLcm)

ggplot(pil,aes(x=TLcm,y=weight.g))+geom_jitter(aes(color=as.factor(month(pil$date))))+theme(legend.position="bottom")
ggplot(pil,aes(x=TLcm,y=weight.g))+geom_jitter(aes(color=vessel))+theme(legend.position="bottom")

ggplot(pil,aes(x=TLcm))+geom_density(aes(color=vessel))+theme(legend.position="bottom")
ggplot(pil,aes(x=TLcm))+geom_density(aes(color=as.factor(month(pil$date))))+theme(legend.position="bottom")

ggplot(pil,aes(x=TLcm,fill=vessel))+geom_density(alpha=0.4)+theme(legend.position="bottom")
ggplot(pil,aes(x=TLcm,fill=as.factor(month(pil$date))))+geom_density(alpha=0.4)+theme(legend.position="bottom")


#add sample weight----
head(pil)
sample <- with (pil,aggregate(weight.g,list(date=date,vessel=vessel),sum))
sample
colnames(sample)[3] <- "Wsample_g"

pil <- merge(pil,sample)


#format it
head(pil)
pil$month <- month(pil$date)
pil$year <- year(pil$date)
pil$source <- "processor"
pil$processor <- "INTERFISH"
pil$fishingseason <- "2021-2022"

pilfat <- pil[,c("date","vessel","bins","catchlanded.kg","intake","TLcm","weight.g","Wsample_g","fat","month","year","source","fishingseason","species","division")]
pil <- pil[,c("date","vessel","bins","catchlanded.kg","intake","TLcm","weight.g","Wsample_g","month","year","source","fishingseason","species","division")]

#save the aggregated data and ready to start the analysis
write.csv(pil, file=paste(out_dir,"PIL_Interfish_untilnov.csv",sep="/"),quote=F,row.names = F)
write.csv(pilfat,file=paste(out_dir,"PIL_Interfish_fat.csv",sep="/"),quote=F,row.names=F)

# ===================================================--
# 2. PIL FALFISH----
# ===================================================--
inp_dir_Falfish <- file.path(getwd(),"Data/Processors/PIL/Falfish/")
list.files(inp_dir_Falfish)

# read excel files (make sure all are in .xlsx format, sometimes Billy sends it in a different format)
myfiles <- list.files(path=inp_dir_Falfish, pattern=".xlsx", recursive = FALSE, full.names = FALSE, include.dirs=F)

### Length-Weight Data
#library(openxlsx)
mydata <- list()
for(i in 1:length(myfiles)){
  # temporary object with sheet names
  temp <- unlist(getSheetNames(paste(inp_dir_Falfish,myfiles[i], sep="/")))
  # if less than 3 sheets, issue a warning and move on to next iteration
  if(length(temp)>2) {
    mydata[[myfiles[i]]] <- xlsx::read.xlsx(paste(inp_dir_Falfish,myfiles[i], sep="/"), sheetName=temp[3])} else {
      warning('Check ', myfiles[i], ": sheet LW might be missing")}
  # create list with 3rd sheet
}

#Only keep length and weight columns
for(i in 1:length(myfiles)){
  mydata[[myfiles[[i]]]] <- mydata[[myfiles[i]]][1:2]
}

# Add column with file name to each list element
mydata <- mapply(cbind, mydata, "SampleID"=myfiles, SIMPLIFY=F)
#check if column names are identical between two data frames
#identical(names(mydata), myfiles)
mydata <- data.table(do.call(rbind.data.frame, mydata))


# Extract date
dates <- list()
for(i in 1:length(myfiles)){
  dates[[myfiles[i]]] <- xlsx::read.xlsx(paste(inp_dir_Falfish,myfiles[i], sep="/"), sheetIndex=1, startRow=1, rowIndex = 1, header=F, colIndex = 29)
  if(is.na(dates[[myfiles[i]]])) dates[[myfiles[i]]] <- xlsx::read.xlsx(paste(inp_dir_Falfish,myfiles[i], sep="/"), sheetIndex=2, startRow=1, rowIndex = 1, header=F, colIndex = 29) 
} 
dates <- mapply(cbind, dates, "SampleID"=myfiles, SIMPLIFY=F)
dates <- data.table(do.call(rbind.data.frame, dates))

#dates$date<-strftime(strptime(dates$X29,"%d/%m/%Y"),"%d/%m/%Y")  #changing from one format to another one
dates$date<-strftime(strptime(dates$X29,"%d/%m/%Y"),"%d/%m/%y")  #changing from one format to another one


# Extract species
sp <- list()
for(i in 1:length(myfiles)){
  sp[[myfiles[i]]] <- xlsx::read.xlsx(paste(inp_dir_Falfish,myfiles[i], sep="/"), sheetIndex=1, startRow=1, rowIndex = 1, header=F, colIndex = 16)
  if(sp[[myfiles[[i]]]]!="Sardines") warning('Check ', myfiles[i], ": wrong species?")
  #if(is.na(dates[[myfiles[i]]])) dates[[myfiles[i]]] <- xlsx::read.xlsx(paste(inp_dir_Falfish,myfiles[i], sep="/"), sheetIndex=2, startRow=1, rowIndex = 1, header=F, colIndex = 29) 
} 

# Extract weight
sampleWT <- list()
for(i in 1:length(myfiles)){
  sampleWT[[myfiles[i]]] <- xlsx::read.xlsx(paste(inp_dir_Falfish,myfiles[i], sep="/"), sheetIndex=1, rowIndex = 39, header=F, colIndex = 18)} 
sampleWT <- mapply(cbind, sampleWT, "SampleID"=myfiles, SIMPLIFY=F)
sampleWT <- data.table(do.call(rbind.data.frame, sampleWT))

# Merge everything in a data.table
FALFISH <- merge(mydata, dates, by="SampleID", all=T)
FALFISH <- merge(FALFISH, sampleWT, by="SampleID", all=T)
names(FALFISH) <- c("ID", "weight", "TL", "todelete", "Date", "SampleWt")
FALFISH <- FALFISH[,c( "ID", "weight",  "TL","Date", "SampleWt")]

# Round TL (some are at mm instead of half cm)
plot(weight~TL,FALFISH)
#subset(FALFISH,TL<100)# need to do 2 LWR because small sardines did appear at the end of the fishing season
table(FALFISH$TL)
FALFISH[,TLcm := (round(TL/5)*5)/10]
table(FALFISH$TLcm)

plot(weight~TLcm, data=FALFISH)
#subset(FALFISH,TLcm<13.0)# those 2 seem outliers, just removed them
#FALFISH <- subset(FALFISH,!TL<100)

# convert date in date format
FALFISH[,Date:=as.Date(Date, format = "%d/%m/%y")]
# column for month
FALFISH[,month:=month(Date)]
FALFISH[,year:=year(Date)]

#add landings----
head(FALFISH)  

for (i in 1:nrow(FALFISH)){
  {if(as.character(substr(FALFISH$ID[i],1,3)=="GHA"))
    FALFISH$Vessel[i]="GOLDEN HARVEST"}
  {if(as.character(substr(FALFISH$ID[i],1,3)=="LYO"))
    FALFISH$Vessel[i]="LYONESSE"}
  {if(as.character(substr(FALFISH$ID[i],1,3)=="PLM"))
    FALFISH$Vessel[i]="PELAGIC MARKSMAN"}
  {if(as.character(substr(FALFISH$ID[i],1,3)=="GAL"))
    FALFISH$Vessel[i]="GALWADY MOR"}
  {if(as.character(substr(FALFISH$ID[i],1,3)=="PRI"))
    FALFISH$Vessel[i]="PRIDE OF CORNWALL"}
  {if(as.character(substr(FALFISH$ID[i],1,3)=="CFR"))
    FALFISH$Vessel[i]="CONSTANT FRIEND"}  #normally sprat boat, but in nov had sardines
}

table(FALFISH$Vessel)
FALFISH$ID2 <- substr(FALFISH$ID,1,8)

FALFISH <- FALFISH[,c("Date","Vessel","SampleWt","month","year","TLcm","weight","ID2")]

##merge with landings
#inp_dir_land="C:/Users/SRC01/OneDrive - CEFAS/SC/Rscripts/FSP1920/Data/Processors/PIL/Falfish/landings" 
#list.files(inp_dir_land)

#land <- read.csv(paste(inp_dir_land,"/SardineWeightLengthAnalysis20192020Season.csv",sep=""),sep=",",header=FALSE)

#head(land)

#land <- land[-c(1:3),]
#land <- land[,c("V2","V3")]

##merge it with the LW
#land
#names(land) <- c("ID2","landings_kg")

#FALFISH_final <- merge(FALFISH,land,by="ID2")
#FALFISH_final$processor <- "Falfish"
#FALFISH_final$species <- "PIL"


#add processor in sample ID----
FALFISH$ID <- paste("FALFISH",FALFISH$ID2,sep="_")
FALFISH <- FALFISH[,c("Date","Vessel","SampleWt", "month", "year", "TLcm", "weight","ID")]

#save the file
#write.csv(FALFISH, file=paste(out_dir,"/PIL_FALFISH_2022.csv",sep="/"),quote=F,row.names = F)


# ===================================================--
# 3. PIL OCEANFISH----
# ===================================================--
#the files that they give to me earlier were in .ods format, and i've i tried to put them in excel, they lose the format, so i will merge it to the new files
inp_dir_ocean <- file.path(getwd(),"Data/Processors/PIL/OceanFish/")
list.files(inp_dir_ocean)

# read excel files #remove the port from the files for the script to work propoerly
#myfiles <- list.files(path=inp_dir_ocean, pattern=c("*2020.xlsx|*2021.xlsx"), recursive = FALSE, full.names = FALSE, include.dirs=F)
myfiles <- list.files(path=inp_dir_ocean, pattern=c("*.xlsx"), recursive = FALSE, full.names = FALSE, include.dirs=F)


### Extract Length-Weight Data from the processors files
mydata <- list()
#library(openxlsx)
for(i in 1:length(myfiles)){
  # temporary object with sheetnames
  temp <- unlist(getSheetNames(paste(inp_dir_ocean,myfiles[i], sep="/")))
  {mydata[[myfiles[i]]] <- xlsx::read.xlsx(paste(inp_dir_ocean,myfiles[i], sep="/"), sheetName=temp[1],header=T,stringsAsFactors = FALSE)
    #mydata[[i]]$Date <- strftime(strptime(mydata[[i]]$Date,"%Y/%m/%d"))}# as.is avoids data to be converted to factors
  }}
##changed
#do the same with the function, not working properly
#readfiles(inp_dir_ocean,myfiles)

str(mydata)
head(mydata[[1]]) # check everything is alright

file2 <- mydata
file2 <- do.call(rbind, lapply(mydata, as.data.frame))

colnames(file2) <- c("Date","Vessel","Landing_kg","size_g1","size_g2","size_g3","size_g4","size_g5","size_g6","size_g7","size_g8","size_g9","size_g10", 
                     "length_cm1","length_cm2","length_cm3","length_cm4","length_cm5","length_cm6","length_cm7","length_cm8","length_cm9","length_cm10",
                     "Fishpkilo","avgsize_g","avglength_cm")
head(file2)
dim(file2);str(file2)

#remove blank spaces in the Vessel name
unique(file2$Vessel)
file2$Vessel[file2$Vessel == "Athore"] <- "Asthore"
file2$Vessel[file2$Vessel == "Vesta "] <- "Vesta"
file2$Vessel <- trimws(file2$Vessel)

#unify the names of the vessels
table(file2$Vessel)
file2$Vessel <- tolower(as.character(file2$Vessel))

#eliminate columns that you don't want
file2 <- file2[,c("Date","Vessel","Landing_kg","size_g1","size_g2","size_g3","size_g4","size_g5","size_g6","size_g7","size_g8","size_g9","size_g10", 
                  "length_cm1","length_cm2","length_cm3","length_cm4","length_cm5","length_cm6","length_cm7","length_cm8","length_cm9","length_cm10")]

# convert date in date format
file2$Date<-strftime(strptime(file2$Date,"%Y-%m-%d"),"%d/%m/%Y")  #changing from one format to another one

#fill up empty spaces
for(i in 1:nrow(file2)){
  {if(is.na(file2$Date[i]))
    file2$Date[i]=file2$Date[i-1]}
  {if(is.na(file2$Vessel[i]))
    file2$Vessel[i]=file2$Vessel[i-1]}
  {if(is.na(file2$Landing_kg[i]))
    file2$Landing_kg[i]=file2$Landing_kg[i-1]}
  {if(is.na(file2$size_g1[i]))
    file2$size_g1[i]=0}
  {if(is.na(file2$size_g2[i]))
    file2$size_g2[i]=0}
  {if(is.na(file2$size_g3[i]))
    file2$size_g3[i]=0}
  {if(is.na(file2$size_g4[i]))
    file2$size_g4[i]=0}
  {if(is.na(file2$size_g5[i]))
    file2$size_g5[i]=0}
  {if(is.na(file2$size_g6[i]))
    file2$size_g6[i]=0}
  {if(is.na(file2$size_g7[i]))
    file2$size_g7[i]=0}
  {if(is.na(file2$size_g8[i]))
    file2$size_g8[i]=0}
  {if(is.na(file2$size_g9[i]))
    file2$size_g9[i]=0}
  {if(is.na(file2$size_g10[i]))
    file2$size_g10[i]=0}
  {if(is.na(file2$length_cm1[i]))
    file2$length_cm1[i]=0}
  {if(is.na(file2$length_cm2[i]))
    file2$length_cm2[i]=0}
  {if(is.na(file2$length_cm3[i]))
    file2$length_cm3[i]=0}
  {if(is.na(file2$length_cm4[i]))
    file2$length_cm4[i]=0}
  {if(is.na(file2$length_cm5[i]))
    file2$length_cm5[i]=0}
  {if(is.na(file2$length_cm6[i]))
    file2$length_cm6[i]=0}
  {if(is.na(file2$length_cm7[i]))
    file2$length_cm7[i]=0}
  {if(is.na(file2$length_cm8[i]))
    file2$length_cm8[i]=0}
  {if(is.na(file2$length_cm9[i]))
    file2$length_cm9[i]=0}
  {if(is.na(file2$length_cm10[i]))
    file2$length_cm10[i]=0}
}

#View(file2)
summary(file2)

# column for month and year
file2$month <- month(file2$Date)
file2$month <- format(as.Date(file2$Date, format="%d/%m/%Y"),"%m")
#file2$year <- year(file2$Date, format="%d/%m/%Y")
file2$year <- format(as.Date(file2$Date, format="%d/%m/%Y"),"%Y")


#create a size  and length database
size <- file2[,c("Date","year","Vessel","Landing_kg","size_g1","size_g2","size_g3","size_g4","size_g5","size_g6","size_g7","size_g8","size_g9","size_g10")]
length <- file2[,c("Date","year","Vessel","Landing_kg","length_cm1","length_cm2","length_cm3","length_cm4","length_cm5","length_cm6","length_cm7","length_cm8","length_cm9","length_cm10")]

size;dim(size)
length;dim(length)

table(size$Vessel)
table(length$Vessel)

#subset first the vessels that you have
asthore <- subset(size,Vessel=="asthore")
mayflower <-subset(size,Vessel=="mayflower")
vesta <- subset(size,Vessel=="vesta")
resolute <- subset(size,Vessel=="resolute")
serene <- subset(size,Vessel=="serene dawn")

ves=c("asthore","mayflower","vesta","resolute","serene")
dat <- unique(size$Date)
num=as.vector(1:length(dat))
#dat <- unique(as.POSIXct(as.POSIXlt(size$Date))) 

#for size (length in cm)
all <- list()
pre <- list()

for(j in num){
  all[[j]] <- get(ves[j])
  #all[[j]]$Date <- as.POSIXct(as.POSIXlt(all[[j]]$Date)) 
  pre[[j]] <- melt(all[[j]],id=c("Date","Vessel","Landing_kg","year"))
}

allpre <- do.call(rbind.data.frame,pre)
#View(allpre)

#for length
#subset first the vessels that you have
asthore <- subset(length,Vessel=="asthore")
mayflower <-subset(length,Vessel=="mayflower")
vesta <- subset(length,Vessel=="vesta")
resolute <- subset(length,Vessel=="resolute")
serene <- subset(length,Vessel=="serene dawn")

len <- list()
lenses <- list()

for(j in num){
  len[[j]] <- get(ves[j])
  #len[[j]]$Date <- as.POSIXct(as.POSIXlt(len[[j]]$Date)) 
  lenses[[j]] <- melt(len[[j]],id=c("Date","Vessel","Landing_kg","year"))
}

lenlenses <- do.call(rbind.data.frame,lenses)
#View(lenlenses)

#merge both length and weight (order first)
dim(lenlenses)
dim(allpre)

allpre<-allpre[with(allpre,order(Date,Vessel)),]
lenlenses<-lenlenses[with(lenlenses,order(Date,Vessel)),]

colnames(allpre)[5:6] <- c("size_g","weight_g")
colnames(lenlenses)[5:6] <- c("measure_cm","length_cm")

LW <- cbind(allpre,lenlenses)
head(LW)
LW <- LW[c("Date","Vessel","Landing_kg", "year","length_cm","weight_g")]
LW$species <- "PIL"
plot(weight_g~length_cm,LW, main="PIL Oceanfish 2020/2021")

aver <- subset(LW,!length_cm==0) #without the zeros
plot(weight_g~length_cm,aver, main="PIL Oceanfish 2020/2021")
#View(subset(LW,length_cm<12.0))

#correct errors
#LW$length_cm[LW$length_cm == "18.54"] <- "18.5"
#LW$length_cm[LW$length_cm == "21.54"] <- "21.5"
#LW$length_cm[LW$length_cm == "19.1"] <- "19"

table(LW$length_cm)

str(LW$Date)
#LW$Date <- format(as.Date(LW$Date), format="%Y-%m-%d"),"%d/%m/%Y")
#file2$year <- year(file2$Date, format="%d/%m/%Y")

#LW$month <- month(LW$Date)
LW$month<- format(as.Date(LW$Date, format="%d/%m/%Y"),"%m")
head(LW)

ggplot(LW)+geom_point(aes(y=weight_g,x=length_cm,colour=as.factor(month)))
ggplot(LW)+geom_jitter(aes(y=weight_g,x=length_cm,colour=as.factor(month)))
ggplot(LW)+geom_smooth(aes(y=weight_g,x=length_cm,colour=as.factor(month)))
ggplot(LW)+geom_density(aes(x=length_cm,colour=as.factor(month))) # you can see length variation by month


#need to transform bins and dolavs to kg in the landing
#1 bin or dolav=350 kg sardine
table(LW$Landing_kg)

#transform the landings into kg
LW$Landing_kg <- gsub("bins|bin|dolavs|Bins|Dolavs","   ", LW$Landing_kg)
table(LW$Landing_kg)
LW$Landing_kg <- as.numeric(LW$Landing_kg)*350


#add sample weight----
sample <- with (LW,aggregate(weight_g,list(Date=Date,month=month,year=year,Vessel=Vessel),sum))
sample
colnames(sample)[5] <- "Wsample_g"

LW <- merge(LW,sample)

head(LW)
LW$ID <- paste(LW$Vessel,"Oceanfish",sep="_")

#save file
#write.csv(LW, file=paste(out_dir,"PIL_OceanfishLW_2020_vs4.csv",sep="/"),quote=F,row.names = F)# until 26/11/2020


# ===================================================--
# 4. PIL COOMBEFISHERIES----
# ===================================================--
##reading directly the fils in the formt they've send them
inp_dir_cf <- file.path(getwd(),"Data/Processors/PIL/CoombeFisheries/")
list.files(inp_dir_cf)
# read excel files
myfiles <- list.files(path=inp_dir_cf, pattern="*.xlsx", recursive = FALSE, full.names = FALSE, include.dirs=F)


## Extract Length-Weight Data from the processors files
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

#for anchovies they only weight them, so we don't have lengths

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

# Extract the weight(g) of the individuals (they can be in 3 different columns)
llista <- list()
for(i in 1:length(mydata2)){
  llista[[i]] <- mydata2[[i]][rowSums(is.na(mydata2[[i]][,0:ncol(mydata2[[i]])]))<ncol(mydata2[[i]]),c(2,18,40,62)]
}

for(i in 1:length(llista)){
  llista[[i]] <- llista[[i]][c(3:48),]
}


llista <- mapply(cbind, llista, "SampleID"=myfiles, SIMPLIFY=F)
llista <- data.table(do.call(rbind.data.frame, llista))
llista <- as.data.frame(llista)

head(dates)
head(llista)
llista <- merge(dates,llista,by=c("SampleID"))
names(llista) <- c("SampleID","date","todel","X18","X40","X62")

# tidyr library to reorder the database
library(tidyr)
tb <- as_tibble(llista, stringsAsFactors = FALSE)
tb

tb2 <- tb %>%
  pivot_longer(
    X18:X62,
    names_to = "variable",
    values_to = "weight",
    values_drop_na = TRUE
  )

#View(tb2)
head(tb2)

tb3 <- tb2 %>%
  separate(weight, c("w1", "w2"), 2)
tb3
#View(tb3)

tb3 <- as.data.frame(tb3)
str(tb3)

table(tb3$w2)# some of the columns have more than 2 values, need to split
table(tb3$SampleID)
tb3$SampleID <- gsub(" ", "",tb3$SampleID)

table(tb3$date,tb3$SampleID)
tb3$date <- substr(tb3$SampleID,3,10)
#tb3$date[tb3$date==".10.20AN"] <- "28.10.20"
table(tb3$date)
tb3$date <- as.Date(tb3$date,"%d.%m.%y")

tb3 <- tb3[,c("w1","w2","date")]

w1 <- tb3[,c("w1","date")]
w2 <- tb3[,c("w2","date")]

head(w1)
w1$w1 <- as.numeric(w1$w1)
w1$w1[w1$w1=="11"] <- "119"
str(w1)
w1$w1 <- as.numeric(as.character(w1$w1))
plot(w1$w1)

# w2$w2 <- gsub("[^0-9]","",w2)
head(w2)
table(w2$w2)
w2$w2[w2$w2=="9  52"] <- "52"
w2$w2[w2$w2=="77  49"] <- "49"
w2$w2 <- as.numeric(w2$w2)
plot(w2$w2)


#also from tydr library
#w3 <- separate_rows(w2, w2, convert = TRUE)
#View(w3)
#w3 <- drop_na(w3)

head(w1);dim(w1)
head(w2);dim(w2)
#w3 <- as.data.frame(w3)
colnames(w2) <- c("w1","date")
w4 <- rbind(w1,w2)

head(w4);dim(w4)
w4$month <- month(w4$date)
w4$year <- year(w4$date)
table(w4$month)

#add sample weight----
w4$w1 <- as.numeric(as.character(w4$w1))
summary(w4$w1) #need to transform the NA in zeros
w4$w1[is.na(w4$w1)] <- 0
sample <- with (w4,aggregate(w1,list(date=date,month=month,year=year),sum))
sample
colnames(sample)[4] <- "Wsample_g"

w4 <- merge(w4,sample)

#format it
head(w4)
w4$species <- "PIL"
w4$vessel <- "GIRLRONA"
w4$processor <- "CombeeFisheries"
w4$source <- "processor"
w4$landing_kg <- ""
w4$length_cm <- ""

w4 <- w4[,c("date","month","year","species","vessel","landing_kg","length_cm","w1","Wsample_g","processor","source")]
colnames(w4) <- c("date","month","year","species","vessel","landing_kg","length_cm","Weight_g","Wsample_g","processor","source")

#save the aggregated data and ready to start the analysis
#write.csv(w4, file=paste(out_dir,"final/PIL_CoombeFisheriesW_vs2.csv",sep="/"),quote=F,row.names = F)
######################################################################################################################--

# ===================================================--
# 5. PIL LANDINGS (Gus)----
# ===================================================--
# inp_land <- "C:/Users/SRC01/OneDrive - CEFAS/SC/Rscripts/FSP2021/Data/Processors/PIL/"
# list.files(inp_land)
# 
# #land <- read.xlsx(paste(inp_land,"SardinelandingsjULY_AUG2020.xlsx",sep=""),sep=" ",1) #uncorrect dates
# library(readxl)
# land <- read_excel(paste(inp_land,"SardinelandingsjULY_AUG2020.xlsx",sep="")) #reads the dates correctly
# 
# land <- as.data.frame(land)
# land[is.na(land)] <- 0
# summary(land)
# 
# colnames(land)[1] <- "date"
# str(land$date)
# 
# 
# land$date <- as.Date(land$date,format="%Y-%m-%d")
# 
# plot(land$date,land$`SERENE DAWN`)
# plot(land$date,land$RESOLUTE,type="l")
# ggplot(aes(date),data=land)
# 

# ===================================================--
# 6. ALL PROCESSORS----
# ===================================================--
out_dir <- file.path(getwd(), "Data/Output/")
list.files(out_dir)
#get all the final plots from all the processors
list.files(out_dir, pattern="PIL*")

#READ final files
fal <- read.csv(paste(out_dir,"/PIL_FALFISH_2022.csv",sep=""),sep=",",header=TRUE,stringsAsFactors = F)
head(fal)
#fal$month <- month(fal$Date)
#fal$year <- year(fal$Date)
fal$Date <- as.Date(fal$Date)
fal$landings_kg <- ""
fal$processor <- "Falfish"
fal$species <- "PIL"
fal <- fal[,c("Date","Vessel","TLcm","weight","SampleWt","landings_kg","species","month","year","processor","ID")]
names(fal) <- c("date","vessel","TL_cm","weight_g","SampleWt","landings_kg","species","month","year","processor","id")  
plot(weight_g~TL_cm,fal)
summary(fal)
(fal[!complete.cases(fal),]) #1
fal$TL_cm[is.na(fal$TL_cm)] <- 0

inter <- read.table(paste(out_dir,"/PIL_Interfish_untilnov.csv",sep=""),sep=",",header=TRUE,stringsAsFactors = F)
head(inter);str(inter)
inter$date <- as.Date(inter$date)
inter$month <- month(inter$date)
inter$year <- year(inter$date)
inter$processor <- "Interfish"

inter <- inter[,c("date","vessel","TLcm","weight.g","Wsample_g","catchlanded.kg","species","month","year","processor","intake")]
names(inter) <- c("date","vessel","TL_cm","weight_g","SampleWt","landings_kg","species","month","year","processor","id")  
plot(weight_g~TL_cm,inter)


# ocean <- read.csv(paste(out_dir,"/PIL_OceanfishLW_2020_vs4.csv",sep=""),sep=",",header=TRUE,stringsAsFactors = F)
# head(ocean)
# #ocean$Date <-strftime(strptime(ocean$Date ,"%d/%m/%y"),"%Y-%m-%d") 
# ocean$Date <- as.Date(ocean$Date,"%d/%m/%y")
# ocean$Vessel<- toupper(ocean$Vessel)
# ocean$processor <- "Oceanfish"
# ocean$id2 <- paste("Oceanfish",ocean$Vessel,sep="_")
# #ocean$id <- "nosource"
# ocean <- ocean[,c("Date","Vessel","length_cm","weight_g","Wsample_g","Landing_kg","species","month","year","processor","id2")]
# names(ocean) <- c("date","vessel","TL_cm","weight_g","SampleWt","landings_kg","species","month","year","processor","id")  
# plot(weight_g~TL_cm,ocean)
# dim(ocean) #60 NA without TL
# (ocean[!complete.cases(ocean),]) #60
# 
# ocean$TL_cm[is.na(ocean$TL_cm)] <- 0
# 
# dim(subset(ocean,TL_cm=="0"))#60


# combe <- read.csv(paste(out_dir,"/PIL_CoombeFisheriesW_vs2.csv",sep=""),sep=",",header=T,stringsAsFactors = F)
# head(combe)
# combe$date <- as.Date(combe$date)
# combe$id <- paste("CombeeFisheries",combe$vessel,sep="_")
# combe <- combe[,c("date","vessel","length_cm","Weight_g","Wsample_g","landing_kg","species","month","year","processor","id")]
# names(combe) <- c("date","vessel","TL_cm","weight_g","SampleWt","landings_kg","species","month","year","processor","id")  
# str(combe)
# combe$weight_g <- as.numeric(combe$weight_g)
#  #only weight in this one!!! no length

#merge all the processors----
str(fal);dim(fal);summary(fal)
str(inter);dim(inter);summary(inter)
#str(ocean);dim(ocean);summary(ocean)
#str(combe);dim(combe);summary(combe)


#all<- rbind(fal,inter,ocean,combe)
all<- rbind(fal,inter)
dim(all);head(all)
summary(all);dim(all)

table(all$month,all$year)
table(all$TL_cm)
table(all$id)

# #combe did not measure length
# all2 <- subset(all,!processor=="CombeeFisheries")
# table(all2$processor)

plot(weight_g~TL_cm,all)

table(all$TL_cm)
dim(subset(all,TL_cm=="0"))


#format it
all$source <- "processor" #all the data collected
#all3$source <- "processor" #only walid LW data

names(all) <- c("date","vessel","length_cm","weight_g", "samplewt_g","totalcatch_kg","species","month","year","processor","sampleID","source")
#names(all3) <- c("date","vessel","length_cm","weight_g", "samplewt_g","totalcatch_kg","species","month","year","processor","sampleID","source")

all <- all[,c("date","vessel","totalcatch_kg","length_cm","weight_g","samplewt_g","sampleID","month","year","source","processor","species")]
#all3 <- all3[,c("date","vessel","totalcatch_kg","length_cm","weight_g","samplewt_g","sampleID","month","year","source","processor","species")]

#save it
write.csv(all,file=paste(out_dir,"/PIL_processors_2022.csv",sep=""),row.names=F)
#write.csv(all3,file=paste(out_dir,"/PIL_processorsLW_2021.csv",sep=""),row.names=F)

###########################################################################################################################--

#next script: A2.PIL_Plots_Processors #-----

########################################### END ##############################################################################-----
