#Extracting data for ANE from processors FSP
#@silvia Rodriguez Climent
# 06-11-2020;last modified: 09-09-2022
#---------------------------------------------------------------------##

# ===================================================--
# 00. Set directories ----
# ===================================================--

rm(list=ls())
# set input, output directories
inp_dir <- file.path(getwd(), "Data/Processors/ANE/")  #dont have ANE data
plot_dir <- file.path(getwd(), "Data/plots/ANE")
out_dir <- file.path(getwd(), "Data/Output/final/")
list.files(inp_dir,recursive=TRUE) # recursive TRUE to see inside the different folders

#options(java.parameters = "-Xmx1000m")

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

lib(packages)


# ===================================================--
# 1. ANE COOMBEFISHERIES ----
# ===================================================--
##reading directly the fils in the formt they've send them
inp_dir_cf <- file.path(getwd(),"Data/Processors/ANE/CoombeFisheries/")
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

#for anchovies they only weight them, so we don't have lengths

# Extract date (should be in 10/01/2020 format in the original file)
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


head(dates);head(llista)
llista <- merge(dates,llista,by=c("SampleID"))
names(llista) <- c("SampleID","date","todel","X18","x40","x62")


# tidyr library to reorder the database
library(tidyr)
tb <- as_tibble(llista, stringsAsFactors = FALSE)

tb2 <- tb %>%
  pivot_longer(
    X18:x62,
    names_to = "variable",
    values_to = "weight",
    values_drop_na = TRUE
  )

#View(tb2)
head(tb2)

tb3 <- tb2 %>%
  separate(weight, c("w1", "w2"), 2)
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

# w2$w2 <- gsub("[^0-9]","",w2)
# head(w2)

#also from tydr library
w3 <- separate_rows(w2, w2, convert = TRUE)
#View(w3)
w3 <- drop_na(w3)

head(w1);dim(w1)
head(w3);dim(w3)
w3 <- as.data.frame(w3)
colnames(w3) <- c("w1","date")
w4 <- rbind(w1,w3)

head(w4);dim(w4)
w4$month <- month(w4$date)
w4$year <- year(w4$date)
table(w4$month)

#add sample weight----
sample <- with (w4,aggregate(w1,list(date=date,month=month,year=year),sum))
colnames(sample)[4] <- "Wsample_g"

w4 <- merge(w4,sample)

w4$species <- "ANE"
w4$vessel <- "GIRLRONA"
w4$processor <- "CoombeFisheries"
w4$source <- "processor"
w4$landing_kg <- ""
w4$length_cm <- ""
w4$division <- "27.7e"
w4$fishingseason <- "2020-2021"
w4$sampleID <- paste(w4$processor,w4$vessel,sep="_")

w4 <- w4[,c("date","vessel","landing_kg","length_cm","w1","Wsample_g","sampleID","month","year","source","fishingseason","species","division")]

colnames(w4) <- c("date","vessel","total_catch_kg","length_cm","weight_g","samplewt_g","sampleID",
                  "month","year","source","fishing_season","species","division")

w4$processor <- "CoombeFisheries"

#save the aggregated data and ready to start the analysis
#write.csv(w4, file=paste(out_dir,"ANE_CoombeFisheriesW_vs1.csv",sep="/"),quote=F,row.names = F)
######################################################################################################################################--          
#
# ===================================================--
# 2. ANE INTERFISH ----
# ===================================================--
inp_dir_inter <- file.path(getwd(),"Data/Processors/ANE/Interfish/")
list.files(inp_dir_inter)

# read excel files
myfiles <- list.files(path=inp_dir_inter, pattern=".xlsx", recursive = FALSE, full.names = FALSE, include.dirs=F)

#a <- data.table(xlsx::read.xlsx(paste(inp_dirPIL, "/201908_Interfish_PIL.xlsx", sep=""), sheetName="RACHEL ANNE"))
#summary(a)
filename <- "C:/Users/SRC01/OneDrive - CEFAS/SC/Rscripts/FSP2021/Data/Processors/ANE/Interfish/202009_Interfish_ANE.xlsx" #SEPT 2020
filename2 <- "C:/Users/SRC01/OneDrive - CEFAS/SC/Rscripts/FSP2021/Data/Processors/ANE/Interfish/202010_Interfish_ANE.xlsx" #OCT 2020
filename3 <- "C:/Users/SRC01/OneDrive - CEFAS/SC/Rscripts/FSP2021/Data/Processors/ANE/Interfish/202011_Interfish_ANE.xlsx" #NOV 2020

library(readxl) # to keep the format of the dates, if you use openxlsx the data gets unformatted
#names <- excel_sheets(filename)
aneinterfish0920 <- lapply(excel_sheets(filename), read_excel, path = filename)
aneinterfish1020 <- lapply(excel_sheets(filename2), read_excel, path = filename2)
aneinterfish1120 <- lapply(excel_sheets(filename3), read_excel, path = filename3)

#to a dataframe
library(plyr)
#09-2020
aneinterfish0920 <- ldply(aneinterfish0920, data.frame)
dim(aneinterfish0920);head(aneinterfish0920)
ggplot(aneinterfish0920,aes(x=length.cm.,y=weight.g.))+geom_point(aes(color=vessel))
#geom_line(aes(group = vessel, linetype = vessel))
#save the aggregated data and ready to start the analysis
#write.csv(aneinterfish0920, file=paste(out_dir,"ANE_Interfish_092020.csv",sep="/"),quote=F,row.names = F)

#10-2020
aneinterfish1020 <- ldply(aneinterfish1020, data.frame)
dim(aneinterfish1020);head(aneinterfish1020) #50 10
ggplot(aneinterfish1020,aes(x=length.cm.,y=weight.g.))+geom_point(aes(color=vessel))
#geom_line(aes(group = vessel, linetype = vessel))
#save the aggregated data and ready to start the analysis
#write.csv(aneinterfish1020, file=paste(out_dir,"ANE_Interfish_102020.csv",sep="/"),quote=F,row.names = F)

#11-2020
aneinterfish1120 <- ldply(aneinterfish1120, data.frame)
dim(aneinterfish1120);head(aneinterfish1120) #50 10
ggplot(aneinterfish1120,aes(x=length.cm.,y=weight.g.))+geom_point(aes(color=vessel))
#geom_line(aes(group = vessel, linetype = vessel))
#save the aggregated data and ready to start the analysis
#write.csv(aneinterfish1120, file=paste(out_dir,"ANE_Interfish_112020.csv",sep="/"),quote=F,row.names = F)


#merge the files
#and then you just merge it month by month to create the final database
head(aneinterfish0920);aneinterfish0920 <- aneinterfish0920[,c("processor","species","total.catch..kg.","vessel", "Intake.number","date","sample","length.cm.","weight.g.")]
head(aneinterfish1020);aneinterfish1020 <- aneinterfish1020[,c("processor","species","total.catch..kg.","vessel", "Intake.number","date","sample","length.cm.","weight.g.")]
head(aneinterfish1120);aneinterfish1120 <- aneinterfish1120[,c("processor","species","total.catch..kg.","vessel", "Intake.number","date","sample","length.cm.","weight.g.")]

anedataall <- rbind(aneinterfish0920,aneinterfish1020,aneinterfish1120)
dim(anedataall)
head(anedataall)
summary(anedataall)
anedataall$month <- month(anedataall$date)
anedataall$year <-year(anedataall$date)

#add sample weight----
sampleinter <- with (anedataall,aggregate(weight.g.,list(date=date,month=month,year=year,vessel=vessel),sum))
colnames(sampleinter)[5] <- "Wsample_g"

anedataall <- merge(anedataall,sampleinter)

anedataall$source <- "processor"
anedataall$sampleID <- paste(anedataall$processor,anedataall$Intake.number,sep="_")
anedataall$fishingseason <- "2020-2021"
anedataall$division <- "27.7e"

anedataall <- anedataall[,c("date","vessel","total.catch..kg.","length.cm.","weight.g.","Wsample_g","sampleID","month","year","source","fishingseason","species","division")]

colnames(anedataall) <- c("date","vessel","total_catch_kg","length_cm","weight_g","samplewt_g","sampleID",
                          "month","year","source","fishing_season","species","division")

anedataall$processor <- "Interfish"

#save the aggregated data and ready to start the analysis
#write.csv(anedataall, file=paste(out_dir,"ANE_Interfish_sepoctnov20.csv",sep="/"),quote=F,row.names = F)

######################################################################################################################################--          
#
# ===================================================--
# 3. Unify processors into the common database format----
# ===================================================--

#Common database format have the fields
# AGG (Fishers+processors): 
#     date/vessel/totalcatch_kg/length_cm/n/samplewt_g/sampleID/month/year/source/fishing_season/species/division
# n aggregated by length+date+vessel
# NON-AGG (Processors):
#     date/vessel/totalcatch_kg/length_cm/samplewt_g/sampleID/month/year/source/fishing_season/species/division

head(w4);dim(w4)
head(anedataall);dim(anedataall)

aneproc <- rbind(w4,anedataall)
dim(aneproc)

table(aneproc$month)
head(aneproc);dim(aneproc)
aneproc$processor <- toupper(aneproc$processor)

#write.csv(aneproc, file=paste(out_dir,"/ane_processors_2021.csv",sep="/"),quote=F,row.names = F)#with sample weight

######################################################################################################################################--          


# ===================================================--
# 4. PLOTS ----
# ===================================================--

# ===================================================--
# 4.1 Length distribution plots in a loop ----
# ===================================================--

#only for Interfish i=2

#your data
db <- aneproc
#your species
species=unique(db$species)
#your variable
var <- c(unique(db$processor))
#variable you want to plot
db$sel <- db$processor

head(db)

#loop (check names are correct in line 27 or change them)

plot1.TL1 <- list()
plot1.TL2 <- list()
plot1.TL3 <- list()

i=2
plot1=db[db$sel==var[i],]

names(plot1) <- c("date","vessel","total_catch_kg","length_cm","weight_g","samplewt_g","sampleID","month","year","source","fishing_season","species","division","processor","sel")
# Length distribution per month
plot1_cumMonth <- data.table(table(plot1$length_cm,plot1$month))
plot1_cum <- data.table(table(plot1$length_cm))
names(plot1_cumMonth) <- c("TL", "Month", "Freq")
names(plot1_cum) <- c("TL", "Freq")

# weighted average overall and weighted average by month
plot1_cumMonth[,TL:=as.numeric(TL)]
plot1_cumMonth[,Freq:=as.numeric(Freq)]
plot1_cumMonth[,wtMean:=weighted.mean(TL,Freq),by="Month"]

plot1_cum[,TL:=as.numeric(TL)]
plot1_cum[,wtMean:=weighted.mean(TL,Freq)]

#put name into months and order them
plot1_cumMonth$Month[plot1_cumMonth$Month=="7"] <- "Jul"
plot1_cumMonth$Month[plot1_cumMonth$Month=="8"] <- "Aug"
plot1_cumMonth$Month[plot1_cumMonth$Month=="9"] <- "Sep"
plot1_cumMonth$Month[plot1_cumMonth$Month=="10"] <- "Oct"
plot1_cumMonth$Month[plot1_cumMonth$Month=="11"] <- "Nov"
plot1_cumMonth$Month[plot1_cumMonth$Month=="12"] <- "Dec"
plot1_cumMonth$Month[plot1_cumMonth$Month=="1"] <- "Jan"
plot1_cumMonth$Month[plot1_cumMonth$Month=="2"] <- "Feb"

plot1_cumMonth$Month<- factor(plot1_cumMonth$Month,levels = c("Jul","Aug","Sep","Oct","Nov","Dec","Jan","Feb"),ordered=TRUE)  

#save the file
write.csv(plot1_cumMonth,paste(plot_dir,"/",var[i],"_cumMonth.csv",sep=""),row.names = F)
write.csv(plot1_cum,paste(plot_dir,"/",var[i],"_cum.csv",sep=""),row.names = F)


##Overall LFD--
plot1.TL1[[i]] <- ggplot(plot1_cum, aes(TL, Freq)) + geom_bar(stat="identity", position="dodge",fill="grey") +
  theme_bw(25) +ylab("N")+xlab("Total length (cm)")+ 
  geom_vline(aes(xintercept=wtMean), col="red", size=1.2,lty=2)+
  theme(legend.position = "none")+ggtitle(paste(var[i]))+
  scale_y_continuous(limits=c(0,max(plot1_cum$Freq)+10),expand=c(0,0))

ggsave(filename = paste(plot_dir,paste(var[i],species,"aTL.png",sep="_"),sep="/"), 
       plot = plot1.TL1[[i]], width = 25, 
       height = 20, units = "cm", dpi = 300, type = "cairo-png") 


##LFD by month--
plot1_cumMonth <- na.omit(plot1_cumMonth)
plot1.TL2[[i]] <- ggplot(plot1_cumMonth, aes(TL,Freq, group=factor(Month), fill=factor(Month))) + geom_bar(stat="identity") + 
  theme_bw(25) + scale_y_continuous(limits=c(0,max(plot1_cumMonth$Freq)+10), expand=c(0,0)) + 
  ylab("N") + xlab("Total length (cm)")+ facet_grid(rows = vars(Month)) + #facet_grid(.~Month) by column
  geom_vline(aes(xintercept=wtMean), col="red", size=1.2,lty=2) +
  theme(legend.position="none")+
  ggtitle(paste(var[i])) 

#make text smaller
plot1.TL3[[i]] <- plot1.TL2[[i]]+theme(text = element_text(size=rel(5.0)))+ scale_fill_brewer(palette="Set1")

ggsave(filename = paste(plot_dir,paste(var[i],species,"bTLbyMonth.png",sep="_"),sep="/"), 
       plot = plot1.TL3[[i]], width = 25, 
       height = 20, units = "cm", dpi = 300, type = "cairo-png") 




## 4.2 CoombeFisheries----
#some plots of coombefisheries, only Weight data

plot1 <- subset(aneproc, processor=="COOMBEFISHERIES")
str(plot1)

#some plots
ggplot(plot1,aes(x=weight_g))+geom_density(alpha=0.4)
ggplot(plot1,aes(x=weight_g,group=month,colour=as.factor(month)))+geom_density(alpha=0.4)

#more complete graphs-----
#plot1 <- (subset(plot1,weight_g>0))

combe <- ggplot(plot1,aes(x=weight_g,group=month,fill=as.factor(month)))+
  geom_density(alpha=0.3)+ theme_bw()+
  labs(title="",x="Weight (g)", y = "Density")+
  theme(legend.position=c(0.8, 0.7))+
  scale_fill_discrete(name = "Month", labels = c("Oct", "Nov", "Dec"))


#ggsave(filename = paste(plot_dir,paste(species,"Coombefisheries_WbyMonth.png",sep="_"),sep="/"), 
#       plot = combe, width = 25,height = 20, units = "cm", dpi = 300, type = "cairo-png") 

#save plots
#ggsave(file=paste0(plot_dir, "/ANE_CoombeFisheries_W.png"), ane, width=24, height=16, units="cm", dpi=200)



# ===================================================--
# 4.3 Adjust a linear LWR plots in a loop ----
# ===================================================--

#your data
db <- aneproc
head(db)

#your species
species=unique(db$species)
#your variable
var <- c(unique(db$processor))
#variable you want to plot
db$sel <- db$processor

str(db)
db$length_cm <- as.numeric(db$length_cm)

#loop (check names are correct in line 27 or change them)
#only for Interfish

SP_LWrel <- list()

#for(i in 1:length(var)){
i=2  
plot1=db[db$sel==var[i],]

plot1 <- db[,c("date","vessel","total_catch_kg","length_cm","weight_g","samplewt_g","sampleID","month","year","source","processor","species","sel")]

names(plot1) <- c("date","vessel","totalcatch_kg","length_cm","weight_g","samplewt_g","sampleID","month","year","source","processor","species","sel")

#Fit a Linear regression (cm/g) and log transform--
plot1$logL <- log(plot1$length_cm)
plot1$logW <- log(plot1$weight_g)

fit <- lm(logW~logL,data=plot1)
#fitPlot(fit,xlab="log Total Length (cm)",ylab="log Weight (g)",main="plot1")
summary(fit)
par(mfrow=c(2,2));plot(fit)# LWR normally do not follow a regression pattern

a= exp(coefficients(fit)[1])
b= coefficients(fit)[2] #(intercept)

#fit the linear regression
ggplotRegression <- function (fit) {  
  require(ggplot2)
  ggplot(fit$model, aes_string(x = names(fit$model)[2], y = names(fit$model)[1])) + 
    geom_point() +
    stat_smooth(method = "lm", col = "red")+
    ggtitle(paste(var[i],"\n","Adj R2 = ",signif(summary(fit)$adj.r.squared,2),
                  "; a =",round(exp(signif(fit$coef[[1]])),5),
                  "; b =",signif(fit$coef[[2]],3),
                  "; p-value =",signif(summary(fit)$coef[2,4],5)))+
    theme_bw()
}

SP_LWrel[[i]] <- ggplotRegression(lm(logW~logL,data=plot1))

ggsave(filename = paste(plot_dir,paste(var[i],species,"LWrel.png",sep="_"),sep="/"), 
       plot = SP_LWrel[[i]], width = 25, 
       height = 20, units = "cm", dpi = 300, type = "cairo-png") 



print(SP_LWrel[[2]])



################################## END ############################################################################----