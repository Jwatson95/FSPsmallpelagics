#Databases for PIL FSP
#@silvia Rodriguez Climent
# 12-03-2021; last updated 09/09/2022
#---------------------------------------------------------------------## 

#setwd <-"C:/Users/SRC01/OneDrive - CEFAS/SC/Rscripts/FSP2122/"
setwd("C:/Users/JW30/OneDrive - CEFAS/Documents/MD007B_2122/Working_area")
# set input, output directories
inp_dir <- file.path(getwd(), "Data/Processors/PIL/")
plot_dir <- file.path(getwd(), "Data/plots/PIL/")
out_dir <- file.path(getwd(), "Data/Output/")

#libraries
library(ggplot2);library(lubridate)

#read files
all3 <- read.csv(paste(out_dir,"PIL_processors_2022.csv",sep="/"),sep=",",header=T, stringsAsFactors = F)

###########################################################################################################################--
#1 Database NON-AGGREGATED per Length category (processors only)----

head(all3) #this is your non-aggregated database

table(all3$month,all3$year)
all3$Division <- "27.7e" # i do not have it for falfish, but for Interfish was always sub-division 7e
all3$fishing_season <- "2021-2022"

all3 <- all3[,c("date","vessel","totalcatch_kg","length_cm","weight_g","samplewt_g","sampleID","month","year","source","processor","fishing_season","species","Division")]
#write.csv(all3,file=paste("C:/Users/SRC01/OneDrive - CEFAS/SC/Rscripts/FSP_database/PIL/Output","/PIL_nonagg2122.csv",sep=""),row.names=F)
#write.csv(all3,paste(out_dir,"PIL_nonagg2122.csv",sep="/"),row.names = F)

###########################################################################################################################--

#2 Database AGGREGATED per Length category (need to add the fishers data)----

## 2.1 processors data----
head(all3)
summary(all3);dim(all3)
all3[is.na(all3)] <- 0 

pilproc_aggrL<- with(all3,aggregate(weight_g,list(length_cm=length_cm,date=date,vessel=vessel,species=species,month=month,samplewt_g=samplewt_g, processor=processor,sampleID=sampleID ,totalcatch_kg=totalcatch_kg),sum))
pilproc_aggrL$division <- "27.7e"
head(pilproc_aggrL)
dim(pilproc_aggrL)

pilproc_aggrL <- pilproc_aggrL[,c("species","date","division","length_cm","x","samplewt_g","totalcatch_kg","processor","vessel","sampleID")]

pilproc_aggrL$source <- "processor"
names(pilproc_aggrL) <- c("species","date","division","length_cm","N","samplewt_g","totalcatch_kg","processor","vessel","sampleID","source")


###########################################################################################################################--
## 2.2 fishers data----
list.files(out_dir)

#read latest file logbook
lbfishers <- read.csv(paste(out_dir,"/PIL_LBfishers_2122(2).csv",sep=""),sep=",",header=T,stringsAsFactors=F)# vs(3) has an error corrected
dim(lbfishers)

#read latest file length log
lenfishers <- read.csv(paste(out_dir,"/PIL_TLfishers_2122.csv",sep=""),sep=",",header=T,stringsAsFactors=F)
dim(lenfishers)

head(lbfishers)
head(lenfishers)

length(unique(lbfishers$date))
table(unique(lbfishers$date))

length(unique(lenfishers$date))
table(unique(lenfishers$date))


#Add estimated sampled weight-----
#LWR fitted in PIL_processors2122, we will use Falfish values to fit them all (best fit from the 2 processors)
#a=0.03514
#b=2.65

head(lenfishers)

lenfishers$a <- 0.03514
lenfishers$b <- 2.65

lenfishers$w <- lenfishers$a*((lenfishers$TL)^lenfishers$b)
lenfishers$estimatedsamplew <- round(lenfishers$w*lenfishers$N,0)

lenfishers[is.na(lenfishers)] <- 0
#subset(lenfishers,date=="2019-07-31")
samplewt <- with(lenfishers,aggregate(estimatedsamplew,list(date=date),sum))

head(lenfishers)

#this only includes the length data collected by the fishers
fishers2 <- merge(lenfishers,samplewt)
summary(fishers2)
table(fishers2$x)
fishers2$date <- as.Date(fishers2$date)# we loose here dates while doing the merge----
subset(fishers2,date=="2021-09-22")


plot(fishers2$w~fishers2$TL) #check the estimated weight is correct
str(fishers2)

#two step transformation to avoid Na by coercion
fishers2$haul_weight_t <- as.factor(as.character(fishers2$haul_weight_t))
fishers2$haul_weight_t <- as.numeric(as.factor(fishers2$haul_weight_t))

fishers2$haul_weight_kg <- fishers2$haul_weight_t*1000
fishers2<- fishers2[,c("date","SampleID","TL","N","x","haul_weight_kg","vessel")]

fishers2$species <- "PIL"
fishers2$division <- "27.7e"
fishers2$processor <- ""
fishers2$source <- "fishers"

fishers2<- fishers2[,c("species","date","SampleID","division","TL","N","x","haul_weight_kg","processor","vessel","source")]
names(fishers2) <- c("species","date","sampleID","division","length_cm","N","samplewt_g","totalcatch_kg","processor","vessel","source")
fishers2$vessel <- toupper(fishers2$vessel)
head(fishers2)

###########################################################################################################################--
## 2.3 processors + fishers ----
head(pilproc_aggrL);dim(pilproc_aggrL)
head(fishers2);dim(fishers2)

pilproc_aggrL <- pilproc_aggrL[,c("species","date","sampleID","division","length_cm","N","samplewt_g","totalcatch_kg","processor","vessel","source")]


#change date to the correct format
str(pilproc_aggrL)
str(fishers2)

pilproc_aggrL$date <- as.Date(pilproc_aggrL$date, format="%Y-%m-%d")
fishers2$date <- as.Date(fishers2$date, format="%Y-%m-%d")

agg2021 <- rbind(fishers2,pilproc_aggrL)
dim(agg2021) #1293 11
table(agg2021$vessel)
agg2021$vessel[agg2021$vessel=="GALWADY MOR"] <- "GALWAD-Y-MOR"

#save file
#write.csv(agg2021,paste(out_dir, "/PIL_selfsampling_2122_WGHANSA.csv",sep=""),quote=F, row.names = F)

#format it for the database
head(agg2021)
agg2021 <- agg2021[,c("date","vessel","totalcatch_kg","length_cm","N","samplewt_g","sampleID","source","processor","division")]

agg2021$month <- month(agg2021$date)
agg2021$year <- year(agg2021$date)
agg2021$fishing_season <- "2021-2022"

agg2021 <- agg2021[,c("date","vessel","totalcatch_kg","length_cm","N","samplewt_g","sampleID","month","year","source","fishing_season","processor","division")]


#write.csv(agg2021,file=paste("C:/Users/SRC01/OneDrive - CEFAS/SC/Rscripts/FSP_database/PIL/Output","/PIL_agg2122.csv",sep=""),row.names=F)
#write.csv(agg2021,paste(out_dir,"PIL_agg2021.csv",sep="/"),row.names = F)

########################################### END ##############################################################################-----
