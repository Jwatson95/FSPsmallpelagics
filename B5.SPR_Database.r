#Databases for SPR FSP
#@silvia Rodriguez Climent
# 06-04-2021; last modified: 09-09-2022
#---------------------------------------------------------------------## 

# set input, output directories
inp_dir <- file.path(getwd(), "Data/Processors/SPR/")
plot_dir <- file.path(getwd(), "Data/plots/SPR/")
out_dir <- file.path(getwd(), "Data/Output/final/")

#libraries
library(ggplot2);library(lubridate)

#read files
proc <- read.csv(paste(out_dir,"SPR_processors_2021.csv",sep="/"),sep=",",header=T, stringsAsFactors = F) #missing file 

###########################################################################################################################--
# ===================================================--
# 1. DATBASE NON-AGGREGATED PER LENGTH CATEGORY ----
# ===================================================--

head(proc) #this is your non-aggregated database

table(proc$month,proc$year)
proc$division <- "27.7e"
proc$fishing_season <- "2020-2021"

proc <- proc[,c("date","vessel","totalcatch_kg","length_cm","weight_g","samplewt_g","sampleID","month","year","source","processor","fishing_season","species","division")]

#write.csv(proc,paste(out_dir,"SPR_nonagg2021.csv",sep="/"),row.names = F)


###########################################################################################################################--
# ===================================================--
# 2. DATBASE AGGREGATED PER LENGTH CATEGORY ----
# ===================================================--

head(proc)
dim(proc)
table(proc$vessel)
summary(proc)# need to transform NA in zeros, otherwise i lose the ones that don't have totalcatch info
str(proc)

proc[is.na(proc)] <- 0 

table(proc$totalcatch_kg)
summary(proc$totalcatch_kg)


proc_aggrL<- with(proc,aggregate(weight_g,list(length_cm=length_cm,date=date,vessel=vessel,species=species,month=month,samplewt_g=samplewt_g, 
                                               processor=processor,totalcatch_kg=totalcatch_kg,sampleID=sampleID,year=year),sum))
proc_aggrL$division <- "27.7e"
head(proc_aggrL)

names(proc_aggrL) <- c("length_cm","date","vessel","species","month","samplewt_g","processor","totalcatch_kg","sampleID","year","n","division")
proc_aggrL$source <- "processor"
proc_aggrL <- proc_aggrL[,c("date","vessel","totalcatch_kg","length_cm","n","samplewt_g","sampleID","month","year","source","species","division")]

# write csv
#write.csv(proc_aggrL,paste(out_dir, "/SPR_proc_2021_aggLNOFISHERS.csv",sep=""),quote=F, row.names = F)

########################################### END ##############################################################################-----
