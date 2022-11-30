#Extracting data for PIL from fishers FSP
#@silvia Rodriguez Climent
# 09-11-2020; last modified 09/09/2022
#---------------------------------------------------------------------##
rm(list=ls())

#.rs.restartR();#xlcFreeMemory();#gc() #see memmory used
#options(java.parameters = "-Xmx1000m")  ## memory set to 2 GB
install.packages("rlang")

packages <- c("XLConnect", "rJava","ggplot2","data.table","xlsx","openxlsx","dplyr","readxl","stringr","plyr",
              "tidyr","reshape2", "maps","mapdata","mapproj","mapplots","lubridate","rgdal","raster","ggspatial")


lib(packages)


# library(XLConnect);library(rJava);library(ggplot2);library(data.table);library(xlsx);library(openxlsx);
# library(dplyr);library(readxl);library(stringr);library(plyr);library(tidyr);library(reshape2);library(maps);
# library(mapdata);library(mapproj);library(mapplots);library(lubridate);library(rgdal);library(raster);library(ggspatial)


#######################################################--
# *****************************************************--
# 0. Set up directories ----
# *****************************************************--
#######################################################--

#inp_dir <- file.path(getwd(), "Data/Fishers/")
#try with the 2021 data for the moment

#inp_dir <- "C:/Users/SRC01/OneDrive - CEFAS/SC/Rscripts/FSP2122/Data/Fishers/"  ##unsure whar=t this is?
inp_dir <- "C:/Users/JW30/OneDrive - CEFAS/Documents/FSP_MF083_2021/Working_area/Data/Fishers" 

out_dir <- file.path(getwd(), "Data/Output/")
list.files(inp_dir)

#enter species------
species <- "PIL"

#######################################################--
# *****************************************************--
# 2. Read Logbooks (LB)----
# *****************************************************--
#######################################################--

# List of files
list.files(inp_dir)
#temp <- unlist(getSheetNames(paste(inp_dir, "FSP_Database_Fishers2021_vs5.xlsx", sep="/"))) #does not work
#getSheetNames("C:/Users/SRC01/OneDrive - CEFAS/SC/Rscripts/FSP2021/Data/Fishers/FSP_Database_Fishers2021_vs5.xlsx")
temp <- unlist(getSheetNames(paste(inp_dir, "FSP_Database_Fishers2122.xlsx", sep="")))

sheetLOG <- temp[grepl("^LB", temp)]
# Before reading them in, make sure shooting and hauling time are in time format in excel and that there are no comments in those columns.
# create list with all my Logbooks
LOG <- list()
for(i in sheetLOG) LOG[[i]] <- data.table(xlsx::read.xlsx(paste(inp_dir, "FSP_Database_Fishers2122.xlsx", sep="/"), header=TRUE, sheetName = i))
LOG <- LOG[sapply(LOG, function(x) dim(x)[1]) > 0]

# add column with ID
LOGb <- mapply(cbind, LOG, "SampleID"=names(LOG), SIMPLIFY=F)
summary(LOGb)

str(LOGb)
LOGb$LB_PELAGIC_MARKSMAN$Shoot.time<- as.numeric(LOGb$LB_PELAGIC_MARKSMAN$Shoot.time) #need to be able to join them
LOGb$LB_CHARLOTTE_CLARE$Shoot.time<- as.numeric(LOGb$LB_CHARLOTTE_CLARE$Shoot.time) #need to be able to join them

# from list to DF
LOGc <- do.call(rbind.data.frame,LOGb)
summary(LOGc)
dim(LOGc[is.na(LOGc$Latitude),])

# convert date
LOGc[,Date:=as.Date(Date, format="%Y-%m-%d")]
# add month
LOGc[,month:=month(Date)]
LOGc[,year:=year(Date)]


#add fishing season
for (i in 1:nrow(LOGc)){
  {if (LOGc$month[i]%in%c("1","2","3")&LOGc$year[i]=="2021")
    LOGc$fishingseason[i] <-"2020-2021"}
  {if (LOGc$month[i]%in%c("7","8","9","10","11","12")&LOGc$year[i]=="2021")
    LOGc$fishingseason[i] <-"2021-2022"}
  {if (LOGc$month[i]%in%c("1","2")&LOGc$year[i]=="2022")
    LOGc$fishingseason[i] <-"2021-2022"}
}

table(LOGc$month,LOGc$year,LOGc$fishingseason)

#-------------------------------------------#
# Convert lat long into decimal degrees
# For lat long columns: makes sure that there are Degrees, minutes and seconds (if seconds not there just add 00) separated by a space
# If missing lat long, either leave blank space or put NA
# If necessary, add space between degree minutes seconds

head(LOGc);str(LOGc)
table(LOGc$Latitude);table(LOGc$Longitue)

LOGc$LAT1 <- sub("\\s+$", "", gsub('(.{2})', '\\1 ',LOGc$Latitude))
table(LOGc$LAT1)

LOGc$LON1 <- sub("\\s+$", "", gsub('(.{2})', '\\1 ',paste0("0",LOGc$Longitue,sep="")))
table(LOGc$LON1)

##Decimal Degrees = degrees + (min/60) + (sec/3600)
LOGc$lat <-(as.numeric(substr(LOGc$LAT1,1,2))+((as.numeric(substr(LOGc$LAT1,4,5))/60)+(as.numeric(substr(LOGc$LAT1,7,8))/3600)))
LOGc$lon <-(as.numeric(substr(LOGc$LON1,1,2))+((as.numeric(substr(LOGc$LON1,4,5))/60)+(as.numeric(substr(LOGc$LON1,7,8))/3600)))
LOGc$lon <- LOGc$lon*-1
table(LOGc$lat)
table(LOGc$lon)

plot(lat~lon,LOGc)
library(EchoR)
coast()


#link with the code that you already had
df <- LOGc

#correct format
head(df);str(df)
#df2$date<-strftime(strptime(df2$date,"%Y-%m-%d"),"%d/%m/%Y")  #changing from one format to another one
#df$prova2 <- openxlsx::convertToDateTime(as.vector(df[,"Shoot.time"]))
table(df$Shoot.time)
str(df$Shoot.time)

df$shoot <- as.POSIXct(x=as.numeric(df$Shoot.time),format="%H:%M",tz="UTC",origin=lubridate::origin)
df$shoot <- substr(df$shoot,13,20)
head(df)
table(df$shoot)


#Map lat and lon correct------

gClip <- function(shp, bb){
  if(class(bb) == "matrix") b_poly <- as(extent(as.vector(t(bb))), "SpatialPolygons")
  else b_poly <- as(extent(bb), "SpatialPolygons")
  proj4string(b_poly) <- proj4string(shp)
  gIntersection(shp, b_poly, byid = TRUE)
}

# Load europe coastline
wmap <- raster::shapefile("C:/Users/SRC01/OneDrive - CEFAS/01. PELTIC/Maps Peltic/Europe//EuropeESRI_high.shp")
#mybox <- matrix(c(-6,50,0,51.5), nrow = 2, ncol = 2, dimnames = list(c("x","y"), c("min","max")))#whole cornish peninsula
mybox <- matrix(c(-6,49.8,-1,51.5), nrow = 2, ncol = 2, dimnames = list(c("x","y"), c("min","max")))#little zoom cornish peninsula
#mybox <- matrix(c(-4.0,49.75,-2.0,51.0), nrow = 2, ncol = 2, dimnames = list(c("x","y"), c("min","max"))) #Lyme bay

# clip spatialLines
ec_map <- gClip(wmap, mybox)
ec <- df_spatial(ec_map)

ggplot()+
  geom_polygon(data=ec_map,aes(x=long,y=lat,group=group),col="black")+
  geom_point(data=df,aes(x=lon,y=lat),col="red",size=2)+
  theme_bw(20)+ ylab("Latitude")+xlab("Longitude")

###################################---

#select wanted columns and change the names
df2 <- df[,c("Vessel.Name","Vessel.registration","Skipper","Date","lat","lon","Depth.of.fish..m.",
             "Sardines..kg._retained","Anchovy..kg._retained","Sprats..kg._retained","Herring...kg._retained",
             "Mackerel..kg._retained","Scad..kg._retained","Bycatch..kg.","Slipped.fish..kg.","Dead.fish..kg.",
             "Time.at.sea..total.h.","birds_bycatch","seals_bycatch","dolphins_bycatch","tuna_bycatch","shoot",
             "month","year","fishingseason")]
names(df2)
colnames(df2) <- c("vessel","reg","skipper","date","lat","lon","depth","pil","ane","spr","her","mac","hom",
                   "bycatch","slipped","dead","hsea","birdsbc","sealsbc","dolphinsbc","tunabc","shoot","month",
                   "year","fishingseason")#scad assumed to be HOM

head(df2);dim(df2) #383 25
summary(df2)
df2[is.na(df2)] <- 0

#remove NA's
row.has.na <- apply(df2, 1, function(x){any(is.na(x))})
sum(row.has.na) #totes les rows tenen NA per tant no les puc eliminar
#final.filtered <- df2[!row.has.na,]
summary(final.filtered)

#save it
#write.csv(df2,file=paste(out_dir,"/",species,"_LBfishers_2021.csv",sep=""),row.names=F)

#select your season
#season 2021-2022--
df3 <- subset(df2,fishingseason=="2021-2022")
table(df3$month,df3$year)
dim(df3) #328 25
summary(df3)
df3[is.na(df3)] <- 0
#saveit
#write.csv(df3,file=paste(out_dir,"/",species,"_LBfishers_2122.csv",sep=""),row.names = F)


#######################################################--
# *****************************************************--
#2.1 Read electronic Logbooks (from Clean Catch app)----
# *****************************************************--
#######################################################--

app <- read.csv(paste("C:/Users/SRC01/OneDrive - CEFAS/SC/Rscripts/FSP2122/Data/Fishers/","CEFAS Production CatchReport_Combined_corr.csv",sep=""))

#select only the fishers from group 2
#Fisher.ID FM0000007 = VESTA  // Fisher.ID FM0000008 = LYONESSE // Fisher.ID FM0000009 = GIRLRONA (SPRAT)
app <- subset(app,Fisher.ID%in%c("FM000008","FM000009"))
dim(app)

app <- app[,c("group","category","Fisher.ID","Shoot.Start","Shoot.Start.Location","Shot.Depth..m.","Comments","Main.Species.Index",
              "Main.Species","Main.Catch.Weight..kg.","Record.Bycatch..Y.N.","Fisher.Sampling..Y.N.","Bycatch.Index","Bycatch.Species",
              "Individual...Group","Sex","Alive.Count","Dead.Count","Retained.Count")]

names(app)
table(app$Fisher.ID)
head(app)

#add vessel names
app$vessel[app$Fisher.ID=="FM000008"] <- "LYONESSE"
app$vessel[app$Fisher.ID=="FM000009"] <- "GIRLRONA"
table(app$vessel)

#add skipper
app$skipper[app$Fisher.ID=="FM000008"] <- "Will Treener"
app$skipper[app$Fisher.ID=="FM000009"] <- "Will Burton"
table(app$skipper)

#add registration
app$reg[app$Fisher.ID=="FM000008"] <- "PZ81"
app$reg[app$Fisher.ID=="FM000009"] <- "TH117"
table(app$reg)

#separate date and shoot time
app$date <- as.Date(substr(app$Shoot.Start,1,10))
app$shoot <- substr(app$Shoot.Start,12,19)
head(app)

#lat and long
app$lat <- substr(app$Shoot.Start.Location,1,7)
app$lon <- substr(app$Shoot.Start.Location,9,14)

app$LAT1 <- sub("[.]", "",gsub(' ','\\1 ',app$lat)) #eliminate the dot
app$LAT1 <- sub("[,]","",gsub('(.{2})',"\\1",app$LAT1)) #eliminate the comma
table(app$LAT1)

table(app$lon)
app$LON1 <- sub("[.]", "", gsub(' ', '\\1 ',paste0("0",app$lon)))
#app$LON1 <- sub("[,]", "", gsub(' ', '\\1 ',paste0("0",app$lon)))
table(app$LON1)

##Decimal Degrees = degrees + (min/60) + (sec/3600)
app$lat2 <-(as.numeric(substr(app$LAT1,1,2))+((as.numeric(substr(app$LAT1,3,4))/60)+(as.numeric(substr(app$LAT1,5,6))/3600)))
app$lon2 <-(as.numeric(substr(app$LON1,1,2))+((as.numeric(substr(app$LON1,3,4))/60)+(as.numeric(substr(app$LON1,5,6))/3600)))
app$lon2 <- app$lon2*-1
table(app$lat2)
table(app$lon2)

plot(lat2~lon2,app)
library(EchoR)
coast()


#select appropriate columns
app <- app[,c("vessel","reg","skipper","date","lat2","lon2","Shot.Depth..m.","shoot","Main.Species","Main.Catch.Weight..kg.",
              "Bycatch.Species", "Alive.Count","Dead.Count","Retained.Count")]

colnames(app) <- c("vessel","reg","skipper","date","lat","lon","depth","shoot","species","weightcatch_kg","bycatch","alive_count","dead_count","retained_count")

app$bycatch[app$bycatch=="anchovie"] <- "anchovy"
app$bycatch[app$bycatch=="john Dory"] <- "john dory"
app$bycatch[app$bycatch=="mackerel "] <- "mackerel"
app$bycatch[app$bycatch=="scad "] <- "scad"

table(app$bycatch)


#select sardine and save sprat for later
table(app$species)
appsprat <- subset(app,species=="Sprat")
#save it
#write.csv(appsprat,paste(out_dir,"SPR_LBelectronic.csv",sep="/"),row.names=F)

apppil <- subset(app,species=="Sardine")
head(apppil);dim(apppil) #144 14
apppil <- apppil[,-which(names(apppil)%in%c("alive_count","dead_count","species"))]
colnames(apppil)[9] <- "pil"
#write.csv(apppil,paste(out_dir,"PIL_LBelectronic(2).csv",sep="/"),row.names=F)


#not working, leave it for now-----
# #dcast(apppil,retained_count~bycatch) # is this but the values should be the retained_count value
# 
# dim(apppil) #144
# apppil[is.na(apppil)] <- 0
# summary(apppil)
# View(apppil)
# 
# sol <- dcast(apppil,date+vessel+shoot+depth+retained_count~bycatch)
# dim(sol) #141, still i miss 3 values on the way...
# #dcast(apppil,date~bycatch,value.var = "retained_count") # it does not work because there is more than 1 value per each
# sol <- sol[,c("date","retained_count","anchovy","bass", "Gull (unspecified)", "Herring gull", "john dory","mackerel", "scad","smooth hound", "Sun fish " )]
# dim(sol)
# View(sol)
# 
# #working
# for(j in 1:length(1:ncol(sol))){
#   if(j>=3){
#     for(i in 1:length(1:nrow(sol))){
#       #print(paste("i = " , i, "; j = ", j))
#       if (sol[i,j]>0){
#         sol[i,j]<- sol$retained_count[i]
#       }
#     }
#   }
# }
# 
# sol <- sol[,c("date","anchovy", "bass","Gull (unspecified)","Herring gull", "john dory", "mackerel", "scad", "smooth hound", "Sun fish ")]
# dim(sol)
# dim(apppil)
# 
# apppil2 <- apppil[,c("vessel","reg","skipper","date","lat","lon","depth","shoot","pil")]
# head(apppil2);dim(apppil2)
# 
# apppil3 <- cbind(apppil2,sol,by="date") # hey do not have the same dimensions
# dim(apppil3)


#2.2 Merge it with your manual-entered logbooks (and exclude bycatch for now as it's not working the merge)----
head(df3); dim(df3)
apppil2 <- apppil
head(apppil2);dim(apppil2)

apppil2 <- apppil[,c("vessel","reg","skipper","date","lat","lon","depth","shoot","pil")]

apppil2$ane <- 0
apppil2$spr <- 0
apppil2$her <- 0
apppil2$mac <- 0
apppil2$hom <- 0
apppil2$bycatch <- 0
apppil2$slipped <- 0
apppil2$dead <- 0
apppil2$hsea <- 0
apppil2$birdsbc <- 0
apppil2$sealsbc <- 0
apppil2$dolphinsbc <- 0
apppil2$tunabc <- 0
apppil2$month <- month(apppil2$date)
apppil2$year <- year(apppil2$date)
apppil2$fishingseason <- "2021-2022"

apppil2 <- apppil2[,c("vessel","reg","skipper", "date","lat" ,"lon" ,"depth","pil" ,"ane", "spr" ,"her", "mac", "hom",
                      "bycatch", "slipped", "dead", "hsea", "birdsbc", "sealsbc", "dolphinsbc","tunabc","shoot", "month", "year", "fishingseason")]

df4 <- rbind(df3,apppil2)

dim(df4)
df4[is.na(df4)] <- 0

#saveit
#write.csv(df4,file=paste(out_dir,"/",species,"_LBfishers_2122(3).csv",sep=""),row.names = F)


#######################################################--
# *****************************************************--
# 3. Read Lengths (TL) ----
# *****************************************************--
#######################################################--

list.files(inp_dir)
temp <- unlist(getSheetNames(paste(inp_dir, "FSP_Database_Fishers2122.xlsx", sep="/"))) #trying with .xls version (less memory used), getSheetNames does not work with .xls

sheetTL <- temp[grepl("^TL", temp)]

TL <- list()
for(i in sheetTL) TL[[i]] <- data.table(xlsx::read.xlsx(paste(inp_dir,"FSP_Database_Fishers2122.xlsx", sep="/"),header=TRUE,sheetName = i,stringsAsFactors=F))

# add column with ID
TLb <- mapply(cbind,TL, "SampleID"=names(TL), SIMPLIFY=F)
View(TLb)

#remove the extra column from GALWAD Y MOR
#TLb$TL_GALWADYMOR<- as.data.frame(TLb$TL_GALWADYMOR)
#str(TLb$TL_GALWADYMOR)

#TLb$TL_GALWADYMOR <- TLb$TL_GALWADYMOR[,!(names(TLb$TL_GALWADYMOR)%in%c("SAMPLE.WEIGHT"))]
#TLb$TL_GALWADYMOR<- data.table(TLb$TL_GALWADYMOR)
str(TLb)


# from list to DF
TLc <- do.call(rbind.data.frame,TLb)

TLc$Date <- as.Date(TLc$Date,format="%Y-%m-%d")
TLc$month <- month(TLc$Date)
TLc$year <- year(TLc$Date)

colnames(TLc) <- c("vessel","reg","measurer","date","haul","haul_weight_t","sp","TL","N","comm","SampleID","month","year")

table(TLc$month)
table(TLc$year)

table(TLc$month,TLc$year)

#season 2021-22--
table(TLc$month,TLc$year)

#TL19 <- subset(TLc,month%in%c("1","2","3")& year=="2020")
#table(TL19$month,TL19$year)

#TL20 <- subset(TLc,month%in%c("7","8","9","10","11","12")& year=="2020")
#table(TL20$month,TL20$year)

#dim(TL19) #107 13
#dim(TL20) #1761 13

TotL <- TLc
summary(TotL)
TotL[is.na(TotL)] <- 0

# sum up lengths
TLc1 <- TotL[!is.na(N),sum(N), by=.(TL)]
TLc2 <- TotL[!is.na(N),sum(N), by=.(TL,month,vessel)]

# calculate median and weighted average
stat2 <- TLc1 %>%
  dplyr::summarize(median=median(TL), weighted.mean(TL, V1))

stat1 <- TLc2 %>%
  group_by(month,vessel) %>%
  dplyr::summarize(median=median(TL), weighted.mean(TL, V1))


#saveit
#write.csv(TotL,file=paste(out_dir,"/",species,"_TLfishers_2122.csv",sep=""),row.names = F)
###########################################################################################################################--

#next script: A4.PIL_Plots_Fishers #-----

########################################### END ##############################################################################-----
