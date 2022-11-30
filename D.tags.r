#Extracting data from Temperature tags-FSP
#@silvia Rodriguez Climent
# 09-02-2021; last modified 09-09-2022
#---------------------------------------------------------------------##

setwd("C:/Users/SRC01/OneDrive - CEFAS/SC/Rscripts/FSP2122/Data/")

# ===================================================--
# 0. Set directories----
# ===================================================--
rm(list=ls())
# set input, output directories
inp_dir <- file.path(getwd(), "tags/") #dont have these files 
plot_dir <- file.path(getwd(), "plots/tags/")
out_dir <- file.path(getwd(), "Output/")
list.files(inp_dir,recursive=TRUE) # recursive TRUE to see inside the different folders

# load libraries
sessionInfo()
library(data.table);library(ggplot2);library(doBy):library(dplyr):library(cowplot);library(rlang)

# ===================================================--
# 1. READ DATA ----
# ===================================================--
##tag1 -----
tag1 <- read.csv(paste(inp_dir,"A17984_03-03-2022 GIRL RONA.csv",sep="/"),sep=",",header=F,skip=52) #remove all the details from the begging

sums<- tag1[1:101,]
head(sums)
names(sums) <- c("missionday","date","max_temp","min_temp","max_depth","depth","bat_v")
sums$max_temp <- as.numeric(sums$max_temp)

str(sums)
sums$date <- as.Date(sums$date,"%d/%m/%Y")
plot(sums$max_depth~sums$min_temp) # hauls

#simple graph of the summary data
plot(sums$date,sums$max_temp,type="l",pch=21,col="red",xlab="time",
     ylab="temperature,°C",lty=1,lwd=2,ylim = c(-1,25),main="GirlRona")
lines(sums$date,sums$min_temp, pch = 18, col = "blue", type = "l", lty = 2,lwd=2)
lines(sums$date,sums$max_depth, pch = 18, col = "grey", type = "l", lty = 3,lwd=1)
#lines(sums$date,sums$depth, pch = 18, col = "orange", type = "l", lty = 4,lwd=1)
legend("topright", legend=c("max.T", "min.T","max.depth"),
       col=c("red","blue","grey"), lty = 1:3, cex=0.8)

#dev.copy(png, paste(plot_dir,'/GirlRona_summary_tag.png',sep=""),width=25,height=17,unit='cm',res=300)
#dev.off()


#all the data
nrow(tag1)

tag1 <- tag1[131:604930,]
head(tag1)
colnames(tag1) <-c("date","pressure","temp")
tag1$sec <- rep(seq(0,5,1),nrow(tag1)/6)# 10 second resolution
tag1$date2 <- paste0(tag1$date,":",tag1$sec,"0")
tag1 <- tag1[,c("date","pressure","temp","DT")]
#tag1[is.na(tag1)] <- 0
tag1$pressure <- as.numeric(as.character(tag1$pressure))
tag1$temp <- as.numeric(as.character(tag1$temp))
tag1$date2 <- as.POSIXct(tag1$date2,format="%d/%m/%Y %H:%M:%S") # to second level
tag1$date <- as.Date(substring(tag1$date2,1,10),format="%Y-%m-%d") # to date level
str(tag1)
summary(tag1)

tag1.2<- summaryBy(temp + pressure ~ date + vs, data = tag1,
                   FUN = function(x) { c(m = mean(x), s = sd(x),min=min(x),max=max(x)) } )
tag1.2$ID <- "GIRLRONA"
#write.csv(tag1.2, file=paste(out_dir,"/tag1.csv",sep="/"),row.names = F)

#more complicated graph
tempcol <- "#69b3a2"
presscol <- "blue"
data <- tag1.2

a <- ggplot(data,aes(x=date,y=temp.m)) +         
  geom_line(aes(x=date,y=temp.m),color=tempcol,size=1,alpha=1)+
  geom_ribbon(aes(x=date,ymin=temp.min,ymax=temp.max,fill=tempcol),alpha=0.1)+
  geom_line(aes(x=date,y=pressure.m),color=presscol,size=1,alpha=1)+
  geom_ribbon(aes(x=date,ymin=pressure.min,ymax=pressure.max,fill=presscol),alpha=0.1)+
  theme_bw()+ theme(legend.position="bottom") + ylab("temperature (°C)") + xlab("time")+
  scale_fill_manual("",values=c(tempcol,presscol),labels=c("temperature","pressure"))+
  ggtitle(paste(unique(data$ID),sep="_"))
a

#ggsave(filename = paste(plot_dir,paste(unique(tag1.2$ID),"tag.png",sep="_"),sep="/"), 
#       plot = a, width = 25,height = 20, units = "cm", dpi = 300, type = "cairo-png") 

Accl <- 12 # 2 min delay in start 
Dep_Thresh <- 2
tagsum <- data.frame(matrix(ncol=ncol(tag1),nrow=0)) 
colnames(tagsum) <- colnames(tag1)
for(date in unique(as.character(tag1$date))){ # loop to create daily summary figure 
  #date <- unique(as.character(tag1$date))[5]#5
  
  if(max(tag1$pressure[which(tag1$date == date)]) >Dep_Thresh){
    St <- min(which(tag1$pressure[which(tag1$date == date)] > Dep_Thresh))+Accl # extract start of haul with accl
    En <- max(which(tag1$pressure[which(tag1$date == date)] > Dep_Thresh)) # extract end of haul
    NR <- tag1[which(tag1$date == date)[St:En],] # new row
    colnames(NR) <- colnames(tagsum)
    tagsum <- data.frame(rbind(tagsum,NR))
    
    g1 <- ggplot(tag1[which(tag1$date == date),],aes(x=date2,y=temp)) +         
      geom_line(aes(x=date2,y=temp),color=tempcol,size=1,alpha=1)+
      theme_bw()+ theme(legend.position="bottom") + ylab("temperature (°C)") + xlab("time")+
      geom_ribbon(data=NR,aes(xmin=min(date2),xmax=max(date2),y=temp),alpha=0.4)+
      ggtitle(paste0(unique(data$ID),"_",date)); g1
    g2 <- ggplot(tag1[which(tag1$date == date),],aes(x=date2,y=temp)) +         
      geom_line(aes(x=date2,y=-pressure),color=presscol,size=1,alpha=1)+
      geom_ribbon(data=NR,aes(xmin=min(date2),xmax=max(date2),y=-pressure),alpha=0.4)+
      theme_bw()+ theme(legend.position="bottom") + ylab("Depth (m)") + xlab("time")+
      ggtitle(paste0(unique(data$ID),"_",date)); g2
    plot_grid(g1,g2,ncol=1,align="v")
    ggsave(paste0(out_dir,"/",paste0(unique(data$ID),"_",date,".png")),height=7,width=8)
  } else {
    g1 <- ggplot(tag1[which(tag1$date == date),],aes(x=date2,y=temp)) +         
      geom_line(aes(x=date2,y=temp),color=tempcol,size=1,alpha=1)+
      theme_bw()+ theme(legend.position="bottom") + ylab("temperature (°C)") + xlab("time")+
      ggtitle(paste0(unique(data$ID),"_",date)); g1
    g2 <- ggplot(tag1[which(tag1$date == date),],aes(x=date2,y=temp)) +         
      geom_line(aes(x=date2,y=-pressure),color=presscol,size=1,alpha=1)+
      theme_bw()+ theme(legend.position="bottom") + ylab("Depth (m)") + xlab("time")+
      ggtitle(paste0(unique(data$ID),"_",date)); g2
    plot_grid(g1,g2,ncol=1,align="v")
    ggsave(paste0(out_dir,"/",paste0(unique(data$ID),"_",date,".png")),height=7,width=8)    
  }
}
#write.csv(tagsum,aes(out_dir))

ggplot(data=tagsum)+
  geom_point(aes(x=pressure,y=temp))


##############################################################################################################--
##tag2----
list.files(inp_dir)

tag2 <- read.csv(paste(inp_dir,"A17983_27-02-2022.csv",sep="/"),sep=",",header=F,skip=49) #something wrong with the format, without skip i don't get the right format
head(tag2)

sums2<- tag2[1:220,]
head(sums2)
names(sums2) <- c("missionday","date","max_temp","min_temp","max_depth","depth","bat_v")

str(sums2)
sums2$date <- as.Date(sums2$date,"%d/%m/%Y")
plot(sums2$max_depth~sums2$min_temp) # hauls

#simple graph of the summary data
plot(sums2$date,sums2$max_temp,type="l",pch=21,col="red",xlab="time",
     ylab="temperature,°C",lty=1,lwd=2,ylim = c(0,35),main="Lyonesse")
lines(sums2$date,sums2$min_temp, pch = 18, col = "blue", type = "l", lty = 2,lwd=2)
lines(sums2$date,sums2$max_depth, pch = 18, col = "grey", type = "l", lty = 3,lwd=1)
#lines(sums$date,sums$depth, pch = 18, col = "orange", type = "l", lty = 4,lwd=1)
legend("topright", legend=c("max.T", "min.T","max.depth"),
       col=c("red","blue","grey"), lty = 1:3, cex=0.8)

#dev.copy(png, paste(plot_dir,'/Lyonesse_summary_tag.png',sep=""),width=25,height=17,unit='cm',res=300)
#dev.off()


#all the data
nrow(tag2)

tag2 <- tag2[227:1649751,]
head(tag2)
colnames(tag2) <-c("date","pressure","temp")
tag2 <- tag2[,c("date","pressure","temp")]
tag2 <- tag2[-1,]

tag2$pressure <- as.numeric(as.character(tag2$pressure))
tag2$temp <- as.numeric(as.character(tag2$temp))
tag2$date <- as.Date(tag2$date,"%d/%m/%Y %H:%M")
str(tag2)

summary(tag2)

tag2.2<- summaryBy(temp + pressure ~ date + vs, data = tag2,
                   FUN = function(x) { c(m = mean(x), s = sd(x),min=min(x),max=max(x)) } )

tag2.2$ID <- "LYONESSE"
#write.csv(tag2.2, file=paste(out_dir,"tag2.csv",sep="/"),row.names = F)


#more complicated graph
tempcol <- "#69b3a2"
presscol <- "blue"
data <- tag2.2

b <- ggplot(data,aes(x=date,y=temp.m)) +         
  geom_line(aes(x=date,y=temp.m),color=tempcol,size=1,alpha=1)+
  #geom_line(aes(x=date,y=temp.max),color="grey",size=1,alpha=0.6,linetype = "dotted")+
  #geom_line(aes(x=date,y=temp.min),color="grey",size=1,alpha=0.6,linetype = "dotted")+
  geom_ribbon(aes(x=date,ymin=temp.min,ymax=temp.max,fill=tempcol),alpha=0.5)+
  geom_line(aes(x=date,y=pressure.m),color=presscol,size=1,alpha=1)+
  geom_ribbon(aes(x=date,ymin=pressure.min,ymax=pressure.max,fill=presscol),alpha=0.2)+
  theme_bw()+ theme(legend.position="bottom") + ylab("temperature (°C)") + xlab("time")+
  scale_fill_manual("",values=c(tempcol,presscol),labels=c("temperature","pressure"))+
  ggtitle(paste(unique(data$ID),sep="_"))

b

#ggsave(filename = paste(plot_dir,paste(unique(tag2.2$ID),"tag.png",sep="_"),sep="/"), 
#       plot = b, width = 25,height = 20, units = "cm", dpi = 300, type = "cairo-png") 


#another plot
c <- ggplot(data = data, mapping = aes(x = date)) +
  geom_pointrange(mapping = aes(y=temp.m,ymin = temp.min, ymax = temp.max),size=0.5,color='blue',fatten = 1)+
  theme_bw()+
  geom_line(aes(x=date,y=pressure.max), inherit.aes = FALSE,color='red')+
  scale_y_continuous( name = 'temp', sec.axis = sec_axis(~.,name = "pressure"))+
  theme(axis.text.y  = element_text(color = 'blue'),
        axis.title.y = element_text(color='blue'),
        axis.text.y.right =  element_text(color = 'red'),
        axis.title.y.right = element_text(color='red'))+
  ggtitle(paste(unique(data$ID),sep="_"))

c

#ggsave(filename = paste(plot_dir,paste(unique(tag2.2$ID),"tag2.png",sep="_"),sep="/"), 
#       plot =c, width = 25,height = 20, units = "cm", dpi = 300, type = "cairo-png") 




#add a second axis for the pressure
f.1 <- ggplot(data,aes(x=date,y=temp.m))+ #+ facet_grid(ID ~ .) +   
  geom_line(aes(x=date,y=temp.m),color=tempcol,size=1,alpha=1)+
  geom_ribbon(aes(x=date,ymin=temp.min,ymax=temp.max,fill=tempcol),alpha=0.5)+
  geom_line(aes(x=date,y=pressure.m),color=presscol,size=1,alpha=1)+
  geom_ribbon(aes(x=date,ymin=pressure.min,ymax=pressure.max,fill=presscol),alpha=0.2)+
  scale_fill_manual("",values=c(tempcol,presscol),labels=c("temperature","pressure"))+
  theme_bw()+ ylab("temperature (°C)") + xlab("time")+
  theme(legend.position="bottom",axis.title.y = element_text(color="grey30"),axis.title.y.right = element_text(color = "grey30"))+
  scale_x_date(name = "time") +
  scale_y_continuous(name = "temperature (°C)", 
                     sec.axis = sec_axis(~., name = "pressure (decibars)"))

f.1   

#ggsave(filename = paste(plot_dir,paste("Lyonesse_P.png",sep="_"),sep="/"), 
#       plot = f.1, width = 25,height = 20, units = "cm", dpi = 300, type = "cairo-png") 


# ===================================================--
# 2. JOINT PLOTS ----
# ===================================================--

head(sums)
sums$ID <- "GIRLRONA"
head(sums2)
sums2$ID <- "LYONESSE"

tag3 <- rbind(sums,sums2)

head(tag3)
str(tag3)
tag3$max_temp <- as.numeric(tag3$max_temp)
tag3$min_temp <- as.numeric(tag3$min_temp)

tempcol <- "limegreen"
presscol <- "green4"
data <- tag3

d <- ggplot(data,aes(x=date))+facet_grid(ID ~ .) +   
  geom_area(aes(y=max_depth,color="red"),size=1,alpha=0.2,color="grey")+
  geom_line(aes(y=max_temp),size=1,alpha=1,color="red")+
  geom_line(aes(y=min_temp),size=1,alpha=1,color="blue")+
  scale_y_continuous(name = "maximum depth (m)", 
                     sec.axis = sec_axis(~.*1, name = "temperature (°C)")) + 
  scale_colour_manual(name='legend',
                      breaks=c('max_depth', 'max_temp', 'min_temp'),
                      values=c('max_depth'='pink', 'max_temp'='blue', 'min_temp'='purple'))


d
#ggsave(filename = paste(plot_dir,paste("summaries_tag.png",sep="_"),sep="/"), 
#       plot = d, width = 25,height = 20, units = "cm", dpi = 300, type = "cairo-png") 




#with legend (although not able to change the colours)
d2 <- ggplot(data,aes(date,max_temp)) + facet_wrap(ID~.,ncol=1,strip.position="top") +       
  geom_area(aes(x=date,y=max_depth,fill="max_depth"),alpha=0.5,size=1)+
  geom_line(aes(x=date,y=max_temp,color="max_temp"),size=1)+
  geom_line(aes(x=date,y=min_temp,color="min_temp"),size=1)+
  theme_bw()+ 
  theme(legend.position="bottom")+
  ylab("temperature (°C)") + xlab("time")+
  scale_y_continuous(name = "maximum depth (m)", 
                     sec.axis = sec_axis(~.*1, name = "temperature (°C)")) + 
  
  ggtitle("")
#scale_colour_identity(name='legend',
#                    breaks=c('max_depth', 'max_temp', 'min_temp'),
#                   values=c('max_depth'='pink', 'max_temp'='blue', 'min_temp'='purple'))

d2

#ggsave(filename = paste(plot_dir,paste("summaries_tag2.png",sep="_"),sep="/"), 
#       plot = d2, width = 25,height = 20, units = "cm", dpi = 300, type = "cairo-png") 


#numbers for the report----

tapply(tag3$max_temp, tag3$ID, summary) #pressumable out of the water
tapply(tag3$min_temp, tag3$ID, summary)
tapply(tag3$depth, tag3$ID, summary)

subset(tag3,max_temp=="31.875")
subset(tag3,min_temp=="0.016")

#see maximum temperature under the water
tag4 <- subset(tag3,depth>1)
tapply(tag4$max_temp, tag4$ID, summary)
subset(tag4,max_temp=="31.875")

####
tag3 <- as.data.table(tag3)
head(tag3)

str(tag3)
tag3$max_temp <- as.numeric(as.character(tag3$max_temp))
tag3$ID <- as.factor(as.character(tag3$ID))

#there was a package interference between plyr and dyplyr, just define which one you are referring to
tag3 %>% dplyr::group_by(ID)%>%
  dplyr::summarize(meanT=mean(max_temp),
                   minT=min(min_temp),
                   maxT=max(max_temp),
                   meanD=mean(depth),
                   minD=min(max_depth),
                   maxD=max(max_depth))

tag3 <- as.data.frame(tag3)
subset(tag3,max_depth>"58.0")


#common dates
tag1.3 <- subset(tag1.2,!date=="2020-11-25")
tag2.3 <- subset(tag2.2,!date=="2021-02-04")
summary(tag1.3)
summary(tag2.3)

tag3 <- rbind(tag1.3,tag2.3)

tempcol <- "limegreen"
presscol <- "green4"
data <- tag3

d <- ggplot(data,aes(x=date,y=temp.m)) +
  geom_line(aes(x=date,y=temp.m,color=ID),size=1,alpha=1)+
  #geom_ribbon(aes(x=date,ymin=temp.min,ymax=temp.max,fill=ID),alpha=0.2)+
  theme_bw()+ theme(legend.position="bottom") + ylab("temperature (°C)") + xlab("time")+
  scale_colour_manual("vessel",values=c(tempcol,presscol))+
  #scale_fill_manual("",values=c(tempcol,presscol))+
  ggtitle("temperature")

d
# ggsave(filename = paste(plot_dir,paste("Ttag.png",sep="_"),sep="/"), 
#        plot = d, width = 25,height = 20, units = "cm", dpi = 300, type = "cairo-png") 


tempcol <- "cyan2"
presscol <- "slateblue"

e <- ggplot(data,aes(x=date,y=temp.m,group=ID)) +
  geom_line(aes(x=date,y=pressure.m,color=ID),size=1,alpha=1)+
  geom_ribbon(aes(x=date,ymin=pressure.min,ymax=pressure.max,fill=ID),fill=presscol,alpha=0.1)+
  theme_bw()+ theme(legend.position="bottom") + ylab("temperature (°C)") + xlab("time")+
  scale_colour_manual("vessel",values=c(tempcol,presscol))+
  scale_fill_manual("",values=c(tempcol,presscol))+
  ggtitle("hauls")

e
# ggsave(filename = paste(plot_dir,paste("Ptag.png",sep="_"),sep="/"), 
#        plot = e, width = 25,height = 20, units = "cm", dpi = 300, type = "cairo-png") 



#more complicated graph
tempcol <- "#69b3a2"
presscol <- "blue"
data <- tag3

f <- ggplot(data,aes(x=date,y=temp.m)) + facet_grid(ID ~ .) +
  geom_line(aes(x=date,y=temp.m),color=tempcol,size=1,alpha=1)+
  #geom_line(aes(x=date,y=temp.max),color="grey",size=1,alpha=0.6,linetype = "dotted")+
  #geom_line(aes(x=date,y=temp.min),color="grey",size=1,alpha=0.6,linetype = "dotted")+
  geom_ribbon(aes(x=date,ymin=temp.min,ymax=temp.max,fill=tempcol),alpha=0.5)+
  geom_line(aes(x=date,y=pressure.m),color=presscol,size=1,alpha=1)+
  geom_ribbon(aes(x=date,ymin=pressure.min,ymax=pressure.max,fill=presscol),alpha=0.2)+
  theme_bw()+ theme(legend.position="bottom") + ylab("temperature (°C)") + xlab("time")+
  scale_fill_manual("",values=c(tempcol,presscol),labels=c("temperature","pressure"))

f
# ggsave(filename = paste(plot_dir,paste("allcombinedtag.png",sep="_"),sep="/"), 
#        plot = f, width = 25,height = 20, units = "cm", dpi = 300, type = "cairo-png") 


summary(tag3)

#add a second axis for the pressure
f.1 <- ggplot(data,aes(x=date,y=temp.m)) + facet_grid(ID ~ .) +
        geom_line(aes(x=date,y=temp.m),color=tempcol,size=1,alpha=1)+
        #geom_line(aes(x=date,y=temp.max),color="grey",size=1,alpha=0.6,linetype = "dotted")+
        #geom_line(aes(x=date,y=temp.min),color="grey",size=1,alpha=0.6,linetype = "dotted")+
        geom_ribbon(aes(x=date,ymin=temp.min,ymax=temp.max,fill=tempcol),alpha=0.5)+
        geom_line(aes(x=date,y=pressure.m),color=presscol,size=1,alpha=1)+
        geom_ribbon(aes(x=date,ymin=pressure.min,ymax=pressure.max,fill=presscol),alpha=0.2)+
        scale_fill_manual("",values=c(tempcol,presscol),labels=c("temperature","pressure"))+
        theme_bw()+ ylab("temperature (°C)") + xlab("time")+
        theme(legend.position="bottom",axis.title.y = element_text(color="grey30"),axis.title.y.right = element_text(color = "grey30"))+
        scale_x_date(name = "time") +
        scale_y_continuous(name = "temperature (°C)",
                           sec.axis = sec_axis(~., name = "pressure (decibars)"))

f.1

# ggsave(filename = paste(plot_dir,paste("allcombinedtag2.png",sep="_"),sep="/"), 
#        plot = f.1, width = 25,height = 20, units = "cm", dpi = 300, type = "cairo-png") 
# 


# ===================================================--
# 3. ADD CATCHES ----
# ===================================================--

cat <- read.csv(paste(out_dir,"PIL_LBfishers_2122(2).csv",sep="/"),header=T)
cat <- subset(cat,vessel=="LYONESSE")# interested in pel.marksman

dim(cat);summary(cat)
str(cat)
table(cat$month,cat$year)

cat$pil <- as.numeric(cat$pil)
cat$date <- as.Date(cat$date)
plot(cat$pil~cat$date,type="p")

table(data$date)
table(cat$date)
#head(data);dim(data)
head(tag2.2);dim(tag2.2)
head(cat);dim(cat)


#select specific dates and merge
cat2 <- subset(cat,date%in%c("2021-0z-31","2021-09-14"))
cat2

data_new <- cat[cat$date > "2021-07-23" &    # Extract data frame subset
                  cat$date < "2022-01-30", ]

dim(data_new)

cat3 <- data_new[,c("date","depth","pil","ane")]
str(cat3)

cat3$ane <- as.numeric(as.character(cat3$ane))
cat3 <- as.data.table(cat3)

meancat <- cat3 %>%
  group_by(date) %>%
  summarise_each(funs(mean))

meancat <- as.data.frame(meancat)
meancat
meancat$date <- as.Date(as.character(meancat$date,"%Y-%m-%d"))

str(tag2.2)
str(meancat)

prova <- merge(x= tag2.2, y= meancat, by= 'date', all.x= T)
prova[is.na(prova)] <- 0
plot(prova$date,prova$pil, type="l")



#plot----
#only Lyonesse 
tempcol <- "#69b3a2"
presscol <- "blue"
data <- prova

g <- ggplot() + 
  geom_line(mapping = aes(x = data$date, y = data$pil/1000), size = 1, color = "black",alpha=0.5,linetype="solid") + 
  
  geom_line(mapping = aes(x = data$date, y = data$temp.m),size=1,color=tempcol,alpha=1,linetype="solid") +
  geom_ribbon(aes(x=data$date,ymin=data$temp.min,ymax=data$temp.max,fill=tempcol),alpha=0.5)+
  
  geom_line(aes(x=data$date,y=data$pressure.m),color=presscol,size=1,alpha=0.5,linetype="solid")+
  geom_ribbon(aes(x=data$date,ymin=data$pressure.min,ymax=data$pressure.max,fill=presscol),alpha=0.2)+
  
  scale_x_date(name = "time") +
  scale_y_continuous(name = "temperature (°C)", 
                     sec.axis = sec_axis(~., name = "sardine catches (t)")) + 
  theme_bw()+
  theme(axis.title.y = element_text(color = tempcol),
        axis.title.y.right = element_text(color = "grey30"),
        legend.position = "bottom")+ 
  ggtitle(paste(unique(data$ID),sep="_"))+
  scale_fill_manual("",values=c(tempcol,presscol),labels=c("temperature","pressure"))+
  scale_colour_manual("",values = c("red","green","blue"))

g


#ggsave(filename = paste(plot_dir,paste("withcatches_lyonesse.png",sep="_"),sep="/"), 
#       plot = g, width = 25,height = 20, units = "cm", dpi = 300, type = "cairo-png") 


#add catches in the legend (not working)

g1 <- ggplot() + geom_line(mapping = aes(x = data$date, y = data$pil/1000),color ="red", size = 1,alpha=0.5,linetype="solid") + 
  
  geom_line(mapping = aes(x = data$date, y = data$temp.m,color=tempcol),size=1,alpha=1,linetype="solid") +
  geom_ribbon(aes(x=data$date,ymin=data$temp.min,ymax=data$temp.max,fill=tempcol),alpha=0.5)+
  
  geom_line(aes(x=data$date,y=data$pressure.m,color=presscol),size=1,alpha=0.5,linetype="solid")+
  geom_ribbon(aes(x=data$date,ymin=data$pressure.min,ymax=data$pressure.max,fill=presscol),alpha=0.2)+
  
  
  scale_x_date(name = "time") +
  scale_y_continuous(name = "temperature (°C)", 
                     sec.axis = sec_axis(~., name = "sardine catches (t)")) + 
  theme_bw()+
  theme(axis.title.y = element_text(color = tempcol),
        axis.title.y.right = element_text(color = "grey30"),
        legend.position = "bottom")+ 
  ggtitle(paste(unique(data$ID),sep="_"))+
  #scale_colour_manual("",values = c(tempcol,presscol,"black"),labels=c("mean temperature","mean pressure","catches"))+
  #scale_fill_manual("",values=c("red"),labels=c("catches"))
  
  g1


# 4. FINDING RELATIONSHIPS----

head(data)
plot(pil~temp.m,data)
plot(pil~temp.max,data)
plot(pil~temp.min,data)
plot(pil~depth,data)
plot(pil~pressure.max,data)

boxplot(pil~depth,data,ylab="sardine catches (kg)",xlab="depth (m)")

#dev.copy(png, paste(plot_dir,'/depth_tag.png',sep=""),width=25,height=17,unit='cm',res=300)
#dev.off()


#############################################END##########################################################################----