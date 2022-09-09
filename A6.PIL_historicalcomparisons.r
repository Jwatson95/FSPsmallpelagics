#Historical comparison for PIL FSP 2020-2021 with previous years (2017,2018,2019)
#@silvia Rodriguez Climent
#01/04/2021 last modified 16/03/2022
#---------------------------------------------------------------------## 

# set input, output directories
inp_dir <- "C:/Users/SRC01/OneDrive - CEFAS/SC/Rscripts/FSP_database/PIL/Output/"
list.files(inp_dir)
plot_dir <- file.path(getwd(), "plots/PIL/Historic_comp/")

#libraries
library(ggplot2);library(lubridate); library(ggpubr); library(data.table); library(dplyr)

#read files
db<- read.csv(paste(inp_dir,"PIL_db_agg171819202122.csv",sep=""),sep=",",header=T, stringsAsFactors = F)
head(db);dim(db) #15706 13

###########################################################################################################################--
#1 Length distributions plots----

db <- subset(db,!length_cm==0)
head(db);dim(db)

species="PIL"

## explore the data----
#library("ggpubr")

ggboxplot(db, x = "source", y = "length_cm", 
          fill = "vessel", palette = rainbow(20),
          #order = c("COOMBEFISHERIES", "FALFISH", "INTERFISH"),
          ylab = "length", xlab = "source")


# Box plot
boxplot(length_cm ~ source, data = db,
        xlab = "source", ylab = "length",
        frame = FALSE, col = c("#00AFBB", "#E7B800", "#FC4E07"))


# plotmeans (library gplots)
plotmeans(length_cm ~ source, data = db, frame = FALSE,
          xlab = "length", ylab = "processor",
          main="Mean Plot with 95% CI") 


#some data summaries----
head(db);str(db)

sum1 <- db %>% 
  dplyr::group_by(fishingseason)%>%
  dplyr::summarize(meanL=mean(length_cm),
                   minL=min(length_cm),
                   maxL=max(length_cm),
                   N=sum(n))

sum2 <- db %>% 
  dplyr::group_by(fishingseason,month)%>%
  dplyr::summarize(meanL=mean(length_cm),
                   minL=min(length_cm),
                   maxL=max(length_cm),
                   N=sum(n))

sum3 <- db %>% 
  dplyr::group_by(fishingseason,month,source)%>%
  dplyr::summarize(meanL=mean(length_cm),
                   minL=min(length_cm),
                   maxL=max(length_cm),
                   N=sum(n))


#aggregate data
tlyear <- with(db,aggregate(n,list(length_cm=length_cm,month=month,year=year,fishingseason=fishingseason),sum))
tlyearsource <- with(db,aggregate(n,list(length_cm=length_cm,year=year,source=source,fishingseason=fishingseason),sum))
tlyearsourceM <- with(db,aggregate(n,list(length_cm=length_cm,month=month,year=year,source=source,fishingseason=fishingseason),sum))

all_cum <- as.data.table(tlyear)
all_cum2 <- as.data.table(tlyearsource)
all_cumMonth<- as.data.table(tlyearsourceM)

names(all_cum) <- c("TL","month","year","fishingseason","Freq")
names(all_cum2) <- c("TL","year","source","fishingseason","Freq")
names(all_cumMonth) <- c("TL","month","year","source","fishingseason","Freq")

# weighted average overall and weighted average by month
all_cum[,TL:=as.numeric(TL)]
all_cum[,wtMean:=weighted.mean(TL,Freq),by=c("month","fishingseason")]

all_cum2[,TL:=as.numeric(TL)]
all_cum2[,wtMean:=weighted.mean(TL,Freq),by=c("source","fishingseason")]

all_cumMonth[,TL:=as.numeric(TL)]
all_cumMonth[,Freq:=as.numeric(Freq)]
all_cumMonth[,wtMean:=weighted.mean(TL,Freq),by=c("source","month","fishingseason")]

#put name into months and order them
all_cum$month[all_cum$month=="7"] <- "Jul"
all_cum$month[all_cum$month=="8"] <- "Aug"
all_cum$month[all_cum$month=="9"] <- "Sep"
all_cum$month[all_cum$month=="10"] <- "Oct"
all_cum$month[all_cum$month=="11"] <- "Nov"
all_cum$month[all_cum$month=="12"] <- "Dec"
all_cum$month[all_cum$month=="1"] <- "Jan"
all_cum$month[all_cum$month=="2"] <- "Feb"
all_cum$month[all_cum$month=="3"] <- "Mar"

all_cumMonth$month[all_cumMonth$month=="7"] <- "Jul"
all_cumMonth$month[all_cumMonth$month=="8"] <- "Aug"
all_cumMonth$month[all_cumMonth$month=="9"] <- "Sep"
all_cumMonth$month[all_cumMonth$month=="10"] <- "Oct"
all_cumMonth$month[all_cumMonth$month=="11"] <- "Nov"
all_cumMonth$month[all_cumMonth$month=="12"] <- "Dec"
all_cumMonth$month[all_cumMonth$month=="1"] <- "Jan"
all_cumMonth$month[all_cumMonth$month=="2"] <- "Feb"
all_cumMonth$month[all_cumMonth$month=="3"] <- "Mar"

all_cum$month<- factor(all_cum$month,levels = c("Jul","Aug","Sep","Oct","Nov","Dec","Jan","Feb","Mar"),ordered=TRUE)  
all_cumMonth$month<- factor(all_cumMonth$month,levels = c("Jul","Aug","Sep","Oct","Nov","Dec","Jan","Feb","Mar"),ordered=TRUE)  


#### PLOTS ####----
## 1.1 Overall LFD----

all.TL1 <- ggplot(all_cum, aes(TL,Freq)) + geom_bar(stat="identity", position="dodge",fill="grey") +
  theme_bw(25) +ylab("N")+xlab("Total length (cm)")+#scale_y_continuous(limits=c(0,max(all_cum$Freq)+10), expand=c(0,0))+
  theme(legend.position = "none",text = element_text(size=15))+ggtitle(paste(species,"self-sampling",sep="-"))+
  facet_grid(rows=vars(fishingseason),vars(month), scales = "free")+
  geom_vline(aes(xintercept=wtMean), col="red", size=1.2,linetype=1,alpha=0.5)

all.TL1

ggsave(filename = paste(plot_dir,paste(species,"FSP_2017_2022","_TL.png",sep="_"),sep="/"), 
       plot = all.TL1, width = 40,height = 25, units = "cm", dpi = 300, type = "cairo-png") 

#save the file
#write.csv(all_cum,paste(plot_dir,"/",species,"_cum1722.csv",sep=""),row.names = F)


## 1.2 LFD by source (fisher/processor----

all.TL2 <- ggplot(all_cum2, aes(TL,Freq,fill=source)) + geom_bar(stat="identity", position="dodge",alpha=1) +
  theme_bw(25) +ylab("N")+xlab("Total length (cm)")+#scale_y_continuous(limits=c(0,max(all_cum2$Freq)+10), expand=c(0,0))+
  theme(legend.position = "none")+ggtitle(paste(species,"self-sampling",sep="-"))+
  #geom_vline(data=all_cum2, aes(xintercept=wtMean, color=Source),linetype="dashed",size=1.2)+
  theme(legend.position = "bottom",text=element_text(size=15))+
  facet_grid(rows=vars(fishingseason),scales="free")


all.TL2 <- ggplot(all_cum2, aes(TL,Freq,fill=source)) + geom_bar(stat="identity", position="dodge",alpha=1) +
  theme_bw(25) +ylab("N")+xlab("Total length (cm)")+#scale_y_continuous(limits=c(0,max(all_cum2$Freq)+10), expand=c(0,0))+
  theme(legend.position = "none")+ggtitle(paste(species,"self-sampling",sep="-"))+
  geom_vline(data=all_cum2, aes(xintercept=wtMean, color=source),linetype="dashed",size=1.2,alpha=0.5)+
  theme(legend.position = "bottom")+
  #facet_grid(rows=vars(fishingseason),vars(source),scales="free_y")
  facet_grid(source~fishingseason,scales="free")
all.TL2

ggsave(filename = paste(plot_dir,paste(species,"FSP_2017_2022_fisherproc","_TL.png",sep="_"),sep="/"), 
       plot = all.TL2, width = 35,height = 22, units = "cm", dpi = 300, type = "cairo-png") 
#save the file
#write.csv(all_cum2,paste(plot_dir,"/",species,"_cum1722source.csv",sep=""),row.names = F)


all.TL3 <- ggplot(all_cum2, aes(TL, Freq,fill=source)) + 
  geom_bar(stat="identity", position="identity",alpha=0.3) +
  theme_bw(25) +ylab("N")+xlab("Total length (cm)")+#scale_y_continuous(limits=c(0,max(all_cum2$Freq)+10), expand=c(0,0))+
  theme(legend.position = "none")+ggtitle(paste(species,"self-sampling",sep="-"))+
  geom_vline(data=all_cum2, aes(xintercept=wtMean, color=source),linetype="dashed",size=1.2)+
  theme(legend.position = "bottom",text=element_text(size=20))+
  facet_grid(rows=vars(fishingseason),scales="free")
#facet_grid(.~fishingseason)
all.TL3
#ggsave(filename = paste(plot_dir,paste(species,"FSP_2017_2022_fisherproc(2)","_TL.png",sep="_"),sep="/"), 
#       plot = all.TL3, width = 25,height = 20, units = "cm", dpi = 300, type = "cairo-png") 


all.TL4 <- ggplot(all_cum2, aes(TL,fill=source)) + 
  #geom_bar(stat="identity", position="identity",alpha=0.3) +
  geom_density(alpha=.25)+
  theme_bw(25) +ylab("N")+xlab("Total length (cm)")+#scale_y_continuous(limits=c(0,max(all_cum2$Freq)+10), expand=c(0,0))+
  theme(legend.position = "none")+ggtitle(paste(species,"self-sampling",sep="-"))+
  geom_vline(data=all_cum2, aes(xintercept=wtMean, color=source),linetype="dashed",size=1.2)+
  theme(legend.position = "bottom",text=element_text(size=20))+
  facet_grid(rows=vars(fishingseason))

all.TL4
#ggsave(filename = paste(plot_dir,paste(species,"FSP_2017_2022_fisherproc(3)","_TL.png",sep="_"),sep="/"), 
#       plot = all.TL4, width = 25,height = 20, units = "cm", dpi = 300, type = "cairo-png") 




all.TL5 <- ggplot(all_cumMonth, aes(TL,fill=source)) + 
  #geom_bar(stat="identity", position="identity",alpha=0.3) +
  geom_density(alpha=.25)+
  theme_bw(25) +ylab("Density")+xlab("Total length (cm)")+#scale_y_continuous(limits=c(0,max(all_cum2$Freq)+10), expand=c(0,0))+
  theme(legend.position = "none")+
  #ggtitle(paste(species,"self-sampling",sep="-"))+
  geom_vline(data=all_cum2, aes(xintercept=wtMean, color=source),linetype="dashed",size=1.2)+
  theme(legend.position = "bottom", legend.justification = "right", legend.margin = margin(0, 0, 0, 0),
        legend.spacing.x = unit(2, "pt"),text=element_text(size=15))+
  facet_grid(rows=vars(fishingseason),vars(month))
all.TL5

#ggsave(filename = paste(plot_dir,paste(species,"FSP_2017_2022_fisherproc(4)","_TL.png",sep="_"),sep="/"), 
#       plot = all.TL5, width = 35,height = 25, units = "cm", dpi = 300, type = "cairo-png") 



# all.TL6 <- ggplot(all_cumMonth, aes(TL,Freq,fill=source)) + 
#   geom_bar(stat="identity", position="identity",alpha=0.3) +
#   #geom_density(alpha=.25)+
#   theme_bw(25) +ylab("N")+xlab("Total length (cm)")+#scale_y_continuous(limits=c(0,max(all_cum2$Freq)+10), expand=c(0,0))+
#   theme(legend.position = "none")+ggtitle(paste(species,"self-sampling",sep="-"))+
#   geom_vline(data=all_cum2, aes(xintercept=wtMean, color=source),linetype="dashed",size=1.2)+
#   theme(legend.position = "bottom",text=element_text(size=20))+
#   facet_grid(rows=vars(fishingseason),vars(month))
# 
# all.TL6
# ggsave(filename = paste(plot_dir,paste(species,"FSP_2017_2021_fisherproc(5)","_TL.png",sep="_"),sep="/"), 
#        plot = all.TL6, width = 25,height = 20, units = "cm", dpi = 300, type = "cairo-png") 


## 1.3 LFD by month,source and fishingseason----

#fishers
data <- all_cumMonth[source=="fisher"]

all.TL5 <- ggplot(data, aes(TL,Freq, fill=month)) +
  geom_bar(stat="identity",position=position_dodge(2),width=0.45,alpha=1) + 
  #theme_bw(25) + scale_y_continuous(limits=c(0,max(data$Freq)+10), expand=c(0,0)) + 
  ylab("N") + xlab("Total length (cm)")+
  geom_vline(aes(xintercept=wtMean), col="red", size=1.2) +
  theme(legend.position="none",strip.text.x=element_text(size=12,colour="black"),
        strip.text.y = element_text(size=12),axis.text.x = element_text(color = "black", size = 10),
        axis.text.y = element_text(color = "black", size = 10),
        axis.title.y = element_text(color = "black", size = 15),axis.title.x = element_text(color = "black", size = 15))+
  #facet_grid(rows=vars(source),vars(fishingseason),scales="free")
  facet_grid(month~fishingseason,scales="free")+
  ggtitle(paste(species,"fishers",sep="-"))
all.TL5

#ggsave(filename = paste(plot_dir,paste(species,"FSP_2017_2022_fishers_freescale","_TL.png",sep="_"),sep="/"), 
#       plot = all.TL5, width = 35,height = 25, units = "cm", dpi = 300, type = "cairo-png") 

#save the file
#write.csv(all_cumMonth,paste(plot_dir,"/",species,"_cumMonth1722.csv",sep=""),row.names = F)


#processors
data <- all_cumMonth[source=="processor"]

all.TL6 <- ggplot(data, aes(TL,Freq, fill=month)) +
  geom_bar(stat="identity",position=position_dodge(2),width=0.45,alpha=1) + 
  #theme_bw(25) + scale_y_continuous(limits=c(0,max(data$Freq)+10), expand=c(0,0)) + 
  ylab("N") + xlab("Total length (cm)")+
  geom_vline(aes(xintercept=wtMean), col="red", size=1.2) +
  theme(legend.position="none",strip.text.x=element_text(size=12,colour="black"),
        strip.text.y = element_text(size=12),axis.text.x = element_text(color = "black", size = 10),
        axis.text.y = element_text(color = "black", size = 10),
        axis.title.y = element_text(color = "black", size = 15),axis.title.x = element_text(color = "black", size = 15))+
  #facet_grid(rows=vars(source),vars(fishingseason),scales="free")
  facet_grid(month~fishingseason,scales="free")+
  ggtitle(paste(species,"processors",sep="-"))
all.TL6

#ggsave(filename = paste(plot_dir,paste(species,"FSP_2017_2022_processors_freescale","_TL.png",sep="_"),sep="/"), 
#       plot = all.TL6, width = 35,height = 25, units = "cm", dpi = 300, type = "cairo-png") 


#split in two plots for better visualization
table(data$fishingseason)

data1 <- subset(data,fishingseason%in%c("2017-2018","2018-2019"))
all.TL7 <- ggplot(data1, aes(TL,Freq, fill=month)) +
  geom_bar(stat="identity",position=position_dodge(2),width=0.45,alpha=1) + 
  #theme_bw(25) + scale_y_continuous(limits=c(0,max(data$Freq)+10), expand=c(0,0)) + 
  ylab("N") + xlab("Total length (cm)")+
  geom_vline(aes(xintercept=wtMean), col="red", size=1.2) +
  theme(legend.position="none",strip.text.x=element_text(size=12,colour="black"),
        strip.text.y = element_text(size=12),axis.text.x = element_text(color = "black", size = 10),
        axis.text.y = element_text(color = "black", size = 10),
        axis.title.y = element_text(color = "black", size = 15),axis.title.x = element_text(color = "black", size = 15))+
  #facet_grid(rows=vars(source),vars(fishingseason),scales="free")
  facet_grid(fishingseason~month,scales="free")+
  ggtitle(paste(species,"processors",sep="-"))
all.TL7


data1 <- subset(data,fishingseason%in%c("2019-2020","2020-2021","2021-2022"))
all.TL8 <- ggplot(data1, aes(TL,Freq, fill=month)) +
  geom_bar(stat="identity",position=position_dodge(2),width=0.45,alpha=1) + 
  #theme_bw(25) + scale_y_continuous(limits=c(0,max(data$Freq)+10), expand=c(0,0)) + 
  ylab("N") + xlab("Total length (cm)")+
  geom_vline(aes(xintercept=wtMean), col="red", size=1.2) +
  theme(legend.position="none",strip.text.x=element_text(size=12,colour="black"),
        strip.text.y = element_text(size=12),axis.text.x = element_text(color = "black", size = 10),
        axis.text.y = element_text(color = "black", size = 10),
        axis.title.y = element_text(color = "black", size = 15),axis.title.x = element_text(color = "black", size = 15))+
  #facet_grid(rows=vars(source),vars(fishingseason),scales="free")
  facet_grid(fishingseason~month,scales="free")+
  ggtitle(paste(species,"processors",sep="-"))
all.TL8


#ggsave(filename = paste(plot_dir,paste(species,"FSP_2017_2019_processors_freescale","_TL.png",sep="_"),sep="/"), 
#       plot = all.TL7, width = 35,height = 25, units = "cm", dpi = 300, type = "cairo-png") 

#ggsave(filename = paste(plot_dir,paste(species,"FSP_2020_2022_processors_freescale","_TL.png",sep="_"),sep="/"), 
#       plot = all.TL8, width = 35,height = 25, units = "cm", dpi = 300, type = "cairo-png") 


## 1.4 Total catch (from the processors)----

head(db):str(db)

Catch <- with(db,aggregate(totalcatch_kg,list(vessel=vessel,month=month,year=year,fishingseason=fishingseason),sum))

Catch$month2 <- Catch$month

Catch$month2[Catch$month2=="7"] <- "Jul"
Catch$month2[Catch$month2=="8"] <- "Aug"
Catch$month2[Catch$month2=="9"] <- "Sep"
Catch$month2[Catch$month2=="10"] <- "Oct"
Catch$month2[Catch$month2=="11"] <- "Nov"
Catch$month2[Catch$month2=="12"] <- "Dec"
Catch$month2[Catch$month2=="1"] <- "Jan"
Catch$month2[Catch$month2=="2"] <- "Feb"
Catch$month2[Catch$month2=="3"] <- "Mar"

Catch$month2 <- factor(Catch$month2,levels = c("Jul","Aug","Sep","Oct","Nov","Dec","Jan","Feb","Mar"),ordered=TRUE)  

str(Catch)
table(Catch$vessel)
#only vessels with more than 10 observations were kept
Catch <- subset(Catch,!vessel%in%c("21958 G","21959 B","CELTIC DAWN","CONSTANT FRIEND","HANNAH JACK","MARY ANNE","PRIDE OF CORNWALL"))

T_Catch<- ggplot(Catch, aes(month2, x/1000, group=vessel, col=vessel)) + geom_line(size=1.2) +geom_point()+
  theme_bw(25) + ylab("tonnes") + xlab("month") + 
  theme(legend.position="top",legend.text=element_text(color="black",size=10),
        legend.title=element_text(color="black",size=12),text=element_text(size=12))+
  guides(col = guide_legend(nrow = 2))+
  facet_grid(fishingseason~vessel,scales="free")

T_Catch

#ggsave(filename = paste(plot_dir,paste(species,"FSP_2017_2022_freescale","_TCatch.png",sep="_"),sep="/"), 
#       plot = T_Catch, width = 40,height = 25, units = "cm", dpi = 300, type = "cairo-png") 


#total catch in 3 bits
table(Catch$vessel)
Catch1 <- subset(Catch,vessel%in%c("pelagic marksman"))

T_Catch1<- ggplot(Catch1, aes(month2, x/1000, group=vessel, col=vessel)) + geom_line(size=1.2) +geom_point()+
  theme_bw(25) + ylab("tonnes") + xlab("month") + 
  theme(legend.position="top",legend.text=element_text(color="black",size=10),
        legend.title=element_text(color="black",size=12),text=element_text(size=12))+
  guides(col = guide_legend(nrow = 2))+
  facet_grid(fishingseason~vessel,scales="free")
T_Catch1
#ggsave(filename = paste(plot_dir,paste(species,"FSP_2017_2022_v1","_TCatch.png",sep="_"),sep="/"), 
#       plot = T_Catch1, width = 40,height = 25, units = "cm", dpi = 300, type = "cairo-png") 


table(Catch$vessel)
Catch2 <- subset(Catch,vessel%in%c("asthore","charlotte clare","galwad-y-mor","golden harvest","lyonesse"))
T_Catch2<- ggplot(Catch2, aes(month2, x/1000, group=vessel, col=vessel)) + geom_line(size=1.2) +geom_point()+
  theme_bw(25) + ylab("tonnes") + xlab("month") + 
  theme(legend.position="top",legend.text=element_text(color="black",size=10),
        legend.title=element_text(color="black",size=12),text=element_text(size=12))+
  guides(col = guide_legend(nrow = 2))+
  facet_grid(fishingseason~vessel,scales="free")

T_Catch2
#ggsave(filename = paste(plot_dir,paste(species,"FSP_2017_2021_v2","_TCatch.png",sep="_"),sep="/"), 
#       plot = T_Catch2, width = 40,height = 25, units = "cm", dpi = 300, type = "cairo-png") 


Catch3 <- subset(Catch,vessel%in%c("mayflower","rachel anne","resolute","serene danw","vesta"))
T_Catch3<- ggplot(Catch3, aes(month2, x/1000, group=vessel, col=vessel)) + geom_line(size=1.2) +geom_point()+
  theme_bw(25) + ylab("tonnes") + xlab("month") + 
  theme(legend.position="top",legend.text=element_text(color="black",size=10),
        legend.title=element_text(color="black",size=12),text=element_text(size=12))+
  guides(col = guide_legend(nrow = 2))+
  facet_grid(fishingseason~vessel,scales="free")

T_Catch3
#ggsave(filename = paste(plot_dir,paste(species,"FSP_2017_2021_v3","_TCatch.png",sep="_"),sep="/"), 
#       plot = T_Catch3, width = 40,height = 25, units = "cm", dpi = 300, type = "cairo-png") 

#############################################END##########################################################################----
