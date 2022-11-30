#Plotting data for SPR from processors FSP
#@silvia Rodriguez Climent
# 13-01-2021; last modified 09-09-2022
#---------------------------------------------------------------------## 

rm(list=ls())
# set input, output directories
inp_dir <- file.path(getwd(), "Data/Processors/SPR/")
plot_dir <- file.path(getwd(), "Data/plots/SPR")
out_dir <- file.path(getwd(), "Data/Output/")
list.files(out_dir,recursive=TRUE) # recursive TRUE to see inside the different folders

# load libraries
library(ggplot2);library(data.table);library(xlsx);library(openxlsx);library(dplyr)
library(readxl);library(stringr);library(plyr);library(tidyr);library(reshape2);library(maps)
library(mapdata);library(mapproj);library(mapplots);library(lubridate);library(rgdal)
library(raster);library(GISTools);library(ggspatial);library(XLConnect);library(openxlsx)
library(data.table);library(sjmisc);library(lubridate); library(ggpubr);library(gplots)

#get all the final plots from all the processors
list.files(out_dir, pattern="SPR*")

# ===================================================--
# 0. Latest file all processors ----
# ===================================================--

# From script: B1.SPR_DataExtraction_Processor2021-----

proc <- read.table(paste(out_dir,"/SPR_processors_2022.csv",sep=''),sep=",",header=T,stringsAsFactors = F)

head(proc);dim(proc)
str(proc)
proc$date <- as.Date(as.character(proc$date,format="%Y-%m-%d"))
table(proc$month)
table(proc$processor)
table(proc$vessel)

head(proc)
proc <- proc[,c("date","vessel","totalcatch_kg","length_cm","weight_g","samplewt_g","sampleID","month","year","source","processor","species")]

# ===================================================--
# 1. Length distribution plots in a loop ----
# ===================================================--

#your data
db <- proc
#your species
species=unique(db$species)
#your variable
var <- c(unique(db$processor))
#variable you want to plot
db$sel <- db$processor

#loop (check names are correct in line 27 or change them)

plot1.TL1 <- list()
plot1.TL2 <- list()
plot1.TL3 <- list()

for(i in 1:length(var)){
  
  plot1=db[db$sel==var[i],]
  
  names(plot1) <- c("date","vessel","totalcatch_kg","length_cm","weight_g","samplewt_g","sampleID","month","year","source","processor","species","sel")
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
  
}



# ===================================================--
# 2. Adjust a linear LWR plots in a loop ----
# ===================================================--

#your data
db <- proc

#your species
species=unique(db$species)
#your variable
var <- c(unique(db$processor))
#variable you want to plot
db$sel <- db$processor

#loop (check names are correct in line 27 or change them)

SP_LWrel <- list()

for(i in 1:length(var)){
  plot1=db[db$sel==var[i],]
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
  
}


print(SP_LWrel[[1]])
print(SP_LWrel[[2]])
print(SP_LWrel[[3]])


# ===================================================--
# 3. Adjust NON linear LWR plots in a loop ----
# ===================================================--

#your data
db <- proc
#your species
species=unique(db$species)
#your variable
var <- unique(db$processor)
#variable you want to plot
db$sel <- db$processor


#loop (check names are correct in line 27 or change them)

SP_NONLWrel <- list()

for(i in 1:length(var)){
  plot1=db[db$sel==var[i],]
  names(plot1) <- c("date","vessel","totalcatch_kg","length_cm","weight_g","samplewt_g","sampleID","month","year","source","processor","species","sel")
  
  ## Fit NON linear regression (cm/g)--
  nls_fit_w11 <- nls(weight_g ~ a*(length_cm)^b, plot1, start = list(a = 0.0005, b = 3))
  plot1$LW.W11 <- predict(nls_fit_w11)
  
  
  SP_NONLWrel[[i]] <- ggplot(plot1, aes(length_cm, weight_g)) + geom_point() + 
    geom_line(aes(length_cm,LW.W11), col="red") + theme_bw(25) +
    theme(legend.position="bottom") + ylab("Weight (g)") + xlab("TL (cm)") +
    #scale_colour_discrete(name = "Month") +
    ggtitle(paste(var[i]))+
    labs(subtitle = paste("a = ", round(coefficients(nls_fit_w11)[["a"]],digits=5),
                          " b =", round(coefficients(nls_fit_w11)[["b"]],digits=5)))
  
  ggsave(filename = paste(plot_dir,paste(var[i],species,"NONLWrel.png",sep="_"),sep="/"), 
         plot = SP_NONLWrel[[i]], width = 25, 
         height = 20, units = "cm", dpi = 300, type = "cairo-png") 
}

print(SP_NONLWrel[[1]])
print(SP_NONLWrel[[2]])
print(SP_NONLWrel[[3]])


###########################################################################################################################--
#3. All processors----

# prepare data for the length-weight plots
head(proc);dim (proc)
summary(proc)
proc <- proc %>% drop_na(length_cm,weight_g)
table(proc$length_cm)

#3.0 check for statistical differences-----
#Explore the data visually

## a)length----
#library("ggpubr")
ggboxplot(proc, x = "processor", y = "length_cm", 
          color = "processor", palette = c("#00AFBB", "#E7B800", "#FC4E07"),
          order = c("COOMBEFISHERIES", "FALFISH", "INTERFISH"),
          ylab = "length", xlab = "processor")

ggline(proc, x = "processor", y = "length_cm", 
       add = c("mean_se", "jitter"), 
       order = c("COOMBEFISHERIES", "FALFISH", "INTERFISH"),
       ylab = "length", xlab = "processor")

# Box plot
boxplot(length_cm ~ processor, data = proc,
        xlab = "length", ylab = "processor",
        frame = FALSE, col = c("#00AFBB", "#E7B800", "#FC4E07"))
boxplot(proc$length_cm~proc$processor)

# plotmeans (library gplots)
plotmeans(length_cm ~ processor, data = proc, frame = FALSE,
          xlab = "length", ylab = "processor",
          main="Mean Plot with 95% CI") 

# Compute the analysis of variance
res.aov <- aov(length_cm ~ processor, data = proc)  ##issues due to only one processers data availible 
# Summary of the analysis
summary(res.aov) #significant differences
capture_a <- summary(res.aov)

TukeyHSD(res.aov)
capture_t <- TukeyHSD(res.aov)

#save it
capture.output(capture_a, file = "spr_anovaresults_processors.txt")#save the file
capture.output(capture_t, file = "spr_tuckeyresults_processors.txt")#save the file


##b) weight----

#library("ggpubr")
ggboxplot(proc, x = "processor", y = "weight_g", 
          color = "processor", palette = c("#00AFBB", "#E7B800", "#FC4E07"),
          order = c("COOMBEFISHERIES", "FALFISH", "INTERFISH"),
          ylab = "weight", xlab = "processor")

ggline(proc, x = "processor", y = "weight_g", 
       add = c("mean_se", "jitter"), 
       order = c("COOMBEFISHERIES", "FALFISH", "INTERFISH"),
       ylab = "length", xlab = "processor")

# Box plot
boxplot(weight_g ~ processor, data = proc,
        xlab = "length", ylab = "processor",
        frame = FALSE, col = c("#00AFBB", "#E7B800", "#FC4E07"))
boxplot(proc$weight_g~proc$processor)

# plotmeans (library gplots)
plotmeans(weight_g ~ processor, data = proc, frame = FALSE,
          xlab = "processor", ylab = "weight",
          main="Mean Plot with 95% CI") 

# Compute the analysis of variance
res.aov <- aov(weight_g ~ processor, data = proc)
# Summary of the analysis
summary(res.aov) #significant differences
capture_a <- summary(res.aov)

TukeyHSD(res.aov)
capture_t <- TukeyHSD(res.aov)

#save it
capture.output(capture_a, file = "spr_anovaresults_processorsW.txt")#save the file
capture.output(capture_t, file = "spr_tuckeyresults_processorsW.txt")#save the file

######----######--

all_cumMonth <- data.table(table(proc$length_cm,proc$month,proc$processor))
all_cum <- data.table(table(proc$length_cm))
all_cum2 <- data.table(table(proc$length_cm,proc$processor))

names(all_cumMonth) <- c("TL", "Month", "Proc","Freq")
names(all_cum) <- c("TL", "Freq")
names(all_cum2) <- c("TL","Proc" ,"Freq")

# weighted average overall and weighted average by month
str(all_cumMonth)
all_cumMonth[,TL:=as.numeric(TL)]
all_cumMonth[,Freq:=as.numeric(Freq)]
all_cumMonth[,wtMean:=weighted.mean(TL,Freq),by="Month"]

all_cum[,TL:=as.numeric(TL)]
all_cum[,wtMean:=weighted.mean(TL,Freq)]

all_cum2[,TL:=as.numeric(TL)]
all_cum2[,wtMean:=weighted.mean(TL,Freq),by="Proc"]

str(all_cumMonth)
table(all_cumMonth$Month)


#put name into months and order them
#all_cumMonth$Month[all_cumMonth$Month=="7"] <- "July"
all_cumMonth$Month[all_cumMonth$Month=="8"] <- "Aug"
all_cumMonth$Month[all_cumMonth$Month=="9"] <- "Sep"
all_cumMonth$Month[all_cumMonth$Month=="10"] <- "Oct"
all_cumMonth$Month[all_cumMonth$Month=="11"] <- "Nov"
#all_cumMonth$Month[all_cumMonth$Month=="12"] <- "December"
#all_cumMonth$Month[all_cumMonth$Month=="1"] <- "January"
#all_cumMonth$Month[all_cumMonth$Month=="2"] <- "February"

all_cumMonth$Month<- factor(all_cumMonth$Month,levels = c("Aug","Sep","Oct", "Nov"),ordered=TRUE)  


## 3.1 Overall LFD----

all.TL1 <- ggplot(all_cum, aes(TL, Freq)) + geom_bar(stat="identity", position="dodge",fill="grey") +
  theme_bw(25) +ylab("N")+xlab("Total length (cm)")+scale_y_continuous(limits=c(0,max(all_cum$Freq)+10), expand=c(0,0))+
  theme(legend.position = "none")+ggtitle(paste(species,"all processors",sep="-"))+
  geom_vline(aes(xintercept=wtMean), col="red", size=1.2)

#ggsave(filename = paste(plot_dir,paste(species,"allprocessors","_TL.png",sep="_"),sep="/"), 
#       plot = all.TL1, width = 25,height = 20, units = "cm", dpi = 300, type = "cairo-png") 


#view the contribution of the processors

all.TL1.2 <- ggplot(all_cum2, aes(TL, Freq,fill=Proc)) + geom_bar(stat="identity", position="dodge",alpha=0.3) +
  theme_bw(25) +ylab("N")+xlab("Total length (cm)")+scale_y_continuous(limits=c(0,max(all_cum2$Freq)+10), expand=c(0,0))+
  theme(legend.position = "none")+ggtitle(paste(species,"all processors",sep="-"))+
  geom_vline(data=all_cum2, aes(xintercept=wtMean, color=Proc),linetype="dashed",size=1.2)+
  theme(legend.position = "bottom")


#ggsave(filename = paste(plot_dir,paste(species,"allprocessors2","_TL.png",sep="_"),sep="/"), 
#       plot = all.TL1.2, width = 25,height = 20, units = "cm", dpi = 300, type = "cairo-png") 


# # Change line colors by groups
# ggplot(proc, aes(x=length_cm, color=processor , fill=processor )) +
#   geom_histogram(aes(y=..density..), position="identity", alpha=0.5)+
#   geom_density(alpha=0.6)+
#   geom_vline(data=all_cum2, aes(xintercept=wtMean, color=Proc),linetype="dashed")+
#   scale_color_manual(values=c("#999999", "#E69F00", "#56B4E9","red"))+
#   scale_fill_manual(values=c("#999999", "#E69F00", "#56B4E9","red"))+
#   labs(title="Weight histogram plot",x="Weight(kg)", y = "Density")+
#   theme_classic()
# 


## 3.2 LFD by month----

all.TL2 <- ggplot(all_cumMonth, aes(TL,Freq, group=factor(Month), fill=factor(Month))) +
  geom_bar(stat="identity",position=position_dodge(2),width=0.45) + 
  theme_bw(25) + scale_y_continuous(limits=c(0,max(all_cumMonth$Freq)+10), expand=c(0,0)) + 
  ylab("N") + xlab("Total length (cm)")+facet_grid(rows=vars(Month),vars(Proc)) +
  geom_vline(aes(xintercept=wtMean), col="red", size=1.2) +
  theme(legend.position="none",strip.text.x=element_text(size=12,colour="black"),
        strip.text.y = element_text(size=12),axis.text.x = element_text(color = "black", size = 10),
        axis.text.y = element_text(color = "black", size = 10),
        axis.title.y = element_text(color = "black", size = 15),axis.title.x = element_text(color = "black", size = 15))
#+ ggtitle("Sprat-All processors") 


#better visulization:m months in columns
all.TL2 <- ggplot(all_cumMonth, aes(TL,Freq, group=factor(Month), fill=factor(Month))) +
  geom_bar(stat="identity",position=position_dodge(2),width=0.45) + 
  theme_bw(25) + scale_y_continuous(limits=c(0,max(all_cumMonth$Freq)+10), expand=c(0,0)) + 
  ylab("N") + xlab("Total length (cm)")+facet_grid(rows=vars(Proc),vars(Month)) +
  geom_vline(aes(xintercept=wtMean), col="red", size=1.2) +
  theme(legend.position="none",strip.text.x=element_text(size=12,colour="black"),
        strip.text.y = element_text(size=12),axis.text.x = element_text(color = "black", size = 10),
        axis.text.y = element_text(color = "black", size = 10),
        axis.title.y = element_text(color = "black", size = 15),axis.title.x = element_text(color = "black", size = 15))
#+ ggtitle("Sprat-All processors") 
all.TL2

#ggsave(filename = paste(plot_dir,paste(species,"allprocessors_TLbyMonth.png",sep="_"),sep="/"), 
#       plot = all.TL2, width = 25,height = 20, units = "cm", dpi = 300, type = "cairo-png") 


#overlapping histograms

all.TL3 <- ggplot(all_cumMonth, aes(TL,fill=Proc)) +
  geom_bar(aes(y=Freq),stat="identity",position="identity",alpha=0.5)+
  theme_bw(25) + scale_y_continuous(limits=c(0,max(all_cumMonth$Freq)+10), expand=c(0,0)) + 
  ylab("N") + xlab("Total length (cm)")+facet_grid(rows=vars(Month)) +
  geom_vline(aes(xintercept=wtMean), col="red", size=1.2)+
  theme(legend.position = "bottom",legend.title = element_text(color = "black", size = 12),
        legend.text = element_text(color = "black", size = 8))+
  scale_fill_viridis_d(name="Processor: ")

#ggsave(filename = paste(plot_dir,paste(species,"allprocessors_TLbyMonth_agg.png",sep="_"),sep="/"), 
#       plot = all.TL3, width = 25,height = 20, units = "cm", dpi = 300, type = "cairo-png") 



#3.3 LWR all processors----

db <- proc

head(db)
table(db$processor)

ggplot(db, aes(length_cm ,weight_g, group=processor , col=processor , shape=processor)) + geom_point()

#with non transformed data
ggplot(db, aes(length_cm, weight_g, col=processor)) + geom_point() +
  geom_smooth(method="lm", se=FALSE)+theme_bw()

nonLWR <- ggplot(db, aes(length_cm,weight_g))+
  geom_point(aes(color = processor)) +
  geom_smooth(aes(color = processor, fill = processor), method = "lm") + 
  scale_color_brewer(palette="Dark2")+
  scale_fill_brewer(palette="Dark2") +
  theme_bw() +
  theme(legend.position = "bottom")


#ggsave(filename = paste(plot_dir,paste(species,"allprocessors_NONLWR.png",sep="_"),sep="/"), 
#       plot = nonLWR, width = 30,height = 20, units = "cm", dpi = 300, type = "cairo-png") 


#with transformed data
ggplot(db, aes(log(length_cm), log(weight_g), col=processor)) + geom_point() +
  geom_smooth(method="lm", se=TRUE)+theme_bw()+
  scale_color_brewer(palette = "Dark2")


#darker palette
LWR <- ggplot(db, aes(log(length_cm),log(weight_g)))+
  geom_point(aes(color = processor)) +
  geom_smooth(aes(color = processor, fill = processor), method = "lm") + 
  scale_color_brewer(palette="Dark2")+
  scale_fill_brewer(palette="Dark2") +
  theme_bw() +
  theme(legend.position = "bottom")

#ggsave(filename = paste(plot_dir,paste(species,"allprocessors_LWR.png",sep="_"),sep="/"), 
#       plot = LWR, width = 30,height = 20, units = "cm", dpi = 300, type = "cairo-png") 



###################################### END #############################################################################-----
