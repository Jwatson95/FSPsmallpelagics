#Plotting data for PIL from processors FSP
#@silvia Rodriguez Climent
# 17-02-2021; last modified 09/09/2022
#---------------------------------------------------------------------## 

rm(list=ls())
# set input, output directories
inp_dir <- file.path(getwd(), "Data/Processors/PIL/")
plot_dir <- file.path(getwd(), "Data/plots/PIL")
out_dir <- file.path(getwd(), "Data/Output/")
list.files(out_dir,recursive=TRUE) # recursive TRUE to see inside the different folders

# load libraries----
packages <- c("ggplot2","data.table","xlsx","openxlsx","dplyr","readxl","stringr","plyr","tidyr","reshape2",
              "maps","mapdata","mapproj","mapplots","lubridate","rgdal","raster","GISTools","ggspatial","XLConnect",
              "openxlsx","sjmisc","viridis","ggpubr")

lib(packages) #install packages through own function


#get all the final plots from all the processors
list.files(out_dir, pattern="PIL*")

# ===================================================--
# 0. Latest unify file all processors PIL ----
# ===================================================--

# File produced in script: A1. PIL_DataExtraction_Processor #

#PIL_processorsLW_2021 it includes all the processors with L and W data: Falfish, Interfish and Oceanfish
#PIL_processors_2021 includes all the processors data was collected: Falfish, Interfish, Oceanfish and Coombefisheries (only W)

#pilproc <- read.table(paste(out_dir,"/PIL_processorsLW_2021.csv",sep=''),sep=",",header=TRUE,stringsAsFactors = F)
#head(pilproc);dim(pilproc) # 21255 12

pilproc <- read.table(paste(out_dir,"/PIL_processors_2022.csv",sep=''),sep=",",header=TRUE,stringsAsFactors = F)
head(pilproc);dim(pilproc) # 7551  12 data until february, only Falfish and Interfish
summary(pilproc)

table(pilproc$month)
table(pilproc$processor)
pilproc$processor <- toupper(pilproc$processor)


# ===================================================--
# 1. Length distribution plots in a loop ----
# ===================================================--

#your data
db <- pilproc
#your species
species=unique(db$species)
#your variable
var <- c(unique(db$processor))
#variable you want to plot
db$sel <- db$processor

#loop (check names are correct in line 27 or change them)

plot1.TL1 <- list()
plot1.TL2 <- list()
#plot1.TL3 <- list()

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


#just for Falfish this month to see the recruitment

plot1.TL2[[i]] <- ggplot(plot1_cumMonth, aes(TL,Freq, group=factor(Month), fill=factor(Month))) + geom_bar(stat="identity") + 
  theme_bw(25) + 
  #scale_y_continuous(limits=c(0,max(plot1_cumMonth$Freq)+10), expand=c(0,0)) + 
  ylab("N") + xlab("Total length (cm)")+ facet_grid(rows = vars(Month),scales="free") + #facet_grid(.~Month) by column
  geom_vline(aes(xintercept=wtMean), col="red", size=1.2,lty=2) +
  theme(legend.position="none")+
  ggtitle(paste(var[i])) 

plot1.TL3[[i]] <- plot1.TL2[[i]]+theme(text = element_text(size=rel(5.0)))+ scale_fill_brewer(palette="Set1")

ggsave(filename = paste(plot_dir,paste(var[i],species,"bTLbyMonth_freescale.png",sep="_"),sep="/"), 
       plot = plot1.TL2[[i]], width = 25, 
       height = 20, units = "cm", dpi = 300, type = "cairo-png") 

# ===================================================--
# 2. Adjust a linear LWR plots in a loop ----
# ===================================================--

#need to do 2 this year: one for the recruits (5-8.5), and one for the adults (13-24)
head(pilproc)
table(pilproc$length_cm)
db0 <-subset(pilproc,length_cm<13.0)
db1 <- subset(pilproc,length_cm>8.5)
#########################################################################################--

#your data
db <- db0
#db <- db1

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
  
  #ggsave(filename = paste(plot_dir,paste(var[i],species,"LWrelpetite.png",sep="_"),sep="/"), 
  #       plot = SP_LWrel[[i]], width = 25, 
  #      height = 20, units = "cm", dpi = 300, type = "cairo-png") 
  
  
  ggsave(filename = paste(plot_dir,paste(var[i],species,"LWrel.png",sep="_"),sep="/"), 
         plot = SP_LWrel[[i]], width = 25, 
         height = 20, units = "cm", dpi = 300, type = "cairo-png") 
  
}


print(SP_LWrel[[1]])
print(SP_LWrel[[2]])
print(SP_LWrel[[3]])


# ===================================================--
# 2.1 Analize deeper INTERFISH relationship ----
# ===================================================--

head(pilproc)

db2 <- subset(pilproc,processor=="Interfish")
summary(db2)

db <- db2
#db <- pilproc #analyse for the whole database

db$month[db$month=="7"] <- "Jul"
db$month[db$month=="8"] <- "Aug"
db$month[db$month=="9"] <- "Sep"
db$month[db$month=="10"] <- "Oct"
db$month[db$month=="11"] <- "Nov"
db$month[db$month=="12"] <- "Dec"
db$month[db$month=="1"] <- "Jan"
db$month[db$month=="2"] <- "Feb"

#db$month <- factor(db$month,levels = c("Jul","Aug","Sep","Oct","Nov","Dec","Jan","Feb"),ordered=TRUE)  

table(db$month)

#your variable
var <- c(unique(db$month))
#variable you want to plot
db$sel <- db$month

plot1=db[db$sel==var[1],]
plot1


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
  
  composition <- ggarrange(SP_LWrel[[1]],SP_LWrel[[2]],SP_LWrel[[3]],SP_LWrel[[4]],SP_LWrel[[5]],
                           SP_LWrel[[6]],nrow=2,ncol=3)
  
  ggsave(filename = paste(plot_dir,paste("Interfishallmonths",species,"LWrel.png",sep="_"),sep="/"), 
         plot = composition, width = 25, 
         height = 20, units = "cm", dpi = 300, type = "cairo-png")
  
}



ggarrange(SP_LWrel[[1]],SP_LWrel[[2]],SP_LWrel[[3]],SP_LWrel[[4]],SP_LWrel[[5]],
          SP_LWrel[[6]],nrow=2,ncol=3)



par(mfrow=c(3,3))
print(SP_LWrel[[1]])
print(SP_LWrel[[2]])
print(SP_LWrel[[3]])
print(SP_LWrel[[4]])
print(SP_LWrel[[5]])
print(SP_LWrel[[6]])


#another visualization

#by month
db$month <- factor(db$month,levels = c("Jul","Aug","Sep","Oct","Nov","Dec","Jan","Feb"),ordered=TRUE)  

month <- ggplot(db, aes(length_cm, weight_g, col=month)) + geom_point() +
  geom_smooth(method="lm", se=FALSE)+theme_bw()#+scale_color_brewer(palette="Dark2")


ggsave(filename = paste(plot_dir,paste("Interfish",species,"LWrel_bymonth.png",sep="_"),sep="/"), 
       plot = month, width = 25, 
       height = 20, units = "cm", dpi = 300, type = "cairo-png")


#by vessel
vessel <- ggplot(db, aes(length_cm, weight_g, col=vessel)) + geom_point() +
  geom_smooth(method="lm", se=FALSE)+theme_bw()

ggsave(filename = paste(plot_dir,paste("Interfish",species,"LWrel_byvessel.png",sep="_"),sep="/"), 
       plot = vessel, width = 25, 
       height = 20, units = "cm", dpi = 300, type = "cairo-png")


#by vessel and month
vesmonth <- ggplot(db, aes(length_cm, weight_g, col=month)) + geom_point() +
  geom_smooth(method="lm", se=FALSE)+theme_bw()+
  facet_grid(vessel~month)

ggsave(filename = paste(plot_dir,paste("Interfish",species,"LWrel_byvessel.png",sep="_"),sep="/"), 
       plot = vesmonth, width = 35, 
       height = 20, units = "cm", dpi = 300, type = "cairo-png")


#by vessel and month (2)
vesmonth2 <- ggplot(db, aes(length_cm, weight_g, col=month)) + geom_point() +
  geom_smooth(method="lm", se=FALSE)+theme_bw()+
  facet_wrap(~vessel)

ggsave(filename = paste(plot_dir,paste("Interfish",species,"LWrel_byvessel.png",sep="_"),sep="/"), 
       plot = vesmonth2, width = 35, 
       height = 20, units = "cm", dpi = 300, type = "cairo-png")



#see if the differences are statistically significant
head(db)

#need a summary table with the Length-weight regressions per month and vessel

#
db.lm =  lm(length_cm ~ factor(weight_g)*vessel, data=db)
anova(db.lm)
summary(db.lm)

#
db.lm2 =  lm(length_cm ~ factor(weight_g)*month, data=db)
anova(db.lm2)
summary(db.lm2)

#
db.lm3 =  lm(length_cm ~ factor(weight_g)*vessel*month, data=db)
anova(db.lm3)
summary(db.lm3)


AIC(db.lm,db.lm2,db.lm3)


# ===================================================--
# 3. Adjust NON linear LWR plots in a loop ----
# ===================================================--

#Divide your database in case you have 2 clear size-structured populations
# head(pilproc)
# table(pilproc$length_cm)
# db0 <-subset(pilproc,length_cm<13.0)
# table(db0$length_cm)
# db1 <- subset(pilproc,length_cm>8.5)
# table(db1$length_cm)
#########################################################################################--

#your data
#db <- db0
db <- db1
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
    annotate("text", label=paste("a = ", round(coefficients(nls_fit_w11)[["a"]],digits=5), 
                                 " b =", round(coefficients(nls_fit_w11)[["b"]],digits=5)), x=15,y=125) +
    ggtitle(paste(var[i]))
  
  ggsave(filename = paste(plot_dir,paste(var[i],species,"NONLWrel.png",sep="_"),sep="/"), 
         plot = SP_NONLWrel[[i]], width = 25, 
         height = 20, units = "cm", dpi = 300, type = "cairo-png") 
}

print(SP_NONLWrel[[1]])
print(SP_NONLWrel[[2]])
print(SP_NONLWrel[[3]])


# ===================================================--
# 4. All processors ----
# ===================================================--

# prepare data for the length-weight plots
head(pilproc);dim (pilproc)
summary(pilproc)
pilproc <- pilproc %>% drop_na(length_cm,weight_g)
summary(pilproc$length_cm)
#pilproc <- pilproc %>% filter(length_cm > "10")

#4.0 check for statistical differences-----
#explore the data visually

## a) length----
#library("ggpubr")
ggboxplot(pilproc, x = "processor", y = "length_cm", 
          color = "processor", palette = c("#00AFBB", "#E7B800", "#FC4E07"),
          order = c("OCEANFISH", "FALFISH", "INTERFISH"),
          ylab = "length", xlab = "processor")

ggline(pilproc, x = "processor", y = "length_cm", 
       add = c("mean_se", "jitter"), 
       order = c("OCEANFISH", "FALFISH", "INTERFISH"),
       ylab = "length", xlab = "processor")

# Box plot
par(mfrow=c(1,1))
boxplot(length_cm ~ processor, data = pilproc,
        xlab = "length", ylab = "processor",
        frame = FALSE, col = c("#00AFBB", "#E7B800", "#FC4E07"))
boxplot(pilproc$length_cm~pilproc$processor)

#library("gplots")
plotmeans(length_cm ~ processor, data = pilproc, frame = FALSE,
          xlab = "length", ylab = "processor",
          main="Mean Plot with 95% CI") 

# Compute the analysis of variance
res.aov <- aov(length_cm ~ processor, data = pilproc)
# Summary of the analysis
summary(res.aov) #significant differences
capture_a <- summary(res.aov)

TukeyHSD(res.aov)
capture_t <- TukeyHSD(res.aov)

#save it
capture.output(capture_a, file = "pil_anovaresults_processors.txt")#save the file, goes to workspace
capture.output(capture_t, file = "pil_tuckeyresults_processors.txt")#save the file, goes to workspace



##b) weight----

library("ggpubr")
ggboxplot(pilproc, x = "processor", y = "weight_g",
          color = "processor", palette = c("#00AFBB", "#E7B800", "#FC4E07"),
          order = c("OCEANFISH", "FALFISH", "INTERFISH"),
          ylab = "weight", xlab = "processor")

ggline(pilproc, x = "processor", y = "weight_g",
       add = c("mean_se", "jitter"),
       order = c("OCEANFISH", "FALFISH", "INTERFISH"),
       ylab = "length", xlab = "processor")

# Box plot
boxplot(weight_g ~ processor, data = pilproc,
        xlab = "processor", ylab = "weight",
        frame = FALSE, col = c("#00AFBB", "#E7B800", "#FC4E07"))
boxplot(pilproc$weight_g~pilproc$processor)

# plotmeans (library gplots)
plotmeans(weight_g ~ processor, data = pilproc, frame = FALSE,
          xlab = "processor", ylab = "weight",
          main="Mean Plot with 95% CI")

# Compute the analysis of variance
res.aov <- aov(weight_g ~ processor, data = pilproc)
# Summary of the analysis
summary(res.aov) #significant differences
capture_a <- summary(res.aov)

TukeyHSD(res.aov)
capture_t <- TukeyHSD(res.aov)

#save it
capture.output(capture_a, file = "pil_anovaresults_processorsW.txt")#save the file
capture.output(capture_t, file = "pil_tuckeyresults_processorsW.txt")#save the file

######----######--

all_cumMonth <- data.table(table(pilproc$length_cm,pilproc$month,pilproc$processor))
all_cum <- data.table(table(pilproc$length_cm))
names(all_cumMonth) <- c("TL", "Month", "Proc","Freq")
names(all_cum) <- c("TL", "Freq")

# weighted average overall and weighted average by month
str(all_cumMonth)
all_cumMonth[,TL:=as.numeric(TL)]
all_cumMonth[,Freq:=as.numeric(Freq)]
all_cumMonth[,wtMean:=weighted.mean(TL,Freq),by="Month"]

all_cum[,TL:=as.numeric(TL)]
all_cum[,wtMean:=weighted.mean(TL,Freq)]

str(all_cumMonth)
table(all_cumMonth$Month)

#put name into months and order them
all_cumMonth$Month[all_cumMonth$Month=="7"] <- "Jul"
all_cumMonth$Month[all_cumMonth$Month=="8"] <- "Aug"
all_cumMonth$Month[all_cumMonth$Month=="9"] <- "Sep"
all_cumMonth$Month[all_cumMonth$Month=="10"] <- "Oct"
all_cumMonth$Month[all_cumMonth$Month=="11"] <- "Nov"
all_cumMonth$Month[all_cumMonth$Month=="12"] <- "Dec"
all_cumMonth$Month[all_cumMonth$Month=="1"] <- "Jan"
all_cumMonth$Month[all_cumMonth$Month=="2"] <- "Feb"

all_cumMonth$Month<- factor(all_cumMonth$Month,levels = c("Jul","Aug","Sep","Oct", "Nov","Dec","Jan","Feb"),ordered=TRUE)  


## Overall LFD----

all.TL1 <- ggplot(all_cum, aes(TL, Freq)) + geom_bar(stat="identity", position="dodge",fill="grey") +
  theme_bw(25) +ylab("N")+xlab("Total length (cm)")+scale_y_continuous(limits=c(0,max(all_cum$Freq)+50), expand=c(0,0))+
  theme(legend.position = "none")+ggtitle(paste(species,"all processors",sep="_"))+
  geom_vline(aes(xintercept=wtMean), col="red", size=1.2,linetype=2)

ggsave(filename = paste(plot_dir,paste(species,"allprocessors","aTL.png",sep="_"),sep="/"), 
       plot = all.TL1, width = 25,height = 20, units = "cm", dpi = 300, type = "cairo-png") 


## LFD by month----

all.TL2 <- ggplot(all_cumMonth, aes(TL,Freq, group=factor(Month), fill=factor(Month))) +
  geom_bar(stat="identity",position=position_dodge(2),width=0.45) + 
  theme_bw(25) + scale_y_continuous(limits=c(0,max(all_cumMonth$Freq)+10), expand=c(0,0)) + 
  ylab("N") + xlab("Total length (cm)")+facet_grid(rows=vars(Month),vars(Proc)) +
  geom_vline(aes(xintercept=wtMean), col="red", size=1.2,linetype=2) +
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
  geom_vline(aes(xintercept=wtMean), col="red", size=1.2,linetype=2) +
  theme(legend.position="none",strip.text.x=element_text(size=12,colour="black"),
        strip.text.y = element_text(size=12),axis.text.x = element_text(color = "black", size = 10),
        axis.text.y = element_text(color = "black", size = 10),
        axis.title.y = element_text(color = "black", size = 15),axis.title.x = element_text(color = "black", size = 15))
#+ ggtitle("Sprat-All processors") 

ggsave(filename = paste(plot_dir,paste(species,"allprocessors_TLbyMonth.png",sep="_"),sep="/"), 
       plot = all.TL2, width = 30,height = 20, units = "cm", dpi = 300, type = "cairo-png") 



# 5. How the LWR compare -----

head(pilproc);dim (pilproc) # 7551 12
summary(pilproc)
pilproc <- pilproc %>% drop_na(length_cm,weight_g)  
summary(pilproc$length_cm)

db <- db1 # we only select individuals from 13.0-24.0

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


ggsave(filename = paste(plot_dir,paste(species,"allprocessors1324_NONLWR.png",sep="_"),sep="/"), 
       plot = nonLWR, width = 30,height = 20, units = "cm", dpi = 300, type = "cairo-png") 



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



ggsave(filename = paste(plot_dir,paste(species,"allprocessors1324_LWR.png",sep="_"),sep="/"), 
       plot = LWR, width = 30,height = 20, units = "cm", dpi = 300, type = "cairo-png") 



# #another palette (to bright)
# ggplot(db, aes(log(length_cm), log(weight_g)))+
#   geom_point(aes(color = processor)) +
#   geom_smooth(aes(color = processor, fill = processor), method = "lm") + 
#   scale_color_viridis(discrete = TRUE, option = "D")+
#   scale_fill_viridis(discrete = TRUE) +
#   theme_bw() +
#   theme(legend.position = "bottom")



#6. Coombefisheries only weigt data (analyse separate)----

#pilproc2 <- read.table(paste(out_dir,"/PIL_processors_2021.csv",sep=''),sep=",",header=TRUE,stringsAsFactors = F)
# head(pilproc2);dim(pilproc2) # 28939 12 CoombeFisheries only Weight data
# summary(pilproc2)
# 
# table(pilproc2$month)
# table(pilproc2$processor)
# pilproc2$processor <- toupper(pilproc2$processor)
# 
# 
# plot1 <- subset(pilproc2,processor=="COMBEEFISHERIES")
# str(plot1)
# 
# #weight variation plot
# ggplot(plot1,aes(x=weight_g))+geom_density(alpha=0.4)
# 
# plot1 <- (subset(plot1,weight_g>0))
# 
# combe <- ggplot(plot1,aes(x=weight_g,group=month,fill=as.factor(month)))+
#       geom_density(alpha=0.3)+ theme_bw()+
#       labs(title="",x="Weight (g)", y = "Density")+
#       theme(legend.position=c(0.8, 0.7))+
#       scale_fill_discrete(name = "Month", labels = c("Jan", "Feb", "Sep","Oct","Nov","Dec"))
# 
# 
# ggsave(filename = paste(plot_dir,paste(species,"Coombefisheries_WbyMonth.png",sep="_"),sep="/"), 
#        plot = combe, width = 30,height = 20, units = "cm", dpi = 300, type = "cairo-png") 
# 



#7.Data for the report-----
head(db);str(db)
db2 <-data.frame(db) 

db %>% 
  group_by(date())%>%
  summarize(meanL=mean(length_cm),
            minL=min(length_cm),
            maxL=max(length_cm),
            meanW=mean(weight_g),
            minW=min(weight_g),
            maxW=max(weight_g))





#8.Evolution of fat content (Interfish only)----

interfat <- read.csv(paste(out_dir,"PIL_Interfish_fat.csv",sep="/"))
head(interfat)
interfat$date <- as.Date(interfat$date)

summary(interfat2)
interfat <- interfat[!is.na(interfat$fat),]


meanfat <- mean(interfat$fat)
plot(interfat$fat~interfat$date,type="l",col="green",lty=1,lwd=3,
     main="",xlab="date",ylab="sardine fat content (%)")
abline(h=meanfat , col = "red",lty=2,lwd=3)

#save the file
# dev.print(device=png,
#           filename=paste(plot_dir,"Interfish_evolutionfat.png",sep='/'),
#           width=800,height=800)



##Graph with mean of fat variation per month-----
all_cumMonthfat <- data.table(table(interfat$fat,interfat$month,interfat$date))
names(all_cumMonthfat) <- c("Fat", "Month","Date","Freq")

# weighted average overall and weighted average by month
str(all_cumMonthfat)
all_cumMonthfat[,Fat:=as.numeric(Fat)]
all_cumMonthfat[,Freq:=as.numeric(Freq)]
all_cumMonthfat[,wtMean:=weighted.mean(Fat,Freq),by="Month"]

#put name into months and order them
all_cumMonthfat$Month[all_cumMonthfat$Month=="10"] <- "Oct"
all_cumMonthfat$Month[all_cumMonthfat$Month=="11"] <- "Nov"
all_cumMonthfat$Month[all_cumMonthfat$Month=="12"] <- "Dec"
all_cumMonthfat$Month[all_cumMonthfat$Month=="1"] <- "Jan"
all_cumMonthfat$Month[all_cumMonthfat$Month=="2"] <- "Feb"

all_cumMonthfat$Month<- factor(all_cumMonthfat$Month,levels = c("Oct", "Nov","Dec","Jan","Feb"),ordered=TRUE)  


## Overall evolution of the fat content----

all.TL1 <- ggplot(interfat, aes(date, fat)) + geom_line() + geom_point()+
  theme_bw(25) +ylab("Fat content")+xlab("date")+
  theme(legend.position = "none")+ggtitle("")+
  geom_hline(aes(yintercept=meanfat), col="red", size=0.5,linetype="dashed")

ggsave(filename = paste(plot_dir,paste("Interfish_fatcontent.png",sep="_"),sep="/"), 
       plot = all.TL1, width = 25,height = 20, units = "cm", dpi = 300, type = "cairo-png") 

###################################### END #############################################################################
