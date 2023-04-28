### MS plots

## Directory
wd <- 'C:/Users/Amelie/OneDrive - UW-Madison/Documents/PhDUW/PhD/Github/Reproduction_number'
setwd(wd)


lapply(c('ggplot2','ggpubr','tidyverse', 'dplyr', 'scales',
         'lubridate', 'rphylopic', 'scales', 'png','patchwork'), library, character.only=TRUE)


Zeb_pic <- readPNG("Zebra_silhouette.png", native = TRUE)
Wil_pic <- readPNG("Wildebeest.png", native = TRUE)


## DATA
Extrapolation_10Y <- read.csv('Extrapolation.csv', sep=',')
Location_CFU <- read.csv('Location_CFU.csv', sep=',')
Water_Pan_Camera <- read.csv('Location_Camera.csv', sep=',')


Ind_Z <- subset(read.csv('Camera_all_individuals.csv', header=T, sep=','), Species =="Zebra")
Ind_Z$Season <- factor(Ind_Z$Season, levels= c('Hot_wet','Cool_dry','Hot_dry'))
Ind_Z <- merge(Ind_Z, Water_Pan_Camera, by='ID', all.x=T, sort=F)
Ind_Z$Season <- factor(Ind_Z$Season, levels= c('Hot_wet','Cool_dry','Hot_dry'))
Ind_Z$dTreatment <- ifelse(Ind_Z$Treatment == 'Carcass', 1, 0)


Ind_W <- subset(read.csv('Camera_all_individuals.csv', header=T, sep=','), Species =="Wildebeest")
Ind_W$Season <- factor(Ind_W$Season, levels= c('Hot_wet','Cool_dry','Hot_dry'))
Ind_W <- merge(Ind_W, Water_Pan_Camera, by='ID', all.x=T, sort=F)
Ind_W$Season <- factor(Ind_W$Season, levels= c('Hot_wet','Cool_dry','Hot_dry'))
Ind_W$dTreatment <- ifelse(Ind_W$Treatment == 'Carcass', 1, 0)

##### PLOT 1


Total_Ind <- rbind(Ind_Z, Ind_W)

Total_Ind$Species <- factor(Total_Ind$Species, levels = c('Zebra', 'Wildebeest'))
Total_Ind$Treatment <- ifelse(Total_Ind$Treatment == 'Carcass', 'Reservoir','Control')
Total_Ind <- subset(Total_Ind, Age %in% c('A','SA') & Sex %in% c('F','M'))

Total_Ind$Demo <- paste0(Total_Ind$Age,Total_Ind$Sex)

Total_Ind <- Total_Ind %>% mutate(Demographic = case_when(
  Demo == 'AF' ~ 'Adult \nFemale',
  Demo == 'SAF' ~ 'Sub-adult \nFemale',
  Demo == 'AM' ~ 'Adult \nMale',
  Demo == 'SAM' ~ 'Sub-adult \nMale'
))

Plot1Z <- ggplot(subset(Total_Ind, Species == 'Zebra'), aes(x=Demographic, fill = Treatment))+
  geom_bar(position = position_dodge2(), alpha=0.5)+
  geom_bar(data=subset(Total_Ind, TG > 0 & Species == 'Zebra'), position = position_dodge2())+
  scale_fill_manual(name = '',values= c('blue','red'))+
  scale_y_continuous(breaks = seq(0,2500, 250))+
  xlab('') + ylab('Number of individuals')+
  theme_pubclean()+
  theme(axis.title = element_text(size=8),
        axis.title.x = element_blank(),
        axis.text = element_text(size=6, color='black'),
        legend.title = element_text(size=8),
        legend.key.size = unit(0.25, "cm"),
        axis.ticks.y = element_line(linewidth = 0.2),
        axis.line = element_line(color = 'black',linewidth = 0.2),
        legend.text = element_text(size=6, color='black'),
        strip.background = element_rect(colour="black",fill="white"),
        panel.grid.major.y = element_line(linewidth = 0.1),
        strip.text = element_text(size = 8),
        title = element_text(size = 6))+
  ggtitle('a) Zebra')

Plot1W <- ggplot(subset(Total_Ind, Species == 'Wildebeest'), aes(x=Demographic, fill = Treatment))+
  geom_bar(position = position_dodge2(), alpha=0.5)+
  geom_bar(data=subset(Total_Ind, TG > 0 & Species == 'Wildebeest'), position = position_dodge2())+
  scale_fill_manual(name = '',values= c('blue','red'))+
  scale_y_continuous(breaks = seq(0,300, 50))+
  xlab('') + ylab('Number of individuals')+
  theme_pubclean()+
  theme(axis.title = element_text(size=8),
        axis.title.x = element_blank(),
        axis.text = element_text(size=6, color='black'),
        legend.title = element_text(size=8),
        legend.key.size = unit(0.25, "cm"),
        axis.ticks.y = element_line(linewidth = 0.2),
        axis.line = element_line(color = 'black',linewidth = 0.2),
        legend.text = element_text(size=6, color='black'),
        strip.background = element_rect(colour="black",fill="white"),
        panel.grid.major.y = element_line(linewidth = 0.1),
        strip.text = element_text(size = 8),
        title = element_text(size = 6))+
  ggtitle('b) Wildebeest')



Plot1 <- ggarrange(Plot1Z, Plot1W, common.legend = T, ncol = 1)
Plot1 <- Plot1 + inset_element(p = Zeb_pic, left = 0.8, bottom = 0.8, right = 0.9,  top = 0.9)
Plot1<- Plot1 + inset_element(p = Wil_pic, left = 0.8, bottom = 0.35,  right = 0.9,  top = 0.45)


tiff("Figure1.tif",
     compression = "lzw",width=6,height=9,units="cm",res=200)
plot(Plot1)
dev.off()



### PLOT 2

CFU_plot <- rbind(Extrapolation_10Y[1:3], 
                  setNames(Extrapolation_10Y[,c(1,2,5)], names(Extrapolation_10Y)[1:3]))
CFU_plot$type <- rep(c('Soil','Grass'), each = 442)

CFU_Mean_yad <- CFU_plot %>% group_by(Age, type) %>%
  summarise(CFU = mean(Soil_CFU),
            CFU_sd = sd(Soil_CFU))



CFU_p <- ggplot(CFU_plot, aes(x= as.factor(Age), y = Soil_CFU+1, fill = type))+
  geom_boxplot(size = 0.2, outlier.size = 0.5)+
  xlab('Year after death') + ylab('Concentration of spores / g ')+
  scale_y_log10(limits=c(1, 2000000000), # Says you want the logarithmic scale axis
                breaks=c(1,10,100,1000,10000,100000,1000000,10000000,100000000, 1000000000),
                labels=c('0','10','100','1,000', '10,000','100,000', '1,000,000', '10,000,000','100,000,000', '1,000,000,000')) + 
  annotation_logticks(sides = "l", size = 0.2, short = unit(0.05, 'cm'), mid = unit(0.1, 'cm'), long=unit(0.15, 'cm') )+
  scale_fill_manual(name = '',values= c('chartreuse4','chocolate4'))+
  theme_pubclean()+
  theme(axis.title = element_text(size=8),
        axis.text = element_text(size=6, color='black'),
        legend.title = element_text(size=8),
        legend.key.size = unit(0.25, "cm"),
        axis.ticks.y = element_blank(),
        legend.text = element_text(size=6, color='black'),
        axis.line = element_line(color = 'black',linewidth = 0.2),
        panel.grid.major.y = element_line(linewidth = 0.1),
        strip.background = element_rect(colour="black",fill="white"))
# 

tiff("Figure2.tif",
     compression = "lzw",width=9,height=6,units="cm",res=200)

plot(CFU_p)
dev.off()


#### PLOT 3

Data_analysis <- function(data){
  
  Data_YDR <- data %>% group_by(LD, Year, Rep, Distance, Prop, Wwet, Wdry) %>%
    summarise(Infection = sum(Infected, na.rm=T),
              NV = sum(Visits, na.rm=T),
              NG = sum(Grazing, na.rm=T))
  
  Data_Y <- Data_YDR %>% group_by(LD, Year, Prop, Wwet, Wdry) %>%
    summarise(Infected = mean(Infection),
              Infected_sd = sd(Infection),
              NV = mean(NV),
              NG = mean(NG))
  
  Data_Ymean <- Data_YDR %>% group_by(LD, Year) %>%
    summarise(Infected = mean(Infection),
              Infected_sd = sd(Infection),
              NV = mean(NV),
              NG = mean(NG))
  
  Data_Ymean_space <- subset(Data_YDR, Wwet %in% c(0, 0.1, 0.2, 0.3, 0.4) & Wdry %in% c(0, 0.1, 0.2, 0.3, 0.4)) %>% group_by(LD, Year) %>%
    summarise(Infected = mean(Infection),
              Infected_sd = sd(Infection),
              NV = mean(NV),
              NG = mean(NG))
  
  Data_Y$SD_min = ifelse(Data_Y$Infected - Data_Y$Infected_sd < 0 , 0, Data_Y$Infected - Data_Y$Infected_sd )
  Data_Y$SD_max = Data_Y$Infected + Data_Y$Infected_sd 
  
  Data_Y$Infected_r <- round(Data_Y$Infected, 1)

  Data_Y$Wwet <- Data_Y$Wwet *100
  Data_Y$Wdry <- Data_Y$Wdry *100
  
  Data_Y_R0 <- Data_Y %>% group_by(LD, Prop, Wwet, Wdry) %>%
    summarise(Infected = sum(Infected))
  
  Data_Y_R0$Infected_r <- round(Data_Y_R0$Infected, 1) 
  
  return(Data_Y_R0)
}


plot_fig3 <- function(data, tit, lims){
  
  plot3 <- ggplot(data , aes(x = as.factor(Wwet), y = as.factor(Wdry), fill = Infected)) + 
    geom_tile(color = "white",  lwd = 0.1,  linetype = 1)+
    geom_segment(aes(x=5.5,xend=5.5, y=0.2,yend=5.5), size = 1)+
    geom_segment(aes(x=4.5,xend=4.5, y=4.5,yend=5.5), size = 1)+
    geom_segment(aes(x=3.5,xend=3.5, y=3.5,yend=4.5), size = 1)+
    geom_segment(aes(x=2.5,xend=2.5, y=2.5,yend=3.5), size = 1)+
    geom_segment(aes(x=1.5,xend=1.5, y=1.5, yend=2.5), size = 1)+
    geom_segment(aes(x=4.5,xend=5.5, y=5.5,yend=5.5), size = 1)+
    geom_segment(aes(x=3.5,xend=4.5, y=4.5,yend=4.5), size = 1)+
    geom_segment(aes(x=2.5,xend=3.5, y=3.5,yend=3.5), size = 1)+
    geom_segment(aes(x=1.5,xend=2.5, y=2.5,yend=2.5), size = 1)+
    geom_segment(aes(x=0.5,xend=1.5, y=1.5,yend=1.5), size = 1)+
    geom_text(aes(label = Infected_r), color = "white", size = 1.5) +
    xlab('')+
    ylab('')+
    scale_fill_gradient2('R ', low = 'blue', mid = muted('blue'), high = 'red', limits=lims) +
    theme_pubclean()+
    theme(axis.title = element_text(size=8),
        axis.text = element_text(size=6, color='black'),
        legend.title = element_text(size=8),
        legend.key.size = unit(0.25, "cm"),
        legend.text = element_text(size=6, color='black'),
        axis.line = element_line(color = 'black',linewidth = 0.2),
        strip.text = element_text(size = 1 ),
       strip.background = element_rect(colour="white",fill="white"),
       title = element_text(size = 6))+
    ggtitle(tit)
  
  return(plot3)
}


Zebra_average <- read.csv('Final_data/Zebra_Final_100.csv')
ZebraW_dryP <- read.csv('Final_data/Zebra__Dry_Pathogen_Wet_visit_100.csv')
ZebraW_wetP <- read.csv('Final_data/Zebra__Wet_Pathogen_Wet_visit_100.csv')
ZebraD_dryP <- read.csv('Final_data/Zebra__Dry_Pathogen_Dry_visit_100.csv')
ZebraD_wetP <- read.csv('Final_data/Zebra__Wet_Pathogen_Dry_visit_100.csv')


Zebra_average_RO <- Data_analysis(Zebra_average)
ZebraW_dryP_R0 <- Data_analysis(ZebraW_dryP)
ZebraW_wetP_R0 <- Data_analysis(ZebraW_wetP)
ZebraD_dryP_R0 <- Data_analysis(ZebraD_dryP)
ZebraD_wetP_R0 <- Data_analysis(ZebraD_wetP)


Plot3_Reading_key <- ggplot(data = subset(Zebra_average_RO, LD == 1e7), aes(x = as.factor(Wwet), y = as.factor(Wdry))) + 
  geom_tile(color = "grey95",  fill = 'white', lwd = 1,  linetype = 0.1)+
  
  geom_abline(intercept = 0, slope = 1, col = 'red', linewidth = 0.1, lty = 3) +

  geom_segment(aes(x=5.5,xend=5.5, y=0.2,yend=5.5), size = 0.2)+
  geom_segment(aes(x=4.5,xend=4.5, y=4.5,yend=5.5), size = 0.2)+
  geom_segment(aes(x=3.5,xend=3.5, y=3.5,yend=4.5), size = 0.2)+
  geom_segment(aes(x=2.5,xend=2.5, y=2.5,yend=3.5), size = 0.2)+
  geom_segment(aes(x=1.5,xend=1.5, y=1.5, yend=2.5), size = 0.2)+
  geom_segment(aes(x=4.5,xend=5.5, y=5.5,yend=5.5), size = 0.2)+
  geom_segment(aes(x=3.5,xend=4.5, y=4.5,yend=4.5), size = 0.2)+
  geom_segment(aes(x=2.5,xend=3.5, y=3.5,yend=3.5), size = 0.2)+
  geom_segment(aes(x=1.5,xend=2.5, y=2.5,yend=2.5), size = 0.2)+
  geom_segment(aes(x=0.5,xend=1.5, y=1.5,yend=1.5), size = 0.2)+
  
  geom_segment(aes(x = 6, y = 5.9, xend = 9, yend = 2.5), col ='blue',
               arrow = arrow(length = unit(0.5, "cm")), size = 0.2)+
  
  geom_segment(aes(x = 5.9, y = 6, xend = 3, yend = 9.5), col = 'orange',
               arrow = arrow(length = unit(0.5, "cm")), size = 0.2)+
  
  annotate("text", x = 9.2, y = 1.5, size = 2, col = 'blue',
           label = "Higher ingestion of soil \nin wet season")+
  
  annotate("text", x = 3, y = 10.5, size = 2, col = 'orange',
           label = "Higher ingestion of soil \nin dry season")+
  
  annotate("text", x = 8, y = 9, size = 2, col = 'red', angle = 36,
           label = "No seasonal differences")+
  
  annotate("text", x = 3.7, y = 1.8, size = 2, col = 'black',
           label = "Assumed\n parameter space \nof our system")+
  
  xlab('')+
  ylab('')+
  theme_pubclean()+
  theme(axis.title = element_text(size=8),
        axis.text = element_text(size=6, color='black'),
        legend.title = element_text(size=8),
        legend.key.size = unit(0.25, "cm"),
        legend.text = element_text(size=6, color='black'),
        axis.line = element_line(color = 'black',linewidth = 0.2),
        # panel.grid.major.y = element_line(linewidth = 0.1),
        strip.text = element_text(size = 6),
        strip.background = element_rect(colour="black",fill="white"),
        title = element_text(size = 6))+
  
  ggtitle('a) Reading key')

plot3b <- plot_fig3(data = subset(Zebra_average_RO, LD == 10^7), tit = 'b) All host and reservoir data', lims = c(0,20.5))
plot3c <- plot_fig3(data = subset(ZebraW_wetP_R0, LD == 10^7), tit = 'c) Normal Rainfall; Wet formed reservoir', lims = c(0,20.5))
plot3d <- plot_fig3(data = subset(ZebraW_dryP_R0, LD == 10^7), tit = 'd) Normal Rainfall; Dry formed reservoir', lims = c(0,20.5))
plot3e <- plot_fig3(data = subset(ZebraD_wetP_R0, LD == 10^7), tit = 'e) Drought; Wet formed reservoir ', lims = c(0,20.5))
plot3f <- plot_fig3(data = subset(ZebraD_dryP_R0, LD == 10^7), tit = 'f) Drought; Dry formed reservoir', lims = c(0,20.5))


Plot3 <- ggarrange(Plot3_Reading_key, plot3c, plot3e, plot3b, plot3d, plot3f, common.legend = T, ncol = 3, nrow = 2)
Plot3 <- Plot3 + annotate("text", x = 0.5, y = 0.018, size = 3,  label = "Percentage of animals ingesting soil in the wet season") 
Plot3 <- Plot3 + annotate("text", x = 0.01, y = 0.5, size = 3, angle = 90, label = "Percentage of animals ingesting soil in the dry season") 

tiff("Figure3.tif",
     compression = "lzw",width=18, height=11, units="cm",res=200)

plot(Plot3_other)
dev.off()


#### Plot 4

Zebra_YDR <- Zebra_average %>% group_by(LD, Year, Rep, Distance, Prop, Wwet, Wdry) %>%
  summarise(Infection = sum(Infected, na.rm=T),
            NV = sum(Visits, na.rm=T),
            NG = sum(Grazing, na.rm=T))

Zebra_Ymean <- Zebra_YDR %>% group_by(LD, Year) %>%
  summarise(Infected = mean(Infection),
            Infected_sd = sd(Infection),
            NV = mean(NV),
            NG = mean(NG))

Zebra_Ymean_space <- subset(Zebra_YDR, Prop %in% c('0/0','1/0','2/0','3/0','4/0',
                                                   '1/1','2/1','3/1','4/1',
                                                   '2/2','3/2','4/2',
                                                   '3/3','3/4',
                                                   '4/4')) %>% group_by(LD, Year) %>%
  summarise(Infected = mean(Infection),
            Infected_sd = sd(Infection),
            NV = mean(NV),
            NG = mean(NG))

Zebra_Ymean$SD_min = ifelse(Zebra_Ymean$Infected - Zebra_Ymean$Infected_sd < 0 , 0, Zebra_Ymean$Infected - Zebra_Ymean$Infected_sd )
Zebra_Ymean$SD_max = Zebra_Ymean$Infected + Zebra_Ymean$Infected_sd 

Zebra_Ymean <- Zebra_Ymean %>% mutate(LD1 = case_when(
  LD == 10^5 ~ '100,000',
  LD == 10^6 ~ '1,000,000',
  LD == 10^7 ~ '10,000,000',
  LD == 10^8 ~ '100,000,000'
))
Zebra_Ymean$LD1 <- factor(Zebra_Ymean$LD1, 
                          levels = c('100,000','1,000,000','10,000,000','100,000,000'))


Zebra_Ymean_space$SD_min = ifelse(Zebra_Ymean_space$Infected - Zebra_Ymean_space$Infected_sd < 0 , 0, Zebra_Ymean_space$Infected - Zebra_Ymean_space$Infected_sd )
Zebra_Ymean_space$SD_max = Zebra_Ymean_space$Infected + Zebra_Ymean_space$Infected_sd 

Zebra_Ymean_space <- Zebra_Ymean_space %>% mutate(LD1 = case_when(
  LD == 10^5 ~ '100,000',
  LD == 10^6 ~ '1,000,000',
  LD == 10^7 ~ '10,000,000',
  LD == 10^8 ~ '100,000,000'
))

Zebra_Ymean_space$LD1 <- factor(Zebra_Ymean_space$LD1, 
                                levels = c('100,000','1,000,000','10,000,000','100,000,000'))

Plot4 <- ggplot(Zebra_Ymean, aes(x=as.factor(Year), y = Infected, fill = LD1))+
  geom_bar(position = position_dodge2(), alpha=0.5, stat = 'identity')+
  geom_bar(data=Zebra_Ymean_space, position = position_dodge2(), stat = 'identity')+
  geom_errorbar(data=Zebra_Ymean_space, aes(ymin = SD_min , ymax = SD_max), position = position_dodge2(), lty =2, size = 0.2)+
  geom_errorbar(data=Zebra_Ymean, aes(ymin = SD_min , ymax = SD_max), position = position_dodge2(), size = 0.2)+
  
  scale_y_sqrt(breaks = c(0, 1, 3, 5, 7, 10, 15, 20, 25, 30, 35),
               limits = c(0, 35)) +
  
  scale_fill_manual('Lethal dose threshold ',values= c('blue3','green3','orange1','red2'))+
  
  xlab('Year after death') + ylab('Number of infections')+
  theme_pubclean()+
  theme(axis.title = element_text(size=8),
        axis.text = element_text(size=6, color='black'),
        legend.title = element_text(size=8),
        legend.key.size = unit(0.25, "cm"),
        axis.ticks.y = element_blank(),
        legend.text = element_text(size=6, color='black'),
        axis.line = element_line(color = 'black',linewidth = 0.2),
        panel.grid.major.y = element_line(linewidth = 0.1),
        strip.background = element_rect(colour="black",fill="white"))+
  guides(fill=guide_legend(nrow=2, byrow=TRUE)) 


Plot4 <- Plot4 + inset_element(p = Zeb_pic, left = 0.8, bottom = 0.9, right = 1,  top = 1.1)

tiff("Figure4.tif",
     compression = "lzw",width=9,height=6,units="cm",res=200)

plot(Plot4)
dev.off()





############# SI PLOT MAPS

##### Plot SI 2

Wildebeest_average <- read.csv('Wildebeest_Final_100.csv')
Wildebeest_average_R0 <- Data_analysis(Wildebeest_average)

plotSI2b <- plot_fig3(data = subset(Wildebeest_average_R0, LD == 10^5), tit = 'b) LD = 100,000', lims = c(0,100))
plotSI2c <- plot_fig3(data = subset(Wildebeest_average_R0, LD == 10^7), tit = 'c) LD = 1,000,000', lims = c(0,100))
plotSI2d <- plot_fig3(data = subset(Wildebeest_average_R0, LD == 10^7), tit = 'd) LD = 10,000,000', lims = c(0,100))
plotSI2e <- plot_fig3(data = subset(Wildebeest_average_R0, LD == 10^7), tit = 'e) LD = 100,000,000 ', lims = c(0,100))

PlotSI2 <- ggarrange(Plot3_Reading_key,ggplot()+theme_pubclean(), plotSI2b, plotSI2c, plotSI2d, plotSI2e, common.legend = T, ncol = 2, nrow = 3)
PlotSI2 <- PlotSI2 + annotate("text", x = 0.5, y = 0.01, size = 10,  label = "Percentage of animals ingesting soil in the wet season") 
PlotSI2 <- PlotSI2 + annotate("text", x = 0.01, y = 0.5, size = 10, angle = 90, label = "Percentage of animals ingesting soil in the dry season") 

png("PlotSI2.png", width = 1200, height = 1500)
plot(PlotSI2)
dev.off() 


#### Plot SI3

plotSI3b <- plot_fig3(data = subset(Zebra_average_RO , LD == 10^5), tit = 'b) LD = 100,000', lims = c(0,100))
plotSI3c <- plot_fig3(data = subset(Zebra_average_RO , LD == 10^7), tit = 'c) LD = 1,000,000', lims = c(0,100))
plotSI3d <- plot_fig3(data = subset(Zebra_average_RO , LD == 10^7), tit = 'd) LD = 10,000,000', lims = c(0,100))
plotSI3e <- plot_fig3(data = subset(Zebra_average_RO , LD == 10^7), tit = 'e) LD = 100,000,000 ', lims = c(0,100))

PlotSI3 <- ggarrange(Plot3_Reading_key,ggplot()+theme_pubclean(), plotSI3b, plotSI3c, plotSI3d, plotSI3e, common.legend = T, ncol = 2, nrow = 3)
PlotSI3 <- PlotSI3 + annotate("text", x = 0.5, y = 0.01, size = 10,  label = "Percentage of animals ingesting soil in the wet season") 
PlotSI3 <- PlotSI3 + annotate("text", x = 0.01, y = 0.5, size = 10, angle = 90, label = "Percentage of animals ingesting soil in the dry season") 

png("PlotSI3.png", width = 1200, height = 1500)
plot(PlotSI3)
dev.off() 


#### Plot SI4


WildebeestW_dryP <- read.csv('Final_data/Wildebeest__Dry_Pathogen_Wet_visit_100.csv')
WildebeestW_wetP <- read.csv('Final_data/Wildebeest__Wet_Pathogen_Wet_visit_100.csv')
WildebeestD_dryP <- read.csv('Final_data/Wildebeest__Dry_Pathogen_Dry_visit_100.csv')
WildebeestD_wetP <- read.csv('Final_data/Wildebeest__Wet_Pathogen_Dry_visit_100.csv')


WildebeestW_dryP_R0 <- Data_analysis(WildebeestW_dryP)
WildebeestW_wetP_R0 <- Data_analysis(WildebeestW_wetP)
WildebeestD_dryP_R0 <- Data_analysis(WildebeestD_dryP)
WildebeestD_wetP_R0 <- Data_analysis(WildebeestD_wetP)



plotS4b <- plot_fig3(data = subset(Wildebeest_average_R0, LD == 10^7), tit = 'b) All host and reservoir data', lims = c(0,20.5))
plotS4c <- plot_fig3(data = subset(WildebeestW_wetP_R0, LD == 10^7), tit = 'c) Normal Rainfall; Wet formed reservoir', lims = c(0,20.5))
plotS4d <- plot_fig3(data = subset(WildebeestW_dryP_R0, LD == 10^7), tit = 'd) Normal Rainfall; Dry formed reservoir', lims = c(0,20.5))
plotS4e <- plot_fig3(data = subset(WildebeestD_wetP_R0, LD == 10^7), tit = 'e) Drought; Wet formed reservoir ', lims = c(0,20.5))
plotS4f <- plot_fig3(data = subset(WildebeestD_dryP_R0, LD == 10^7), tit = 'f) Drought; Dry formed reservoir', lims = c(0,20.5))



PlotS4 <- ggarrange(Plot3_Reading_key, plotS4c, plotS4e, plotS4b, plotS4d, plotS4f, common.legend = T, ncol = 3, nrow = 2)
PlotS4 <- PlotS4_other + annotate("text", x = 0.5, y = 0.018, size = 3,  label = "Percentage of animals ingesting soil in the wet season") 
PlotS4 <- PlotS4_other + annotate("text", x = 0.01, y = 0.5, size = 3, angle = 90, label = "Percentage of animals ingesting soil in the dry season") 



tiff("FigureSI4.tif",
     compression = "lzw",width=18, height=11, units="cm",res=200)

plot(PlotS4_other)
dev.off()


### Plot SI5

Wildebeest_YDR <- Wildebeest_average %>% group_by(LD, Year, Rep, Distance, Prop, Wwet, Wdry) %>%
  summarise(Infection = sum(Infected, na.rm=T),
            NV = sum(Visits, na.rm=T),
            NG = sum(Grazing, na.rm=T))

Wildebeest_Ymean <- Wildebeest_YDR %>% group_by(LD, Year) %>%
  summarise(Infected = mean(Infection),
            Infected_sd = sd(Infection),
            NV = mean(NV),
            NG = mean(NG))

Wildebeest_Ymean_space <- subset(Wildebeest_YDR, Wwet %in% c(0, 0.1, 0.2, 0.3, 0.4) & 
                              Wdry %in% c(0, 0.1, 0.2, 0.3, 0.4)) %>% group_by(LD, Year) %>%
  summarise(Infected = mean(Infection),
            Infected_sd = sd(Infection),
            NV = mean(NV),
            NG = mean(NG))

Wildebeest_Ymean$SD_min = ifelse(Wildebeest_Ymean$Infected - Wildebeest_Ymean$Infected_sd < 0 , 0, Wildebeest_Ymean$Infected - Wildebeest_Ymean$Infected_sd )
Wildebeest_Ymean$SD_max = Wildebeest_Ymean$Infected + Wildebeest_Ymean$Infected_sd 

Wildebeest_Ymean <- Wildebeest_Ymean %>% mutate(LD1 = case_when(
  LD == 10^5 ~ '100,000',
  LD == 10^6 ~ '1,000,000',
  LD == 10^7 ~ '10,000,000',
  LD == 10^8 ~ '100,000,000'
))

Wildebeest_Ymean$LD1 <- factor(Wildebeest_Ymean$LD1, 
                                levels = c('100,000','1,000,000','10,000,000','100,000,000'))


Wildebeest_Ymean_space$SD_min = ifelse(Wildebeest_Ymean_space$Infected - Wildebeest_Ymean_space$Infected_sd < 0 , 0, Wildebeest_Ymean_space$Infected - Wildebeest_Ymean_space$Infected_sd )
Wildebeest_Ymean_space$SD_max = Wildebeest_Ymean_space$Infected + Wildebeest_Ymean_space$Infected_sd 

Wildebeest_Ymean_space <- Wildebeest_Ymean_space %>% mutate(LD1 = case_when(
  LD == 10^5 ~ '100,000',
  LD == 10^6 ~ '1,000,000',
  LD == 10^7 ~ '10,000,000',
  LD == 10^8 ~ '100,000,000'
))

Wildebeest_Ymean_space$LD1 <- factor(Wildebeest_Ymean_space$LD1, 
                               levels = c('100,000','1,000,000','10,000,000','100,000,000'))



PlotSI5 <- ggplot(Wildebeest_Ymean, aes(x=as.factor(Year), y = Infected, fill = LD1))+
  geom_bar(position = position_dodge2(), alpha=0.5, stat = 'identity')+
  geom_bar(data=Wildebeest_Ymean_space, position = position_dodge2(), stat = 'identity')+
  geom_errorbar(data=Wildebeest_Ymean_space, aes(ymin = SD_min , ymax = SD_max), position = position_dodge2(), lty =2, size = 0.2)+
  geom_errorbar(data=Wildebeest_Ymean, aes(ymin = SD_min , ymax = SD_max), position = position_dodge2(), size = 0.2)+
  
  scale_y_sqrt(breaks = c(0, 1, 3, 5, 7, 10, 15, 20, 25, 30, 35),
               limits = c(0, 35)) +
  
  scale_fill_manual('Lethal dose threshold ',values= c('blue3','green3','orange1','red2'))+
  xlab('Year after death') + ylab('Number of infections')+
  theme_pubclean()+
  theme(axis.title = element_text(size=8),
        axis.text = element_text(size=6, color='black'),
        legend.title = element_text(size=8),
        legend.key.size = unit(0.25, "cm"),
        axis.ticks.y = element_blank(),
        legend.text = element_text(size=6, color='black'),
        axis.line = element_line(color = 'black',linewidth = 0.2),
        panel.grid.major.y = element_line(linewidth = 0.1),
        strip.background = element_rect(colour="black",fill="white"))+
  guides(fill=guide_legend(nrow=2, byrow=TRUE)) 

 
PlotSI5 <- PlotSI5 + inset_element(p = Wil_pic, left = 0.8, bottom = 0.9, right = 1,  top = 1.1)

tiff("FigureS5.tif",
     compression = "lzw",width=9,height=6,units="cm",res=200)

plot(PlotSI5)
dev.off()




###### PLOT SI6

Zebra_test <- subset(Zebra_YDR, Prop %in% c('0/0','1/1','5/5','10/10')) %>% group_by(LD, Year, Prop) %>%
  summarise(Infected = mean(Infection),
            Infected_sd = sd(Infection),
            NV = mean(NV),
            NG = mean(NG))

Zebra_test$SD_min = ifelse(Zebra_test$Infected - Zebra_test$Infected_sd < 0 , 0, Zebra_test$Infected - Zebra_test$Infected_sd )
Zebra_test$SD_max = Zebra_test$Infected + Zebra_test$Infected_sd 

Zebra_test <- Zebra_test %>% mutate(LD1 = case_when(
  LD == 10^5 ~ 'LD = 100,000',
  LD == 10^6 ~ 'LD = 1,000,000',
  LD == 10^7 ~ 'LD = 10,000,000',
  LD == 10^8 ~ 'LD = 100,000,000'
))

Zebra_test$LD1 <- factor(Zebra_test$LD1, 
                              levels = c('LD = 100,000','LD = 1,000,000','LD = 10,000,000','LD = 100,000,000'))

Zebra_test <- Zebra_test %>% mutate(Prop = case_when(
  Prop == '0/0' ~ '0/0',
  Prop == '1/1' ~ '10/10',
  Prop == '5/5' ~ '50/50',
  Prop == '10/10' ~ '100/100',
))

Zebra_test$Prop <- factor(Zebra_test$Prop, 
                               levels = c('0/0','10/10','50/50','100/100'))


PlotSI6 <- ggplot(Zebra_test, aes(x=as.factor(Year), y = Infected, fill = Prop))+
  facet_wrap(~LD1) +
  geom_bar(position = position_dodge2(), stat = 'identity')+
  geom_errorbar(data=Zebra_test, aes(ymin = SD_min , ymax = SD_max), position = position_dodge2(), size = 0.2)+
  
  scale_y_sqrt(breaks = c(0, 1, 3, 5, 7, 10, 15, 20, 25, 30, 35, 40, 45),
               limits = c(0, 45)) +
  
  scale_fill_manual('Proportion of individuals ingesting soil (Wwet/Wdry) ',
                    values= c("green",'grey30','purple2','chocolate3'))+
  xlab('Year after death') + ylab('Number of infections')+
  theme_pubclean()+
  theme(axis.title = element_text(size=8),
        axis.text = element_text(size=6, color='black'),
        legend.title = element_text(size=8),
        legend.key.size = unit(0.25, "cm"),
        axis.ticks.y = element_blank(),
        legend.text = element_text(size=6, color='black'),
        axis.line = element_line(color = 'black',linewidth = 0.2),
        panel.grid.major.y = element_line(linewidth = 0.1),
        strip.background = element_rect(colour="black",fill="white"),
        strip.text = element_text(size = 8))+
  guides(fill=guide_legend(nrow=2, byrow=TRUE)) 

PlotSI6 <- PlotSI6 + inset_element(p = Zeb_pic, left = 0.85, bottom = 1.1, right = 0.99,  top = 1.2)


tiff("FigureS6.tif",
     compression = "lzw",width=18,height=12,units="cm",res=200)

plot(PlotSI6)
dev.off()



###### PLOT SI7

Wildebeest_test <- subset(Wildebeest_YDR, Prop %in% c('0/0','1/1','5/5','10/10')) %>% group_by(LD, Year, Prop) %>%
  summarise(Infected = mean(Infection),
            Infected_sd = sd(Infection),
            NV = mean(NV),
            NG = mean(NG))

Wildebeest_test$SD_min = ifelse(Wildebeest_test$Infected - Wildebeest_test$Infected_sd < 0 , 0, Wildebeest_test$Infected - Wildebeest_test$Infected_sd )
Wildebeest_test$SD_max = Wildebeest_test$Infected + Wildebeest_test$Infected_sd 

Wildebeest_test <- Wildebeest_test %>% mutate(LD1 = case_when(
  LD == 10^5 ~ 'LD = 100,000',
  LD == 10^6 ~ 'LD = 1,000,000',
  LD == 10^7 ~ 'LD = 10,000,000',
  LD == 10^8 ~ 'LD = 100,000,000'
))

Wildebeest_test$LD1 <- factor(Wildebeest_test$LD1, 
                              levels = c('LD = 100,000','LD = 1,000,000','LD = 10,000,000','LD = 100,000,000'))

Wildebeest_test <- Wildebeest_test %>% mutate(Prop = case_when(
  Prop == '0/0' ~ '0/0',
  Prop == '1/1' ~ '10/10',
  Prop == '5/5' ~ '50/50',
  Prop == '10/10' ~ '100/100',
))

Wildebeest_test$Prop <- factor(Wildebeest_test$Prop, 
                              levels = c('0/0','10/10','50/50','100/100'))


PlotSI7 <- ggplot(Wildebeest_test, aes(x=as.factor(Year), y = Infected, fill = Prop))+
  facet_wrap(~LD1) +
  geom_bar(position = position_dodge2(), stat = 'identity')+
  geom_errorbar(data=Wildebeest_test, aes(ymin = SD_min , ymax = SD_max), position = position_dodge2(), size = 0.2)+
  
  scale_y_sqrt(breaks = c(0, 1, 3, 5, 7, 10, 15, 20, 25, 30, 35),
               limits = c(0, 35)) +
  
  scale_fill_manual('Proportion of individuals ingesting soil (Wwet/Wdry) ',
                    values= c("green",'grey30','purple2','chocolate3'))+
  xlab('Year after death') + ylab('Number of infections')+
  theme_pubclean()+
  theme(axis.title = element_text(size=8),
        axis.text = element_text(size=6, color='black'),
        legend.title = element_text(size=8),
        legend.key.size = unit(0.25, "cm"),
        axis.ticks.y = element_blank(),
        legend.text = element_text(size=6, color='black'),
        axis.line = element_line(color = 'black',linewidth = 0.2),
        panel.grid.major.y = element_line(linewidth = 0.1),
        strip.background = element_rect(colour="black",fill="white"),
        strip.text = element_text(size = 8))+
  guides(fill=guide_legend(nrow=2, byrow=TRUE)) 

PlotSI7 <- PlotSI7 + inset_element(p = Wil_pic, left = 0.85, bottom = 1.1, right = 0.99,  top = 1.2)

tiff("FigureS7.tif",
     compression = "lzw",width=18,height=12,units="cm",res=200)

plot(PlotSI7)
dev.off()





