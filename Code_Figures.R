#### FINAL PLOT MS_ R ####

# @author :  Amélie Dolfi
# @Date 2/2/2023
# @Update 11/1/2023

## Directory
wd <- 'Github/Reproduction_number'
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

########################## MS ################################################


##### PLOT 2 ####


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



Plot2Z <- ggplot(subset(Total_Ind, Species == 'Zebra'), aes(x=Demographic, fill = Treatment))+
  geom_bar(position = position_dodge2(), alpha=0.5)+
  geom_bar(data=subset(Total_Ind, TG > 0 & Species == 'Zebra'), position = position_dodge2())+
  scale_fill_manual(name = '',values= c('blue','red'))+
  scale_y_continuous(breaks = seq(0,2500, 250))+
  xlab('') + ylab('Number of individuals')+
  theme_pubclean()+
  theme(axis.title = element_text(size=10),
        axis.title.x = element_blank(),
        axis.text = element_text(size=8, color='black'),
        legend.title = element_text(size=10),
        legend.key.size = unit(0.4, "cm"),
        axis.ticks.y = element_line(linewidth = 0.2),
        axis.line = element_line(color = 'black',linewidth = 0.2),
        legend.text = element_text(size=10, color='black'),
        strip.background = element_rect(colour="black",fill="white"),
        panel.grid.major.y = element_line(linewidth = 0.1),
        strip.text = element_text(size = 10),
        title = element_text(size = 8))+
  ggtitle('a) ')

Plot2W <- ggplot(subset(Total_Ind, Species == 'Wildebeest'), aes(x=Demographic, fill = Treatment))+
  geom_bar(position = position_dodge2(), alpha=0.5)+
  geom_bar(data=subset(Total_Ind, TG > 0 & Species == 'Wildebeest'), position = position_dodge2())+
  scale_fill_manual(name = '',values= c('blue','red'))+
  scale_y_continuous(breaks = seq(0,300, 50))+
  xlab('') + ylab('Number of individuals')+
  theme_pubclean()+
  theme(axis.title = element_text(size=10),
        axis.title.x = element_blank(),
        axis.text = element_text(size=8, color='black'),
        legend.title = element_text(size=10),
        legend.key.size = unit(0.4, "cm"),
        axis.ticks.y = element_line(linewidth = 0.2),
        axis.line = element_line(color = 'black',linewidth = 0.2),
        legend.text = element_text(size=10, color='black'),
        strip.background = element_rect(colour="black",fill="white"),
        panel.grid.major.y = element_line(linewidth = 0.1),
        strip.text = element_text(size = 10),
        title = element_text(size = 8))+
  ggtitle('b) ')


Total_grazing_time <- subset(Total_Ind, TG >0)
Total_grazing_time$Season2 <- ifelse(Total_grazing_time$Season == 'Hot_wet', 'Wet', 'Dry')
Total_grazing_time$Season2 <- factor(Total_grazing_time$Season2, levels = c('Wet','Dry'))

Plot2c <- ggplot(subset(Total_grazing_time, Species == 'Zebra'), aes(x= Season2, y= TG, fill = Treatment))+
  geom_boxplot(size = 0.2, outlier.size = 0.5)+
  scale_y_log10(breaks = c(1,5,10,30,100, 200, 500),
                label = c(1,5,10,30,100, 200, 500))+
  scale_fill_manual(name = '',values= c('blue','red'))+
  xlab('Season') + ylab('Time spent grazing (s)')+
  theme_pubclean()+
  theme(axis.title = element_text(size=10),
        axis.text = element_text(size=8, color='black'),
        legend.title = element_text(size=10),
        legend.key.size = unit(0.4, "cm"),
        axis.ticks.y = element_line(linewidth = 0.2),
        axis.line = element_line(color = 'black',linewidth = 0.2),
        legend.text = element_text(size=10, color='black'),
        strip.background = element_rect(colour="black",fill="white"),
        panel.grid.major.y = element_line(linewidth = 0.1),
        strip.text = element_text(size = 10),
        title = element_text(size = 8))+
  ggtitle('c) ')

Plot2d <- ggplot(subset(Total_grazing_time, Species == 'Wildebeest' & TG < 1000), aes(x= Season2, y= TG, fill = Treatment))+
  geom_boxplot(size = 0.2, outlier.size = 0.5)+
  scale_y_log10(breaks = c(1,5,10,30,100, 200, 500),
                label = c(1,5,10,30,100, 200, 500))+  
  xlab('Season') + ylab('Time spent grazing (s)')+
  scale_fill_manual(name = '',values= c('blue','red'))+
  theme_pubclean()+
  theme(axis.title = element_text(size=10),
        axis.text = element_text(size=8, color='black'),
        legend.title = element_text(size=10),
        legend.key.size = unit(0.4, "cm"),
        axis.ticks.y = element_line(linewidth = 0.2),
        axis.line = element_line(color = 'black',linewidth = 0.2),
        legend.text = element_text(size=10, color='black'),
        strip.background = element_rect(colour="black",fill="white"),
        panel.grid.major.y = element_line(linewidth = 0.1),
        strip.text = element_text(size = 10),
        title = element_text(size = 8))+
  ggtitle('d) ')


Plot2 <-  ggarrange(Plot2Z, Plot2W, Plot2c, Plot2d, common.legend = T,  ncol = 2, nrow = 2)

Plot2 <- Plot2 + inset_element(p = Zeb_pic, left = 0.14, bottom = 0.86, right = 0.18,  top = 0.93)
Plot2 <- Plot2 + inset_element(p = Zeb_pic, left = 0.14, bottom = 0.4, right = 0.18,  top = 0.47)

Plot2<- Plot2 + inset_element(p = Wil_pic, left = 0.63, bottom = 0.86,  right = 0.67,  top = 0.93)
Plot2<- Plot2 + inset_element(p = Wil_pic, left = 0.63, bottom = 0.4,  right = 0.67,  top = 0.47)

tiff("Final_figures/Figure2abcd.tif",
     compression = "lzw",width=14,height=12,units="cm",res=200)
plot(Plot2)
dev.off()



### PLOT 3 ####

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
  theme(axis.title = element_text(size=10),
        axis.text = element_text(size=8, color='black'),
        legend.title = element_text(size=8),
        legend.key.size = unit(0.25, "cm"),
        axis.ticks.y = element_blank(),
        legend.text = element_text(size=10, color='black'),
        axis.line = element_line(color = 'black',linewidth = 0.2),
        panel.grid.major.y = element_line(linewidth = 0.1),
        strip.background = element_rect(colour="black",fill="white"))
# 

tiff("Final_figures/Figure3.tif",
     compression = "lzw",width=12,height=7.5,units="cm",res=200)

plot(CFU_p)
dev.off()


### PLOT 4 ####


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


plot_fig4 <- function(data, tit, lims){
  
  plot4 <- ggplot(data , aes(x = as.factor(Wwet), y = as.factor(Wdry), fill = Infected)) + 
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
  
  return(plot4)
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


Plot_Reading_key <- ggplot(data = subset(Zebra_average_RO, LD == 1e7), aes(x = as.factor(Wwet), y = as.factor(Wdry))) + 
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

plot4b <- plot_fig4(data = subset(Zebra_average_RO, LD == 10^7), tit = 'b) Average rainfall; \n    all infectious sites', lims = c(0,20.5))
plot4c <- plot_fig4(data = subset(ZebraW_wetP_R0, LD == 10^7), tit = 'c) Average rainfall;\n    wet-season formed infectious sites', lims = c(0,20.5))
plot4d <- plot_fig4(data = subset(ZebraW_dryP_R0, LD == 10^7), tit = 'd) Average rainfall;\n    dry-season formed infectious sites', lims = c(0,20.5))
plot4e <- plot_fig4(data = subset(ZebraD_wetP_R0, LD == 10^7), tit = 'e) Simulated Drought;\n    wet-season formed infectious sites', lims = c(0,20.5))
plot4f <- plot_fig4(data = subset(ZebraD_dryP_R0, LD == 10^7), tit = 'f) Simulated Drought;\n    dry-season formed infectious site', lims = c(0,20.5))


Plot4 <- ggarrange(Plot_Reading_key, plot4c, plot4e, plot4b, plot4d, plot4f, common.legend = T, ncol = 3, nrow = 2)
Plot4 <- Plot4 + annotate("text", x = 0.5, y = 0.018, size = 3,  label = "Percentage of animals ingesting soil in the wet season") 
Plot4 <- Plot4 + annotate("text", x = 0.01, y = 0.5, size = 3, angle = 90, label = "Percentage of animals ingesting soil in the dry season") 

tiff("Final_figures/Figure4.tif",
     compression = "lzw",width=18, height=11, units="cm",res=200)
plot(Plot4)
dev.off()

#### Plot 5 ####

LDF <- read.csv('Final_data/LDF.csv')

LD_average <- as.data.frame(LDF %>% group_by(LD, Step) %>%
                              summarise(Carc_average = mean(CarcTot),
                                        Carc_sd = sd(CarcTot)))

LD_average$Min <- LD_average$Carc_average - LD_average$Carc_sd
LD_average$Min <- ifelse(LD_average$Min <0, 0, LD_average$Min)
LD_average$Max <- LD_average$Carc_average + LD_average$Carc_sd


### Death

Death <- read.csv("C:/Users/Amelie/OneDrive - UW-Madison/Documents/PhDUW/PhD/ENP-KNP/Data_ENP/mortality_data/Mortality_2020.csv", sep=';')


# Keep only the variables I want and for which the death cause is anthrax
Death <- subset(Death, COD == 'ANP', select=c(SPECIES, DATE, TIME, COD, DATE_DIE, Longitude, Latitude))

# Give the Year, month and day of death
Death$YEAR <- format(as.Date(Death$DATE,format="%m/%d/%Y"),"%Y")
Death <- Death %>% group_by(YEAR) %>%
  summarize(Total = n())

Death_count <- data.frame(LD = 'Recorded mortality', Step = seq(1,36), Carc_average = rep(NA, 36),
                          Carc_sd = rep(NA, 36), Min = rep(NA, 36), Max = rep(NA, 36))

for(y in (0:35)){
  years <- seq(1975+y, 1985+y)
  Count <- sum(Death[Death$YEAR %in% years,'Total'])
  
  Death_count[y+1, 'Carc_average'] <- Count
}


LD_average <- rbind(LD_average, 
                    Death_count)

subset(LD_average, LD == 10^8 & Carc_average != 0)

Plot5 <- ggplot()+
 # geom_line(data = Death_count, aes(x=Step, y = Carc_average), col = 'black' )+
  geom_line(data = LD_average, aes(x=Step, y = Carc_average+1, col = as.character(LD), linetype = as.character(LD)))+
  geom_ribbon(data = LD_average, aes(x = Step, ymin = Min+1, ymax = Max+1, fill=as.character(LD)), col = 'grey75', lty = 2, alpha = 0.2)+
  scale_x_continuous(breaks = seq(0,100,10), labels = seq(0,100,10))+
  scale_y_log10(breaks = c(1,100, 1000,10000,100000),
                labels = c('0','100','1,000','10,000','100,000'),
                limits = c(1,150000))+
  annotation_logticks(sides='l')+
  scale_color_manual('Lethal dose threshold',
                     labels = c(expression('10'^5), expression('10'^6), expression('10'^7), expression('10'^8), 'Recorded mortality (1974-2020)'),
                     values = c('blue3','green3','orange1','red2','black'))+
  scale_fill_manual('Lethal dose threshold',
                    labels = c(expression('10'^5), expression('10'^6), expression('10'^7), expression('10'^8), 'Recorded mortality (1974-2020)'),
                    values = c('blue3','green3','orange1','red2','black'))+
  scale_linetype_manual('Lethal dose threshold',
                    labels = c(expression('10'^5), expression('10'^6), expression('10'^7), expression('10'^8), 'Recorded mortality (1974-2020)'),
                    values = c(1,1,1,1,2))+
  ylab('Number of carcasses (aged 0-10)')+
  theme_pubclean()+
  theme(axis.title = element_text(size=10),
        axis.text = element_text(size=8, color='black'),
        legend.title = element_text(size=10),
        legend.key.size = unit(0.5, "cm"),
        axis.ticks.y = element_blank(),
        legend.text = element_text(size=10, color='black'),
        axis.line = element_line(color = 'black',linewidth = 0.1),
        panel.grid.major.y = element_line(linewidth = 0.1),
      #  panel.border = element_rect(colour = "black", fill=NA, linewidth=0.1),
        strip.background = element_rect(colour="black",fill="white"),
        strip.text = element_text(size = 10),
        title = element_text(size = 10))

tiff("Final_figures/Figure5.tif",
     compression = "lzw",width=18,height=12,units="cm",res=200)
plot(Plot5)
dev.off()


#### Plot 6 ####

Zebra_YDR <- Zebra_average %>% group_by(LD, Year, Rep, Distance, Prop, Wwet, Wdry) %>%
  summarise(Infection = sum(Infected, na.rm=T),
            NV = sum(Visits, na.rm=T),
            NG = sum(Grazing, na.rm=T))

Zebra_Ymean <- Zebra_YDR %>% group_by(LD, Year) %>%
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


Zebra_Ymean_space <- subset(Zebra_YDR, Prop %in% c('0/0','1/0','2/0','3/0','4/0',
                                                   '1/1','2/1','3/1','4/1',
                                                   '2/2','3/2','4/2',
                                                   '3/3','3/4',
                                                   '4/4')) %>% 
  group_by(LD, Year) %>%
  summarise(Infected = mean(Infection),
            Infected_sd = sd(Infection),
            NV = mean(NV),
            NG = mean(NG))

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

Plot6 <- ggplot(Zebra_Ymean, aes(x=Year, y = Infected, fill = LD1))+
  geom_bar(position = position_dodge2(), alpha=0.5, stat = 'identity')+
  geom_bar(data=Zebra_Ymean_space, position = position_dodge2(), stat = 'identity')+
  geom_errorbar(data=Zebra_Ymean_space, aes(x = Year + 0.02, ymin = SD_min , ymax = SD_max), position = position_dodge2(), lty =2, size = 0.1)+
  geom_errorbar(data=Zebra_Ymean, aes(x = Year - 0.02, ymin = SD_min , ymax = SD_max), position = position_dodge2(), size = 0.2)+
  
  scale_y_sqrt(breaks = c(0, 1, 3, 5, 7, 10, 15, 20, 25, 30, 35),
               limits = c(0, 35)) +
  
  scale_x_continuous(breaks = seq(0,10))+
  scale_fill_manual('Lethal dose threshold ',values= c('blue3','green3','orange1','red2'), 
                    labels =  c(expression('10'^5), expression('10'^6), expression('10'^7),expression('10'^8)))+  
  xlab('Year after death') + ylab('Number of infections')+
  theme_pubclean()+
  theme(axis.title = element_text(size=10),
        axis.text = element_text(size=8, color='black'),
        legend.title = element_text(size=10),
        legend.key.size = unit(0.4, "cm"),
        axis.ticks.y = element_blank(),
        legend.text = element_text(size=10, color='black'),
        axis.line = element_line(color = 'black',linewidth = 0.2),
        panel.grid.major.y = element_line(linewidth = 0.1),
        strip.background = element_rect(colour="black",fill="white")) 


Plot6 <- Plot6 + inset_element(p = Zeb_pic, left = 0.8, bottom = 0.9, right = 1,  top = 1.1)

tiff("Final_figures/Figure6.tif",
     compression = "lzw",width=12,height=7.5,units="cm",res=200)

plot(Plot6)
dev.off()



###### SUPPLEMENTARY FIGURES ######

### Plot S1 ####

### Plot S2 ####

Cameras <- read.csv('Camera_work_time.csv')
Cameras <- Cameras %>% separate(Month, c('Month', 'Year'))

Cameras$NumMonth <- ifelse(Cameras$Month %in% c('Apr','Jun','Sep','Nov'), 30,
                           ifelse(Cameras$Month == 'Feb', 28, 31))
Cameras[Cameras$Month == 'Feb' & Cameras$Year==12,'NumMonth'] <- 29

Cameras$Missing_carcass <- Cameras$NumMonth - Cameras$Carcass
Cameras$Missing_control <- Cameras$NumMonth - Cameras$Control

# Cameras$Missing_pairs <- apply(Cameras[,c(7,8)], 1, FUN = max)
# Cameras$Missing_pairs <- round(Cameras$Missing_pairs)

Cameras_F <- data.frame(Missing = c(Cameras$Missing_carcass, Cameras$Missing_control))


PlotS2 <- ggplot(Cameras_F, aes(x=Missing))+
  geom_histogram(color = 'black', fill = 'grey', binwidth = 1)+
  ylab('Number of months')+
  xlab('Days of missing data')+
  scale_x_continuous(breaks = seq(0,11), labels = seq(0,11))+
  scale_y_continuous(breaks = seq(0,215,5), labels = c(0,'','','','',25,'','','','',50, '','','','',75, '','','','',100,
                                                       '','','','',125,  '','','','',150,  '','','','',175,  '','','','',200,
                                                       '','',''))+
  theme_pubclean()+
  theme(axis.title = element_text(size=8),
        axis.text = element_text(size=6, color='black'),
        legend.title = element_text(size=8),
        legend.key.size = unit(0.25, "cm"),
        axis.ticks.y = element_line(linewidth = 0.2),
        axis.line = element_line(color = 'black',linewidth = 0.2),
        legend.text = element_text(size=6, color='black'),
        strip.background = element_rect(colour="black",fill="white"),
        panel.grid.major.y = element_line(linewidth = 0.1),
        strip.text = element_text(size = 8),
        title = element_text(size = 6))


tiff("Figures/FigureS2.tif",
     compression = "lzw",width=12,height=7,units="cm",res=200)
plot(PlotS2)
dev.off()


### Plot S3 ####


### ZEBRA
Num_demo_Z <- subset(read.csv('Nmonth_All_FINAL.csv'), Species == "Zebra")
Num_demo_Z <- merge(Num_demo_Z, Water_Pan_Camera, by='ID', all.x=T, sort=F)
Num_demo_Z$Season <- factor(Num_demo_Z$Season, levels= c('Hot_wet','Cool_dry','Hot_dry'))
Num_demo_Z$Visits <- ifelse(Num_demo_Z$NV >0, 1, 0)
Num_demo_Z$dTreatment <- ifelse(Num_demo_Z$Treatment == 'Carcass', 1, 0)

Num_demo_PGV_Z <- Num_demo_Z
Num_demo_PGV_Z$P <- cbind(Num_demo_PGV_Z$NG, Num_demo_PGV_Z$NV)
Num_demo_PGV_Z <- Num_demo_PGV_Z %>% mutate(Year_AD = case_when(
  Month_AD < 12 ~ '0',
  Month_AD >= 12 & Month_AD < 24 ~ '1',
  Month_AD >=24 ~'2'
))

Num_demo_PGV_Z$Distance <- ifelse(Num_demo_PGV_Z$Water_km <= 4, '0-4km',
                                  ifelse(Num_demo_PGV_Z$Water_km > 4 & Num_demo_PGV_Z$Water_km <= 8, '4-8km',
                                         ifelse(Num_demo_PGV_Z$Water_km > 8, '+8km','')))

PGV_Z <- Num_demo_PGV_Z[!is.na(Num_demo_PGV_Z$PropGV)==T,]

Average_NV_Z <- Num_demo_PGV_Z %>% group_by(ID, Year, Year_AD, Distance, Treatment, Season, Age, Sex) %>%
  summarize(NV = sum(NV),
            NG = sum(NG))

Nmonth <- as.data.frame(Num_demo_Z %>% group_by(ID, Year, Season)%>% summarise(Nmonth =n() / 8))
Month_season_ID <- data.frame(ID=rep(c("10-016", "10-022", "10-049", "10-062",
                                       "10-115", "10-184", "10-195", "10-237",
                                       "11-011", "11-039", "11-052","12-010", "12-039"), each = 3*4),
                              Season = rep(c('Hot_wet','Cool_dry','Hot_dry'),  13 * 4),
                              Year = rep(c(2010,2011,2012, 2013), each = 3, 13))
Month_season_ID <- merge(Month_season_ID, Nmonth, by = c('ID','Year','Season'))
Month_season_ID$Season <- factor( Month_season_ID$Season, levels = c('Hot_wet','Cool_dry','Hot_dry'))

Average_NV_Z  <- merge(Average_NV_Z, Month_season_ID, by = c('ID','Year','Season'))
Average_NV_Z$Average_NV <- Average_NV_Z$NV / Average_NV_Z$Nmonth * 4
Average_NV_Z$Average_NG <- Average_NV_Z$NG / Average_NV_Z$Nmonth * 4


### NV PDF

plot_PDF_NV <- function(data, Ag, Se, Sea, Dist){
  
  Control_Visit <-  rbind(subset(data, Age== Ag & Sex == Se & Treatment =='Control' &  Season == Sea & Distance == Dist), 
                          subset(data, Age== Ag & Sex == Se & Treatment =='Carcass' & Year_AD %in% c(2,3), Season == Sea & Distance == Dist))
  pdf_NV_0 <- density(subset(data, Age== Ag & Sex == Se & Treatment =='Carcass' & Year_AD == 0 &  Season == Sea & Distance == Dist)$Average_NV, from=0, bw=10)
  pdf_NV_1 <- density(subset(data, Age== Ag & Sex == Se & Treatment =='Carcass' & Year_AD == 1 &  Season == Sea & Distance == Dist)$Average_NV, from=0, bw=10)
  pdf_NV_210 <- density(Control_Visit$Average_NV, from=0, bw=10)
  
  PDF <- data.frame(Visit = c(pdf_NV_0$x, pdf_NV_1$x, pdf_NV_210$x),
                    Density = c(pdf_NV_0$y, pdf_NV_1$y, pdf_NV_210$y),
                    Year = c(rep(0, length(pdf_NV_0$x)),
                             rep(1, length(pdf_NV_1$x)),
                             rep('2-10', length(pdf_NV_210$x))),
                    Season = rep(Sea, length(pdf_NV_0$x)+length(pdf_NV_1$x)+length(pdf_NV_210$x)),
                    Dist = rep(Dist, length(pdf_NV_0$x)+length(pdf_NV_1$x)+length(pdf_NV_210$x)),
                    Age = rep(Ag, length(pdf_NV_0$x)+length(pdf_NV_1$x)+length(pdf_NV_210$x)),
                    Sex = rep(Se, length(pdf_NV_0$x)+length(pdf_NV_1$x)+length(pdf_NV_210$x)))
  
  return(PDF)
}


pdf_NG_Z <- data.frame(Visits = numeric(), Density = numeric(),
                       Year = character(), Season = character(),
                       Dist = character(), Age = character(), Sex = character())

for (Ag in c('A','SA')){
  for (Se in c('F','M')){
    for (Sea in c('Hot_wet','Cool_dry','Hot_dry')){
      for (Dist in c('0-4km','4-8km','+8km')){
        
        pdf <- plot_PDF_NV(Average_NV_Z, Ag, Se , Sea, Dist)
        pdf_NG_Z <- rbind(pdf_NG_Z, pdf)
      }
    }
  }
}

pdf_NG_Z$Season <- factor(pdf_NG_Z$Season, levels =c('Hot_wet', 'Cool_dry','Hot_dry'))
pdf_NG_Z$Dist <- factor(pdf_NG_Z$Dist, levels =c('0-4km', '4-8km','+8km'))


PlotS3AF <- ggplot(subset(pdf_NG_Z, Age == 'A' & Sex == 'F'), aes(x= Visit, y= Density, col = Year))+
  facet_grid(Dist ~ Season) +
  geom_line()+
  xlab('Number of visits')+
  scale_x_continuous(breaks = seq(0,120,20), labels = seq(0,120,20))+
  scale_color_manual(values = c('burlywood3', 'springgreen3', 'slateblue3'))+
  theme_pubclean()+
  theme(axis.title = element_text(size=10),
        axis.text = element_text(size=8, color='black'),
        legend.title = element_text(size=10),
        legend.key.size = unit(0.5, "cm"),
        axis.ticks.y = element_blank(),
        legend.text = element_text(size=10, color='black'),
        axis.line = element_line(color = 'black',linewidth = 0.2),
        panel.grid.major.y = element_line(linewidth = 0.1),
        panel.border = element_rect(colour = "black", fill=NA, size=0.1),
        strip.background = element_rect(colour="black",fill="white"),
        strip.text = element_text(size = 10),
        title = element_text(size = 10))+
  ggtitle('Adult Female')

PlotS3AM <- ggplot(subset(pdf_NG_Z, Age == 'A' & Sex == 'M'), aes(x= Visit, y= Density, col = Year))+
  facet_grid(Dist ~ Season) +
  geom_line()+
  xlab('Number of visits')+
  scale_x_continuous(breaks = seq(0,120,20), labels = seq(0,120,20))+
  scale_color_manual(values = c('burlywood3', 'springgreen3', 'slateblue3'))+
  theme_pubclean()+
  theme(axis.title = element_text(size=10),
        axis.text = element_text(size=8, color='black'),
        legend.title = element_text(size=10),
        legend.key.size = unit(0.5, "cm"),
        axis.ticks.y = element_blank(),
        legend.text = element_text(size=10, color='black'),
        axis.line = element_line(color = 'black',linewidth = 0.2),
        panel.grid.major.y = element_line(linewidth = 0.1),
        panel.border = element_rect(colour = "black", fill=NA, size=0.1),
        strip.background = element_rect(colour="black",fill="white"),
        strip.text = element_text(size = 10),
        title = element_text(size = 10))+
  ggtitle('Adult Male')

PlotS3SAF <- ggplot(subset(pdf_NG_Z, Age == 'SA' & Sex == 'F'), aes(x= Visit, y= Density, col = Year))+
  facet_grid(Dist ~ Season) +
  geom_line()+
  xlab('Number of visits')+
  scale_x_continuous(breaks = seq(0,120,20), labels = seq(0,120,20))+
  scale_color_manual(values = c('burlywood3', 'springgreen3', 'slateblue3'))+
  theme_pubclean()+
  theme(axis.title = element_text(size=10),
        axis.text = element_text(size=8, color='black'),
        legend.title = element_text(size=10),
        legend.key.size = unit(0.5, "cm"),
        axis.ticks.y = element_blank(),
        legend.text = element_text(size=10, color='black'),
        axis.line = element_line(color = 'black',linewidth = 0.2),
        panel.grid.major.y = element_line(linewidth = 0.1),
        panel.border = element_rect(colour = "black", fill=NA, size=0.1),
        strip.background = element_rect(colour="black",fill="white"),
        strip.text = element_text(size = 10),
        title = element_text(size = 10))+
  ggtitle('Sub-Adult Female')


PlotS3SAM <- ggplot(subset(pdf_NG_Z, Age == 'SA' & Sex == 'M'), aes(x= Visit, y= Density, col = Year))+
  facet_grid(Dist ~ Season) +
  geom_line()+
  xlab('Number of visits')+
  scale_x_continuous(breaks = seq(0,120,20), labels = seq(0,120,20))+
  scale_color_manual(values = c('burlywood3', 'springgreen3', 'slateblue3'))+
  theme_pubclean()+
  theme(axis.title = element_text(size=10),
        axis.text = element_text(size=8, color='black'),
        legend.title = element_text(size=10),
        legend.key.size = unit(0.5, "cm"),
        axis.ticks.y = element_blank(),
        legend.text = element_text(size=10, color='black'),
        axis.line = element_line(color = 'black',linewidth = 0.2),
        panel.grid.major.y = element_line(linewidth = 0.1),
        panel.border = element_rect(colour = "black", fill=NA, size=0.1),
        strip.background = element_rect(colour="black",fill="white"),
        strip.text = element_text(size = 10),
        title = element_text(size = 10))+
  ggtitle('Sub-Adult Male')


PlotS3 <- ggarrange(PlotS3AF, PlotS3AM, PlotS3SAF, PlotS3SAM, ncol = 2, nrow = 2, common.legend = T)

tiff("Final_figures/FigureS3.tif",
     compression = "lzw",width=25,height=18,units="cm",res=200)

plot(PlotS3)
dev.off()

#### Plot S4 ###

pdf_NG_W <- data.frame(Visits = numeric(), Density = numeric(),
                       Year = character(), Season = character(),
                       Dist = character(), Age = character(), Sex = character())

for (Ag in c('A','SA')){
  for (Se in c('F','M')){
    for (Sea in c('Hot_wet','Cool_dry','Hot_dry')){
      for (Dist in c('0-3km','3-6km','+6km')){
        
        pdf <- plot_PDF_NV(Average_NV_W, Ag, Se , Sea, Dist)
        pdf_NG_W <- rbind(pdf_NG_W, pdf)
      }
    }
  }
}

pdf_NG_W$Season <- factor(pdf_NG_W$Season, levels =c('Hot_wet', 'Cool_dry','Hot_dry'))
pdf_NG_W$Dist <- factor(pdf_NG_W$Dist, levels =c('0-3km', '3-6km','+6km'))


PlotS4AF <- ggplot(subset(pdf_NG_W, Age == 'A' & Sex == 'F'), aes(x= Visit, y= Density, col = Year))+
  facet_grid(Dist ~ Season) +
  geom_line()+
  xlab('Number of visits')+
  scale_x_continuous(breaks = seq(0,60,20), labels = seq(0,60,20))+
  scale_color_manual(values = c('burlywood3', 'springgreen3', 'slateblue3'))+
  theme_pubclean()+
  theme(axis.title = element_text(size=10),
        axis.text = element_text(size=8, color='black'),
        legend.title = element_text(size=10),
        legend.key.size = unit(0.5, "cm"),
        axis.ticks.y = element_blank(),
        legend.text = element_text(size=10, color='black'),
        axis.line = element_line(color = 'black',linewidth = 0.2),
        panel.grid.major.y = element_line(linewidth = 0.1),
        panel.border = element_rect(colour = "black", fill=NA, size=0.1),
        strip.background = element_rect(colour="black",fill="white"),
        strip.text = element_text(size = 10),
        title = element_text(size = 10))+
  ggtitle('Adult Female')

PlotS4AM <- ggplot(subset(pdf_NG_W, Age == 'A' & Sex == 'M'), aes(x= Visit, y= Density, col = Year))+
  facet_grid(Dist ~ Season) +
  geom_line()+
  xlab('Number of visits')+
  scale_x_continuous(breaks = seq(0,60,20), labels = seq(0,60,20))+
  scale_color_manual(values = c('burlywood3', 'springgreen3', 'slateblue3'))+
  theme_pubclean()+
  theme(axis.title = element_text(size=10),
        axis.text = element_text(size=8, color='black'),
        legend.title = element_text(size=10),
        legend.key.size = unit(0.5, "cm"),
        axis.ticks.y = element_blank(),
        legend.text = element_text(size=10, color='black'),
        axis.line = element_line(color = 'black',linewidth = 0.2),
        panel.grid.major.y = element_line(linewidth = 0.1),
        panel.border = element_rect(colour = "black", fill=NA, size=0.1),
        strip.background = element_rect(colour="black",fill="white"),
        strip.text = element_text(size = 10),
        title = element_text(size = 10))+
  ggtitle('Adult Male')

PlotS4SAF <- ggplot(subset(pdf_NG_W, Age == 'SA' & Sex == 'F'), aes(x= Visit, y= Density, col = Year))+
  facet_grid(Dist ~ Season) +
  geom_line()+
  xlab('Number of visits')+
  scale_x_continuous(breaks = seq(0,60,20), labels = seq(0,60,20))+
  scale_color_manual(values = c('burlywood3', 'springgreen3', 'slateblue3'))+
  theme_pubclean()+
  theme(axis.title = element_text(size=10),
        axis.text = element_text(size=8, color='black'),
        legend.title = element_text(size=10),
        legend.key.size = unit(0.5, "cm"),
        axis.ticks.y = element_blank(),
        legend.text = element_text(size=10, color='black'),
        axis.line = element_line(color = 'black',linewidth = 0.2),
        panel.grid.major.y = element_line(linewidth = 0.1),
        panel.border = element_rect(colour = "black", fill=NA, size=0.1),
        strip.background = element_rect(colour="black",fill="white"),
        strip.text = element_text(size = 10),
        title = element_text(size = 10))+
  ggtitle('Sub-Adult Female')


PlotS4SAM <- ggplot(subset(pdf_NG_W, Age == 'SA' & Sex == 'M'), aes(x= Visit, y= Density, col = Year))+
  facet_grid(Dist ~ Season) +
  geom_line()+
  xlab('Number of visits')+
  scale_x_continuous(breaks = seq(0,60,20), labels = seq(0,60,20))+
  scale_color_manual(values = c('burlywood3', 'springgreen3', 'slateblue3'))+
  theme_pubclean()+
  theme(axis.title = element_text(size=10),
        axis.text = element_text(size=8, color='black'),
        legend.title = element_text(size=10),
        legend.key.size = unit(0.5, "cm"),
        axis.ticks.y = element_blank(),
        legend.text = element_text(size=10, color='black'),
        axis.line = element_line(color = 'black',linewidth = 0.2),
        panel.grid.major.y = element_line(linewidth = 0.1),
        panel.border = element_rect(colour = "black", fill=NA, size=0.1),
        strip.background = element_rect(colour="black",fill="white"),
        strip.text = element_text(size = 10),
        title = element_text(size = 10))+
  ggtitle('Sub-Adult Male')


PlotS4 <- ggarrange(PlotS4AF, PlotS4AM, PlotS4SAF, PlotS4SAM, ncol = 2, nrow = 2, common.legend = T)

tiff("Final_figures/FigureS4.tif",
     compression = "lzw",width=25,height=18,units="cm",res=200)

plot(PlotS4)
dev.off()


### Plot S5 ####

### NG PDF
plot_PDF_NG <- function(data, Ag, Se){
  
  Control_PG <-  rbind(subset(data, Age== Ag & Sex == Se & Treatment =='Carcass' & Year_AD %in% c(2,3)),
                       subset(data, Age== Ag & Sex == Se & Treatment =='Control'))
  pdf_0 <- density(subset(data, Age== Ag & Sex == Se & Treatment =='Carcass' & Year_AD == 0)$PropGV, from= 0, to= 1, bw=0.1)
  pdf_1 <- density(subset(data, Age== Ag & Sex == Se & Treatment =='Carcass' & Year_AD == 1)$PropGV, from= 0, to= 1, bw=0.1)
  pdf_210 <- density(Control_PG$PropGV, from=0, to =1, bw=0.1)
  
  PDF <- data.frame(Grazing = c(pdf_0$x, pdf_1$x, pdf_210$x),
                    Density = c(pdf_0$y, pdf_1$y, pdf_210$y),
                    Year = c(rep(0, length(pdf_0$x)),
                             rep(1, length(pdf_1$x)),
                             rep('2-10', length(pdf_210$x))),
                    Age = rep(Ag, length(pdf_0$x)+length(pdf_1$x)+length(pdf_210$x)),
                    Sex = rep(Se, length(pdf_0$x)+length(pdf_1$x)+length(pdf_210$x)))
  
  return(PDF)
}

pdf_NG_AF <- plot_PDF_NG(PGV_Z, 'A','F')
pdf_NG_AM <- plot_PDF_NG(PGV_Z, 'A','M')
pdf_NG_SAF <- plot_PDF_NG(PGV_Z, 'SA','F')
pdf_NG_SAM <- plot_PDF_NG(PGV_Z, 'SA','M')

pdf_NG <- rbind(pdf_NG_AF, pdf_NG_AM, pdf_NG_SAF, pdf_NG_SAM)
pdf_NG$Sex <- ifelse(pdf_NG$Sex == 'F', 'Female','Male')
pdf_NG$Age <- ifelse(pdf_NG$Age == 'A', 'Adult','Sub-Adult')


PlotS5 <- ggplot(pdf_NG, aes(x= Grazing, y= Density, col = Year))+
  facet_grid(Age ~ Sex) +
  geom_line()+
  xlab('Probability of grazing')+
  scale_x_continuous(breaks = seq(0,1,0.2), labels = seq(0,1,0.2))+
  scale_color_manual(values = c('burlywood3', 'springgreen3', 'slateblue3'))+
  theme_pubclean()+
  theme(axis.title = element_text(size=10),
        axis.text = element_text(size=8, color='black'),
        legend.title = element_text(size=10),
        legend.key.size = unit(0.5, "cm"),
        axis.ticks.y = element_blank(),
        legend.text = element_text(size=10, color='black'),
        axis.line = element_line(color = 'black',linewidth = 0.2),
        panel.grid.major.y = element_line(linewidth = 0.1),
        panel.border = element_rect(colour = "black", fill=NA, size=0.1),
        strip.background = element_rect(colour="black",fill="white"),
        strip.text = element_text(size = 10),
        title = element_text(size = 10))

tiff("Final_figures/FigureS5.tif",
     compression = "lzw",width=18,height=12,units="cm",res=200)

plot(PlotS5)
dev.off()


#### Plot S6 ###

pdf_NG_AF_W <- plot_PDF_NG(PGV_W, 'A','F')
pdf_NG_AM_W <- plot_PDF_NG(PGV_W, 'A','M')
pdf_NG_SAF_W <- plot_PDF_NG(PGV_W, 'SA','F')
pdf_NG_SAM_W <- plot_PDF_NG(PGV_W, 'SA','M')

pdf_NG_W <- rbind(pdf_NG_AF_W, pdf_NG_AM_W, pdf_NG_SAF_W, pdf_NG_SAM_W)
pdf_NG_W$Sex <- ifelse(pdf_NG_W$Sex == 'F', 'Female','Male')
pdf_NG_W$Age <- ifelse(pdf_NG_W$Age == 'A', 'Adult','Sub-Adult')

PlotS6 <- ggplot(pdf_NG_W, aes(x= Grazing, y= Density, col = Year))+
  facet_grid(Age ~ Sex) +
  geom_line()+
  xlab('Probability of grazing')+
  scale_x_continuous(breaks = seq(0,1,0.2), labels = seq(0,1,0.2))+
  scale_color_manual(values = c('burlywood3', 'springgreen3', 'slateblue3'))+
  theme_pubclean()+
  theme(axis.title = element_text(size=10),
        axis.text = element_text(size=8, color='black'),
        legend.title = element_text(size=10),
        legend.key.size = unit(0.5, "cm"),
        axis.ticks.y = element_blank(),
        legend.text = element_text(size=10, color='black'),
        axis.line = element_line(color = 'black',linewidth = 0.2),
        panel.grid.major.y = element_line(linewidth = 0.1),
        panel.border = element_rect(colour = "black", fill=NA, size=0.1),
        strip.background = element_rect(colour="black",fill="white"),
        strip.text = element_text(size = 10),
        title = element_text(size = 10))

tiff("Final_figures/FigureS6.tif",
     compression = "lzw",width=18,height=12,units="cm",res=200)

plot(PlotS6)
dev.off()


##### Plot S7 ###
## TG PDF

pdf_TG_HW <- density(log10(subset(IndG_demo_Z, Season == 'Hot_wet')$TG +1), from=0, bw=0.05)
pdf_TG_CD <- density(log10(subset(IndG_demo_Z, Season == 'Cool_dry')$TG +1), from=0, bw=0.05)
pdf_TG_HD <- density(log10(subset(IndG_demo_Z, Season == 'Hot_dry')$TG +1), from=0, bw=0.05)

pdf_TG <- data.frame(TG = c(pdf_TG_HW$x, pdf_TG_CD$x, pdf_TG_HD$x),
                     Density = c(pdf_TG_HW$y, pdf_TG_CD$y, pdf_TG_HD$y),
                     Season = c(rep('Hot_wet', length(pdf_TG_HW$x)),
                                rep('Cool_dry', length(pdf_TG_CD$x)),
                                rep('Hot_dry', length(pdf_TG_HD$x))))

pdf_TG$Season <- factor(pdf_TG$Season, levels = c('Hot_wet','Cool_dry','Hot_dry'))


PlotS7 <- ggplot(pdf_TG, aes(x= 10^TG - 1, y= Density, col = Season))+
  geom_line()+
  xlab('Time spent grazing (s)')+
  scale_x_continuous(breaks = seq(0,600,50), labels = seq(0,600,50))+
  scale_color_manual(values = c('blue3', 'orange2', 'red3'))+
  theme_pubclean()+
  theme(axis.title = element_text(size=10),
        axis.text = element_text(size=8, color='black'),
        legend.title = element_text(size=10),
        legend.key.size = unit(0.5, "cm"),
        axis.ticks.y = element_blank(),
        legend.text = element_text(size=10, color='black'),
        axis.line = element_line(color = 'black',linewidth = 0.2),
        panel.grid.major.y = element_line(linewidth = 0.1),
        panel.border = element_rect(colour = "black", fill=NA, size=0.1),
        strip.background = element_rect(colour="black",fill="white"),
        strip.text = element_text(size = 10),
        title = element_text(size = 10))

tiff("Final_figures/FigureS7.tif",
     compression = "lzw",width=18,height=12,units="cm",res=200)

plot(PlotS7)
dev.off()

### Plot S8 ####

pdf_TG_HW_W <- density(log10(subset(IndG_demo_W, Season == 'Hot_wet')$TG +1), from=0, bw=0.05)
pdf_TG_CD_W <- density(log10(subset(IndG_demo_W, Season == 'Cool_dry')$TG +1), from=0, bw=0.05)
pdf_TG_HD_W <- density(log10(subset(IndG_demo_W, Season == 'Hot_dry')$TG +1), from=0, bw=0.05)

pdf_TG_W <- data.frame(TG = c(pdf_TG_HW_W$x, pdf_TG_CD_W$x, pdf_TG_HD_W$x),
                       Density = c(pdf_TG_HW_W$y, pdf_TG_CD_W$y, pdf_TG_HD_W$y),
                       Season = c(rep('Hot_wet', length(pdf_TG_HW_W$x)),
                                  rep('Cool_dry', length(pdf_TG_CD_W$x)),
                                  rep('Hot_dry', length(pdf_TG_HD_W$x))))

pdf_TG_W$Season <- factor(pdf_TG_W$Season, levels = c('Hot_wet','Cool_dry','Hot_dry'))

PlotS8 <- ggplot(pdf_TG_W, aes(x= 10^TG - 1, y= Density, col = Season))+
  geom_line()+
  xlab('Time spent grazing (s)')+
  scale_x_continuous(breaks = seq(0,600,50), labels = seq(0,600,50))+
  scale_color_manual(values = c('blue3', 'orange2', 'red3'))+
  theme_pubclean()+
  theme(axis.title = element_text(size=10),
        axis.text = element_text(size=8, color='black'),
        legend.title = element_text(size=10),
        legend.key.size = unit(0.5, "cm"),
        axis.ticks.y = element_blank(),
        legend.text = element_text(size=10, color='black'),
        axis.line = element_line(color = 'black',linewidth = 0.2),
        panel.grid.major.y = element_line(linewidth = 0.1),
        panel.border = element_rect(colour = "black", fill=NA, size=0.1),
        strip.background = element_rect(colour="black",fill="white"),
        strip.text = element_text(size = 10),
        title = element_text(size = 10))

tiff("Final_figures/FigureS8.tif",
     compression = "lzw",width=18,height=12,units="cm",res=200)
plot(PlotS8)
dev.off()

#### Plot S9 ####


plotS9b <- plot_fig4(data = subset(Zebra_average_RO , LD == 10^5), tit = expression('b) LD = 10'^5), lims = c(0,100))
plotS9c <- plot_fig4(data = subset(Zebra_average_RO , LD == 10^6), tit =  expression('c) LD = 10'^6), lims = c(0,100))
plotS9d <- plot_fig4(data = subset(Zebra_average_RO , LD == 10^7), tit = expression('d) LD = 10'^7), lims = c(0,100))
plotS9e <- plot_fig4(data = subset(Zebra_average_RO , LD == 10^8), tit = expression('e) LD = 10'^8), lims = c(0,100))

PlotS9 <- ggarrange(Plot_Reading_key, plotS9b, plotS9c, ggplot()+theme_pubclean(), plotS9d, plotS9e, common.legend = T, ncol = 3, nrow = 2)
PlotS9 <- PlotS9 + annotate("text", x = 0.5, y = 0.018, size = 3,  label = "Percentage of animals ingesting soil in the wet season") 
PlotS9 <- PlotS9 + annotate("text", x = 0.01, y = 0.5, size = 3, angle = 90, label = "Percentage of animals ingesting soil in the dry season") 
PlotS9 <- PlotS9 + inset_element(p = Zeb_pic, left = 0.9, bottom = 0.9, right = 0.99,  top = 0.99)

tiff("Final_figures/FigureS9.tif",
     compression = "lzw",width=18, height=11, units="cm",res=200)
plot(PlotS9)
dev.off()


### Plot S10 ####

Wildebeest_average <- read.csv('Final_data/Wildebeest_Final_100.csv')
Wildebeest_average_R0 <- Data_analysis(Wildebeest_average)

plotS10b <- plot_fig4(data = subset(Wildebeest_average_R0, LD == 10^5), tit = expression('b) LD = 10'^5), lims = c(0,100))
plotS10c <- plot_fig4(data = subset(Wildebeest_average_R0, LD == 10^6), tit = expression('c) LD = 10'^6), lims = c(0,100))
plotS10d <- plot_fig4(data = subset(Wildebeest_average_R0, LD == 10^7), tit = expression('d) LD = 10'^7), lims = c(0,100))
plotS10e <- plot_fig4(data = subset(Wildebeest_average_R0, LD == 10^8), tit = expression('e) LD = 10'^8), lims = c(0,100))

PlotS10 <- ggarrange(Plot_Reading_key, plotS10b, plotS10c, ggplot()+theme_pubclean(), plotS10d, plotS10e, common.legend = T, ncol = 3, nrow = 2)
PlotS10 <- PlotS10 + annotate("text", x = 0.5, y = 0.018, size = 3,  label = "Percentage of animals ingesting soil in the wet season") 
PlotS10 <- PlotS10 + annotate("text", x = 0.01, y = 0.5, size = 3, angle = 90, label = "Percentage of animals ingesting soil in the dry season") 
PlotS10 <- PlotS10 + inset_element(p = Wil_pic, left = 0.9, bottom = 0.9, right = 0.99,  top = 0.99)


tiff("Final_figures/FigureS10.tif",
     compression = "lzw",width=18, height=11, units="cm",res=200)
plot(PlotS10)
dev.off()



#### Plot S11


WildebeestW_dryP <- read.csv('Final_data/Wildebeest__Dry_Pathogen_Wet_visit_100.csv')
WildebeestW_wetP <- read.csv('Final_data/Wildebeest__Wet_Pathogen_Wet_visit_100.csv')
WildebeestD_dryP <- read.csv('Final_data/Wildebeest__Dry_Pathogen_Dry_visit_100.csv')
WildebeestD_wetP <- read.csv('Final_data/Wildebeest__Wet_Pathogen_Dry_visit_100.csv')


WildebeestW_dryP_R0 <- Data_analysis(WildebeestW_dryP)
WildebeestW_wetP_R0 <- Data_analysis(WildebeestW_wetP)
WildebeestD_dryP_R0 <- Data_analysis(WildebeestD_dryP)
WildebeestD_wetP_R0 <- Data_analysis(WildebeestD_wetP)



plotS11b <- plot_fig4(data = subset(Wildebeest_average_R0, LD == 10^7), tit = 'b) Average rainfall; \n    all infectious sites', lims = c(0,20.5))
plotS11c <- plot_fig4(data = subset(WildebeestW_wetP_R0, LD == 10^7), tit = 'c) Average rainfall;\n    wet-season formed infectious sites', lims = c(0,20.5))
plotS11d <- plot_fig4(data = subset(WildebeestW_dryP_R0, LD == 10^7), tit = 'd) Average rainfall;\n    dry-season formed infectious sites', lims = c(0,20.5))
plotS11e <- plot_fig4(data = subset(WildebeestD_wetP_R0, LD == 10^7), tit = 'e) Simulated Drought;\n    wet-season formed infectious sites', lims = c(0,20.5))
plotS11f <- plot_fig4(data = subset(WildebeestD_dryP_R0, LD == 10^7), tit = 'f) Simulated Drought;\n    dry-season formed infectious site', lims = c(0,20.5))


PlotS11 <- ggarrange(Plot_Reading_key, plotS11c, plotS11e, plotS11b, plotS11d, plotS11f, common.legend = T, ncol = 3, nrow = 2)
PlotS11 <- PlotS11 + annotate("text", x = 0.5, y = 0.018, size = 3,  label = "Percentage of animals ingesting soil in the wet season") 
PlotS11 <- PlotS11 + annotate("text", x = 0.01, y = 0.5, size = 3, angle = 90, label = "Percentage of animals ingesting soil in the dry season") 
PlotS11 <- PlotS11 + inset_element(p = Wil_pic, left = 0.9, bottom = 0.9, right = 0.99,  top = 0.99)

tiff("Final_figures/FigureS11.tif",
     compression = "lzw",width=18, height=11, units="cm",res=200)

plot(PlotS11)
dev.off()


### Plot S12 ####

Wildebeest_YDR <- Wildebeest_average %>% group_by(LD, Year, Rep, Distance, Prop, Wwet, Wdry) %>%
  summarise(Infection = sum(Infected, na.rm=T),
            NV = sum(Visits, na.rm=T),
            NG = sum(Grazing, na.rm=T))

Wildebeest_Ymean <- Wildebeest_YDR %>% group_by(LD, Year) %>%
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

Wildebeest_Ymean_space <- subset(Wildebeest_YDR, Wwet %in% c(0, 0.1, 0.2, 0.3, 0.4) & 
                                   Wdry %in% c(0, 0.1, 0.2, 0.3, 0.4)) %>% group_by(LD, Year) %>%
  summarise(Infected = mean(Infection),
            Infected_sd = sd(Infection),
            NV = mean(NV),
            NG = mean(NG))

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



PlotS12 <- ggplot(Wildebeest_Ymean, aes(x=Year, y = Infected, fill = LD1))+
  geom_bar(position = position_dodge2(), alpha=0.5, stat = 'identity')+
  geom_bar(data=Wildebeest_Ymean_space, position = position_dodge2(), stat = 'identity')+
  geom_errorbar(data=Wildebeest_Ymean_space, aes(x = Year +0.02, ymin = SD_min , ymax = SD_max), position = position_dodge2(), lty =2, size = 0.2)+
  geom_errorbar(data=Wildebeest_Ymean, aes(x = Year - 0.02, ymin = SD_min , ymax = SD_max), position = position_dodge2(), size = 0.2)+
  
  scale_y_sqrt(breaks = c(0, 1, 3, 5, 7, 10, 15, 20, 25, 30, 35),
               limits = c(0, 35)) +
  
  scale_x_continuous(breaks = seq(0,10))+
  scale_fill_manual('Lethal dose threshold ',values= c('blue3','green3','orange1','red2'), 
                    labels =  c(expression('10'^5), expression('10'^6), expression('10'^7),expression('10'^8)))+  
  xlab('Year after death') + ylab('Number of infections')+
  theme_pubclean()+
  theme(axis.title = element_text(size=10),
        axis.text = element_text(size=8, color='black'),
        legend.title = element_text(size=10),
        legend.key.size = unit(0.25, "cm"),
        axis.ticks.y = element_blank(),
        legend.text = element_text(size=10, color='black'),
        axis.line = element_line(color = 'black',linewidth = 0.2),
        panel.grid.major.y = element_line(linewidth = 0.1),
        strip.background = element_rect(colour="black",fill="white"))


PlotS12 <- PlotS12 + inset_element(p = Wil_pic, left = 0.8, bottom = 0.9, right = 1,  top = 1.1)

tiff("Final_figures/FigureS12.tif",
     compression = "lzw",width=11,height=8,units="cm",res=200)

plot(PlotS12)
dev.off()


###### PLOT S13

Zebra_test <- subset(Zebra_YDR, Prop %in% c('0/0','2/1','3/2','4/4','10/10')) %>% group_by(LD, Year, Prop) %>%
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


Zebra_test <- Zebra_test %>% mutate(Prop = case_when(
  Prop == '0/0' ~ '0% / 0%',
  Prop == '2/1' ~ '20% / 10%',
  Prop == '3/2' ~ '30% / 20%',
  Prop == '4/4' ~ '40% / 40%',
  Prop == '10/10' ~ '100% / 100%',
))

Zebra_test$Prop <- factor(Zebra_test$Prop, 
                          levels = c('0% / 0%','20% / 10%','30% / 20%','40% / 40%','100% / 100%'))


PlotS13a <- ggplot(subset(Zebra_test, LD == 10^5), aes(x=as.factor(Year), y = Infected, fill = Prop))+
  geom_bar(position = position_dodge2(), stat = 'identity')+
  geom_errorbar(data=subset(Zebra_test, LD == 10^5), aes(ymin = SD_min , ymax = SD_max), position = position_dodge2(), size = 0.2)+
  
  scale_y_sqrt(breaks = c(0, 1, 3, 5, 7, 10, 15, 25, 35, 45),
               limits = c(0, 45)) +
  
  scale_fill_manual('Proportion of individuals ingesting soil per season (wet/dry) ',
                    values= c("green",'grey30','purple2','chocolate3','lightblue'))+
  
  xlab('Year after death') + ylab('Number of infections')+
  theme_pubclean()+
  theme(axis.title = element_text(size=10),
        axis.text = element_text(size=8, color='black'),
        legend.title = element_text(size=10),
        legend.key.size = unit(0.4, "cm"),
        axis.ticks.y = element_blank(),
        legend.text = element_text(size=10, color='black'),
        axis.line = element_line(color = 'black',linewidth = 0.2),
        panel.grid.major.y = element_line(linewidth = 0.1),
        strip.background = element_rect(colour="black",fill="white"),
        title = element_text(size = 10))+
  guides(fill=guide_legend(nrow=2, byrow=TRUE)) +
  ggtitle(expression('a) LD = 10'^5))

PlotS13b <- ggplot(subset(Zebra_test, LD == 10^6), aes(x=as.factor(Year), y = Infected, fill = Prop))+
  geom_bar(position = position_dodge2(), stat = 'identity')+
  geom_errorbar(data=subset(Zebra_test, LD == 10^6), aes(ymin = SD_min , ymax = SD_max), position = position_dodge2(), size = 0.2)+
  
  scale_y_sqrt(breaks = c(0, 1, 3, 5, 7, 10, 15, 25, 35, 45),
               limits = c(0, 45)) +
  
  scale_fill_manual('Proportion of individuals ingesting soil per season (wet/dry) ',
                    values= c("green",'grey30','purple2','chocolate3','lightblue'))+
  
  xlab('Year after death') + ylab('Number of infections')+
  theme_pubclean()+
  theme(axis.title = element_text(size=10),
        axis.text = element_text(size=8, color='black'),
        legend.title = element_text(size=10),
        legend.key.size = unit(0.4, "cm"),
        axis.ticks.y = element_blank(),
        legend.text = element_text(size=10, color='black'),
        axis.line = element_line(color = 'black',linewidth = 0.2),
        panel.grid.major.y = element_line(linewidth = 0.1),
        strip.background = element_rect(colour="black",fill="white"),
        title = element_text(size = 10))+
  guides(fill=guide_legend(nrow=2, byrow=TRUE)) +
  ggtitle(expression('b) LD = 10'^6))

PlotS13c <- ggplot(subset(Zebra_test, LD == 10^7), aes(x=as.factor(Year), y = Infected, fill = Prop))+
  geom_bar(position = position_dodge2(), stat = 'identity')+
  geom_errorbar(data=subset(Zebra_test, LD == 10^7), aes(ymin = SD_min , ymax = SD_max), position = position_dodge2(), size = 0.2)+
  
  scale_y_sqrt(breaks = c(0, 1, 3, 5, 7, 10, 15, 25 ,35 ,45),
               limits = c(0, 45)) +
  
  scale_fill_manual('Proportion of individuals ingesting soil per season (wet/dry) ',
                    values= c("green",'grey30','purple2','chocolate3','lightblue'))+
  
  xlab('Year after death') + ylab('Number of infections')+
  theme_pubclean()+
  theme(axis.title = element_text(size=10),
        axis.text = element_text(size=8, color='black'),
        legend.title = element_text(size=10),
        legend.key.size = unit(0.4, "cm"),
        axis.ticks.y = element_blank(),
        legend.text = element_text(size=10, color='black'),
        axis.line = element_line(color = 'black',linewidth = 0.2),
        panel.grid.major.y = element_line(linewidth = 0.1),
        strip.background = element_rect(colour="black",fill="white"),
        title = element_text(size = 10))+
  guides(fill=guide_legend(nrow=2, byrow=TRUE)) +
  ggtitle(expression('c) LD = 10'^7))

PlotS13d <- ggplot(subset(Zebra_test, LD == 10^8), aes(x=as.factor(Year), y = Infected, fill = Prop))+
  geom_bar(position = position_dodge2(), stat = 'identity')+
  geom_errorbar(data=subset(Zebra_test, LD == 10^8), aes(ymin = SD_min , ymax = SD_max), position = position_dodge2(), size = 0.2)+
  
  scale_y_sqrt(breaks = c(0, 1, 3, 5, 7, 10, 15,25,35,45),
               limits = c(0, 45)) +
  
  scale_fill_manual('Proportion of individuals ingesting soil per season (wet/dry) ',
                    values= c("green",'grey30','purple2','chocolate3','lightblue'))+
  
  xlab('Year after death') + ylab('Number of infections')+
  theme_pubclean()+
  theme(axis.title = element_text(size=10),
        axis.text = element_text(size=8, color='black'),
        legend.title = element_text(size=10),
        legend.key.size = unit(0.4, "cm"),
        axis.ticks.y = element_blank(),
        legend.text = element_text(size=10, color='black'),
        axis.line = element_line(color = 'black',linewidth = 0.2),
        panel.grid.major.y = element_line(linewidth = 0.1),
        strip.background = element_rect(colour="black",fill="white"),
        title = element_text(size = 10))+
  guides(fill=guide_legend(nrow=2, byrow=TRUE)) +
  ggtitle(expression('d) LD = 10'^8))



PlotS13 <- ggarrange(PlotS13a, PlotS13b, PlotS13c, PlotS13d, common.legend = T, nrow = 2, ncol = 2)
PlotS13 <- PlotS13 + inset_element(p = Zeb_pic, left = 0.90, bottom = 0.85, right = 0.98,  top = 0.92)


tiff("Final_figures/FigureS13.tif",
     compression = "lzw",width=18,height=14,units="cm",res=200)

plot(PlotS13)
dev.off()



###### PLOT S14

Wildebeest_test <- subset(Wildebeest_YDR, Prop %in% c('0/0','2/1','3/2','4/4','10/10')) %>% group_by(LD, Year, Prop) %>%
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
  Prop == '0/0' ~ '0% / 0%',
  Prop == '2/1' ~ '20% / 10%',
  Prop == '3/2' ~ '30% / 20%',
  Prop == '4/4' ~ '40% / 40%',
  Prop == '10/10' ~ '100% / 100%',
))

Wildebeest_test$Prop <- factor(Wildebeest_test$Prop, 
                               levels = c('0% / 0%','20% / 10%','30% / 20%','40% / 40%','100% / 100%'))


PlotS14a <- ggplot(subset(Wildebeest_test, LD == 10^5), aes(x=as.factor(Year), y = Infected, fill = Prop))+
  geom_bar(position = position_dodge2(), stat = 'identity')+
  geom_errorbar(data=subset(Wildebeest_test, LD == 10^5), aes(ymin = SD_min , ymax = SD_max), position = position_dodge2(), size = 0.2)+
  
  scale_y_sqrt(breaks = c(0, 1, 3, 5, 7, 10, 15, 20, 25, 30, 35),
               limits = c(0, 35)) +
  
  scale_fill_manual('Proportion of individuals ingesting soil per season (wet/dry) ',
                    values= c("green",'grey30','purple2','chocolate3','lightblue'))+
  
  xlab('Year after death') + ylab('Number of infections')+
  theme_pubclean()+
  theme(axis.title = element_text(size=10),
        axis.text = element_text(size=8, color='black'),
        legend.title = element_text(size=10),
        legend.key.size = unit(0.4, "cm"),
        axis.ticks.y = element_blank(),
        legend.text = element_text(size=10, color='black'),
        axis.line = element_line(color = 'black',linewidth = 0.2),
        panel.grid.major.y = element_line(linewidth = 0.1),
        strip.background = element_rect(colour="black",fill="white"),
        title = element_text(size = 10))+
  guides(fill=guide_legend(nrow=2, byrow=TRUE)) +
  ggtitle(expression('a) LD = 10'^5))


PlotS14b <- ggplot(subset(Wildebeest_test, LD == 10^6), aes(x=as.factor(Year), y = Infected, fill = Prop))+
  geom_bar(position = position_dodge2(), stat = 'identity')+
  geom_errorbar(data=subset(Wildebeest_test, LD == 10^6), aes(ymin = SD_min , ymax = SD_max), position = position_dodge2(), size = 0.2)+
  
  scale_y_sqrt(breaks = c(0, 1, 3, 5, 7, 10, 15, 20, 25, 30, 35),
               limits = c(0, 35)) +
  
  scale_fill_manual('Proportion of individuals ingesting soil per season (wet/dry) ',
                    values= c("green",'grey30','purple2','chocolate3','lightblue'))+
  
  xlab('Year after death') + ylab('Number of infections')+
  theme_pubclean()+
  theme(axis.title = element_text(size=10),
        axis.text = element_text(size=8, color='black'),
        legend.title = element_text(size=10),
        legend.key.size = unit(0.4, "cm"),
        axis.ticks.y = element_blank(),
        legend.text = element_text(size=10, color='black'),
        axis.line = element_line(color = 'black',linewidth = 0.2),
        panel.grid.major.y = element_line(linewidth = 0.1),
        strip.background = element_rect(colour="black",fill="white"),
        title = element_text(size = 10))+
  guides(fill=guide_legend(nrow=2, byrow=TRUE)) +
  ggtitle(expression('b) LD = 10'^6))



PlotS14c <- ggplot(subset(Wildebeest_test, LD == 10^7), aes(x=as.factor(Year), y = Infected, fill = Prop))+
  geom_bar(position = position_dodge2(), stat = 'identity')+
  geom_errorbar(data=subset(Wildebeest_test, LD == 10^7), aes(ymin = SD_min , ymax = SD_max), position = position_dodge2(), size = 0.2)+
  
  scale_y_sqrt(breaks = c(0, 1, 3, 5, 7, 10, 15, 20, 25, 30, 35),
               limits = c(0, 35)) +
  
  scale_fill_manual('Proportion of individuals ingesting soil per season (wet/dry) ',
                    values= c("green",'grey30','purple2','chocolate3','lightblue'))+
  
  xlab('Year after death') + ylab('Number of infections')+
  theme_pubclean()+
  theme(axis.title = element_text(size=10),
        axis.text = element_text(size=8, color='black'),
        legend.title = element_text(size=10),
        legend.key.size = unit(0.4, "cm"),
        axis.ticks.y = element_blank(),
        legend.text = element_text(size=10, color='black'),
        axis.line = element_line(color = 'black',linewidth = 0.2),
        panel.grid.major.y = element_line(linewidth = 0.1),
        strip.background = element_rect(colour="black",fill="white"),
        title = element_text(size = 10))+
  guides(fill=guide_legend(nrow=2, byrow=TRUE)) +
  ggtitle(expression('c) LD = 10'^7))



PlotS14d <- ggplot(subset(Wildebeest_test, LD == 10^8), aes(x=as.factor(Year), y = Infected, fill = Prop))+
  geom_bar(position = position_dodge2(), stat = 'identity')+
  geom_errorbar(data=subset(Wildebeest_test, LD == 10^8), aes(ymin = SD_min , ymax = SD_max), position = position_dodge2(), size = 0.2)+
  
  scale_y_sqrt(breaks = c(0, 1, 3, 5, 7, 10, 15, 20, 25, 30, 35),
               limits = c(0, 35)) +
  
  scale_fill_manual('Proportion of individuals ingesting soil per season (wet/dry) ',
                    values= c("green",'grey30','purple2','chocolate3','lightblue'))+
  
  xlab('Year after death') + ylab('Number of infections')+
  theme_pubclean()+
  theme(axis.title = element_text(size=10),
        axis.text = element_text(size=8, color='black'),
        legend.title = element_text(size=10),
        legend.key.size = unit(0.4, "cm"),
        axis.ticks.y = element_blank(),
        legend.text = element_text(size=10, color='black'),
        axis.line = element_line(color = 'black',linewidth = 0.2),
        panel.grid.major.y = element_line(linewidth = 0.1),
        strip.background = element_rect(colour="black",fill="white"),
        title = element_text(size = 10))+
  guides(fill=guide_legend(nrow=2, byrow=TRUE)) +
  ggtitle(expression('d) LD = 10'^8))


PlotS14 <- ggarrange(PlotS14a, PlotS14b, PlotS14c, PlotS14d, common.legend = T, nrow = 2, ncol = 2)
PlotS14 <- PlotS14 + inset_element(p = Wil_pic, left = 0.90, bottom = 0.85, right = 0.98,  top = 0.92)

tiff("Final_figures/FigureS14.tif",
     compression = "lzw",width=18,height=12,units="cm",res=200)
plot(PlotS14)
dev.off()
