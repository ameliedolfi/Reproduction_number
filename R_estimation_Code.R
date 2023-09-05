####### R0 esrimation model #######


# @author :  Amélie Dolfi
# @ Date 2/2/2023

rm(list=ls())

## Libraries

lapply(c('ggplot2','ggpubr','tidyverse', 'dplyr', 'doParallel', 'purrr',
         'lubridate', 'rphylopic', 'ggbreak', 'parallel', 'foreach'), library, character.only=TRUE)


## Directory
wd <- 'C:/Users/Amelie/OneDrive - UW-Madison/Documents/PhDUW/PhD/Github/Reproduction_number'
setwd(wd)
set.seed(52)


## Load the DATA

### CFU
Extrapolation_10Y <- read.csv('Extrapolation.csv', sep=',')
Location_CFU <- read.csv('Location_CFU.csv', sep=',')

### Camera location
Water_Pan_Camera <- read.csv('Location_Camera.csv', sep=',')


### ZEBRA individuals and monthly

Num_demo_Z <- subset(read.csv('Camera_monthly_summary.csv'), Species == "Zebra")
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

PGV_Z <- Num_demo_PGV_Z[!is.na(Num_demo_PGV_Z$PropGV)==T,]

Ind_Z <- subset(read.csv('Camera_all_individuals.csv', header=T, sep=','), Species =="Zebra")
Ind_Z$Season <- factor(Ind_Z$Season, levels= c('Hot_wet','Cool_dry','Hot_dry'))
Ind_Z <- merge(Ind_Z, Water_Pan_Camera, by='ID', all.x=T, sort=F)
Ind_Z$Season <- factor(Ind_Z$Season, levels= c('Hot_wet','Cool_dry','Hot_dry'))
Ind_Z$dTreatment <- ifelse(Ind_Z$Treatment == 'Carcass', 1, 0)

IndG_demo_Z <- subset(Ind_Z, TG > 0)


Num_demo_PGV_Z$Distance <- ifelse(Num_demo_PGV_Z$Water_km <= 4, '0-4km',
                                  ifelse(Num_demo_PGV_Z$Water_km > 4 & Num_demo_PGV_Z$Water_km <= 8, '4-8km',
                                         ifelse(Num_demo_PGV_Z$Water_km > 8, '+8km','')))

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


### WILDEBEEST individuals and monthly

Num_demo_W <- subset(read.csv('Camera_monthly_summary.csv'), Species == "Wildebeest")
Num_demo_W <- merge(Num_demo_W, Water_Pan_Camera, by='ID', all.x=T, sort=F)
Num_demo_W$Season <- factor(Num_demo_W$Season, levels= c('Hot_wet','Cool_dry','Hot_dry'))
Num_demo_W$Visits <- ifelse(Num_demo_W$NV >0, 1, 0)
Num_demo_W$dTreatment <- ifelse(Num_demo_W$Treatment == 'Carcass', 1, 0)

Num_demo_PGV_W <- Num_demo_W
Num_demo_PGV_W$P <- cbind(Num_demo_PGV_W$NG, Num_demo_PGV_W$NV)
Num_demo_PGV_W <- Num_demo_PGV_W %>% mutate(Year_AD = case_when(
  Month_AD < 12 ~ '0',
  Month_AD >= 12 & Month_AD < 24 ~ '1',
  Month_AD >=24 ~'2'
))

PGV_W <- Num_demo_PGV_W[!is.na(Num_demo_PGV_W$PropGV)==T,]


Ind_W <- subset(read.csv('Camera_all_individuals.csv', header=T, sep=','), Species =="Wildebeest")
Ind_W$Season <- factor(Ind_W$Season, levels= c('Hot_wet','Cool_dry','Hot_dry'))
Ind_W <- merge(Ind_W, Water_Pan_Camera, by='ID', all.x=T, sort=F)
Ind_W$Season <- factor(Ind_W$Season, levels= c('Hot_wet','Cool_dry','Hot_dry'))
Ind_W$dTreatment <- ifelse(Ind_W$Treatment == 'Carcass', 1, 0)

IndG_demo_W <- subset(Ind_W, TG > 0 & TG < 1400)


Num_demo_PGV_W$Distance <- ifelse(Num_demo_PGV_W$Pan_km <= 3, '0-3km',
                                  ifelse(Num_demo_PGV_W$Pan_km > 3 & Num_demo_PGV_W$Pan_km <= 6, '3-6km',
                                         ifelse(Num_demo_PGV_W$Pan_km > 6, '+6km','')))

Average_NV_W <- Num_demo_PGV_W %>% group_by(ID, Year, Year_AD, Distance, Treatment, Season, Age, Sex) %>%
  summarize(NV = sum(NV),
            NG = sum(NG))

Average_NV_W  <- merge(Average_NV_W, Month_season_ID, by = c('ID','Year','Season'))

Average_NV_W$Average_NV <- Average_NV_W$NV / Average_NV_W$Nmonth * 4
Average_NV_W$Average_NG <- Average_NV_W$NG / Average_NV_W$Nmonth * 4




##### Function calculating R

Risk_infection <- function(Year, Boot_s, LD, PG, Seas, Extrapolation, Average_NV,
                           IndG, Grams, BN,  psoil, Distance,  Prop){
  
  ID <- unique(Extrapolation$carcass_ID)
  
  Infection <- crossing(LD, Season, Year, Rep = seq(1, Boot_s), Distance, Age, Sex)
  Infection$Visits <- NA
  Infection$Grazing <- NA
  Infection$Infected <- NA
  
  
  for (y in Year){
    for (s in c(1,2,3)){
      for (w in Distance){
        for(Ag in Age){
          for (Se in Sex){
            
            ### PDF on visit and grazing depends on the years after death
            if(y %in% c(0,1)){
              pdf_NV <- density(subset(Average_NV, Age== Ag & Sex == Se & Treatment =='Carcass' & Year_AD == y &  Season == Seas[s] & Distance == w)$Average_NV, from=0, bw=10)
              pdf_GV <- density(subset(PG, Age== Ag & Sex == Se & Treatment =='Carcass' & Year_AD == y)$PropGV, from= 0, to= 1, bw=0.1)
              
            }else{
              Control_Visit <-  rbind(subset(Average_NV, Age== Ag & Sex == Se & Treatment =='Control' &  Season == Seas[s] & Distance == w), 
                                      subset(Average_NV, Age== Ag & Sex == Se & Treatment =='Carcass' & Year_AD %in% c(2,3), Season == Seas[s] & Distance == w))
              Control_PG <- rbind(subset(PG, Age== Ag & Sex == Se & Treatment =='Carcass' & Year_AD %in% c(2,3)),
                                  subset(PG, Age== Ag & Sex == Se & Treatment =='Control'))
              
              pdf_NV <- density(Control_Visit$Average_NV, from=0, bw=10)
              pdf_GV <- density(Control_PG$PropGV, from=0, to =1, bw=0.1)
              
            }
            
            ### PDF for time spend grazing depends on season only
            pdf_TG <- density(log10(subset(IndG, Season == Seas[[s]])$TG +1), from=0, bw=0.05)
            
            j <- 1 
            repeat{
              Nvisits <- sample(pdf_NV$x, 1, TRUE, pdf_NV$y)
              Pgraze <- sample(pdf_GV$x, Nvisits, TRUE, pdf_GV$y)
              Ngraze <- sum( runif(Nvisits) < Pgraze)
              
              if(Ngraze == 0 ){ 
                j <- j+1
              }else{ 
                Site <- sample(x = subset(Extrapolation, Age == y)$ID, size = 1)
                
                CFU_grassTop <- as.numeric(subset(Extrapolation, ID == Site & Age == y)$Grass_Top_CFU)
                Time_G <- 10^sample(pdf_TG$x, Ngraze , TRUE, pdf_TG$y) - 1
                ngram <- round(Time_G * (BN /60))* Grams
                ngram <- ifelse(ngram ==0, 1, ngram)
                Risk <- c()
                
                for( i in (1:length(ngram))){
                  if(runif(1) > Prop[s]){  ## Gives the proportion of individuals eating grass only or grass+soil
                    Risk <- c(Risk, sum(rpois(ngram[i], CFU_grassTop)))
                  }else{
                    CFU_soil <-  as.numeric(subset(Extrapolation_10Y, ID == Site & Age == y)$Soil_CFU)
                    Risk <- c(Risk, sum(rpois(ngram[i], (psoil * CFU_soil) + ((1-psoil)*CFU_grassTop))))
                  }
                }
                
                Infection[Infection$Year == y & Infection$Season == Season[s] & Infection$Distance == w &
                            Infection$Rep == j & Infection$Age == Ag & Infection$Sex == Se, 
                          'Visits'] <- Nvisits
                Infection[Infection$Year == y & Infection$Season == Season[s] & Infection$Distance == w &
                            Infection$Rep == j & Infection$Age == Ag & Infection$Sex == Se,
                          'Grazing'] <- Ngraze
                Infection[Infection$LD == LD[1] & Infection$Year == y & Infection$Season == Season[s] & Infection$Distance == w &
                            Infection$Rep == j & Infection$Age == Ag & Infection$Sex == Se,
                          'Infected'] <- sum(ifelse(Risk >= LD[1], 1, 0))
                Infection[Infection$LD == LD[2] & Infection$Year == y & Infection$Season == Season[s] & Infection$Distance == w &
                            Infection$Rep == j & Infection$Age == Ag & Infection$Sex == Se,
                          'Infected'] <- sum(ifelse(Risk >= LD[2], 1, 0))
                Infection[Infection$LD == LD[3] & Infection$Year == y & Infection$Season == Season[s] & Infection$Distance == w &
                            Infection$Rep == j & Infection$Age == Ag & Infection$Sex == Se,
                          'Infected'] <- sum(ifelse(Risk >= LD[3], 1, 0))
                Infection[Infection$LD == LD[4] & Infection$Year == y & Infection$Season == Season[s] & Infection$Distance == w &
                            Infection$Rep ==  j & Infection$Age == Ag & Infection$Sex == Se,
                          'Infected'] <- sum(ifelse(Risk >= LD[4], 1, 0))
                
                j <- j+1
              }
              
              if(j >= (Boot_s + 1))break}
            
          }
        }
        
        
      }
    }
  }
  return(Infection)
}



############### LAunch the function 

### ALL parameters

Year = seq(0,10)
Boot_s = 100
LD = c(10^5, 10^6, 10^7,10^8)
Season = c('Hot_wet','Cool_dry','Hot_dry')
psoil = 0.1
Age=c('A','SA')
Sex=c('F','M')


# ZEBRA

Gram_Z = 2
BN_Z = 27
Distance_W=  c('0-4km','4-8km','+8km')


####### Zebra Dry season Pathogen on Wet Visits
registerDoParallel(6)

print(Sys.time())
Zebra_DryPathogen_WetVisits = foreach(wet=rep(seq(0,1, by = 0.1), 11), dry = rep(seq(0,1, by = 0.1), each = 11), .packages = c("tidyr")) %dopar%{
  p <- c(wet, dry, dry)
  Zebra_propDryP <- Risk_infection(Year = Year ,Boot_s = Boot_s , LD = LD , PG = PGV_Z, Seas = Season ,
                                   Extrapolation = subset(Extrapolation_10Y, Season != 'Hot_Wet'), Average_NV = Average_NV_Z,
                                   IndG = IndG_demo_Z , Grams = Gram_Z, BN = BN_Z, psoil = psoil, 
                                   Distance = Distance_W, Prop = p)
  
  Zebra_propDryP$Prop <- paste0(as.character(p[1]*10),'/',as.character(p[2]*10))
  Zebra_propDryP$Wwet <- wet
  Zebra_propDryP$Wdry <- dry
  return(Zebra_propDryP)
  
}
print(Sys.time())


Zebra_DryPathogen_WetVisits[sapply(Zebra_DryPathogen_WetVisits, is.null)] <- NULL
Zebra_DryPathogen_WetVisits_Final <- map_dfr(Zebra_DryPathogen_WetVisits, bind_rows) 


write.csv(Zebra_DryPathogen_WetVisits_Final,'Zebra__Dry_Pathogen_Wet_visit_100.csv', row.names = F)

#######  Zebra Wet season Pathogen on Wet Visits

print(Sys.time())

Zebra_WetPathogen_WetVisits = foreach(wet=rep(seq(0,1, by = 0.1), 11), dry = rep(seq(0,1, by = 0.1), each = 11), .packages = c("tidyr")) %dopar%{
  p <- c(wet, dry, dry)
  Zebra_propWetP <- Risk_infection(Year = Year ,Boot_s = Boot_s , LD = LD , PG = PGV_Z, Seas = Season ,
                                   Extrapolation = subset(Extrapolation_10Y, Season == 'Hot_Wet'), Average_NV = Average_NV_Z,
                                   IndG = IndG_demo_Z , Grams = Gram_Z, BN = BN_Z, psoil = psoil, 
                                   Distance = Distance_W, Prop = p)
  
  Zebra_propWetP$Prop <- paste0(as.character(p[1]*10),'/',as.character(p[2]*10))
  Zebra_propWetP$Wwet <- wet
  Zebra_propWetP$Wdry <- dry
  return(Zebra_propWetP)
  
}
print(Sys.time())


Zebra_WetPathogen_WetVisits[sapply(Zebra_WetPathogen_WetVisits, is.null)] <- NULL
Zebra_WetPathogen_WetVisits_Final <- map_dfr(Zebra_WetPathogen_WetVisits, bind_rows) 


write.csv(Zebra_WetPathogen_WetVisits_Final,'Zebra__Wet_Pathogen_Wet_visit_100.csv', row.names = F)


#######  Zebra Wet season Pathogen on Dry Visits

Season = c('Hot_dry','Hot_dry','Hot_dry')


print(Sys.time())

Zebra_WetPathogen_DryVisits = foreach(wet=rep(seq(0,1, by = 0.1), 11), dry = rep(seq(0,1, by = 0.1), each = 11), .packages = c("tidyr")) %dopar%{
  p <- c(wet, dry, dry)
  Zebra_propWetP <- Risk_infection(Year = Year ,Boot_s = Boot_s , LD = LD , PG = PGV_Z, Seas = Season ,
                                   Extrapolation = subset(Extrapolation_10Y, Season == 'Hot_Wet'), Average_NV = Average_NV_Z,
                                   IndG = IndG_demo_Z , Grams = Gram_Z, BN = BN_Z, psoil = psoil, 
                                   Distance = Distance_W, Prop = p)
  
  Zebra_propWetP$Prop <- paste0(as.character(p[1]*10),'/',as.character(p[2]*10))
  Zebra_propWetP$Wwet <- wet
  Zebra_propWetP$Wdry <- dry
  return(Zebra_propWetP)
  
}
print(Sys.time())


Zebra_WetPathogen_DryVisits[sapply(Zebra_WetPathogen_DryVisits, is.null)] <- NULL
Zebra_WetPathogen_DryVisits_Final <- map_dfr(Zebra_WetPathogen_DryVisits_Final, bind_rows) 


write.csv(Zebra_WetPathogen_DryVisits_Final,'Zebra__Wet_Pathogen_Dry_visit_100.csv', row.names = F)




#######  Zebra Dry season Pathogen on Dry Visits

Season = c('Hot_dry','Hot_dry','Hot_dry')


print(Sys.time())

Zebra_DryPathogen_DryVisits = foreach(wet=rep(seq(0,1, by = 0.1), 11), dry = rep(seq(0,1, by = 0.1), each = 11), .packages = c("tidyr")) %dopar%{
  p <- c(wet, dry, dry)
  Zebra_propDryP <- Risk_infection(Year = Year ,Boot_s = Boot_s , LD = LD , PG = PGV_Z, Seas = Season ,
                                   Extrapolation = subset(Extrapolation_10Y, Season != 'Hot_Wet'), Average_NV = Average_NV_Z,
                                   IndG = IndG_demo_Z , Grams = Gram_Z, BN = BN_Z, psoil = psoil, 
                                   Distance = Distance_W, Prop = p)
  
  Zebra_propDryP$Prop <- paste0(as.character(p[1]*10),'/',as.character(p[2]*10))
  Zebra_propDryP$Wwet <- wet
  Zebra_propDryP$Wdry <- dry
  return(Zebra_propDryP)
  
}
print(Sys.time())


Zebra_DryPathogen_DryVisits[sapply(Zebra_DryPathogen_DryVisits, is.null)] <- NULL
Zebra_DryPathogen_DryVisits_Final <- map_dfr(Zebra_DryPathogen_DryVisits_Final, bind_rows) 


write.csv(Zebra_DryPathogen_DryVisits_Final,'Zebra__Dry_Pathogen_Dry_visit_100.csv', row.names = F)



############ Wildebeest 

Gram_W = 1
BN_W = 26
Distance_P =  c('0-3km','3-6km','+6km')
Season = c('Hot_wet','Cool_dry','Hot_dry')


####### Wildebeest Dry season Pathogen on Wet Visits
print(Sys.time())
Wildebeest_DryPathogen_WetVisits = foreach(wet=rep(seq(0,1, by = 0.1), 11), dry = rep(seq(0,1, by = 0.1), each = 11), .packages = c("tidyr")) %dopar%{
  p <- c(wet, dry, dry)
  Wildebeest_propDryP <- Risk_infection(Year = Year ,Boot_s = Boot_s , LD = LD , PG = PGV_W, Seas = Season ,
                                   Extrapolation = subset(Extrapolation_10Y, Season != 'Hot_Wet'), Average_NV = Average_NV_W,
                                   IndG = IndG_demo_W , Grams = Gram_W, BN = BN_W, psoil = psoil, 
                                   Distance = Distance_P, Prop = p)
  
  Wildebeest_propDryP$Prop <- paste0(as.character(p[1]*10),'/',as.character(p[2]*10))
  Wildebeest_propDryP$Wwet <- wet
  Wildebeest_propDryP$Wdry <- dry
  return(Wildebeest_propDryP)
  
}
print(Sys.time())


Wildebeest_DryPathogen_WetVisits[sapply(Wildebeest_DryPathogen_WetVisits, is.null)] <- NULL
Wildebeest_DryPathogen_WetVisits_Final <- map_dfr(Wildebeest_DryPathogen_WetVisits, bind_rows) 


write.csv(Wildebeest_DryPathogen_WetVisits_Final,'Wildebeest__Dry_Pathogen_Wet_visit_100.csv', row.names = F)


#######  Wildebeest Wet season Pathogen on Wet Visits

print(Sys.time())

Wildebeest_WetPathogen_WetVisits = foreach(wet=rep(seq(0,1, by = 0.1), 11), dry = rep(seq(0,1, by = 0.1), each = 11), .packages = c("tidyr")) %dopar%{
  p <- c(wet, dry, dry)
  Wildebeest_propWetP <- Risk_infection(Year = Year ,Boot_s = Boot_s , LD = LD , PG = PGV_W, Seas = Season ,
                                   Extrapolation = subset(Extrapolation_10Y, Season == 'Hot_Wet'), Average_NV = Average_NV_W,
                                   IndG = IndG_demo_W , Grams = Gram_W, BN = BN_W, psoil = psoil, 
                                   Distance = Distance_P, Prop = p)
  
  Wildebeest_propWetP$Prop <- paste0(as.character(p[1]*10),'/',as.character(p[2]*10))
  Wildebeest_propWetP$Wwet <- wet
  Wildebeest_propWetP$Wdry <- dry
  return(Wildebeest_propWetP)
  
}
print(Sys.time())


Wildebeest_WetPathogen_WetVisits[sapply(Wildebeest_WetPathogen_WetVisits, is.null)] <- NULL
Wildebeest_WetPathogen_WetVisits_Final <- map_dfr(Wildebeest_WetPathogen_WetVisits, bind_rows) 


write.csv(Wildebeest_WetPathogen_WetVisits_Final,'Wildebeest__Wet_Pathogen_Wet_visit_100.csv', row.names = F)


#######  Wildebeest Wet season Pathogen on Dry Visits

Season = c('Hot_dry','Hot_dry','Hot_dry')


print(Sys.time())

Wildebeest_WetPathogen_DryVisits = foreach(wet=rep(seq(0,1, by = 0.1), 11), dry = rep(seq(0,1, by = 0.1), each = 11), .packages = c("tidyr")) %dopar%{
  p <- c(wet, dry, dry)
  Wildebeest_propWetP <- Risk_infection(Year = Year ,Boot_s = Boot_s , LD = LD , PG = PGV_W, Seas = Season ,
                                   Extrapolation = subset(Extrapolation_10Y, Season == 'Hot_Wet'), Average_NV = Average_NV_W,
                                   IndG = IndG_demo_W , Grams = Gram_W, BN = BN_W, psoil = psoil, 
                                   Distance = Distance_P, Prop = p)
  
  Wildebeest_propWetP$Prop <- paste0(as.character(p[1]*10),'/',as.character(p[2]*10))
  Wildebeest_propWetP$Wwet <- wet
  Wildebeest_propWetP$Wdry <- dry
  return(Wildebeest_propWetP)
  
}
print(Sys.time())


Wildebeest_WetPathogen_DryVisits[sapply(Wildebeest_WetPathogen_DryVisits, is.null)] <- NULL
Wildebeest_WetPathogen_DryVisits_Final <- map_dfr(Wildebeest_WetPathogen_DryVisits_Final, bind_rows) 


write.csv(Wildebeest_WetPathogen_DryVisits_Final,'Wildebeest__Wet_Pathogen_Dry_visit_100.csv', row.names = F)




#######  Wildebeest Dry season Pathogen on Dry Visits

Season = c('Hot_dry','Hot_dry','Hot_dry')


print(Sys.time())

Wildebeest_DryPathogen_DryVisits = foreach(wet=rep(seq(0,1, by = 0.1), 11), dry = rep(seq(0,1, by = 0.1), each = 11), .packages = c("tidyr")) %dopar%{
  p <- c(wet, dry, dry)
  Wildebeest_propDryP <- Risk_infection(Year = Year ,Boot_s = Boot_s , LD = LD , PG = PGV_W, Seas = Season ,
                                   Extrapolation = subset(Extrapolation_10Y, Season != 'Hot_Wet'), Average_NV = Average_NV_W,
                                   IndG = IndG_demo_W , Grams = Gram_W, BN = BN_W, psoil = psoil, 
                                   Distance = Distance_P, Prop = p)
  
  Wildebeest_propDryP$Prop <- paste0(as.character(p[1]*10),'/',as.character(p[2]*10))
  Wildebeest_propDryP$Wwet <- wet
  Wildebeest_propDryP$Wdry <- dry
  return(Wildebeest_propDryP)
  
}
print(Sys.time())


Wildebeest_DryPathogen_DryVisits[sapply(Wildebeest_DryPathogen_DryVisits, is.null)] <- NULL
Wildebeest_DryPathogen_DryVisits_Final <- map_dfr(Wildebeest_DryPathogen_DryVisits_Final, bind_rows) 


write.csv(Wildebeest_DryPathogen_DryVisits_Final,'Wildebeest__Dry_Pathogen_Dry_visit_100.csv', row.names = F)



#######  Wildebeest All data

Season = c('Hot_wet','Cool_dry','Hot_dry')


print(Sys.time())

Wildebeest_All = foreach(wet=rep(seq(0,1, by = 0.1), 11), dry = rep(seq(0,1, by = 0.1), each = 11), .packages = c("tidyr")) %dopar%{
  p <- c(wet, dry, dry)
  Wildebeest_propAll <- Risk_infection(Year = Year ,Boot_s = Boot_s , LD = LD , PG = PGV_W, Seas = Season ,
                                        Extrapolation = Extrapolation_10Y, Average_NV = Average_NV_W,
                                        IndG = IndG_demo_W , Grams = Gram_W, BN = BN_W, psoil = psoil, 
                                        Distance = Distance_P, Prop = p)
  
  Wildebeest_propAll$Prop <- paste0(as.character(p[1]*10),'/',as.character(p[2]*10))
  Wildebeest_propAll$Wwet <- wet
  Wildebeest_propAll$Wdry <- dry
  return(Wildebeest_propAll)
  
}
print(Sys.time())


Wildebeest_All[sapply(Wildebeest_All, is.null)] <- NULL
Wildebeest_All_Final <- map_dfr(Wildebeest_All, bind_rows) 


write.csv(Wildebeest_All_Final,'Wildebeest__All_100.csv', row.names = F)


write.csv(Wildebeest_All_Final,'Wildebeest__All_100.csv', row.names = F)


#######  Zebra All data

Season = c('Hot_wet','Cool_dry','Hot_dry')


print(Sys.time())

Zebra_All = foreach(wet=rep(seq(0,1, by = 0.1), 11), dry = rep(seq(0,1, by = 0.1), each = 11), .packages = c("tidyr")) %dopar%{
  p <- c(wet, dry, dry)
  Zebra_All <- Risk_infection(Year = Year ,Boot_s = Boot_s , LD = LD , PG = PGV_Z, Seas = Season ,
                                   Extrapolation = Extrapolation_10Y, Average_NV = Average_NV_Z,
                                   IndG = IndG_demo_Z , Grams = Gram_Z, BN = BN_Z, psoil = psoil, 
                                   Distance = Distance_W, Prop = p)
  
  Zebra_All$Prop <- paste0(as.character(p[1]*10),'/',as.character(p[2]*10))
  Zebra_All$Wwet <- wet
  Zebra_All$Wdry <- dry
  return(Zebra_All)
  
}
print(Sys.time())
stopImplicitCluster()


Zebra_All[sapply(Zebra_All, is.null)] <- NULL
Zebra_All_Final <- map_dfr(Zebra_All_Final, bind_rows) 


write.csv(Zebra_All_Final,'Zebra__All_100.csv', row.names = F)


#### Lethal Dose simulation prediction

##### Prediction over time depending on the LD:

LD_prediction <- function(Zebra, Wildebeest, ZebraD, WilD, LD_test, timeline, sim, Drought = 0 ){
  
  #### Make the data
  Zeb_data <- subset(Zebra, LD == LD_test & Season == 'Hot_wet' &  
                       Prop %in% c('0/0','1/0','2/0','3/0','4/0','1/1','2/1','3/1','4/1', '2/2','3/2','4/2','3/3','4/3','4/4'))
  
  Zeb_data <- Zeb_data %>% group_by(LD, Season, Year, Rep, Distance, Prop) %>%
    summarise(Infected = sum(Infected))
  
  Zeb_dataD <- subset(ZebraD, LD == LD_test & Season == 'Hot_wet' &  
                        Prop %in% c('0/0','1/0','2/0','3/0','4/0','1/1','2/1','3/1','4/1', '2/2','3/2','4/2','3/3','4/3','4/4'))
  
  Zeb_dataD <- Zeb_dataD %>% group_by(LD, Season, Year, Rep, Distance, Prop) %>%
    summarise(Infected = sum(Infected))
  
  
  Wil_data <- subset(Wildebeest, LD == LD_test & Season == 'Hot_wet' &  
                       Prop %in% c('0/0','1/0','2/0','3/0','4/0','1/1','2/1','3/1','4/1', '2/2','3/2','4/2','3/3','4/3','4/4'))
  
  Wil_data <- Wil_data %>% group_by(LD, Season, Year, Rep, Distance, Prop) %>%
    summarise(Infected = sum(Infected))
  
  Wil_dataD <- subset(WilD, LD == LD_test & Season == 'Hot_wet' &  
                        Prop %in% c('0/0','1/0','2/0','3/0','4/0','1/1','2/1','3/1','4/1', '2/2','3/2','4/2','3/3','4/3','4/4'))
  
  Wil_dataD <- Wil_dataD %>% group_by(LD, Season, Year, Rep, Distance, Prop) %>%
    summarise(Infected = sum(Infected))
  
  
  
  ## Start the simulation, using real informations from 2003-2013
  
  Final_Carcasses <- data.frame(Step = numeric(), C0 = numeric(), C1 = numeric(), C2 = numeric(), C3 = numeric(), C4 = numeric(), 
                                C5 = numeric(), C6 = numeric(), C7 = numeric(), C8 = numeric(), C9 =numeric(), C10 = numeric(),
                                LD = numeric(), sim=numeric())
  
  
  for(s in seq(1,sim)){
    print(s)
    Carcasses <-data.frame(ID = seq(1,235), 
                           Age = c(rep(10,12), rep(9,31),rep(8,6), rep(7,29), rep(6,0),rep(5,18), rep(4,39),
                                   rep(3, 64), rep(2, 11), rep(1, 24), rep(0,1)),
                           Step = rep(0,235),
                           Step_dead = rep(NA,235))
    
    Final_Carc <- data.frame(Step = 0, C0 = 1, C1 = 24, C2 = 11, C3 = 64, C4 = 39, 
                             C5 = 18, C6 = 0, C7 = 29, C8 = 6, C9 =31, C10 = 12,
                             LD = LD_test, sim=s, Weather = NA)
    
    for(y in seq(1,timeline)){
      if (runif(1)> Drought){
        Zeb <- Zeb_data
        Wil <- Wil_data
        Weath <- 'Normal'
      }else{
        Zeb <- Zeb_dataD
        Wil <- Wil_dataD
        Weath <- 'Drought'
      }
      
      Carc_per_year <- as.data.frame(subset(Carcasses, Age <=10) %>% group_by(Age) %>%
                                       summarise(Number = n()))
      
      New_carc <- 0
      for (a in Carc_per_year$Age){
        Inf_Zeb <- sample(seq(1, nrow(subset(Zeb, Year == a))), min(Carc_per_year[Carc_per_year$Age==a,'Number'], 15000), replace = T)
        RiskZ <- subset(Zeb,  Year == a)[Inf_Zeb,]
        
        Inf_Wil <- sample(seq(1, nrow(subset(Wil, Year == a))), min(Carc_per_year[Carc_per_year$Age==a,'Number'],15000), replace = T)
        RiskW <- subset(Wil,  Year == a)[Inf_Wil,]
        
        New_carc <- New_carc + sum(RiskZ$Infected, na.rm=T) + sum(RiskW$Infected, na.rm=T)
      }
      
      Carcasses[Carcasses$Age <=10, 'Age'] <- Carcasses[Carcasses$Age <=10, 'Age']  + 1
      
      if (New_carc != 0){
        Carcasses <- rbind(Carcasses, 
                           data.frame(ID = seq(max(Carcasses$ID)+1, max(Carcasses$ID) + New_carc),
                                      Age = rep(0, New_carc),
                                      Step = rep(y, New_carc),
                                      Step_dead = rep(NA, New_carc)))
      }
      
      Carcasses[Carcasses$Age > 10 & is.na(Carcasses$Step_dead), 'Step_dead'] <- y
      
      All_Carc <- merge(data.frame(Age = seq(0,10)), as.data.frame(subset(Carcasses, Age <=10) %>% group_by(Age) %>% summarise(Number = n())),
                        by = 'Age', all.x=T)
      All_Carc[is.na(All_Carc$Number),'Number'] <- 0
      Final_Carc <- rbind(Final_Carc, c(y, All_Carc$Number, LD_test, s, Weath ))
    }
    
    Final_Carcasses <- rbind(Final_Carcasses, Final_Carc)
    
  }
  
  
  return(Final_Carcasses)
}




LD_prediction <- function(Zebra, Wildebeest, ZebraD, WilD, LD_test, timeline, sim, Drought = 0 ){
  
  #### Make the data
  Zeb_data <- subset(Zebra, LD == LD_test & Season == 'Hot_wet' &  
                       Prop %in% c('0/0','1/0','2/0','3/0','4/0','1/1','2/1','3/1','4/1', '2/2','3/2','4/2','3/3','4/3','4/4'))
  
  Zeb_data <- Zeb_data %>% group_by(LD, Season, Year, Rep, Distance, Prop) %>%
    summarise(Infected = sum(Infected))
  
  Zeb_dataD <- subset(ZebraD, LD == LD_test & Season == 'Hot_wet' &  
                        Prop %in% c('0/0','1/0','2/0','3/0','4/0','1/1','2/1','3/1','4/1', '2/2','3/2','4/2','3/3','4/3','4/4'))
  
  Zeb_dataD <- Zeb_dataD %>% group_by(LD, Season, Year, Rep, Distance, Prop) %>%
    summarise(Infected = sum(Infected))
  
  
  Wil_data <- subset(Wildebeest, LD == LD_test & Season == 'Hot_wet' &  
                       Prop %in% c('0/0','1/0','2/0','3/0','4/0','1/1','2/1','3/1','4/1', '2/2','3/2','4/2','3/3','4/3','4/4'))
  
  Wil_data <- Wil_data %>% group_by(LD, Season, Year, Rep, Distance, Prop) %>%
    summarise(Infected = sum(Infected))
  
  Wil_dataD <- subset(WilD, LD == LD_test & Season == 'Hot_wet' &  
                        Prop %in% c('0/0','1/0','2/0','3/0','4/0','1/1','2/1','3/1','4/1', '2/2','3/2','4/2','3/3','4/3','4/4'))
  
  Wil_dataD <- Wil_dataD %>% group_by(LD, Season, Year, Rep, Distance, Prop) %>%
    summarise(Infected = sum(Infected))
  
  
  
  ## Start the simulation, using real informations from 2003-2013
  
  Final_Carcasses <- data.frame(Step = numeric(), C0 = numeric(), C1 = numeric(), C2 = numeric(), C3 = numeric(), C4 = numeric(), 
                                C5 = numeric(), C6 = numeric(), C7 = numeric(), C8 = numeric(), C9 =numeric(), C10 = numeric(),
                                LD = numeric(), sim=numeric())
  
  
  for(s in seq(1,sim)){
    print(s)
    Carcasses <-data.frame(ID = seq(1,235), 
                           Age = c(rep(10,12), rep(9,31),rep(8,6), rep(7,29), rep(6,0),rep(5,18), rep(4,39),
                                   rep(3, 64), rep(2, 11), rep(1, 24), rep(0,1)),
                           Step = rep(0,235),
                           Step_dead = rep(NA,235))
    
    Final_Carc <- data.frame(Step = 0, C0 = 1, C1 = 24, C2 = 11, C3 = 64, C4 = 39, 
                             C5 = 18, C6 = 0, C7 = 29, C8 = 6, C9 =31, C10 = 12,
                             LD = LD_test, sim=s, Weather = NA)
    
    for(y in seq(1,timeline)){
      if (runif(1)> Drought){
        Zeb <- Zeb_data
        Wil <- Wil_data
        Weath <- 'Normal'
      }else{
        Zeb <- Zeb_dataD
        Wil <- Wil_dataD
        Weath <- 'Drought'
      }
      
      Carc_per_year <- as.data.frame(subset(Carcasses, Age <=10) %>% group_by(Age) %>%
                                       summarise(Number = n()))
      
      New_carc <- 0
      for (a in Carc_per_year$Age){
        Inf_Zeb <- sample(seq(1, nrow(subset(Zeb, Year == a))), Carc_per_year[Carc_per_year$Age==a,'Number'], replace = T)
        RiskZ <- subset(Zeb,  Year == a)[Inf_Zeb,]
        
        Inf_Wil <- sample(seq(1, nrow(subset(Wil, Year == a))), Carc_per_year[Carc_per_year$Age==a,'Number'], replace = T)
        RiskW <- subset(Wil,  Year == a)[Inf_Wil,]
        
        New_carc <- New_carc + sum(RiskZ$Infected, na.rm=T) + sum(RiskW$Infected, na.rm=T)
      }
      if (New_carc > 10000){ New_carc <- 10000}
      
      Carcasses[Carcasses$Age <=10, 'Age'] <- Carcasses[Carcasses$Age <=10, 'Age']  + 1
      
      if (New_carc != 0){
        Carcasses <- rbind(Carcasses, 
                           data.frame(ID = seq(max(Carcasses$ID)+1, max(Carcasses$ID) + New_carc),
                                      Age = rep(0, New_carc),
                                      Step = rep(y, New_carc),
                                      Step_dead = rep(NA, New_carc)))
      }
      
      Carcasses[Carcasses$Age > 10 & is.na(Carcasses$Step_dead), 'Step_dead'] <- y
      
      All_Carc <- merge(data.frame(Age = seq(0,10)), as.data.frame(subset(Carcasses, Age <=10) %>% group_by(Age) %>% summarise(Number = n())),
                        by = 'Age', all.x=T)
      All_Carc[is.na(All_Carc$Number),'Number'] <- 0
      Final_Carc <- rbind(Final_Carc, c(y, All_Carc$Number, LD_test, s, Weath ))
    }
    
    Final_Carcasses <- rbind(Final_Carcasses, Final_Carc)
    
  }
  
  
  return(Final_Carcasses)
}


set.seed(123)


Zebra_average <- read.csv('Final_data/Zebra_Final_100.csv')
ZebraD_wetP <- read.csv('Final_data/Zebra__Wet_Pathogen_Dry_visit_100.csv')
Wildebeest_average <- read.csv('Final_data/Wildebeest_Final_100.csv')
WildebeestD_wetP <- read.csv('Final_data/Wildebeest__Wet_Pathogen_Dry_visit_100.csv')



LD5 <- LD_prediction(Zebra_average, Wildebeest_average, ZebraD_wetP, WildebeestD_wetP, LD_test =  10^5, timeline = 100, sim = 100, Drought = 0)
LD6 <- LD_prediction(Zebra_average, Wildebeest_average, ZebraD_wetP, WildebeestD_wetP, LD_test =  10^6, timeline = 100, sim = 100, Drought = 0)
LD7 <- LD_prediction(Zebra_average, Wildebeest_average, ZebraD_wetP, WildebeestD_wetP, LD_test =  10^7, timeline = 100, sim = 100, Drought = 0)
LD8 <- LD_prediction(Zebra_average, Wildebeest_average, ZebraD_wetP, WildebeestD_wetP, LD_test =  10^8, timeline = 100, sim = 100, Drought = 0)


## ISSUE HERE
LDF <- rbind(LD5, LD6, LD7, LD8)
LDF[,seq(1,12)] <- apply(LDF[,seq(1,12)], 2, function(x) as.numeric(as.character(x)))

LDF$CarcTot <- rowSums(LDF[,seq(2,12)])

#write.csv(LDF, 'Final_data/LDF.csv')

### Then move to plot code for plots 