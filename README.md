# Reproduction_number
Files to run model from the manuscript "Season of death, pathogen persistence and wildlife behavior alter number of anthrax secondary infections from environmental reservoirs' 

Scripts:
  R_estimation_Code.R : Run the simulation model
  
  Plots_R_estimation_Code.R : Code to run the plots found in the MS
  
Data :

  Camera_all_individuals.csv : Records each individual zebra and wildebeest observed on the camera. The column are :
    Date = Date of recording
    ID = Camera ID
    Treatment = Carcass or Control site
    Tini = Time at which the individual entered the zone
    Tend = Time at which the individual exited the zone
    Individual = When herd visiting the site at the same time, this record the order at which the individual entered
    Total_Ind = Number of individuals visiting the site at the same time
    Age = Age of the individual : Juvenile : 0-1 year / Sub-adult : 1-2 Years / Adult : +2years
    Sex = Male or Female
    Confidence = Confidence in the age/sex combination : 0 : Could not determine / 1= Low confidence / 2= High confidence / 3: Certain
    TT = Total time spent in the zone, in seconds
    TG =  Total time spent grazing on the zone, in seconds
    Species = Zebra or Wildebeest
    JDay =  Julian day of observation
    deployed = Date of camera deployement
    TimeStart= Number of days since the camera was up
    Season = Season of observation, Hot-Wet , Cool-dry or Hot-dry
    Death_date = Date of individual death, for carcass sites
    Age_AD = Number of days since death
    Carcass_year= Year of death
    Month_AD = Number of months after death
    Grazing= 0: No  1: Yes
    Year_AD = Number of years after death
    Rainfall = Precipitation in mm
    Temp = Temperature
    Rain_AD = Precipitation at the day of death
    NDVI = Normalized Difference Vegetation Index, of the site ID

  Camera_monthly_summary.csv : Summary of the individual dataset to obtain the number of visits per month, the probability of grazing per month. The columns are:  
    Year = Year of observation
    ID = Camera ID
    Month = Month of observation
    Treatment=  Carcass or Control site
    Species = Zebra or Wildebeest
    Age = Age of the individual : Juvenile : 0-1 year / Sub-adult : 1-2 Years / Adult : +2years
    Sex = Male or Female
    NV = Number of visitis
    NG = Number of individuals grazing
    NDVI = Normalized difference vegetation index
    Death_date = date of death
    Season = Season of observation  
    PropGV = Probability of grazing
    Month_AD = Number of month after death
    MY = Month/Year of observation 
    
 Location_camera.csv : Distance of the cameras from the closest perenial water and salt pan edge. The columns are:
  ID = Camera site ID
  Name_water = Closest waterhole name
  Water_km = Distance in km from the water
  Pan_km = Distance in km from the pan edge

  Extrapolation_CFU.csv : Spore concentration in grass and soil at 40 sites for 10 years, obtained from extrapolation and interpolation. The column are :
    ID = Site ID
    Age = Age of site in years after death
    Soil_CFU = Concentration of spores in the soil
    Grass_CFU_above = Concentration of spores in the aboveground grass component (everything above the roots)
    Grass_Top_CFU = Concentration of spores in the top grass component (removing the base of the plant which can collect soil)
    Season = Season of death
 
 Location_CFU.csv : Coordinate of each spore concentration site, and NDVI of the sites. The columns are:
   ID = Site ID
   Lon = Longitude
   Lat = Latitude
   Index = Sample Index
   NDVI_Wet = NDVI of the site at death during the hot-wet season
   NDVI_CDry = NDVI of the site at death during the Cool-dry season
   NDVI_HDry = NDVI of the site at death during the Hot_dry season

  Camera_work_time : Numbers of days of recording per camera trap. The columns are:
    ID: Site ID
    Month: Year-Month of the recording
    Carcass : Number of days of recording for the carcass camera
    Control : Number of days of recording for the control camera
