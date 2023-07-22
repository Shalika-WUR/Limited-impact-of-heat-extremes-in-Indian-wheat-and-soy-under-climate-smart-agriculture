#please change the path to your directory, setwd("/path/to/your/shapefile/")
#change plot margins based on temperature thresholds
#change centered values based on each model, refer main paper

library(maptools)
library(rgeos)
library(rgdal)
#load the district shapefile and export centroid of each district
dis <- readOGR("C:/Users/Shakila Vyas/OneDrive - Wageningen University & Research/CCAFS/Madhur CCAFS/India_District/India_District.shp") 
centr <- gCentroid(dis, byid = TRUE)

# create SpatialPointsDataFrame to export via writeOGR
centr <- SpatialPointsDataFrame(centr, data= dis@data) 
plot(centr)
write.csv(centr, "C:/Users/Shakila Vyas/OneDrive - Wageningen University & Research/CCAFS/Madhur CCAFS/India_District/India_District.csv")

#1. Weather data preparation
#extracting daily weather data
#county analysis
coords_dis<- read.csv("CSA/India_District.csv")
coords_dis1<- coords_dis[c("x","y")]
#the weather data from IMD already downloaded in the folder as an nc file
#max temp
maxtempimd15 <- raster::stack("CSA/IMD/2015.nc")
mxtp15 <- raster::extract(maxtempimd15 ,coords_dis1)
write.csv(mxtp15,"CSA/mxtpimddis15.csv")
maxtempimd16 <- raster::stack("CSA/IMD/2016.nc")
mxtp16 <- raster::extract(maxtempimd16 ,coords_dis1)
write.csv(mxtp16,"CSA/mxtpimddis16.csv")
maxtempimd17 <- raster::stack("CSA/IMD/2017.nc")
mxtp17 <- raster::extract(maxtempimd17 ,coords_dis1)
write.csv(mxtp17,"CSA/mxtpimddis17.csv")
maxtempimd18 <- raster::stack("CSA/IMD/2018.nc")
mxtp18 <- raster::extract(maxtempimd18 ,coords_dis1)
write.csv(mxtp18,"CSA/mxtpimddis18.csv")
maxtempimd19 <- raster::stack("CSA/IMD/2019.nc")
mxtp19 <- raster::extract(maxtempimd19 ,coords_dis1)
write.csv(mxtp19,"CSA/mxtpimddis19.csv")
maxtempimd20 <- raster::stack("CSA/IMD/2020.nc")
mxtp20 <- raster::extract(maxtempimd20 ,coords_dis1)
write.csv(mxtp20,"CSA/mxtpimddis20.csv")

#min temp
mintempimd15 <- raster::stack("CSA/IMD/Min/2015.nc")
mntp15 <- raster::extract(mintempimd15 ,coords_dis1)
write.csv(mntp15,"CSA/mntpimddis15.csv")
mintempimd16 <- raster::stack("CSA/IMD/Min/2016.nc")
mntp16 <- raster::extract(mintempimd16 ,coords_dis1)
write.csv(mntp16,"CSA/mntpimddis16.csv")
mintempimd17 <- raster::stack("CSA/IMD/Min/2017.nc")
mntp17 <- raster::extract(mintempimd17 ,coords_dis1)
write.csv(mntp17,"CSA/mntpimddis17.csv")
mintempimd18 <- raster::stack("CSA/IMD/Min/2018.nc")
mntp18 <- raster::extract(mintempimd18 ,coords_dis1)
write.csv(mntp18,"CSA/mntpimddis18.csv")
mintempimd19 <- raster::stack("CSA/IMD/Min/2019.nc")
mntp19 <- raster::extract(mintempimd19 ,coords_dis1)
write.csv(mntp19,"CSA/mntpimddis19.csv")
mintempimd20 <- raster::stack("CSA/IMD/Min/2020.nc")
mntp20 <- raster::extract(mintempimd20 ,coords_dis1)
write.csv(mntp20,"CSA/mntpimddis20.csv")

#rainfall
rainimd15 <- raster::stack("CSA/IMD/Rain/_Clim_Pred_LRF_New_RF25_IMD0p252015.nc")
rain15 <- raster::extract(rainimd15 ,coords_dis1)
write.csv(rain15,"CSA/rainimddis15.csv")
rainimd16 <- raster::stack("CSA/IMD/Rain/_Clim_Pred_LRF_New_RF25_IMD0p252016.nc")
rain16 <- raster::extract(rainimd16 ,coords_dis1)
write.csv(rain16,"CSA/rainimddis16.csv")
rainimd17 <- raster::stack("CSA/IMD/Rain/_Clim_Pred_LRF_New_RF25_IMD0p252017.nc")
rain17 <- raster::extract(rainimd17 ,coords_dis1)
write.csv(rain17,"CSA/rainimddis17.csv")
rainimd18 <- raster::stack("CSA/IMD/Rain/_Clim_Pred_LRF_New_RF25_IMD0p252018.nc")
rain18 <- raster::extract(rainimd18 ,coords_dis1)
write.csv(rain18,"CSA/rainimddis18.csv")
rainimd19 <- raster::stack("CSA/IMD/Rain/_Clim_Pred_LRF_New_RF25_IMD0p252019.nc")
rain19 <- raster::extract(rainimd19 ,coords_dis1)
write.csv(rain19,"CSA/rainimddis19.csv")
rainimd20 <- raster::stack("CSA/IMD/Rain/_Clim_Pred_LRF_New_RF25_IMD0p252020.nc")
rain20 <- raster::extract(rainimd20 ,coords_dis1)
write.csv(rain20,"CSA/rainimddis20.csv")

#hourly temp splines for IMD data
#hourly splines for IMD data
#if needed, rm(list=ls())
options(scipen=999)
memory.limit(100000)
library(reshape2)
library(beepr)
library(dplyr)
library(lspline)
library(splines)
library(Hmisc)
library(raster)
library(RColorBrewer)
library(lspline)
setwd("C:/Users/Shakila Vyas/OneDrive - Wageningen University & Research/PhD-WUR-Insurance/CSA/County analysis/Weather data")
#read data; farms in rows, daily weather observations in columns
#remove previous data frames
rm(temp.max,temp.min,temp.min.mat,temp.max.mat,temp.min.num,temp.max.num)
#load yearly max, min data, change year names for every year
temp.max <- as.matrix(read.csv("C:/Users/Shakila Vyas/OneDrive - Wageningen University & Research/PhD-WUR-Insurance/CSA/County analysis/Weather data/IMD/Winter/mxtpimddis16.csv")) #load daily minimum temperatures at the district level
temp.min <- as.matrix(read.csv("C:/Users/Shakila Vyas/OneDrive - Wageningen University & Research/PhD-WUR-Insurance/CSA/County analysis/Weather data/IMD/Winter/mntpimddis16.csv")) #load daily minimum temperatures at the district level

#delete rownumber if needed depending on data structure
#temp.min <- temp.min[,-1] 
#temp.max <- temp.max[,-1] 

#delete farms without weather data (farm with mistaken coordinates), not the case with our data
temp.min.mat <- temp.min[complete.cases(temp.min), ]
temp.max.mat <- temp.max[complete.cases(temp.max), ]

#change data frames to numeric matrices
temp.min.num <- apply(temp.min.mat, 2, as.numeric)
temp.max.num <- apply(temp.max.mat, 2, as.numeric)

#calculate daily temp minima and maxima
#create empty matrices
temp.min     <- matrix(nrow=nrow(temp.min.num), ncol=ncol(temp.min.num))
temp.max     <- matrix(nrow=nrow(temp.max.num), ncol=ncol(temp.max.num))

####calculate hourly temp values from the sine curve
#we assume a sine curve for the 12 hours between between thi.min_t and thi.max_t and a new sine curve between thi.max_t and thi_min_t+1
#we thus need a matrix with T+1 columns for thi.min

#add one extra unobserved for temp min
temp.min.num             <- cbind(temp.min.num,  1:nrow(temp.min.num)*NA)

#and we thus get two amplitudes and two middle temp's per day
temp.amplitude_1        <-(temp.max.num-temp.min.num[,1:ncol(temp.max.num)])/2 #from temp.min_t until temp.max_t
temp.amplitude_2        <-(temp.max.num-temp.min.num[,2:ncol(temp.min.num)])/2 #from temp.max_t until temp.min_t+1
temp.mid_1              <- temp.max.num-temp.amplitude_1     #from temp.min_t until temp.max_t
temp.mid_2              <- temp.max.num-temp.amplitude_2     #from temp.max_t until temp.min_t
temp.amplitude_2[,ncol(temp.max.num)] <- temp.amplitude_1[,ncol(temp.max.num)]      # assume that the last day has the same amplitude and mid on the first and second half of the day
temp.mid_2[,ncol(temp.max.num)]       <- temp.mid_1[,ncol(temp.max.num)]

#initial data preparation
hours        <- seq(0,23, by=1) #hours of the day
time.seq     <- unique(substr(colnames(temp.max.num), 2,8)) 
sincurve12_1 <- seq(((-0.5*pi)+(2*pi/48)),(0.5*pi-(2*pi/48)), by=(2*pi/24)) #by 2pi/24 to set the hour steps, and add/substract 2*pi*1/48 to estimate at the middle of the hour
sincurve12_2 <- seq((( 0.5*pi)+(2*pi/48)),(1.5*pi-(2*pi/48)), by=(2*pi/24)) #by 2pi/24 to set the hour steps, and add/substract 2*pi*1/48 to estimate at the middle of the hour

#### array with matrix for each month (rows=642 farms, cols=different for each day)
temp       <- array(NA, dim=c(nrow(temp.amplitude_1),  ncol(temp.amplitude_1),length(hours))) #create an empty array for hours in bin per day from temp.min_t to temp.max_t
for (j in 1:ncol(temp.amplitude_1)){ #do this for every day in month k, so j
  for (i in 1:nrow(temp.amplitude_1)){ #for all firms
    for (h in 1:length(sincurve12_1)){ #for first 12 hours
      temp[i,j,h]    <-  temp.amplitude_1[i,j]*sin(sincurve12_1[h])+temp.mid_1[i,j] #get Temp value in each hour
    }
    for (h in 1:(length(sincurve12_2))){ #for second 12 hours
      temp[i,j,h+12] <-  temp.amplitude_2[i,j]*sin(sincurve12_2[h])+temp.mid_2[i,j]
    }
  }
} 
#change name for each year
write.csv(temp, "C:/Users/Shakila Vyas/OneDrive - Wageningen University & Research/PhD-WUR-Insurance/CSA/County analysis/Weather data/IMD/Winter/hourlytpimddis16.csv", row.names=F)

######hourly reg
library(raster)
library(dplyr)
library(data.table)
library(reshape2)
library(Hmisc)
library(lubridate)
library(plyr)
library(esmisc)
library(sandwich)
library(MASS)
library(e1071)
library(fixest)

#2. Yield data preparation and combining with weather data
data<- read.csv("C:/Users/Shakila Vyas/OneDrive - Wageningen University & Research/PhD-WUR-Insurance/CSA/County analysis/Dis yield_All India/Soy_yields.csv")
head(data)
#remove less than 3 years data
regdata_soy<- subset(data,Year>=2015,select = District:area)
data1<- add_count(regdata_soy, Dis_codename)
regdata_soy1<- subset(data1,n>=3,select = District:n)
head(regdata_soy1)
summary(regdata_soy1)

##preparing data
hourly<- read.csv("C:/Users/Shakila Vyas/OneDrive - Wageningen University & Research/PhD-WUR-Insurance/CSA/County analysis/Weather data/ERA5/Summer/hourlytp15era.csv")
#need this format-colnames(temp_wheat_exposure2) <- c("Farm","Date","Hour", "Temperature")
as.data.frame(hourly)
library(reshape2)
long <- reshape2::melt(hourly, id.vars = c("farm"))
long$year<- 2015

hourly1<- read.csv("C:/Users/Shakila Vyas/OneDrive - Wageningen University & Research/PhD-WUR-Insurance/CSA/County analysis/Weather data/ERA5/Summer/hourlytp16era.csv")
#need this format-colnames(temp_wheat_exposure2) <- c("Farm","Date","Hour", "Temperature")
as.data.frame(hourly1)
long1 <- reshape2::melt(hourly1, id.vars = c("farm"))
long1$year<- 2016

hourly2<- read.csv("C:/Users/Shakila Vyas/OneDrive - Wageningen University & Research/PhD-WUR-Insurance/CSA/County analysis/Weather data/ERA5/Summer/hourlytp17era.csv")
#need this format-colnames(temp_wheat_exposure2) <- c("Farm","Date","Hour", "Temperature")
as.data.frame(hourly2)
long2 <- reshape2::melt(hourly2, id.vars = c("farm"))
long2$year<- 2017

hourly3<- read.csv("C:/Users/Shakila Vyas/OneDrive - Wageningen University & Research/PhD-WUR-Insurance/CSA/County analysis/Weather data/ERA5/Summer/hourlytp18era.csv")
#need this format-colnames(temp_wheat_exposure2) <- c("Farm","Date","Hour", "Temperature")
as.data.frame(hourly3)
long3 <- reshape2::melt(hourly3, id.vars = c("farm"))
long3$year<- 2018

hourly4<- read.csv("C:/Users/Shakila Vyas/OneDrive - Wageningen University & Research/PhD-WUR-Insurance/CSA/County analysis/Weather data/ERA5/Summer/hourlytp19era.csv")
#need this format-colnames(temp_wheat_exposure2) <- c("Farm","Date","Hour", "Temperature")
as.data.frame(hourly4)
long4 <- reshape2::melt(hourly4, id.vars = c("farm"))
long4$year<- 2019

hourly5<- read.csv("C:/Users/Shakila Vyas/OneDrive - Wageningen University & Research/PhD-WUR-Insurance/CSA/County analysis/Weather data/ERA5/Summer/hourlytp20era.csv")
#need this format-colnames(temp_wheat_exposure2) <- c("Farm","Date","Hour", "Temperature")
as.data.frame(hourly5)
long5 <- reshape2::melt(hourly5, id.vars = c("farm"))
long5$year<- 2020

#bind all years together
temp<- rbind(long,long1,long2,long3,long4,long5)
range(temp$value)
hist(temp$value)

####outlier removal
hist(regdata_soy1$Yield..Tonnes.Hectare.)
quantile(regdata_soy1$Yield..Tonnes.Hectare.,probs= c(.99))
regdata_soy1<- subset(regdata_soy1,Yield..Tonnes.Hectare.<quantile(regdata_soy$Yield..Tonnes.Hectare.,probs= c(.99)),select = District:n )
hist(regdata_soy1$Yield..Tonnes.Hectare.)
#convert to kg/ha
regdata_soy1$Yield..Tonnes.Hectare. <- (regdata_soy1$Yield..Tonnes.Hectare.)*1000

#rename temp data
names(temp)[3] <- "temperature"
as.numeric(temp$temperature)
min(temp$temperature)
max(temp$temperature)
range(temp$temperature)
summary(temp$temperature)
hist(temp$temperature,main="Hourly temperature district-summer")
range(temp$temperature)
temp <- na.omit(temp)

#Histogram of temperature exposure
temperature_soy <- ggplot(temp, aes(x=temperature)) + 
  theme(panel.grid.major = element_blank(),
        plot.title = element_text(family="Times New Roman", size=12, colour = "grey25", hjust=0.5),
        axis.title = element_text(family="Times New Roman", size=10, colour = "grey25"),
        axis.text = element_text(family="Times New Roman", size=10),
        panel.grid.minor = element_blank(),
        panel.background = element_blank(),
        axis.line = element_line(colour = "black"),
        legend.position="none")+
  ylab("Total exposure  (no. of hours)")+ xlab("Temperature (°C)")+
  geom_histogram(binwidth=1, color="black", fill="gray41")+
  scale_x_continuous(breaks = seq(0,50, by=5), limits = c(0,50))
#change paths for saving files
ggsave(temperature_soy, filename=paste("C:/Users/Shakila Vyas/OneDrive - Wageningen University & Research/PhD-WUR-Insurance/CSA/County analysis/Results/IMDsoyhist_county.png"), 
       dpi=300, height=13, width=18, units="cm")

#3. Regression analysis
#####################model 1: equal spaced 3 knots
# New TS for temperature support of interest- .225 to get the nice plot with small intervals
temp_knot_df <- c(min(temp$temperature) + ((max(temp$temperature)-min(temp$temperature)) / 4),
                  min(temp$temperature) + 2* ((max(temp$temperature)-min(temp$temperature)) / 4),
                  min(temp$temperature) + 3* ((max(temp$temperature)-min(temp$temperature)) / 4))

temp1 <- as.matrix(Hmisc::rcspline.eval(temp$temperature,knots=temp_knot_df,inclx=T))
temp2 <- cbind(temp,temp1)
names(temp2)
head(temp1)

#adjust variables depending on knots
temp3<-temp2 %>%
  group_by(farm,year)%>%
  summarise_at(vars(x,V2),sum)

#match names with temp
names(regdata_soy1)
colnames(regdata_soy1)[colnames(regdata_soy1) == "Year"] <- "year"
colnames(regdata_soy1)[colnames(regdata_soy1) == "Dis_codename"] <- "farm"
names(regdata_soy1)

#join both  data
temp4 <- join(regdata_soy1,temp3, by=c("farm", "year"))
names(temp4)
temp4$reg_year<- as.numeric(as.factor(temp4$District))*temp4$year

##prelim/exploratory regression to see if their are some patterns
library(fixest)
soy_yield_spline = feols(log(Yield..Tonnes.Hectare.) ~ x + V2 + year + I(year^2) + erarain + I(erarain^2)   | farm , cluster = c( "year","farm"), temp4)
summary(soy_yield_spline)
results<- etable(soy_yield_spline)
write.csv(results,"C:/Users/Shakila Vyas/OneDrive - Wageningen University & Research/PhD-WUR-Insurance/CSA/ERA5_results_eq_knot3_returns.csv")

#####bootstrapping, chnage the temp thresholds based on temp range
set.seed(123)
marginal_temperatures_M1 <- rcspline.eval(seq(0,45,0.225),knots=temp_knot_df, inclx=T)

# Run 1000 models to derive uncertainty in estimates
spline_fitted_M1 <- matrix(NA,nrow=1000, ncol=nrow(marginal_temperatures_M1))

for (l in 1: 1000){
  # Sample by region times year
  data_temp <- temp4[which(temp4$reg_year %in% sample(unique(temp4$reg_year),replace=T)),]
  
  # Run regression with random subsample (data_temp), adjust variables to knots
  fit_boot  <- feols(Yield..Tonnes.Hectare. ~ x + V2  + year + I(year^2) + erarain + I(erarain^2)  | farm , data_temp)
  
  #Save marginal effect of temperatures,adjust variables to knots 
  spline_fitted_M1[l,]<-as.vector((marginal_temperatures_M1  %*% coef(fit_boot)[1:2])) 
  
  # Show progress in %
  print(l/1000 *100)
  rm(fit_boot,data_temp)
}

# Load the packages
library(fixest)
library(ggplot2)
library(gridExtra)
library(robustbase)
library(egg)
library(matrixStats)
library(tidyverse)

#Change structure
spline_sub_melt_M1 <- melt(spline_fitted_M1)
# Get median response for each column (= a temperature from seq(20,40,0.225))
colmedians_spline_M1 <- colMedians(spline_fitted_M1)
# Get distance from median response for each value
dist_colmedian_M1 <- matrix(NA, nrow=nrow(spline_fitted_M1),ncol=ncol(spline_fitted_M1))

for(l in 1:ncol(spline_fitted_M1)){
  dist_colmedian_M1[,l] <- spline_fitted_M1[,l] - colmedians_spline_M1[l]
}
# Normalize distance to median
dist_colmedian_M1_normalized <- matrix(NA, nrow=nrow(spline_fitted_M1),ncol=ncol(spline_fitted_M1))

for(l in 1:ncol(spline_fitted_M1)){
  dist_colmedian_M1_normalized[,l]<-abs(dist_colmedian_M1[,l]/max(abs(dist_colmedian_M1[,l])))
}
dist_colmedian_M1_normalized_melted <- melt(dist_colmedian_M1_normalized)

# Last preparation before plotting
spline_merged_M1 <- merge(spline_sub_melt_M1,dist_colmedian_M1_normalized_melted,by=c("Var1","Var2"))
spline_merged_M1$value.y_minus1 <- 1-spline_merged_M1$value.y

temp1 <- aggregate(spline_merged_M1$value.y_minus1, list(spline_merged_M1$Var1), median, na.rm=T) 
temp2 <- aggregate(spline_merged_M1$value.x, list(spline_merged_M1$Var2), median, na.rm=T)

temp3 <- aggregate(spline_merged_M1$value.x, list(spline_merged_M1$Var2), quantile, probs=0.025,na.rm=T) 
temp4 <- aggregate(spline_merged_M1$value.x, list(spline_merged_M1$Var2), quantile, probs=0.975,na.rm=T)

spline_merged_M1 <- merge(spline_merged_M1, temp1, by.x=c("Var1"), by.y=c("Group.1"))
spline_merged_M1 <- merge(spline_merged_M1, temp2, by.x=c("Var2"), by.y=c("Group.1"))
colnames(spline_merged_M1)[7]<-"average_effect"

spline_merged_M1<-merge(spline_merged_M1, temp3, by.x=c("Var2"), by.y=c("Group.1"))
colnames(spline_merged_M1)[8]<-"lower_bound"

spline_merged_M1<-merge(spline_merged_M1, temp4, by.x=c("Var2"), by.y=c("Group.1"))
colnames(spline_merged_M1)[9]<-"upper_bound"

#choose the temp therehsolds dependign on teh temp histogram
temp5 <- cbind(seq(0,45,0.225),seq(1:length(seq(0,45,0.225))))
colnames(temp5) <- c("Temp_bin", "Var2")
spline_merged_M1 <-merge(spline_merged_M1, temp5, by=c("Var2"))

# Plot: hourly temperature effect
#note the values below are added/subtracted for centering around the mean
model_1 <- ggplot()+ ggtitle("Hourly temperature effects: Equal Knots 3") + 
  theme(panel.grid.major = element_blank(),
        plot.title = element_text(family="Times New Roman", size=20, colour = "grey25", hjust=0.5),
        axis.title = element_text(family="Times New Roman", size=16, colour = "grey25"),
        axis.text = element_text(family="Times New Roman", size=16),
        panel.grid.minor = element_blank(),
        panel.background = element_blank(),
        axis.line = element_line(colour = "black"),
        legend.position="none")+
  xlab("Temperature (C)") + ylab("Log yield response (Kg/ha)")+
  scale_x_continuous(breaks = seq(0,45, by=5), limits = c(0,45))+
  scale_y_continuous(breaks = seq(-2,2, by=.004), limits = c(-2,2))+
  geom_hline(yintercept=c(seq(-2,.2,by=0.004)), colour="gray")+
  geom_vline(xintercept=temp_knot_df, linetype="dotted", colour="gray")+
  geom_line(data=spline_merged_M1, aes(y=value.x+0.001431919, x=Temp_bin, group=Var1, alpha=x.x^200/200), size=.01, colour="brown1")+
  geom_line(data=spline_merged_M1, aes(y=average_effect+0.001431919, x=Temp_bin), alpha=0.85, size=.75, colour="brown4")+
  geom_line(data=spline_merged_M1, aes(y=lower_bound+0.001431919, x=Temp_bin)   , alpha=0.85, size=.75, colour="brown4")+       
  geom_line(data=spline_merged_M1, aes(y=upper_bound+0.001431919, x=Temp_bin)   , alpha=0.85, size=.75, colour="brown4")
model_1
ggsave("C:/Users/Shakila Vyas/OneDrive - Wageningen University & Research/PhD-WUR-Insurance/CSA/County analysis/Results/soydis_era_eq3_cen.png", width = 20, height = 20, units = "cm")

#####################model 3: quantile 3 knots
# 10%, 50%, 90% quantile
temp_knot_df <- c(quantile(temp$temperature,0.1, type=1),
                  quantile(temp$temperature,0.5, type=1),
                  quantile(temp$temperature,0.9, type=1))
temp_knot_df

# New TS for temperature support of interest- .225 to get the nice plot with small intervals
temp1 <- as.matrix(Hmisc::rcspline.eval(temp$temperature,knots=temp_knot_df,inclx=T))
temp2 <- cbind(temp,temp1)
names(temp2)
head(temp1)

#adjust variables depending on knots
temp3<-temp2 %>%
  group_by(farm,year)%>%
  summarise_at(vars(x,V2),sum)

#match names with temp
names(regdata_soy1)
hist(regdata_soy1$Yield..Tonnes.Hectare.)
colnames(regdata_soy1)[colnames(regdata_soy1) == "Year"] <- "year"
colnames(regdata_soy1)[colnames(regdata_soy1) == "Dis_codename"] <- "farm"
names(regdata_soy1)

#join both  data
temp4 <- join(regdata_soy1,temp3, by=c("farm", "year"))
names(temp4)
temp4$reg_year<- as.numeric(as.factor(temp4$District))*temp4$year

#####bootstrapping
set.seed(123)
marginal_temperatures_M1 <- rcspline.eval(seq(0,45,0.225),knots=temp_knot_df, inclx=T)

# Run 1000 models to derive uncertainty in estimates
spline_fitted_M1 <- matrix(NA,nrow=1000, ncol=nrow(marginal_temperatures_M1))

for (l in 1: 1000){
  # Sample by region times year
  data_temp <- temp4[which(temp4$reg_year %in% sample(unique(temp4$reg_year),replace=T)),]
  
  # Run regression with random subsample (data_temp), adjust variables to knots
  fit_boot  <- feols(log(Yield..Tonnes.Hectare.) ~ x + V2  + year + I(year^2) + erarain + I(erarain^2)  | farm , data_temp)
  
  #Save marginal effect of temperatures,adjust variables to knots 
  spline_fitted_M1[l,]<-as.vector((marginal_temperatures_M1  %*% coef(fit_boot)[1:2])) 
  
  # Show progress in %
  print(l/1000 *100)
  rm(fit_boot,data_temp)
}

# Load the packages
#Change structure
spline_sub_melt_M1 <- melt(spline_fitted_M1)
# Get median response for each column (= a temperature from seq(20,40,0.225))
colmedians_spline_M1 <- colMedians(spline_fitted_M1)
# Get distance from median response for each value
dist_colmedian_M1 <- matrix(NA, nrow=nrow(spline_fitted_M1),ncol=ncol(spline_fitted_M1))

for(l in 1:ncol(spline_fitted_M1)){
  dist_colmedian_M1[,l] <- spline_fitted_M1[,l] - colmedians_spline_M1[l]
}
# Normalize distance to median
dist_colmedian_M1_normalized <- matrix(NA, nrow=nrow(spline_fitted_M1),ncol=ncol(spline_fitted_M1))

for(l in 1:ncol(spline_fitted_M1)){
  dist_colmedian_M1_normalized[,l]<-abs(dist_colmedian_M1[,l]/max(abs(dist_colmedian_M1[,l])))
}
dist_colmedian_M1_normalized_melted <- melt(dist_colmedian_M1_normalized)

# Last preparation before plotting
spline_merged_M1 <- merge(spline_sub_melt_M1,dist_colmedian_M1_normalized_melted,by=c("Var1","Var2"))
spline_merged_M1$value.y_minus1 <- 1-spline_merged_M1$value.y

temp1 <- aggregate(spline_merged_M1$value.y_minus1, list(spline_merged_M1$Var1), median, na.rm=T) 
temp2 <- aggregate(spline_merged_M1$value.x, list(spline_merged_M1$Var2), median, na.rm=T)

temp3 <- aggregate(spline_merged_M1$value.x, list(spline_merged_M1$Var2), quantile, probs=0.025,na.rm=T) 
temp4 <- aggregate(spline_merged_M1$value.x, list(spline_merged_M1$Var2), quantile, probs=0.975,na.rm=T)

spline_merged_M1 <- merge(spline_merged_M1, temp1, by.x=c("Var1"), by.y=c("Group.1"))
spline_merged_M1 <- merge(spline_merged_M1, temp2, by.x=c("Var2"), by.y=c("Group.1"))
colnames(spline_merged_M1)[7]<-"average_effect"

spline_merged_M1<-merge(spline_merged_M1, temp3, by.x=c("Var2"), by.y=c("Group.1"))
colnames(spline_merged_M1)[8]<-"lower_bound"

spline_merged_M1<-merge(spline_merged_M1, temp4, by.x=c("Var2"), by.y=c("Group.1"))
colnames(spline_merged_M1)[9]<-"upper_bound"

temp5 <- cbind(seq(0,45,0.225),seq(1:length(seq(0,45,0.225))))
colnames(temp5) <- c("Temp_bin", "Var2")
spline_merged_M1 <-merge(spline_merged_M1, temp5, by=c("Var2"))

# Plot: hourly temperature effect
model_1 <- ggplot()+ ggtitle("Hourly temperature effects: Quantile Knots 3") + 
  theme(panel.grid.major = element_blank(),
        plot.title = element_text(family="Times New Roman", size=20, colour = "grey25", hjust=0.5),
        axis.title = element_text(family="Times New Roman", size=16, colour = "grey25"),
        axis.text = element_text(family="Times New Roman", size=16),
        panel.grid.minor = element_blank(),
        panel.background = element_blank(),
        axis.line = element_line(colour = "black"),
        legend.position="none")+
  xlab("Temperature (°C)") + ylab("Log yield response (Kg/ha)")+
  scale_x_continuous(breaks = seq(0,45, by=5), limits = c(0,45))+
  scale_y_continuous(breaks = seq(-.02,.02, by=.004), limits = c(-.02,.02))+
  geom_hline(yintercept=c(seq(-.02,.02,by=0.004)), colour="gray")+
  geom_vline(xintercept=temp_knot_df, linetype="dotted", colour="gray")+
  geom_line(data=spline_merged_M1, aes(y=value.x+0.002086791, x=Temp_bin, group=Var1, alpha=x.x^200/200), size=.01, colour="brown1")+
  geom_line(data=spline_merged_M1, aes(y=average_effect+0.00208679, x=Temp_bin), alpha=0.85, size=.75, colour="brown4")+
  geom_line(data=spline_merged_M1, aes(y=lower_bound+0.00208679, x=Temp_bin)   , alpha=0.85, size=.75, colour="brown4")+       
  geom_line(data=spline_merged_M1, aes(y=upper_bound+0.00208679, x=Temp_bin)   , alpha=0.85, size=.75, colour="brown4")

model_1
ggsave("C:/Users/Shakila Vyas/OneDrive - Wageningen University & Research/PhD-WUR-Insurance/CSA/County analysis/Results/soydis_era_qq3_cen.png", width = 20, height = 20, units = "cm")

#####################model 2: equal spaced 4 knots, not used in the main results
temp_knot_df <- c(min(temp$temperature) + ((max(temp$temperature)-min(temp$temperature)) / 5),
                  min(temp$temperature) + 2* ((max(temp$temperature)-min(temp$temperature)) / 5),
                  min(temp$temperature) + 3* ((max(temp$temperature)-min(temp$temperature)) / 5),
                  min(temp$temperature) + 4* ((max(temp$temperature)-min(temp$temperature)) / 5))
temp_knot_df
# New TS for temperature support of interest- .225 to get the nice plot with small intervals
temp1 <- as.matrix(Hmisc::rcspline.eval(temp$temperature,knots=temp_knot_df,inclx=T))
temp2 <- cbind(temp,temp1)

names(temp2)
head(temp1)

#adjust variables depending on knots
temp3<-temp2 %>%
  group_by(farm,year)%>%
  summarise_at(vars(x,V2,V3),sum)

#match names with temp
names(regdata_soy1)
colnames(regdata_soy1)[colnames(regdata_soy1) == "Year"] <- "year"
colnames(regdata_soy1)[colnames(regdata_soy1) == "Dis_codename"] <- "farm"
names(regdata_soy1)

#join both  data
temp4 <- join(regdata_soy1,temp3, by=c("farm", "year"))

names(temp4)
temp4$reg_year<- as.numeric(as.factor(temp4$District))*temp4$year

#####bootstrapping
set.seed(123)
marginal_temperatures_M1 <- rcspline.eval(seq(0,45,0.225),knots=temp_knot_df, inclx=T)

# Run 1000 models to derive uncertainty in estimates
spline_fitted_M1 <- matrix(NA,nrow=1000, ncol=nrow(marginal_temperatures_M1))

for (l in 1: 1000){
  # Sample by region times year
  data_temp <- temp4[which(temp4$reg_year %in% sample(unique(temp4$reg_year),replace=T)),]
  
  # Run regression with random subsample (data_temp), adjust variables to knots
  fit_boot  <- feols(log(Yield..Tonnes.Hectare.) ~ x + V2 + V3 + year + I(year^2) + erarain + I(erarain^2)  | farm , data_temp)
  
  #Save marginal effect of temperatures,adjust variables to knots 
  spline_fitted_M1[l,]<-as.vector((marginal_temperatures_M1  %*% coef(fit_boot)[1:3])) 
  
  # Show progress in %
  print(l/1000 *100)
  rm(fit_boot,data_temp)
}

# Load the packages
library(fixest)
library(ggplot2)
library(gridExtra)
library(robustbase)
library(egg)
library(matrixStats)
library(tidyverse)

#Change structure
spline_sub_melt_M1 <- melt(spline_fitted_M1)
# Get median response for each column (= a temperature from seq(20,40,0.225))
colmedians_spline_M1 <- colMedians(spline_fitted_M1)
# Get distance from median response for each value
dist_colmedian_M1 <- matrix(NA, nrow=nrow(spline_fitted_M1),ncol=ncol(spline_fitted_M1))

for(l in 1:ncol(spline_fitted_M1)){
  dist_colmedian_M1[,l] <- spline_fitted_M1[,l] - colmedians_spline_M1[l]
}
# Normalize distance to median
dist_colmedian_M1_normalized <- matrix(NA, nrow=nrow(spline_fitted_M1),ncol=ncol(spline_fitted_M1))

for(l in 1:ncol(spline_fitted_M1)){
  dist_colmedian_M1_normalized[,l]<-abs(dist_colmedian_M1[,l]/max(abs(dist_colmedian_M1[,l])))
}

# Change structure
dist_colmedian_M1_normalized_melted <- melt(dist_colmedian_M1_normalized)

# Last preparation before plotting
spline_merged_M1 <- merge(spline_sub_melt_M1,dist_colmedian_M1_normalized_melted,by=c("Var1","Var2"))
spline_merged_M1$value.y_minus1 <- 1-spline_merged_M1$value.y

temp1 <- aggregate(spline_merged_M1$value.y_minus1, list(spline_merged_M1$Var1), median, na.rm=T) 
temp2 <- aggregate(spline_merged_M1$value.x, list(spline_merged_M1$Var2), median, na.rm=T)

temp3 <- aggregate(spline_merged_M1$value.x, list(spline_merged_M1$Var2), quantile, probs=0.025,na.rm=T) 
temp4 <- aggregate(spline_merged_M1$value.x, list(spline_merged_M1$Var2), quantile, probs=0.975,na.rm=T)

spline_merged_M1 <- merge(spline_merged_M1, temp1, by.x=c("Var1"), by.y=c("Group.1"))
spline_merged_M1 <- merge(spline_merged_M1, temp2, by.x=c("Var2"), by.y=c("Group.1"))
colnames(spline_merged_M1)[7]<-"average_effect"

spline_merged_M1<-merge(spline_merged_M1, temp3, by.x=c("Var2"), by.y=c("Group.1"))
colnames(spline_merged_M1)[8]<-"lower_bound"

spline_merged_M1<-merge(spline_merged_M1, temp4, by.x=c("Var2"), by.y=c("Group.1"))
colnames(spline_merged_M1)[9]<-"upper_bound"

temp5 <- cbind(seq(0,45,0.225),seq(1:length(seq(0,45,0.225))))
colnames(temp5) <- c("Temp_bin", "Var2")
spline_merged_M1 <-merge(spline_merged_M1, temp5, by=c("Var2"))

# Plot: hourly temperature effect
model_2 <- ggplot()+ ggtitle("Hourly temperature effects: Equal knots 4") + 
  theme(panel.grid.major = element_blank(),
        plot.title = element_text(family="Times New Roman", size=20, colour = "grey25", hjust=0.5),
        axis.title = element_text(family="Times New Roman", size=16, colour = "grey25"),
        axis.text = element_text(family="Times New Roman", size=16),
        panel.grid.minor = element_blank(),
        panel.background = element_blank(),
        axis.line = element_line(colour = "black"),
        legend.position="none")+
  xlab("Temperature (°C)") + ylab("Log yield response (Kg/ha)")+
  scale_x_continuous(breaks = seq(0,45, by=5), limits = c(0,45))+
  scale_y_continuous(breaks = seq(-.02,.02, by=.004), limits = c(-.02,.02))+
  geom_hline(yintercept=c(seq(-.02,.02,by=0.004)), colour="gray")+
  geom_vline(xintercept=temp_knot_df, linetype="dotted", colour="gray")+
  geom_line(data=spline_merged_M1, aes(y=value.x+0.001541687, x=Temp_bin, group=Var1, alpha=x.x^200/200), size=.01, colour="brown1")+
  geom_line(data=spline_merged_M1, aes(y=average_effect+0.001541687, x=Temp_bin), alpha=0.85, size=.75, colour="brown4")+
  geom_line(data=spline_merged_M1, aes(y=lower_bound+0.001541687, x=Temp_bin)   , alpha=0.85, size=.75, colour="brown4")+       
  geom_line(data=spline_merged_M1, aes(y=upper_bound+0.001541687, x=Temp_bin)   , alpha=0.85, size=.75, colour="brown4")

model_2

ggsave("C:/Users/Shakila Vyas/OneDrive - Wageningen University & Research/PhD-WUR-Insurance/CSA/County analysis/Results/soydis_era_eq4_cen.png", width = 20, height = 20, units = "cm")

#####################model 4: quantile 4 knots######, not used in the main results
temp_knot_df <- c(quantile(temp$temperature,0.2, type=1),
                  quantile(temp$temperature,0.4, type=1),
                  quantile(temp$temperature,0.6, type=1),
                  quantile(temp$temperature,0.8, type=1))

temp_knot_df

# New TS for temperature support of interest- .225 to get the nice plot with small intervals
temp1 <- as.matrix(Hmisc::rcspline.eval(temp$temperature,knots=temp_knot_df,inclx=T))
temp2 <- cbind(temp,temp1)

names(temp2)
head(temp1)

#adjust variables depending on knots
temp3<-temp2 %>%
  group_by(farm,year)%>%
  summarise_at(vars(x,V2,V3),sum)

#match names with temp
names(regdata_soy1)
colnames(regdata_soy1)[colnames(regdata_soy1) == "Year"] <- "year"
colnames(regdata_soy1)[colnames(regdata_soy1) == "Dis_codename"] <- "farm"
names(regdata_soy1)

#join both  data
temp4 <- join(regdata_soy1,temp3, by=c("farm", "year"))

names(temp4)
temp4$reg_year<- as.numeric(as.factor(temp4$District))*temp4$year

#####bootstrapping
set.seed(123)
marginal_temperatures_M1 <- rcspline.eval(seq(0,45,0.225),knots=temp_knot_df, inclx=T)

# Run 1000 models to derive uncertainty in estimates
spline_fitted_M1 <- matrix(NA,nrow=1000, ncol=nrow(marginal_temperatures_M1))

for (l in 1: 1000){
  # Sample by region times year
  data_temp <- temp4[which(temp4$reg_year %in% sample(unique(temp4$reg_year),replace=T)),]
  
  # Run regression with random subsample (data_temp), adjust variables to knots
  fit_boot  <- feols(log(Yield..Tonnes.Hectare.) ~ x + V2 + V3 + year + I(year^2) + erarain + I(erarain^2)  | farm , data_temp)
  
  #Save marginal effect of temperatures,adjust variables to knots 
  spline_fitted_M1[l,]<-as.vector((marginal_temperatures_M1  %*% coef(fit_boot)[1:3])) 
  
  # Show progress in %
  print(l/1000 *100)
  rm(fit_boot,data_temp)
}

# Load the packages
library(fixest)
library(ggplot2)
library(gridExtra)
library(robustbase)
library(egg)
library(matrixStats)
library(tidyverse)

#Change structure
spline_sub_melt_M1 <- melt(spline_fitted_M1)
# Get median response for each column (= a temperature from seq(20,40,0.225))
colmedians_spline_M1 <- colMedians(spline_fitted_M1)
# Get distance from median response for each value
dist_colmedian_M1 <- matrix(NA, nrow=nrow(spline_fitted_M1),ncol=ncol(spline_fitted_M1))

for(l in 1:ncol(spline_fitted_M1)){
  dist_colmedian_M1[,l] <- spline_fitted_M1[,l] - colmedians_spline_M1[l]
}
# Normalize distance to median
dist_colmedian_M1_normalized <- matrix(NA, nrow=nrow(spline_fitted_M1),ncol=ncol(spline_fitted_M1))

for(l in 1:ncol(spline_fitted_M1)){
  dist_colmedian_M1_normalized[,l]<-abs(dist_colmedian_M1[,l]/max(abs(dist_colmedian_M1[,l])))
}

# Change structure
dist_colmedian_M1_normalized_melted <- melt(dist_colmedian_M1_normalized)

# Last preparation before plotting
spline_merged_M1 <- merge(spline_sub_melt_M1,dist_colmedian_M1_normalized_melted,by=c("Var1","Var2"))
spline_merged_M1$value.y_minus1 <- 1-spline_merged_M1$value.y

temp1 <- aggregate(spline_merged_M1$value.y_minus1, list(spline_merged_M1$Var1), median, na.rm=T) 
temp2 <- aggregate(spline_merged_M1$value.x, list(spline_merged_M1$Var2), median, na.rm=T)

temp3 <- aggregate(spline_merged_M1$value.x, list(spline_merged_M1$Var2), quantile, probs=0.025,na.rm=T) 
temp4 <- aggregate(spline_merged_M1$value.x, list(spline_merged_M1$Var2), quantile, probs=0.975,na.rm=T)

spline_merged_M1 <- merge(spline_merged_M1, temp1, by.x=c("Var1"), by.y=c("Group.1"))
spline_merged_M1 <- merge(spline_merged_M1, temp2, by.x=c("Var2"), by.y=c("Group.1"))
colnames(spline_merged_M1)[7]<-"average_effect"

spline_merged_M1<-merge(spline_merged_M1, temp3, by.x=c("Var2"), by.y=c("Group.1"))
colnames(spline_merged_M1)[8]<-"lower_bound"

spline_merged_M1<-merge(spline_merged_M1, temp4, by.x=c("Var2"), by.y=c("Group.1"))
colnames(spline_merged_M1)[9]<-"upper_bound"

temp5 <- cbind(seq(0,45,0.225),seq(1:length(seq(0,45,0.225))))
colnames(temp5) <- c("Temp_bin", "Var2")
spline_merged_M1 <-merge(spline_merged_M1, temp5, by=c("Var2"))

# Plot: hourly temperature effect
model_2 <- ggplot()+ ggtitle("Hourly temperature effects: Quantile knots 4") + 
  theme(panel.grid.major = element_blank(),
        plot.title = element_text(family="Times New Roman", size=20, colour = "grey25", hjust=0.5),
        axis.title = element_text(family="Times New Roman", size=16, colour = "grey25"),
        axis.text = element_text(family="Times New Roman", size=16),
        panel.grid.minor = element_blank(),
        panel.background = element_blank(),
        axis.line = element_line(colour = "black"),
        legend.position="none")+
  xlab("Temperature (°C)") + ylab("Log yield response (Kg/ha)")+
  scale_x_continuous(breaks = seq(0,45, by=5), limits = c(0,45))+
  scale_y_continuous(breaks = seq(-.02,.02, by=.004), limits = c(-.02,.02))+
  geom_hline(yintercept=c(seq(-.02,.02,by=0.004)), colour="gray")+
  geom_vline(xintercept=temp_knot_df, linetype="dotted", colour="gray")+
  geom_line(data=spline_merged_M1, aes(y=value.x+0.001158199, x=Temp_bin, group=Var1, alpha=x.x^200/200), size=.01, colour="brown1")+
  geom_line(data=spline_merged_M1, aes(y=average_effect+0.001158199, x=Temp_bin), alpha=0.85, size=.75, colour="brown4")+
  geom_line(data=spline_merged_M1, aes(y=lower_bound+0.001158199, x=Temp_bin)   , alpha=0.85, size=.75, colour="brown4")+       
  geom_line(data=spline_merged_M1, aes(y=upper_bound+0.001158199, x=Temp_bin)   , alpha=0.85, size=.75, colour="brown4")

model_2
ggsave("C:/Users/Shakila Vyas/OneDrive - Wageningen University & Research/PhD-WUR-Insurance/CSA/County analysis/Results/soydis_era_qq4_cen.png", width = 20, height = 20, units = "cm")

################################placing knots at 5 degree intervals, not used in main results
hist(temp$temperature)
temp_knot_df <- c(25,30,35)

temp_knot_df

# New TS for temperature support of interest- .225 to get the nice plot with small intervals
temp1 <- as.matrix(Hmisc::rcspline.eval(temp$temperature,knots=temp_knot_df,inclx=T))
temp2 <- cbind(temp,temp1)

names(temp2)
head(temp1)

#adjust variables depending on knots
temp3<-temp2 %>%
  group_by(farm,year)%>%
  summarise_at(vars(x,V2),sum)

#match names with temp
names(regdata_soy1)
colnames(regdata_soy1)[colnames(regdata_soy1) == "Year"] <- "year"
colnames(regdata_soy1)[colnames(regdata_soy1) == "Dis_codename"] <- "farm"
names(regdata_soy1)

#join both  data
temp4 <- join(regdata_soy1,temp3, by=c("farm", "year"))

names(temp4)
temp4$reg_year<- as.numeric(as.factor(temp4$District))*temp4$year

#####bootstrapping
set.seed(123)
marginal_temperatures_M1 <- rcspline.eval(seq(0,45,0.225),knots=temp_knot_df, inclx=T)

# Run 1000 models to derive uncertainty in estimates
spline_fitted_M1 <- matrix(NA,nrow=1000, ncol=nrow(marginal_temperatures_M1))

for (l in 1: 1000){
  # Sample by region times year
  data_temp <- temp4[which(temp4$reg_year %in% sample(unique(temp4$reg_year),replace=T)),]
  
  # Run regression with random subsample (data_temp), adjust variables to knots
  fit_boot  <- feols(log(Yield..Tonnes.Hectare.) ~ x + V2  + year + I(year^2) + erarain + I(erarain^2)  | farm , data_temp)
  
  #Save marginal effect of temperatures,adjust variables to knots 
  spline_fitted_M1[l,]<-as.vector((marginal_temperatures_M1  %*% coef(fit_boot)[1:2])) 
  
  # Show progress in %
  print(l/1000 *100)
  rm(fit_boot,data_temp)
}

# Load the packages
library(fixest)
library(ggplot2)
library(gridExtra)
library(robustbase)
library(egg)
library(matrixStats)
library(tidyverse)

#Change structure
spline_sub_melt_M1 <- melt(spline_fitted_M1)
# Get median response for each column (= a temperature from seq(20,40,0.225))
colmedians_spline_M1 <- colMedians(spline_fitted_M1)
# Get distance from median response for each value
dist_colmedian_M1 <- matrix(NA, nrow=nrow(spline_fitted_M1),ncol=ncol(spline_fitted_M1))

for(l in 1:ncol(spline_fitted_M1)){
  dist_colmedian_M1[,l] <- spline_fitted_M1[,l] - colmedians_spline_M1[l]
}
# Normalize distance to median
dist_colmedian_M1_normalized <- matrix(NA, nrow=nrow(spline_fitted_M1),ncol=ncol(spline_fitted_M1))

for(l in 1:ncol(spline_fitted_M1)){
  dist_colmedian_M1_normalized[,l]<-abs(dist_colmedian_M1[,l]/max(abs(dist_colmedian_M1[,l])))
}

# Change structure
dist_colmedian_M1_normalized_melted <- melt(dist_colmedian_M1_normalized)

# Last preparation before plotting
spline_merged_M1 <- merge(spline_sub_melt_M1,dist_colmedian_M1_normalized_melted,by=c("Var1","Var2"))
spline_merged_M1$value.y_minus1 <- 1-spline_merged_M1$value.y

temp1 <- aggregate(spline_merged_M1$value.y_minus1, list(spline_merged_M1$Var1), median, na.rm=T) 
temp2 <- aggregate(spline_merged_M1$value.x, list(spline_merged_M1$Var2), median, na.rm=T)

temp3 <- aggregate(spline_merged_M1$value.x, list(spline_merged_M1$Var2), quantile, probs=0.025,na.rm=T) 
temp4 <- aggregate(spline_merged_M1$value.x, list(spline_merged_M1$Var2), quantile, probs=0.975,na.rm=T)

spline_merged_M1 <- merge(spline_merged_M1, temp1, by.x=c("Var1"), by.y=c("Group.1"))
spline_merged_M1 <- merge(spline_merged_M1, temp2, by.x=c("Var2"), by.y=c("Group.1"))
colnames(spline_merged_M1)[7]<-"average_effect"

spline_merged_M1<-merge(spline_merged_M1, temp3, by.x=c("Var2"), by.y=c("Group.1"))
colnames(spline_merged_M1)[8]<-"lower_bound"

spline_merged_M1<-merge(spline_merged_M1, temp4, by.x=c("Var2"), by.y=c("Group.1"))
colnames(spline_merged_M1)[9]<-"upper_bound"

temp5 <- cbind(seq(0,45,0.225),seq(1:length(seq(0,45,0.225))))
colnames(temp5) <- c("Temp_bin", "Var2")
spline_merged_M1 <-merge(spline_merged_M1, temp5, by=c("Var2"))

# Plot: hourly temperature effect
model_4 <- ggplot()+ ggtitle("Hourly temperature effects: 5 degree knots ") + 
  theme(panel.grid.major = element_blank(),
        plot.title = element_text(family="Times New Roman", size=20, colour = "grey25", hjust=0.5),
        axis.title = element_text(family="Times New Roman", size=16, colour = "grey25"),
        axis.text = element_text(family="Times New Roman", size=16),
        panel.grid.minor = element_blank(),
        panel.background = element_blank(),
        axis.line = element_line(colour = "black"),
        legend.position="none")+
  xlab("Temperature (°C)") + ylab("Log yield response (Kg/ha)")+
  scale_x_continuous(breaks = seq(0,45, by=5), limits = c(0,45))+
  scale_y_continuous(breaks = seq(-.02,.02, by=.004), limits = c(-.02,.02))+
  geom_hline(yintercept=c(seq(-.02,.02,by=0.004)), colour="gray")+
  geom_vline(xintercept=temp_knot_df, linetype="dotted", colour="gray")+
  geom_line(data=spline_merged_M1, aes(y=value.x+0.002147881, x=Temp_bin, group=Var1, alpha=x.x^200/200), size=.01, colour="brown1")+
  geom_line(data=spline_merged_M1, aes(y=average_effect+0.002147881, x=Temp_bin), alpha=0.85, size=.75, colour="brown4")+
  geom_line(data=spline_merged_M1, aes(y=lower_bound+0.002147881, x=Temp_bin)   , alpha=0.85, size=.75, colour="brown4")+       
  geom_line(data=spline_merged_M1, aes(y=upper_bound+0.002147881, x=Temp_bin)   , alpha=0.85, size=.75, colour="brown4")
model_4

ggsave("C:/Users/Shakila Vyas/OneDrive - Wageningen University & Research/PhD-WUR-Insurance/CSA/County analysis/Results/soydis_era_5degree3_cen.png", width = 20, height = 20, units = "cm")
###done till here only

################################placing knots at 5 degree intervals, not used in main results
hist(temp$temperature)
temp_knot_df <- c(20,25,30)

temp_knot_df

# New TS for temperature support of interest- .225 to get the nice plot with small intervals
temp1 <- as.matrix(Hmisc::rcspline.eval(temp$temperature,knots=temp_knot_df,inclx=T))
temp2 <- cbind(temp,temp1)

names(temp2)
head(temp1)

#adjust variables depending on knots
temp3<-temp2 %>%
  group_by(farm,year)%>%
  summarise_at(vars(x,V2),sum)

#match names with temp
names(regdata_soy1)
colnames(regdata_soy1)[colnames(regdata_soy1) == "Year"] <- "year"
colnames(regdata_soy1)[colnames(regdata_soy1) == "Dis_codename"] <- "farm"
names(regdata_soy1)

#join both  data
temp4 <- join(regdata_soy1,temp3, by=c("farm", "year"))

names(temp4)
temp4$reg_year<- as.numeric(as.factor(temp4$District))*temp4$year

#####bootstrapping
set.seed(123)
marginal_temperatures_M1 <- rcspline.eval(seq(10,50,0.225),knots=temp_knot_df, inclx=T)

# Run 1000 models to derive uncertainty in estimates
spline_fitted_M1 <- matrix(NA,nrow=1000, ncol=nrow(marginal_temperatures_M1))

for (l in 1: 1000){
  # Sample by region times year
  data_temp <- temp4[which(temp4$reg_year %in% sample(unique(temp4$reg_year),replace=T)),]
  
  # Run regression with random subsample (data_temp), adjust variables to knots
  fit_boot  <- feols(log(Yield..Tonnes.Hectare.) ~ x + V2  + year + I(year^2) + summerrainsum + I(summerrainsum^2)  | farm , data_temp)
  
  #Save marginal effect of temperatures,adjust variables to knots 
  spline_fitted_M1[l,]<-as.vector((marginal_temperatures_M1  %*% coef(fit_boot)[1:2])) 
  
  # Show progress in %
  print(l/1000 *100)
  rm(fit_boot,data_temp)
}

# Load the packages
library(fixest)
library(ggplot2)
library(gridExtra)
library(robustbase)
library(egg)
library(matrixStats)
library(tidyverse)

#Change structure
spline_sub_melt_M1 <- melt(spline_fitted_M1)
# Get median response for each column (= a temperature from seq(20,40,0.225))
colmedians_spline_M1 <- colMedians(spline_fitted_M1)
# Get distance from median response for each value
dist_colmedian_M1 <- matrix(NA, nrow=nrow(spline_fitted_M1),ncol=ncol(spline_fitted_M1))

for(l in 1:ncol(spline_fitted_M1)){
  dist_colmedian_M1[,l] <- spline_fitted_M1[,l] - colmedians_spline_M1[l]
}
# Normalize distance to median
dist_colmedian_M1_normalized <- matrix(NA, nrow=nrow(spline_fitted_M1),ncol=ncol(spline_fitted_M1))

for(l in 1:ncol(spline_fitted_M1)){
  dist_colmedian_M1_normalized[,l]<-abs(dist_colmedian_M1[,l]/max(abs(dist_colmedian_M1[,l])))
}

# Change structure
dist_colmedian_M1_normalized_melted <- melt(dist_colmedian_M1_normalized)

# Last preparation before plotting
spline_merged_M1 <- merge(spline_sub_melt_M1,dist_colmedian_M1_normalized_melted,by=c("Var1","Var2"))
spline_merged_M1$value.y_minus1 <- 1-spline_merged_M1$value.y

temp1 <- aggregate(spline_merged_M1$value.y_minus1, list(spline_merged_M1$Var1), median, na.rm=T) 
temp2 <- aggregate(spline_merged_M1$value.x, list(spline_merged_M1$Var2), median, na.rm=T)

temp3 <- aggregate(spline_merged_M1$value.x, list(spline_merged_M1$Var2), quantile, probs=0.025,na.rm=T) 
temp4 <- aggregate(spline_merged_M1$value.x, list(spline_merged_M1$Var2), quantile, probs=0.975,na.rm=T)

spline_merged_M1 <- merge(spline_merged_M1, temp1, by.x=c("Var1"), by.y=c("Group.1"))
spline_merged_M1 <- merge(spline_merged_M1, temp2, by.x=c("Var2"), by.y=c("Group.1"))
colnames(spline_merged_M1)[7]<-"average_effect"

spline_merged_M1<-merge(spline_merged_M1, temp3, by.x=c("Var2"), by.y=c("Group.1"))
colnames(spline_merged_M1)[8]<-"lower_bound"

spline_merged_M1<-merge(spline_merged_M1, temp4, by.x=c("Var2"), by.y=c("Group.1"))
colnames(spline_merged_M1)[9]<-"upper_bound"

temp5 <- cbind(seq(10,50,0.225),seq(1:length(seq(10,50,0.225))))
colnames(temp5) <- c("Temp_bin", "Var2")
spline_merged_M1 <-merge(spline_merged_M1, temp5, by=c("Var2"))

# Plot: hourly temperature effect

model_4 <- ggplot()+ ggtitle("Hourly temperature effects: 5 degree knots (alternate) ") + 
  theme(panel.grid.major = element_blank(),
        plot.title = element_text(family="Times New Roman", size=20, colour = "grey25", hjust=0.5),
        axis.title = element_text(family="Times New Roman", size=16, colour = "grey25"),
        axis.text = element_text(family="Times New Roman", size=16),
        panel.grid.minor = element_blank(),
        panel.background = element_blank(),
        axis.line = element_line(colour = "black"),
        legend.position="none")+
  xlab("Temperature (°C)") + ylab("Log yield response (Kg/ha)")+
  scale_x_continuous(breaks = seq(10,55, by=5), limits = c(5,55))+
  scale_y_continuous(breaks = seq(-.02,.02, by=.004), limits = c(-.02,.02))+
  geom_hline(yintercept=c(seq(-.02,.02,by=0.004)), colour="gray")+
  geom_vline(xintercept=temp_knot_df, linetype="dotted", colour="gray")+
  geom_line(data=spline_merged_M1, aes(y=value.x, x=Temp_bin, group=Var1, alpha=x.x^200/200), size=.01, colour="brown1")+
  geom_line(data=spline_merged_M1, aes(y=average_effect, x=Temp_bin), alpha=0.85, size=.75, colour="brown4")+
  geom_line(data=spline_merged_M1, aes(y=lower_bound, x=Temp_bin)   , alpha=0.85, size=.75, colour="brown4")+       
  geom_line(data=spline_merged_M1, aes(y=upper_bound, x=Temp_bin)   , alpha=0.85, size=.75, colour="brown4")

model_4
ggsave("C:/Users/Shakila Vyas/OneDrive - Wageningen University & Research/PhD-WUR-Insurance/CSA/County analysis/Results/soydis_IMD_5degree3_20_25_30_discombo.png", width = 20, height = 20, units = "cm")

################################placing knots at 5 degree intervals, not used in main results
hist(temp$temperature)
temp_knot_df <- c(20,25,30,35)
temp_knot_df

# New TS for temperature support of interest- .225 to get the nice plot with small intervals
temp1 <- as.matrix(Hmisc::rcspline.eval(temp$temperature,knots=temp_knot_df,inclx=T))
temp2 <- cbind(temp,temp1)

names(temp2)
head(temp1)

#adjust variables depending on knots
temp3<-temp2 %>%
  group_by(farm,year)%>%
  summarise_at(vars(x,V2,V3),sum)

#match names with temp
names(regdata_soy1)
colnames(regdata_soy1)[colnames(regdata_soy1) == "Year"] <- "year"
colnames(regdata_soy1)[colnames(regdata_soy1) == "Dis_codename"] <- "farm"
names(regdata_soy1)

#join both  data
temp4 <- join(regdata_soy1,temp3, by=c("farm", "year"))

names(temp4)
temp4$reg_year<- as.numeric(as.factor(temp4$District))*temp4$year

#####bootstrapping
set.seed(123)
marginal_temperatures_M1 <- rcspline.eval(seq(10,50,0.225),knots=temp_knot_df, inclx=T)

# Run 1000 models to derive uncertainty in estimates
spline_fitted_M1 <- matrix(NA,nrow=1000, ncol=nrow(marginal_temperatures_M1))

for (l in 1: 1000){
  # Sample by region times year
  data_temp <- temp4[which(temp4$reg_year %in% sample(unique(temp4$reg_year),replace=T)),]
  
  # Run regression with random subsample (data_temp), adjust variables to knots
  fit_boot  <- feols(log(Yield..Tonnes.Hectare.) ~ x + V2  + V3+ year + I(year^2) + summerrainsum + I(summerrainsum^2)  | farm , data_temp)
  
  #Save marginal effect of temperatures,adjust variables to knots 
  spline_fitted_M1[l,]<-as.vector((marginal_temperatures_M1  %*% coef(fit_boot)[1:3])) 
  
  # Show progress in %
  print(l/1000 *100)
  rm(fit_boot,data_temp)
}

# Load the packages
library(fixest)
library(ggplot2)
library(gridExtra)
library(robustbase)
library(egg)
library(matrixStats)
library(tidyverse)

#Change structure
spline_sub_melt_M1 <- melt(spline_fitted_M1)
# Get median response for each column (= a temperature from seq(20,40,0.225))
colmedians_spline_M1 <- colMedians(spline_fitted_M1)
# Get distance from median response for each value
dist_colmedian_M1 <- matrix(NA, nrow=nrow(spline_fitted_M1),ncol=ncol(spline_fitted_M1))

for(l in 1:ncol(spline_fitted_M1)){
  dist_colmedian_M1[,l] <- spline_fitted_M1[,l] - colmedians_spline_M1[l]
}
# Normalize distance to median
dist_colmedian_M1_normalized <- matrix(NA, nrow=nrow(spline_fitted_M1),ncol=ncol(spline_fitted_M1))

for(l in 1:ncol(spline_fitted_M1)){
  dist_colmedian_M1_normalized[,l]<-abs(dist_colmedian_M1[,l]/max(abs(dist_colmedian_M1[,l])))
}

# Change structure
dist_colmedian_M1_normalized_melted <- melt(dist_colmedian_M1_normalized)

# Last preparation before plotting
spline_merged_M1 <- merge(spline_sub_melt_M1,dist_colmedian_M1_normalized_melted,by=c("Var1","Var2"))
spline_merged_M1$value.y_minus1 <- 1-spline_merged_M1$value.y

temp1 <- aggregate(spline_merged_M1$value.y_minus1, list(spline_merged_M1$Var1), median, na.rm=T) 
temp2 <- aggregate(spline_merged_M1$value.x, list(spline_merged_M1$Var2), median, na.rm=T)

temp3 <- aggregate(spline_merged_M1$value.x, list(spline_merged_M1$Var2), quantile, probs=0.025,na.rm=T) 
temp4 <- aggregate(spline_merged_M1$value.x, list(spline_merged_M1$Var2), quantile, probs=0.975,na.rm=T)

spline_merged_M1 <- merge(spline_merged_M1, temp1, by.x=c("Var1"), by.y=c("Group.1"))
spline_merged_M1 <- merge(spline_merged_M1, temp2, by.x=c("Var2"), by.y=c("Group.1"))
colnames(spline_merged_M1)[7]<-"average_effect"

spline_merged_M1<-merge(spline_merged_M1, temp3, by.x=c("Var2"), by.y=c("Group.1"))
colnames(spline_merged_M1)[8]<-"lower_bound"

spline_merged_M1<-merge(spline_merged_M1, temp4, by.x=c("Var2"), by.y=c("Group.1"))
colnames(spline_merged_M1)[9]<-"upper_bound"

temp5 <- cbind(seq(10,50,0.225),seq(1:length(seq(10,50,0.225))))
colnames(temp5) <- c("Temp_bin", "Var2")
spline_merged_M1 <-merge(spline_merged_M1, temp5, by=c("Var2"))

# Plot: hourly temperature effect
model_4 <- ggplot()+ ggtitle("Hourly temperature effects: 5 degree knots ") + 
  theme(panel.grid.major = element_blank(),
        plot.title = element_text(family="Times New Roman", size=20, colour = "grey25", hjust=0.5),
        axis.title = element_text(family="Times New Roman", size=16, colour = "grey25"),
        axis.text = element_text(family="Times New Roman", size=16),
        panel.grid.minor = element_blank(),
        panel.background = element_blank(),
        axis.line = element_line(colour = "black"),
        legend.position="none")+
  xlab("Temperature (°C)") + ylab("Log yield response (Kg/ha)")+
  scale_x_continuous(breaks = seq(10,55, by=5), limits = c(5,55))+
  scale_y_continuous(breaks = seq(-.04,.04, by=.004), limits = c(-.04,.04))+
  geom_hline(yintercept=c(seq(-.04,.04,by=0.004)), colour="gray")+
  geom_vline(xintercept=temp_knot_df, linetype="dotted", colour="gray")+
  geom_line(data=spline_merged_M1, aes(y=value.x, x=Temp_bin, group=Var1, alpha=x.x^200/200), size=.01, colour="brown1")+
  geom_line(data=spline_merged_M1, aes(y=average_effect, x=Temp_bin), alpha=0.85, size=.75, colour="brown4")+
  geom_line(data=spline_merged_M1, aes(y=lower_bound, x=Temp_bin)   , alpha=0.85, size=.75, colour="brown4")+       
  geom_line(data=spline_merged_M1, aes(y=upper_bound, x=Temp_bin)   , alpha=0.85, size=.75, colour="brown4")

model_4

ggsave("C:/Users/Shakila Vyas/OneDrive - Wageningen University & Research/PhD-WUR-Insurance/CSA/County analysis/Results/soydis_IMD_5degree3_knots4_discombo.png", width = 20, height = 20, units = "cm")

#####best fit model
#best knots based on RSS
# lower and upper bound define lowest and highest knot location
lower_bound <- ceiling(quantile(temp$temperature,0.05, type=1))
upper_bound <- floor(quantile(temp$temperature,0.95, type=1))
temp_range <- seq(lower_bound, upper_bound,1) 
kn3_combi <- combn(temp_range,3, simplify = T)
# Minimum space between 2 knots
required_space <- 5
# Get all knot combinations
differences <- matrix(NA, nrow=3, ncol=ncol(kn3_combi))
differences[1,] <- abs(kn3_combi[1,] - kn3_combi[2,])
differences[2,] <- abs(kn3_combi[1,] - kn3_combi[3,])
differences[3,] <- abs(kn3_combi[2,] - kn3_combi[3,])
knots_3_combi_good <- kn3_combi[,which(differences[1,] >= required_space & differences[2,] >= required_space & differences[3,] >= required_space)]
# vector containing residual sum of squares (RSS)
RSS_3knots <- vector(length=ncol(knots_3_combi_good ))

# Get the RSS for each model
for (c in 1:ncol(knots_3_combi_good)){
  
  # Get new time series and aggregate hourly values  
  temp1 <- as.matrix(rcspline.eval(temp$temperature,knots= knots_3_combi_good[,c], inclx=T))
  temp2 <- cbind(temp,temp1)
  
  temp3<-temp2 %>%
    group_by(farm,year)%>%
    summarise_at(vars(x,V2),sum)
  
  # Match yearly values with yearly yields
  temp4 <- join(regdata_soy1,temp3, by=c("farm", "year"))
  
  # Run regression
  temp_reg <- feols(log(Yield..Tonnes.Hectare.) ~ x + V2  +  year + I(year^2) + summerrainsum + I(summerrainsum^2)  | farm , temp4)
  RSS_3knots[c] <- sum(resid(temp_reg)^2)
  
}

# Best number of knots
temp_knot_df <- knots_3_combi_good[,which.min(RSS_3knots)]
rm(RSS_3knots, knots_3_combi_good, differences, kn3_combi,temp_range,upper_bound, lower_bound)

# resume reg
temp1 <- as.matrix(Hmisc::rcspline.eval(temp$temperature,knots=temp_knot_df,inclx=T))
temp2 <- cbind(temp,temp1)

names(temp2)
head(temp1)

#adjust variables depending on knots
temp3<-temp2 %>%
  group_by(farm,year)%>%
  summarise_at(vars(x,V2),sum)

#match names with temp
names(regdata_soy1)
colnames(regdata_soy1)[colnames(regdata_soy1) == "Year"] <- "year"
colnames(regdata_soy1)[colnames(regdata_soy1) == "Dis_codename"] <- "farm"
names(regdata_soy1)

#join both  data
temp4 <- join(regdata_soy1,temp3, by=c("farm", "year"))

names(temp4)
temp4$reg_year<- as.numeric(as.factor(temp4$District))*temp4$year

#####bootstrapping
set.seed(123)
marginal_temperatures_M1 <- rcspline.eval(seq(0,45,0.225),knots=temp_knot_df, inclx=T)

# Run 1000 models to derive uncertainty in estimates
spline_fitted_M1 <- matrix(NA,nrow=1000, ncol=nrow(marginal_temperatures_M1))

for (l in 1: 1000){
  # Sample by region times year
  data_temp <- temp4[which(temp4$reg_year %in% sample(unique(temp4$reg_year),replace=T)),]
  
  # Run regression with random subsample (data_temp), adjust variables to knots
  fit_boot  <- feols(log(Yield..Tonnes.Hectare.) ~ x + V2  +  year + I(year^2) + summerrainsum + I(summerrainsum^2)  | farm , data_temp)
  
  #Save marginal effect of temperatures,adjust variables to knots 
  spline_fitted_M1[l,]<-as.vector((marginal_temperatures_M1  %*% coef(fit_boot)[1:2])) 
  
  # Show progress in %
  print(l/1000 *100)
  rm(fit_boot,data_temp)
}

#Change structure
spline_sub_melt_M1 <- melt(spline_fitted_M1)
# Get median response for each column (= a temperature from seq(20,40,0.225))
colmedians_spline_M1 <- colMedians(spline_fitted_M1)
# Get distance from median response for each value
dist_colmedian_M1 <- matrix(NA, nrow=nrow(spline_fitted_M1),ncol=ncol(spline_fitted_M1))

for(l in 1:ncol(spline_fitted_M1)){
  dist_colmedian_M1[,l] <- spline_fitted_M1[,l] - colmedians_spline_M1[l]
}
# Normalize distance to median
dist_colmedian_M1_normalized <- matrix(NA, nrow=nrow(spline_fitted_M1),ncol=ncol(spline_fitted_M1))

for(l in 1:ncol(spline_fitted_M1)){
  dist_colmedian_M1_normalized[,l]<-abs(dist_colmedian_M1[,l]/max(abs(dist_colmedian_M1[,l])))
}

# Change structure
dist_colmedian_M1_normalized_melted <- melt(dist_colmedian_M1_normalized)

# Last preparation before plotting
spline_merged_M1 <- merge(spline_sub_melt_M1,dist_colmedian_M1_normalized_melted,by=c("Var1","Var2"))
spline_merged_M1$value.y_minus1 <- 1-spline_merged_M1$value.y

temp1 <- aggregate(spline_merged_M1$value.y_minus1, list(spline_merged_M1$Var1), median, na.rm=T) 
temp2 <- aggregate(spline_merged_M1$value.x, list(spline_merged_M1$Var2), median, na.rm=T)

temp3 <- aggregate(spline_merged_M1$value.x, list(spline_merged_M1$Var2), quantile, probs=0.025,na.rm=T) 
temp4 <- aggregate(spline_merged_M1$value.x, list(spline_merged_M1$Var2), quantile, probs=0.975,na.rm=T)

spline_merged_M1 <- merge(spline_merged_M1, temp1, by.x=c("Var1"), by.y=c("Group.1"))
spline_merged_M1 <- merge(spline_merged_M1, temp2, by.x=c("Var2"), by.y=c("Group.1"))
colnames(spline_merged_M1)[7]<-"average_effect"

spline_merged_M1<-merge(spline_merged_M1, temp3, by.x=c("Var2"), by.y=c("Group.1"))
colnames(spline_merged_M1)[8]<-"lower_bound"

spline_merged_M1<-merge(spline_merged_M1, temp4, by.x=c("Var2"), by.y=c("Group.1"))
colnames(spline_merged_M1)[9]<-"upper_bound"

temp5 <- cbind(seq(0,45,0.225),seq(1:length(seq(0,45,0.225))))
colnames(temp5) <- c("Temp_bin", "Var2")
spline_merged_M1 <-merge(spline_merged_M1, temp5, by=c("Var2"))

# Plot: hourly temperature effect
model_4 <- ggplot()+ ggtitle("Hourly temperature effects: Best fit") + 
  theme(panel.grid.major = element_blank(),
        plot.title = element_text(family="Times New Roman", size=20, colour = "grey25", hjust=0.5),
        axis.title = element_text(family="Times New Roman", size=16, colour = "grey25"),
        axis.text = element_text(family="Times New Roman", size=16),
        panel.grid.minor = element_blank(),
        panel.background = element_blank(),
        axis.line = element_line(colour = "black"),
        legend.position="none")+
  xlab("Temperature (°C)") + ylab("Log yield response (Kg/ha)")+
  scale_x_continuous(breaks = seq(0,45, by=5), limits = c(0,45))+
  scale_y_continuous(breaks = seq(-.02,.02, by=.004), limits = c(-.02,.02))+
  geom_hline(yintercept=c(seq(-.02,.02,by=0.004)), colour="gray")+
  geom_vline(xintercept=temp_knot_df, linetype="dotted", colour="gray")+
  geom_line(data=spline_merged_M1, aes(y=value.x+0.0007127207, x=Temp_bin, group=Var1, alpha=x.x^200/200), size=.01, colour="brown1")+
  geom_line(data=spline_merged_M1, aes(y=average_effect+0.0007127207, x=Temp_bin), alpha=0.85, size=.75, colour="brown4")+
  geom_line(data=spline_merged_M1, aes(y=lower_bound+0.0007127207, x=Temp_bin)   , alpha=0.85, size=.75, colour="brown4")+       
  geom_line(data=spline_merged_M1, aes(y=upper_bound+0.0007127207, x=Temp_bin)   , alpha=0.85, size=.75, colour="brown4")

model_4
ggsave("C:/Users/Shakila Vyas/OneDrive - Wageningen University & Research/PhD-WUR-Insurance/CSA/County analysis/Results/soydis_IMD_bestfit_cen.png", width = 20, height = 20, units = "cm")

#main fig for best fit
# Best number of knots
temp_knot_df <- c(24,28,34) #from prev model of RSS

# resume reg
temp1 <- as.matrix(Hmisc::rcspline.eval(temp$temperature,knots=temp_knot_df,inclx=T))
temp2 <- cbind(temp,temp1)

names(temp2)
head(temp1)

#adjust variables depending on knots
temp3<-temp2 %>%
  group_by(farm,year)%>%
  summarise_at(vars(x,V2),sum)

#match names with temp
names(regdata_soy1)
colnames(regdata_soy1)[colnames(regdata_soy1) == "Year"] <- "year"
colnames(regdata_soy1)[colnames(regdata_soy1) == "Dis_codename"] <- "farm"
names(regdata_soy1)

#join both  data
temp4 <- join(regdata_soy1,temp3, by=c("farm", "year"))

names(temp4)
temp4$reg_year<- as.numeric(as.factor(temp4$District))*temp4$year

#####bootstrapping
set.seed(123)
marginal_temperatures_M1 <- rcspline.eval(seq(10,50,0.225),knots=temp_knot_df, inclx=T)

# Run 1000 models to derive uncertainty in estimates
spline_fitted_M1 <- matrix(NA,nrow=1000, ncol=nrow(marginal_temperatures_M1))

for (l in 1: 1000){
  # Sample by region times year
  data_temp <- temp4[which(temp4$reg_year %in% sample(unique(temp4$reg_year),replace=T)),]
  
  # Run regression with random subsample (data_temp), adjust variables to knots
  fit_boot  <- feols(log(Yield..Tonnes.Hectare.) ~ x + V2  +  year + I(year^2) + summerrainsum + I(summerrainsum^2)  | farm , data_temp)
  
  #Save marginal effect of temperatures,adjust variables to knots 
  spline_fitted_M1[l,]<-as.vector((marginal_temperatures_M1  %*% coef(fit_boot)[1:2])) 
  
  # Show progress in %
  print(l/1000 *100)
  rm(fit_boot,data_temp)
}

#Change structure
spline_sub_melt_M1 <- melt(spline_fitted_M1)
# Get median response for each column (= a temperature from seq(20,40,0.225))
colmedians_spline_M1 <- colMedians(spline_fitted_M1)
# Get distance from median response for each value
dist_colmedian_M1 <- matrix(NA, nrow=nrow(spline_fitted_M1),ncol=ncol(spline_fitted_M1))

for(l in 1:ncol(spline_fitted_M1)){
  dist_colmedian_M1[,l] <- spline_fitted_M1[,l] - colmedians_spline_M1[l]
}
# Normalize distance to median
dist_colmedian_M1_normalized <- matrix(NA, nrow=nrow(spline_fitted_M1),ncol=ncol(spline_fitted_M1))

for(l in 1:ncol(spline_fitted_M1)){
  dist_colmedian_M1_normalized[,l]<-abs(dist_colmedian_M1[,l]/max(abs(dist_colmedian_M1[,l])))
}

# Change structure
dist_colmedian_M1_normalized_melted <- melt(dist_colmedian_M1_normalized)

# Last preparation before plotting
spline_merged_M1 <- merge(spline_sub_melt_M1,dist_colmedian_M1_normalized_melted,by=c("Var1","Var2"))
spline_merged_M1$value.y_minus1 <- 1-spline_merged_M1$value.y

temp1 <- aggregate(spline_merged_M1$value.y_minus1, list(spline_merged_M1$Var1), median, na.rm=T) 
temp2 <- aggregate(spline_merged_M1$value.x, list(spline_merged_M1$Var2), median, na.rm=T)

temp3 <- aggregate(spline_merged_M1$value.x, list(spline_merged_M1$Var2), quantile, probs=0.025,na.rm=T) 
temp4 <- aggregate(spline_merged_M1$value.x, list(spline_merged_M1$Var2), quantile, probs=0.975,na.rm=T)

spline_merged_M1 <- merge(spline_merged_M1, temp1, by.x=c("Var1"), by.y=c("Group.1"))
spline_merged_M1 <- merge(spline_merged_M1, temp2, by.x=c("Var2"), by.y=c("Group.1"))
colnames(spline_merged_M1)[7]<-"average_effect"

spline_merged_M1<-merge(spline_merged_M1, temp3, by.x=c("Var2"), by.y=c("Group.1"))
colnames(spline_merged_M1)[8]<-"lower_bound"

spline_merged_M1<-merge(spline_merged_M1, temp4, by.x=c("Var2"), by.y=c("Group.1"))
colnames(spline_merged_M1)[9]<-"upper_bound"

temp5 <- cbind(seq(10,50,0.225),seq(1:length(seq(10,50,0.225))))
colnames(temp5) <- c("Temp_bin", "Var2")
spline_merged_M1 <-merge(spline_merged_M1, temp5, by=c("Var2"))

# Plot: hourly temperature effect
model_4 <- ggplot()+ 
  theme(panel.grid.major = element_blank(),
        plot.title = element_text(family="Times New Roman", size=18, colour = "grey25", hjust=0.5),
        axis.title = element_text(family="Times New Roman", size=18, colour = "grey25"),
        axis.text = element_text(family="Times New Roman", size=18),
        panel.grid.minor = element_blank(),
        panel.background = element_blank(),
        axis.line = element_line(colour = "black"),
        legend.position="none")+
  xlab("Temperature (C)") + ylab("Log yield response (Kg/ha)")+
  scale_x_continuous(breaks = seq(10,55, by=5), limits = c(5,55))+
  scale_y_continuous(breaks = seq(-.04,.04, by=.004), limits = c(-.04,.04))+
  geom_hline(yintercept=c(seq(-.04,.04,by=0.004)), colour="gray")+
  geom_vline(xintercept=temp_knot_df, linetype="dotted", colour="gray")+
  geom_line(data=spline_merged_M1, aes(y=value.x, x=Temp_bin, group=Var1, alpha=x.x^200/200), size=.01, colour="brown1")+
  geom_line(data=spline_merged_M1, aes(y=average_effect, x=Temp_bin), alpha=0.85, size=.75, colour="brown4")+
  geom_line(data=spline_merged_M1, aes(y=lower_bound, x=Temp_bin)   , alpha=0.85, size=.75, colour="brown4")+       
  geom_line(data=spline_merged_M1, aes(y=upper_bound, x=Temp_bin)   , alpha=0.85, size=.75, colour="brown4")

model_4
ggsave("C:/Users/Shakila Vyas/OneDrive - Wageningen University & Research/PhD-WUR-Insurance/CSA/County analysis/Results/soydis_ERA_bestfit.png", width = 20, height = 20, units = "cm")

#main fig adding wheat here for panel fig

#best fit for wheat
# Plot: hourly temperature effect, copying from wheat results below

model_1 <- ggplot()+ 
  theme(panel.grid.major = element_blank(),
        plot.title = element_text(family="Times New Roman", size=18, colour = "grey25", hjust=0.5),
        axis.title = element_text(family="Times New Roman", size=18, colour = "grey25"),
        axis.text = element_text(family="Times New Roman", size=18),
        panel.grid.minor = element_blank(),
        panel.background = element_blank(),
        axis.line = element_line(colour = "black"),
        legend.position="none")+
  xlab("Temperature (C)") + ylab("Log yield response (Kg/ha)")+
  scale_x_continuous(breaks = seq(5,45, by=5), limits = c(5,45))+
  scale_y_continuous(breaks = seq(-.02,.02, by=.004), limits = c(-.02,.02))+
  geom_hline(yintercept=c(seq(-.02,.02,by=0.004)), colour="gray")+
  geom_vline(xintercept=temp_knot_df, linetype="dotted", colour="gray")+
  geom_line(data=spline_merged_M1, aes(y=value.x, x=Temp_bin, group=Var1, alpha=x.x^200/200), size=.01, colour="darkorange1")+
  geom_line(data=spline_merged_M1, aes(y=average_effect, x=Temp_bin), alpha=0.85, size=.75, colour="darkorange4")+
  geom_line(data=spline_merged_M1, aes(y=lower_bound, x=Temp_bin)   , alpha=0.85, size=.75, colour="darkorange4")+       
  geom_line(data=spline_merged_M1, aes(y=upper_bound, x=Temp_bin)   , alpha=0.85, size=.75, colour="darkorange4")

model_1

finplot<- ggarrange(model_4, model_1, 
                    labels = c("a)", "b)"),
                    ncol = 2, nrow = 1)


ggsave(plot=finplot, file="C:/Users/Shakila Vyas/OneDrive - Wageningen University & Research/PhD-WUR-Insurance/CSA/County analysis/Results/countybestfit_era.png", width = 30, height = 20, units = "cm")

#robustness checks
#subset region
data<- read.csv("C:/Users/Shakila Vyas/OneDrive - Wageningen University & Research/PhD-WUR-Insurance/CSA/County analysis/Dis yield_All India/Soy_yields.csv")
head(data)
#remove less than 3 years data
regdata_soy<- subset(data,Year>=2015,select = District:area)

data1<- add_count(regdata_soy, Dis_codename)
regdata_soy1<- subset(data1,n>=3,select = District:n)
head(regdata_soy1)
table(regdata_soy1$area)
regdata_soy_s<- subset(regdata_soy1,area=="south",select = District:n)

##preparing data
hourly<- read.csv("C:/Users/Shakila Vyas/OneDrive - Wageningen University & Research/PhD-WUR-Insurance/CSA/County analysis/Weather data/ERA5/Summer/hourlytp15era.csv")
#need this format-colnames(temp_wheat_exposure2) <- c("Farm","Date","Hour", "Temperature")
as.data.frame(hourly)
library(reshape2)
long <- reshape2::melt(hourly, id.vars = c("farm"))
long$year<- 2015

hourly1<- read.csv("C:/Users/Shakila Vyas/OneDrive - Wageningen University & Research/PhD-WUR-Insurance/CSA/County analysis/Weather data/ERA5/Summer/hourlytp16era.csv")
#need this format-colnames(temp_wheat_exposure2) <- c("Farm","Date","Hour", "Temperature")
as.data.frame(hourly1)
long1 <- reshape2::melt(hourly1, id.vars = c("farm"))
long1$year<- 2016

hourly2<- read.csv("C:/Users/Shakila Vyas/OneDrive - Wageningen University & Research/PhD-WUR-Insurance/CSA/County analysis/Weather data/ERA5/Summer/hourlytp17era.csv")
#need this format-colnames(temp_wheat_exposure2) <- c("Farm","Date","Hour", "Temperature")
as.data.frame(hourly2)
long2 <- reshape2::melt(hourly2, id.vars = c("farm"))
long2$year<- 2017

hourly3<- read.csv("C:/Users/Shakila Vyas/OneDrive - Wageningen University & Research/PhD-WUR-Insurance/CSA/County analysis/Weather data/ERA5/Summer/hourlytp18era.csv")
#need this format-colnames(temp_wheat_exposure2) <- c("Farm","Date","Hour", "Temperature")
as.data.frame(hourly3)
long3 <- reshape2::melt(hourly3, id.vars = c("farm"))
long3$year<- 2018

hourly4<- read.csv("C:/Users/Shakila Vyas/OneDrive - Wageningen University & Research/PhD-WUR-Insurance/CSA/County analysis/Weather data/ERA5/Summer/hourlytp19era.csv")
#need this format-colnames(temp_wheat_exposure2) <- c("Farm","Date","Hour", "Temperature")
as.data.frame(hourly4)
long4 <- reshape2::melt(hourly4, id.vars = c("farm"))
long4$year<- 2019

hourly5<- read.csv("C:/Users/Shakila Vyas/OneDrive - Wageningen University & Research/PhD-WUR-Insurance/CSA/County analysis/Weather data/ERA5/Summer/hourlytp20era.csv")
#need this format-colnames(temp_wheat_exposure2) <- c("Farm","Date","Hour", "Temperature")
as.data.frame(hourly5)
long5 <- reshape2::melt(hourly5, id.vars = c("farm"))
long5$year<- 2020

temp<- rbind(long,long1,long2,long3,long4,long5)

#centering
#rename temp data
names(temp)[3] <- "temperature"
as.numeric(temp$temperature)
table(regdata_soy_s$farm)
names(regdata_soy_s)
colnames(regdata_soy_s)[colnames(regdata_soy_s) == "Year"] <- "year"
colnames(regdata_soy_s)[colnames(regdata_soy_s) == "Dis_codename"] <- "farm"
names(regdata_soy_s)
tempav<-join(regdata_soy_s,temp, by=c("farm", "year"))
summary(tempav$temperature)

####outlier removal
hist(regdata_soy_s$Yield..Tonnes.Hectare.)
quantile(regdata_soy_s$Yield..Tonnes.Hectare.,probs= c(.99))
regdata_soy_s<- subset(regdata_soy_s,Yield..Tonnes.Hectare.<quantile(regdata_soy_s$Yield..Tonnes.Hectare.,probs= c(.99)),select = District:n )
hist(regdata_soy_s$Yield..Tonnes.Hectare.)

#rename temp data
names(temp)[3] <- "temperature"
as.numeric(temp$temperature)
min(temp$temperature)
max(temp$temperature)
range(temp$temperature)
summary(temp$temperature)
hist(temp$temperature,main="Hourly temperature district-summer")

range(temp$temperature)
temp <- na.omit(temp)

#####################model 1: equal spaced 3 knots at 5 degree intervals
temp_knot_df <- c(min(temp$temperature) + ((max(temp$temperature)-min(temp$temperature)) / 4),
                  min(temp$temperature) + 2* ((max(temp$temperature)-min(temp$temperature)) / 4),
                  min(temp$temperature) + 3* ((max(temp$temperature)-min(temp$temperature)) / 4))

temp_knot_df <- c(22,27,32)

# New TS for temperature support of interest- .225 to get the nice plot with small intervals
temp1 <- as.matrix(Hmisc::rcspline.eval(temp$temperature,knots=temp_knot_df,inclx=T))
temp2 <- cbind(temp,temp1)

names(temp2)
head(temp1)

#adjust variables depending on knots
temp3<-temp2 %>%
  group_by(farm,year)%>%
  summarise_at(vars(x,V2),sum)

#match names with temp
names(regdata_soy_s)
colnames(regdata_soy_s)[colnames(regdata_soy_s) == "Year"] <- "year"
colnames(regdata_soy_s)[colnames(regdata_soy_s) == "Dis_codename"] <- "farm"
names(regdata_soy_s)

#join both  data
temp4 <- join(regdata_soy_s,temp3, by=c("farm", "year"))
names(temp4)
temp4$reg_year<- as.numeric(as.factor(temp4$District))*temp4$year

#prelim rgression
library(fixest)
soy_yield_spline = feols(log(Yield..Tonnes.Hectare.) ~ x + V2 + year + I(year^2) + erarain + I(erarain^2)   | farm , cluster = c( "year","farm"), temp4)
summary(soy_yield_spline)
results<- etable(soy_yield_spline)
write.csv(results,"C:/Users/Shakila Vyas/OneDrive - Wageningen University & Research/PhD-WUR-Insurance/CSA/ERA5_results_eq_knot3_returns.csv")

#####bootstrapping
set.seed(123)
marginal_temperatures_M1 <- rcspline.eval(seq(15,42,0.225),knots=temp_knot_df, inclx=T)

# Run 1000 models to derive uncertainty in estimates
spline_fitted_M1 <- matrix(NA,nrow=1000, ncol=nrow(marginal_temperatures_M1))

for (l in 1: 1000){
  # Sample by region times year
  data_temp <- temp4[which(temp4$reg_year %in% sample(unique(temp4$reg_year),replace=T)),]
  
  # Run regression with random subsample (data_temp), adjust variables to knots
  fit_boot  <- feols(Yield..Tonnes.Hectare. ~ x + V2  + year + I(year^2) + erarain + I(erarain^2)  | farm , data_temp)
  
  #Save marginal effect of temperatures,adjust variables to knots 
  spline_fitted_M1[l,]<-as.vector((marginal_temperatures_M1  %*% coef(fit_boot)[1:2])) 
  
  # Show progress in %
  print(l/1000 *100)
  rm(fit_boot,data_temp)
}

# Load the packages
library(fixest)
library(ggplot2)
library(gridExtra)
library(robustbase)
library(egg)
library(matrixStats)
library(tidyverse)

#Change structure
spline_sub_melt_M1 <- melt(spline_fitted_M1)
# Get median response for each column (= a temperature from seq(20,40,0.225))
colmedians_spline_M1 <- colMedians(spline_fitted_M1)
# Get distance from median response for each value
dist_colmedian_M1 <- matrix(NA, nrow=nrow(spline_fitted_M1),ncol=ncol(spline_fitted_M1))

for(l in 1:ncol(spline_fitted_M1)){
  dist_colmedian_M1[,l] <- spline_fitted_M1[,l] - colmedians_spline_M1[l]
}
# Normalize distance to median
dist_colmedian_M1_normalized <- matrix(NA, nrow=nrow(spline_fitted_M1),ncol=ncol(spline_fitted_M1))

for(l in 1:ncol(spline_fitted_M1)){
  dist_colmedian_M1_normalized[,l]<-abs(dist_colmedian_M1[,l]/max(abs(dist_colmedian_M1[,l])))
}

# Change structure
dist_colmedian_M1_normalized_melted <- melt(dist_colmedian_M1_normalized)

# Last preparation before plotting
spline_merged_M1 <- merge(spline_sub_melt_M1,dist_colmedian_M1_normalized_melted,by=c("Var1","Var2"))
spline_merged_M1$value.y_minus1 <- 1-spline_merged_M1$value.y

temp1 <- aggregate(spline_merged_M1$value.y_minus1, list(spline_merged_M1$Var1), median, na.rm=T) 
temp2 <- aggregate(spline_merged_M1$value.x, list(spline_merged_M1$Var2), median, na.rm=T)

temp3 <- aggregate(spline_merged_M1$value.x, list(spline_merged_M1$Var2), quantile, probs=0.025,na.rm=T) 
temp4 <- aggregate(spline_merged_M1$value.x, list(spline_merged_M1$Var2), quantile, probs=0.975,na.rm=T)

spline_merged_M1 <- merge(spline_merged_M1, temp1, by.x=c("Var1"), by.y=c("Group.1"))
spline_merged_M1 <- merge(spline_merged_M1, temp2, by.x=c("Var2"), by.y=c("Group.1"))
colnames(spline_merged_M1)[7]<-"average_effect"

spline_merged_M1<-merge(spline_merged_M1, temp3, by.x=c("Var2"), by.y=c("Group.1"))
colnames(spline_merged_M1)[8]<-"lower_bound"

spline_merged_M1<-merge(spline_merged_M1, temp4, by.x=c("Var2"), by.y=c("Group.1"))
colnames(spline_merged_M1)[9]<-"upper_bound"

temp5 <- cbind(seq(15,42,0.225),seq(1:length(seq(15,42,0.225))))
colnames(temp5) <- c("Temp_bin", "Var2")
spline_merged_M1 <-merge(spline_merged_M1, temp5, by=c("Var2"))

# Plot: hourly temperature effect
model_1_s <- ggplot()+ ggtitle("Hourly temperature effects: South") + 
  theme(panel.grid.major = element_blank(),
        plot.title = element_text(family="Times New Roman", size=20, colour = "grey25", hjust=0.5),
        axis.title = element_text(family="Times New Roman", size=16, colour = "grey25"),
        axis.text = element_text(family="Times New Roman", size=16),
        panel.grid.minor = element_blank(),
        panel.background = element_blank(),
        axis.line = element_line(colour = "black"),
        legend.position="none")+
  xlab("Temperature (C)") + ylab("Log yield response (Kg/ha)")+
  scale_x_continuous(breaks = seq(10,50, by=5), limits = c(10,50))+
  scale_y_continuous(breaks = seq(-.02,.02, by=.004), limits = c(-.02,.02))+
  geom_hline(yintercept=c(seq(-.02,.02,by=0.004)), colour="gray")+
  geom_vline(xintercept=temp_knot_df, linetype="dotted", colour="gray")+
  geom_line(data=spline_merged_M1, aes(y=value.x+0.001857344, x=Temp_bin, group=Var1, alpha=x.x^200/200), size=.01, colour="brown1")+
  geom_line(data=spline_merged_M1, aes(y=average_effect+0.001857344, x=Temp_bin), alpha=0.85, size=.75, colour="brown4")+
  geom_line(data=spline_merged_M1, aes(y=lower_bound+0.001857344, x=Temp_bin)   , alpha=0.85, size=.75, colour="brown4")+       
  geom_line(data=spline_merged_M1, aes(y=upper_bound+0.001857344, x=Temp_bin)   , alpha=0.85, size=.75, colour="brown4")

model_1_s

ggsave("C:/Users/Shakila Vyas/OneDrive - Wageningen University & Research/PhD-WUR-Insurance/CSA/County analysis/Results/soydis_era_bestfit_south_cen.png", width = 20, height = 20, units = "cm")

############################################################WHEAT#############################################################
######hourly reg-WHEAT ERA
library(raster)
library(dplyr)
library(data.table)
library(reshape2)
library(Hmisc)
library(lubridate)
library(plyr)
library(esmisc)
library(sandwich)
library(MASS)
library(e1071)
library(fixest)

data<- read.csv("C:/Users/Shakila Vyas/OneDrive - Wageningen University & Research/PhD-WUR-Insurance/CSA/County analysis/Dis yield_All India/Wheat_yields_rev.csv")
head(data)
#remove less than 3 years data
data1<- add_count(data, Dist_code)
regdata_wheat<- subset(data1,n>=3,select = Crop.Production.Statistics:n)
head(regdata_wheat)
summary(regdata_wheat)

##preparing data
hourly<- read.csv("C:/Users/Shakila Vyas/OneDrive - Wageningen University & Research/PhD-WUR-Insurance/CSA/County analysis/Weather data/ERA5/Winter/hourlytp15era.csv")
#need this format-colnames(temp_wheat_exposure2) <- c("Farm","Date","Hour", "Temperature")
as.data.frame(hourly)
library(reshape2)
long <- reshape2::melt(hourly, id.vars = c("farm"))
long$year<- 2015

hourly1<- read.csv("C:/Users/Shakila Vyas/OneDrive - Wageningen University & Research/PhD-WUR-Insurance/CSA/County analysis/Weather data/ERA5/Winter/hourlytp16era.csv")
#need this format-colnames(temp_wheat_exposure2) <- c("Farm","Date","Hour", "Temperature")
as.data.frame(hourly1)
long1 <- reshape2::melt(hourly1, id.vars = c("farm"))
long1$year<- 2016

hourly2<- read.csv("C:/Users/Shakila Vyas/OneDrive - Wageningen University & Research/PhD-WUR-Insurance/CSA/County analysis/Weather data/ERA5/Winter/hourlytp17era.csv")
#need this format-colnames(temp_wheat_exposure2) <- c("Farm","Date","Hour", "Temperature")
as.data.frame(hourly2)
long2 <- reshape2::melt(hourly2, id.vars = c("farm"))
long2$year<- 2017

hourly3<- read.csv("C:/Users/Shakila Vyas/OneDrive - Wageningen University & Research/PhD-WUR-Insurance/CSA/County analysis/Weather data/ERA5/Winter/hourlytp18era.csv")
#need this format-colnames(temp_wheat_exposure2) <- c("Farm","Date","Hour", "Temperature")
as.data.frame(hourly3)
long3 <- reshape2::melt(hourly3, id.vars = c("farm"))
long3$year<- 2018

hourly4<- read.csv("C:/Users/Shakila Vyas/OneDrive - Wageningen University & Research/PhD-WUR-Insurance/CSA/County analysis/Weather data/ERA5/Winter/hourlytp19era.csv")
#need this format-colnames(temp_wheat_exposure2) <- c("Farm","Date","Hour", "Temperature")
as.data.frame(hourly4)
long4 <- reshape2::melt(hourly4, id.vars = c("farm"))
long4$year<- 2019

hourly5<- read.csv("C:/Users/Shakila Vyas/OneDrive - Wageningen University & Research/PhD-WUR-Insurance/CSA/County analysis/Weather data/ERA5/Winter/hourlytp20era.csv")
#need this format-colnames(temp_wheat_exposure2) <- c("Farm","Date","Hour", "Temperature")
as.data.frame(hourly5)
long5 <- reshape2::melt(hourly5, id.vars = c("farm"))
long5$year<- 2020

temp<- rbind(long,long1,long2,long3,long4,long5)
range(temp$value)

####outlier removal
hist(regdata_wheat$Yield..Tonnes.Hectare.)
range(regdata_wheat$Yield..Tonnes.Hectare.)

quantile(regdata_wheat$Yield..Tonnes.Hectare.,probs= c(.99))
regdata_wheat1<- subset(regdata_wheat,Yield..Tonnes.Hectare.<quantile(regdata_wheat$Yield..Tonnes.Hectare.,probs= c(.99)),select = Crop.Production.Statistics:n )

#rename temp data
names(temp)[3] <- "temperature"
as.numeric(temp$temperature)
min(temp$temperature)
max(temp$temperature)
range(temp$temperature)
summary(temp$temperature)
hist(temp$temperature,main="Hourly temperature district-Winter")
range(temp$temperature)
temp <- na.omit(temp)
summary(temp$temperature)

# Histogram of temperature exposure-for a random farm
temperature_wheat <- ggplot(temp, aes(x=temperature)) + 
  theme(panel.grid.major = element_blank(),
        plot.title = element_text(family="Times New Roman", size=12, colour = "grey25", hjust=0.5),
        axis.title = element_text(family="Times New Roman", size=10, colour = "grey25"),
        axis.text = element_text(family="Times New Roman", size=10),
        panel.grid.minor = element_blank(),
        panel.background = element_blank(),
        axis.line = element_line(colour = "black"),
        legend.position="none")+
  ylab("Total exposure  (no. of hours)")+ xlab("Temperature (°C)")+
  geom_histogram(binwidth=1, color="black", fill="gray41")+
  scale_x_continuous(breaks = seq(0,45, by=5), limits = c(0,45))

ggsave(temperature_wheat, filename=paste("C:/Users/Shakila Vyas/OneDrive - Wageningen University & Research/PhD-WUR-Insurance/CSA/County analysis/Results/EraWheathist_county.png"), 
       dpi=300, height=13, width=18, units="cm")

#####################model 1: equal spaced 3 knots
temp_knot_df <- c(min(temp$temperature) + ((max(temp$temperature)-min(temp$temperature)) / 4),
                  min(temp$temperature) + 2* ((max(temp$temperature)-min(temp$temperature)) / 4),
                  min(temp$temperature) + 3* ((max(temp$temperature)-min(temp$temperature)) / 4))

# New TS for temperature support of interest- .225 to get the nice plot with small intervals
temp1 <- as.matrix(Hmisc::rcspline.eval(temp$temperature,knots=temp_knot_df,inclx=T))
temp2 <- cbind(temp,temp1)

names(temp2)
head(temp1)

#adjust variables depending on knots
temp3<-temp2 %>%
  group_by(farm,year)%>%
  summarise_at(vars(x,V2),sum)

#match names with temp
names(regdata_wheat1)
colnames(regdata_wheat1)[colnames(regdata_wheat) == "Year.1"] <- "year"
colnames(regdata_wheat1)[colnames(regdata_wheat) == "Dist_code"] <- "farm"
names(regdata_wheat1)

#join both  data
temp4 <- join(regdata_wheat1,temp3, by=c("farm", "year"))
names(temp4)
temp4$reg_year<- as.numeric(as.factor(temp4$District))*temp4$year

library(fixest)
wheat_yield_spline = feols(log(Yield..Tonnes.Hectare.) ~ x + V2 + year + I(year^2) + erarain + I(erarain^2)   | farm , cluster = c( "year","farm"), temp4)
summary(wheat_yield_spline)

results<- etable(soy_yield_spline)
write.csv(results,"C:/Users/Shakila Vyas/OneDrive - Wageningen University & Research/PhD-WUR-Insurance/CSA/ERA5_results_eq_knot3_returns.csv")

#####bootstrapping
set.seed(123)
marginal_temperatures_M1 <- rcspline.eval(seq(0,43,0.225),knots=temp_knot_df, inclx=T)

# Run 1000 models to derive uncertainty in estimates
spline_fitted_M1 <- matrix(NA,nrow=1000, ncol=nrow(marginal_temperatures_M1))

for (l in 1: 1000){
  # Sample by region times year
  data_temp <- temp4[which(temp4$reg_year %in% sample(unique(temp4$reg_year),replace=T)),]
  
  # Run regression with random subsample (data_temp), adjust variables to knots
  fit_boot  <- feols(Yield..Tonnes.Hectare. ~ x + V2  + year + I(year^2) + erarain + I(erarain^2)  | farm , data_temp)
  
  #Save marginal effect of temperatures,adjust variables to knots 
  spline_fitted_M1[l,]<-as.vector((marginal_temperatures_M1  %*% coef(fit_boot)[1:2])) 
  
  # Show progress in %
  print(l/1000 *100)
  rm(fit_boot,data_temp)
}

# Load the packages
library(fixest)
library(ggplot2)
library(gridExtra)
library(robustbase)
library(egg)
library(matrixStats)
library(tidyverse)

#Change structure
spline_sub_melt_M1 <- melt(spline_fitted_M1)
# Get median response for each column (= a temperature from seq(20,40,0.225))
colmedians_spline_M1 <- colMedians(spline_fitted_M1)
# Get distance from median response for each value
dist_colmedian_M1 <- matrix(NA, nrow=nrow(spline_fitted_M1),ncol=ncol(spline_fitted_M1))

for(l in 1:ncol(spline_fitted_M1)){
  dist_colmedian_M1[,l] <- spline_fitted_M1[,l] - colmedians_spline_M1[l]
}
# Normalize distance to median
dist_colmedian_M1_normalized <- matrix(NA, nrow=nrow(spline_fitted_M1),ncol=ncol(spline_fitted_M1))

for(l in 1:ncol(spline_fitted_M1)){
  dist_colmedian_M1_normalized[,l]<-abs(dist_colmedian_M1[,l]/max(abs(dist_colmedian_M1[,l])))
}

# Change structure
dist_colmedian_M1_normalized_melted <- melt(dist_colmedian_M1_normalized)

# Last preparation before plotting
spline_merged_M1 <- merge(spline_sub_melt_M1,dist_colmedian_M1_normalized_melted,by=c("Var1","Var2"))
spline_merged_M1$value.y_minus1 <- 1-spline_merged_M1$value.y

temp1 <- aggregate(spline_merged_M1$value.y_minus1, list(spline_merged_M1$Var1), median, na.rm=T) 
temp2 <- aggregate(spline_merged_M1$value.x, list(spline_merged_M1$Var2), median, na.rm=T)

temp3 <- aggregate(spline_merged_M1$value.x, list(spline_merged_M1$Var2), quantile, probs=0.025,na.rm=T) 
temp4 <- aggregate(spline_merged_M1$value.x, list(spline_merged_M1$Var2), quantile, probs=0.975,na.rm=T)

spline_merged_M1 <- merge(spline_merged_M1, temp1, by.x=c("Var1"), by.y=c("Group.1"))
spline_merged_M1 <- merge(spline_merged_M1, temp2, by.x=c("Var2"), by.y=c("Group.1"))
colnames(spline_merged_M1)[7]<-"average_effect"

spline_merged_M1<-merge(spline_merged_M1, temp3, by.x=c("Var2"), by.y=c("Group.1"))
colnames(spline_merged_M1)[8]<-"lower_bound"

spline_merged_M1<-merge(spline_merged_M1, temp4, by.x=c("Var2"), by.y=c("Group.1"))
colnames(spline_merged_M1)[9]<-"upper_bound"

temp5 <- cbind(seq(0,43,0.225),seq(1:length(seq(0,43,0.225))))
colnames(temp5) <- c("Temp_bin", "Var2")
spline_merged_M1 <-merge(spline_merged_M1, temp5, by=c("Var2"))

# Plot: hourly temperature effect
model_1 <- ggplot()+ ggtitle("Hourly temperature effects: Equal Knots 3") + 
  theme(panel.grid.major = element_blank(),
        plot.title = element_text(family="Times New Roman", size=20, colour = "grey25", hjust=0.5),
        axis.title = element_text(family="Times New Roman", size=16, colour = "grey25"),
        axis.text = element_text(family="Times New Roman", size=16),
        panel.grid.minor = element_blank(),
        panel.background = element_blank(),
        axis.line = element_line(colour = "black"),
        legend.position="none")+
  xlab("Temperature (°C)") + ylab("Log yield response (Kg/ha)")+
  scale_x_continuous(breaks = seq(0,43, by=5), limits = c(0,43))+
  scale_y_continuous(breaks = seq(-.005,.005, by=.001), limits = c(-.005,.005))+
  geom_hline(yintercept=c(seq(-.05,.05,by=0.001)), colour="gray")+
  geom_vline(xintercept=temp_knot_df, linetype="dotted", colour="gray")+
  geom_line(data=spline_merged_M1, aes(y=value.x-0.0002807591, x=Temp_bin, group=Var1, alpha=x.x^200/200), size=.01, colour="darkorange1")+
  geom_line(data=spline_merged_M1, aes(y=average_effect-0.0002807591, x=Temp_bin), alpha=0.85, size=.75, colour="darkorange4")+
  geom_line(data=spline_merged_M1, aes(y=lower_bound-0.0002807591, x=Temp_bin)   , alpha=0.85, size=.75, colour="darkorange4")+       
  geom_line(data=spline_merged_M1, aes(y=upper_bound-0.0002807591, x=Temp_bin)   , alpha=0.85, size=.75, colour="darkorange4")

model_1

ggsave("C:/Users/Shakila Vyas/OneDrive - Wageningen University & Research/PhD-WUR-Insurance/CSA/County analysis/Results/wheatdis_era_5degree_dis_cen.png", width = 20, height = 20, units = "cm")

#####################model 3: quantile 3 knots
# 10%, 50%, 90% quantile
temp_knot_df <- c(quantile(temp$temperature,0.1, type=1),
                  quantile(temp$temperature,0.5, type=1),
                  quantile(temp$temperature,0.9, type=1))
temp_knot_df
summary(temp$temperature)
# New TS for temperature support of interest- .225 to get the nice plot with small intervals
temp1 <- as.matrix(Hmisc::rcspline.eval(temp$temperature,knots=temp_knot_df,inclx=T))
temp2 <- cbind(temp,temp1)

names(temp2)
head(temp1)

#adjust variables depending on knots
temp3<-temp2 %>%
  group_by(farm,year)%>%
  summarise_at(vars(x,V2),sum)

#match names with temp
names(regdata_wheat1)
colnames(regdata_wheat1)[colnames(regdata_wheat) == "Year.1"] <- "year"
colnames(regdata_wheat1)[colnames(regdata_wheat) == "Dist_code"] <- "farm"
names(regdata_wheat1)

#join both  data
temp4 <- join(regdata_wheat1,temp3, by=c("farm", "year"))
names(temp4)
temp4$reg_year<- as.numeric(as.factor(temp4$District))*temp4$year


#####bootstrapping
set.seed(123)
marginal_temperatures_M1 <- rcspline.eval(seq(0,43,0.225),knots=temp_knot_df, inclx=T)

# Run 1000 models to derive uncertainty in estimates
spline_fitted_M1 <- matrix(NA,nrow=1000, ncol=nrow(marginal_temperatures_M1))

for (l in 1: 1000){
  # Sample by region times year
  data_temp <- temp4[which(temp4$reg_year %in% sample(unique(temp4$reg_year),replace=T)),]
  
  # Run regression with random subsample (data_temp), adjust variables to knots
  fit_boot  <- feols(log(Yield..Tonnes.Hectare.) ~ x + V2  + year + I(year^2) + erarain + I(erarain^2)  | farm , data_temp)
  
  #Save marginal effect of temperatures,adjust variables to knots 
  spline_fitted_M1[l,]<-as.vector((marginal_temperatures_M1  %*% coef(fit_boot)[1:2])) 
  
  # Show progress in %
  print(l/1000 *100)
  rm(fit_boot,data_temp)
}

# Load the packages
#Change structure
spline_sub_melt_M1 <- melt(spline_fitted_M1)
# Get median response for each column (= a temperature from seq(20,40,0.225))
colmedians_spline_M1 <- colMedians(spline_fitted_M1)
# Get distance from median response for each value
dist_colmedian_M1 <- matrix(NA, nrow=nrow(spline_fitted_M1),ncol=ncol(spline_fitted_M1))

for(l in 1:ncol(spline_fitted_M1)){
  dist_colmedian_M1[,l] <- spline_fitted_M1[,l] - colmedians_spline_M1[l]
}
# Normalize distance to median
dist_colmedian_M1_normalized <- matrix(NA, nrow=nrow(spline_fitted_M1),ncol=ncol(spline_fitted_M1))

for(l in 1:ncol(spline_fitted_M1)){
  dist_colmedian_M1_normalized[,l]<-abs(dist_colmedian_M1[,l]/max(abs(dist_colmedian_M1[,l])))
}

# Change structure
dist_colmedian_M1_normalized_melted <- melt(dist_colmedian_M1_normalized)

# Last preparation before plotting
spline_merged_M1 <- merge(spline_sub_melt_M1,dist_colmedian_M1_normalized_melted,by=c("Var1","Var2"))
spline_merged_M1$value.y_minus1 <- 1-spline_merged_M1$value.y

temp1 <- aggregate(spline_merged_M1$value.y_minus1, list(spline_merged_M1$Var1), median, na.rm=T) 
temp2 <- aggregate(spline_merged_M1$value.x, list(spline_merged_M1$Var2), median, na.rm=T)

temp3 <- aggregate(spline_merged_M1$value.x, list(spline_merged_M1$Var2), quantile, probs=0.025,na.rm=T) 
temp4 <- aggregate(spline_merged_M1$value.x, list(spline_merged_M1$Var2), quantile, probs=0.975,na.rm=T)

spline_merged_M1 <- merge(spline_merged_M1, temp1, by.x=c("Var1"), by.y=c("Group.1"))
spline_merged_M1 <- merge(spline_merged_M1, temp2, by.x=c("Var2"), by.y=c("Group.1"))
colnames(spline_merged_M1)[7]<-"average_effect"

spline_merged_M1<-merge(spline_merged_M1, temp3, by.x=c("Var2"), by.y=c("Group.1"))
colnames(spline_merged_M1)[8]<-"lower_bound"

spline_merged_M1<-merge(spline_merged_M1, temp4, by.x=c("Var2"), by.y=c("Group.1"))
colnames(spline_merged_M1)[9]<-"upper_bound"

temp5 <- cbind(seq(0,43,0.225),seq(1:length(seq(0,43,0.225))))
colnames(temp5) <- c("Temp_bin", "Var2")
spline_merged_M1 <-merge(spline_merged_M1, temp5, by=c("Var2"))

# Plot: hourly temperature effect
model_1 <- ggplot()+ ggtitle("Hourly temperature effects: Quantile Knots 3") + 
  theme(panel.grid.major = element_blank(),
        plot.title = element_text(family="Times New Roman", size=20, colour = "grey25", hjust=0.5),
        axis.title = element_text(family="Times New Roman", size=16, colour = "grey25"),
        axis.text = element_text(family="Times New Roman", size=16),
        panel.grid.minor = element_blank(),
        panel.background = element_blank(),
        axis.line = element_line(colour = "black"),
        legend.position="none")+
  xlab("Temperature (°C)") + ylab("Log yield response (Kg/ha)")+
  scale_x_continuous(breaks = seq(0,43, by=5), limits = c(0,43))+
  scale_y_continuous(breaks = seq(-.004,.004, by=.001), limits = c(-.004,.004))+
  geom_hline(yintercept=c(seq(-.004,.004,by=0.001)), colour="gray")+
  geom_vline(xintercept=temp_knot_df, linetype="dotted", colour="gray")+
  geom_line(data=spline_merged_M1, aes(y=value.x+0.0000008294489, x=Temp_bin, group=Var1, alpha=x.x^200/200), size=.01, colour="darkorange1")+
  geom_line(data=spline_merged_M1, aes(y=average_effect+0.0000008294489, x=Temp_bin), alpha=0.85, size=.75, colour="darkorange4")+
  geom_line(data=spline_merged_M1, aes(y=lower_bound+0.0000008294489, x=Temp_bin)   , alpha=0.85, size=.75, colour="darkorange4")+       
  geom_line(data=spline_merged_M1, aes(y=upper_bound+0.0000008294489, x=Temp_bin)   , alpha=0.85, size=.75, colour="darkorange4")

model_1
ggsave("C:/Users/Shakila Vyas/OneDrive - Wageningen University & Research/PhD-WUR-Insurance/CSA/County analysis/Results/wheatdis_era_qq3_dis_cen.png", width = 20, height = 20, units = "cm")

#####################model 2: equal spaced 4 knots, not used in the main results
temp_knot_df <- c(min(temp$temperature) + ((max(temp$temperature)-min(temp$temperature)) / 5),
                  min(temp$temperature) + 2* ((max(temp$temperature)-min(temp$temperature)) / 5),
                  min(temp$temperature) + 3* ((max(temp$temperature)-min(temp$temperature)) / 5),
                  min(temp$temperature) + 4* ((max(temp$temperature)-min(temp$temperature)) / 5))
temp_knot_df
# New TS for temperature support of interest- .225 to get the nice plot with small intervals
temp1 <- as.matrix(Hmisc::rcspline.eval(temp$temperature,knots=temp_knot_df,inclx=T))
temp2 <- cbind(temp,temp1)

names(temp2)
head(temp1)

#adjust variables depending on knots
temp3<-temp2 %>%
  group_by(farm,year)%>%
  summarise_at(vars(x,V2,V3),sum)

#match names with temp
names(regdata_wheat1)
colnames(regdata_wheat1)[colnames(regdata_wheat) == "Year.1"] <- "year"
colnames(regdata_wheat1)[colnames(regdata_wheat) == "Dist_code"] <- "farm"
names(regdata_wheat1)

#join both  data
temp4 <- join(regdata_wheat1,temp3, by=c("farm", "year"))

names(temp4)
temp4$reg_year<- as.numeric(as.factor(temp4$District))*temp4$year

#####bootstrapping
set.seed(123)
marginal_temperatures_M1 <- rcspline.eval(seq(0,43,0.225),knots=temp_knot_df, inclx=T)

# Run 1000 models to derive uncertainty in estimates
spline_fitted_M1 <- matrix(NA,nrow=1000, ncol=nrow(marginal_temperatures_M1))

for (l in 1: 1000){
  # Sample by region times year
  data_temp <- temp4[which(temp4$reg_year %in% sample(unique(temp4$reg_year),replace=T)),]
  
  # Run regression with random subsample (data_temp), adjust variables to knots
  fit_boot  <- feols(log(Yield..Tonnes.Hectare.) ~ x + V2 + V3 + year + I(year^2) + erarain + I(erarain^2)  | farm , data_temp)
  
  #Save marginal effect of temperatures,adjust variables to knots 
  spline_fitted_M1[l,]<-as.vector((marginal_temperatures_M1  %*% coef(fit_boot)[1:3])) 
  
  # Show progress in %
  print(l/1000 *100)
  rm(fit_boot,data_temp)
}

# Load the packages
library(fixest)
library(ggplot2)
library(gridExtra)
library(robustbase)
library(egg)
library(matrixStats)
library(tidyverse)

#Change structure
spline_sub_melt_M1 <- melt(spline_fitted_M1)
# Get median response for each column (= a temperature from seq(20,40,0.225))
colmedians_spline_M1 <- colMedians(spline_fitted_M1)
# Get distance from median response for each value
dist_colmedian_M1 <- matrix(NA, nrow=nrow(spline_fitted_M1),ncol=ncol(spline_fitted_M1))

for(l in 1:ncol(spline_fitted_M1)){
  dist_colmedian_M1[,l] <- spline_fitted_M1[,l] - colmedians_spline_M1[l]
}
# Normalize distance to median
dist_colmedian_M1_normalized <- matrix(NA, nrow=nrow(spline_fitted_M1),ncol=ncol(spline_fitted_M1))

for(l in 1:ncol(spline_fitted_M1)){
  dist_colmedian_M1_normalized[,l]<-abs(dist_colmedian_M1[,l]/max(abs(dist_colmedian_M1[,l])))
}

# Change structure
dist_colmedian_M1_normalized_melted <- melt(dist_colmedian_M1_normalized)

# Last preparation before plotting
spline_merged_M1 <- merge(spline_sub_melt_M1,dist_colmedian_M1_normalized_melted,by=c("Var1","Var2"))
spline_merged_M1$value.y_minus1 <- 1-spline_merged_M1$value.y

temp1 <- aggregate(spline_merged_M1$value.y_minus1, list(spline_merged_M1$Var1), median, na.rm=T) 
temp2 <- aggregate(spline_merged_M1$value.x, list(spline_merged_M1$Var2), median, na.rm=T)

temp3 <- aggregate(spline_merged_M1$value.x, list(spline_merged_M1$Var2), quantile, probs=0.025,na.rm=T) 
temp4 <- aggregate(spline_merged_M1$value.x, list(spline_merged_M1$Var2), quantile, probs=0.975,na.rm=T)

spline_merged_M1 <- merge(spline_merged_M1, temp1, by.x=c("Var1"), by.y=c("Group.1"))
spline_merged_M1 <- merge(spline_merged_M1, temp2, by.x=c("Var2"), by.y=c("Group.1"))
colnames(spline_merged_M1)[7]<-"average_effect"

spline_merged_M1<-merge(spline_merged_M1, temp3, by.x=c("Var2"), by.y=c("Group.1"))
colnames(spline_merged_M1)[8]<-"lower_bound"

spline_merged_M1<-merge(spline_merged_M1, temp4, by.x=c("Var2"), by.y=c("Group.1"))
colnames(spline_merged_M1)[9]<-"upper_bound"

temp5 <- cbind(seq(0,43,0.225),seq(1:length(seq(0,43,0.225))))
colnames(temp5) <- c("Temp_bin", "Var2")
spline_merged_M1 <-merge(spline_merged_M1, temp5, by=c("Var2"))

# Plot: hourly temperature effect
model_2 <- ggplot()+ ggtitle("Hourly temperature effects: Equal knots 4") + 
  theme(panel.grid.major = element_blank(),
        plot.title = element_text(family="Times New Roman", size=20, colour = "grey25", hjust=0.5),
        axis.title = element_text(family="Times New Roman", size=16, colour = "grey25"),
        axis.text = element_text(family="Times New Roman", size=16),
        panel.grid.minor = element_blank(),
        panel.background = element_blank(),
        axis.line = element_line(colour = "black"),
        legend.position="none")+
  xlab("Temperature (°C)") + ylab("Log yield response (Kg/ha)")+
  scale_x_continuous(breaks = seq(0,43, by=5), limits = c(0,43))+
  scale_y_continuous(breaks = seq(-.004,.004, by=.001), limits = c(-.004,.004))+
  geom_hline(yintercept=c(seq(-.004,.004,by=0.001)), colour="gray")+
  geom_vline(xintercept=temp_knot_df, linetype="dotted", colour="gray")+
  geom_line(data=spline_merged_M1, aes(y=value.x+0.0002642821, x=Temp_bin, group=Var1, alpha=x.x^200/200), size=.01, colour="darkorange1")+
  geom_line(data=spline_merged_M1, aes(y=average_effect+0.0002642821, x=Temp_bin), alpha=0.85, size=.75, colour="darkorange4")+
  geom_line(data=spline_merged_M1, aes(y=lower_bound+0.0002642821, x=Temp_bin)   , alpha=0.85, size=.75, colour="darkorange4")+       
  geom_line(data=spline_merged_M1, aes(y=upper_bound+0.0002642821, x=Temp_bin)   , alpha=0.85, size=.75, colour="darkorange4")

model_2

ggsave("C:/Users/Shakila Vyas/OneDrive - Wageningen University & Research/PhD-WUR-Insurance/CSA/County analysis/Results/wheatdis_era_eq4_dis_cen.png", width = 20, height = 20, units = "cm")

#####################model 4: quantile 4 knots######, not used in the main results
temp_knot_df <- c(quantile(temp$temperature,0.2, type=1),
                  quantile(temp$temperature,0.4, type=1),
                  quantile(temp$temperature,0.6, type=1),
                  quantile(temp$temperature,0.8, type=1))

temp_knot_df

# New TS for temperature support of interest- .225 to get the nice plot with small intervals
temp1 <- as.matrix(Hmisc::rcspline.eval(temp$temperature,knots=temp_knot_df,inclx=T))
temp2 <- cbind(temp,temp1)

names(temp2)
head(temp1)

#adjust variables depending on knots
temp3<-temp2 %>%
  group_by(farm,year)%>%
  summarise_at(vars(x,V2,V3),sum)

#join both  data
temp4 <- join(regdata_wheat1,temp3, by=c("farm", "year"))

names(temp4)
temp4$reg_year<- as.numeric(as.factor(temp4$District))*temp4$year

#####bootstrapping
set.seed(123)
marginal_temperatures_M1 <- rcspline.eval(seq(0,43,0.225),knots=temp_knot_df, inclx=T)

# Run 1000 models to derive uncertainty in estimates
spline_fitted_M1 <- matrix(NA,nrow=1000, ncol=nrow(marginal_temperatures_M1))

for (l in 1: 1000){
  # Sample by region times year
  data_temp <- temp4[which(temp4$reg_year %in% sample(unique(temp4$reg_year),replace=T)),]
  
  # Run regression with random subsample (data_temp), adjust variables to knots
  fit_boot  <- feols(log(Yield..Tonnes.Hectare.) ~ x + V2 + V3 + year + I(year^2) + erarain + I(erarain^2)  | farm , data_temp)
  
  #Save marginal effect of temperatures,adjust variables to knots 
  spline_fitted_M1[l,]<-as.vector((marginal_temperatures_M1  %*% coef(fit_boot)[1:3])) 
  
  # Show progress in %
  print(l/1000 *100)
  rm(fit_boot,data_temp)
}

# Load the packages
library(fixest)
library(ggplot2)
library(gridExtra)
library(robustbase)
library(egg)
library(matrixStats)
library(tidyverse)

#Change structure
spline_sub_melt_M1 <- melt(spline_fitted_M1)
# Get median response for each column (= a temperature from seq(20,40,0.225))
colmedians_spline_M1 <- colMedians(spline_fitted_M1)
# Get distance from median response for each value
dist_colmedian_M1 <- matrix(NA, nrow=nrow(spline_fitted_M1),ncol=ncol(spline_fitted_M1))

for(l in 1:ncol(spline_fitted_M1)){
  dist_colmedian_M1[,l] <- spline_fitted_M1[,l] - colmedians_spline_M1[l]
}
# Normalize distance to median
dist_colmedian_M1_normalized <- matrix(NA, nrow=nrow(spline_fitted_M1),ncol=ncol(spline_fitted_M1))

for(l in 1:ncol(spline_fitted_M1)){
  dist_colmedian_M1_normalized[,l]<-abs(dist_colmedian_M1[,l]/max(abs(dist_colmedian_M1[,l])))
}

# Change structure
dist_colmedian_M1_normalized_melted <- melt(dist_colmedian_M1_normalized)

# Last preparation before plotting
spline_merged_M1 <- merge(spline_sub_melt_M1,dist_colmedian_M1_normalized_melted,by=c("Var1","Var2"))
spline_merged_M1$value.y_minus1 <- 1-spline_merged_M1$value.y

temp1 <- aggregate(spline_merged_M1$value.y_minus1, list(spline_merged_M1$Var1), median, na.rm=T) 
temp2 <- aggregate(spline_merged_M1$value.x, list(spline_merged_M1$Var2), median, na.rm=T)

temp3 <- aggregate(spline_merged_M1$value.x, list(spline_merged_M1$Var2), quantile, probs=0.025,na.rm=T) 
temp4 <- aggregate(spline_merged_M1$value.x, list(spline_merged_M1$Var2), quantile, probs=0.975,na.rm=T)

spline_merged_M1 <- merge(spline_merged_M1, temp1, by.x=c("Var1"), by.y=c("Group.1"))
spline_merged_M1 <- merge(spline_merged_M1, temp2, by.x=c("Var2"), by.y=c("Group.1"))
colnames(spline_merged_M1)[7]<-"average_effect"

spline_merged_M1<-merge(spline_merged_M1, temp3, by.x=c("Var2"), by.y=c("Group.1"))
colnames(spline_merged_M1)[8]<-"lower_bound"

spline_merged_M1<-merge(spline_merged_M1, temp4, by.x=c("Var2"), by.y=c("Group.1"))
colnames(spline_merged_M1)[9]<-"upper_bound"

temp5 <- cbind(seq(0,43,0.225),seq(1:length(seq(0,43,0.225))))
colnames(temp5) <- c("Temp_bin", "Var2")
spline_merged_M1 <-merge(spline_merged_M1, temp5, by=c("Var2"))

# Plot: hourly temperature effect
model_2 <- ggplot()+ ggtitle("Hourly temperature effects: Quantile knots 4") + 
  theme(panel.grid.major = element_blank(),
        plot.title = element_text(family="Times New Roman", size=20, colour = "grey25", hjust=0.5),
        axis.title = element_text(family="Times New Roman", size=16, colour = "grey25"),
        axis.text = element_text(family="Times New Roman", size=16),
        panel.grid.minor = element_blank(),
        panel.background = element_blank(),
        axis.line = element_line(colour = "black"),
        legend.position="none")+
  xlab("Temperature (°C)") + ylab("Log yield response (Kg/ha)")+
  scale_x_continuous(breaks = seq(0,43, by=5), limits = c(0,43))+
  scale_y_continuous(breaks = seq(-.004,.004, by=.001), limits = c(-.004,.004))+
  geom_hline(yintercept=c(seq(-.004,.004,by=0.001)), colour="gray")+
  geom_vline(xintercept=temp_knot_df, linetype="dotted", colour="gray")+
  geom_line(data=spline_merged_M1, aes(y=value.x+0.0002043188, x=Temp_bin, group=Var1, alpha=x.x^200/200), size=.01, colour="darkorange1")+
  geom_line(data=spline_merged_M1, aes(y=average_effect+0.0002043188, x=Temp_bin), alpha=0.85, size=.75, colour="darkorange4")+
  geom_line(data=spline_merged_M1, aes(y=lower_bound+0.0002043188, x=Temp_bin)   , alpha=0.85, size=.75, colour="darkorange4")+       
  geom_line(data=spline_merged_M1, aes(y=upper_bound+0.0002043188, x=Temp_bin)   , alpha=0.85, size=.75, colour="darkorange4")

model_2
ggsave("C:/Users/Shakila Vyas/OneDrive - Wageningen University & Research/PhD-WUR-Insurance/CSA/County analysis/Results/wheatdis_era_qq4_dis_cen.png", width = 20, height = 20, units = "cm")

################################placing knots at 5 degree intervals, not used in main results
hist(temp$temperature)
temp_knot_df <- c(10,20,30)

temp_knot_df

# New TS for temperature support of interest- .225 to get the nice plot with small intervals
temp1 <- as.matrix(Hmisc::rcspline.eval(temp$temperature,knots=temp_knot_df,inclx=T))
temp2 <- cbind(temp,temp1)

names(temp2)
head(temp1)

#adjust variables depending on knots
temp3<-temp2 %>%
  group_by(farm,year)%>%
  summarise_at(vars(x,V2),sum)

#join both  data
temp4 <- join(regdata_wheat1,temp3, by=c("farm", "year"))

names(temp4)
temp4$reg_year<- as.numeric(as.factor(temp4$District))*temp4$year

#####bootstrapping
set.seed(123)
marginal_temperatures_M1 <- rcspline.eval(seq(0,43,0.225),knots=temp_knot_df, inclx=T)

# Run 1000 models to derive uncertainty in estimates
spline_fitted_M1 <- matrix(NA,nrow=1000, ncol=nrow(marginal_temperatures_M1))

for (l in 1: 1000){
  # Sample by region times year
  data_temp <- temp4[which(temp4$reg_year %in% sample(unique(temp4$reg_year),replace=T)),]
  
  # Run regression with random subsample (data_temp), adjust variables to knots
  fit_boot  <- feols(log(Yield..Tonnes.Hectare.) ~ x + V2  + year + I(year^2) + erarain + I(erarain^2)  | farm , data_temp)
  
  #Save marginal effect of temperatures,adjust variables to knots 
  spline_fitted_M1[l,]<-as.vector((marginal_temperatures_M1  %*% coef(fit_boot)[1:2])) 
  
  # Show progress in %
  print(l/1000 *100)
  rm(fit_boot,data_temp)
}

# Load the packages
library(fixest)
library(ggplot2)
library(gridExtra)
library(robustbase)
library(egg)
library(matrixStats)
library(tidyverse)

#Change structure
spline_sub_melt_M1 <- melt(spline_fitted_M1)
# Get median response for each column (= a temperature from seq(20,40,0.225))
colmedians_spline_M1 <- colMedians(spline_fitted_M1)
# Get distance from median response for each value
dist_colmedian_M1 <- matrix(NA, nrow=nrow(spline_fitted_M1),ncol=ncol(spline_fitted_M1))

for(l in 1:ncol(spline_fitted_M1)){
  dist_colmedian_M1[,l] <- spline_fitted_M1[,l] - colmedians_spline_M1[l]
}
# Normalize distance to median
dist_colmedian_M1_normalized <- matrix(NA, nrow=nrow(spline_fitted_M1),ncol=ncol(spline_fitted_M1))

for(l in 1:ncol(spline_fitted_M1)){
  dist_colmedian_M1_normalized[,l]<-abs(dist_colmedian_M1[,l]/max(abs(dist_colmedian_M1[,l])))
}

# Change structure
dist_colmedian_M1_normalized_melted <- melt(dist_colmedian_M1_normalized)

# Last preparation before plotting
spline_merged_M1 <- merge(spline_sub_melt_M1,dist_colmedian_M1_normalized_melted,by=c("Var1","Var2"))
spline_merged_M1$value.y_minus1 <- 1-spline_merged_M1$value.y

temp1 <- aggregate(spline_merged_M1$value.y_minus1, list(spline_merged_M1$Var1), median, na.rm=T) 
temp2 <- aggregate(spline_merged_M1$value.x, list(spline_merged_M1$Var2), median, na.rm=T)

temp3 <- aggregate(spline_merged_M1$value.x, list(spline_merged_M1$Var2), quantile, probs=0.025,na.rm=T) 
temp4 <- aggregate(spline_merged_M1$value.x, list(spline_merged_M1$Var2), quantile, probs=0.975,na.rm=T)

spline_merged_M1 <- merge(spline_merged_M1, temp1, by.x=c("Var1"), by.y=c("Group.1"))
spline_merged_M1 <- merge(spline_merged_M1, temp2, by.x=c("Var2"), by.y=c("Group.1"))
colnames(spline_merged_M1)[7]<-"average_effect"

spline_merged_M1<-merge(spline_merged_M1, temp3, by.x=c("Var2"), by.y=c("Group.1"))
colnames(spline_merged_M1)[8]<-"lower_bound"

spline_merged_M1<-merge(spline_merged_M1, temp4, by.x=c("Var2"), by.y=c("Group.1"))
colnames(spline_merged_M1)[9]<-"upper_bound"

temp5 <- cbind(seq(0,43,0.225),seq(1:length(seq(0,43,0.225))))
colnames(temp5) <- c("Temp_bin", "Var2")
spline_merged_M1 <-merge(spline_merged_M1, temp5, by=c("Var2"))

# Plot: hourly temperature effect
model_4 <- ggplot()+ ggtitle("Hourly temperature effects: 5 degree knots ") + 
  theme(panel.grid.major = element_blank(),
        plot.title = element_text(family="Times New Roman", size=20, colour = "grey25", hjust=0.5),
        axis.title = element_text(family="Times New Roman", size=16, colour = "grey25"),
        axis.text = element_text(family="Times New Roman", size=16),
        panel.grid.minor = element_blank(),
        panel.background = element_blank(),
        axis.line = element_line(colour = "black"),
        legend.position="none")+
  xlab("Temperature (°C)") + ylab("Log yield response (Kg/ha)")+
  scale_x_continuous(breaks = seq(0,43, by=5), limits = c(0,43))+
  scale_y_continuous(breaks = seq(-.004,.004, by=.001), limits = c(-.004,.004))+
  geom_hline(yintercept=c(seq(-.004,.004,by=0.001)), colour="gray")+
  geom_vline(xintercept=temp_knot_df, linetype="dotted", colour="gray")+
  geom_line(data=spline_merged_M1, aes(y=value.x, x=Temp_bin, group=Var1, alpha=x.x^200/200), size=.01, colour="brown1")+
  geom_line(data=spline_merged_M1, aes(y=average_effect, x=Temp_bin), alpha=0.85, size=.75, colour="brown4")+
  geom_line(data=spline_merged_M1, aes(y=lower_bound, x=Temp_bin)   , alpha=0.85, size=.75, colour="brown4")+       
  geom_line(data=spline_merged_M1, aes(y=upper_bound, x=Temp_bin)   , alpha=0.85, size=.75, colour="brown4")

model_4
ggsave("C:/Users/Shakila Vyas/OneDrive - Wageningen University & Research/PhD-WUR-Insurance/CSA/County analysis/Results/wheatdis_IMD_5degree3_dis_cen.png", width = 20, height = 20, units = "cm")

################################placing knots at 5 degree intervals, not used in main results
hist(temp$temperature)
temp_knot_df <- c(20,25,30)

temp_knot_df

# New TS for temperature support of interest- .225 to get the nice plot with small intervals
temp1 <- as.matrix(Hmisc::rcspline.eval(temp$temperature,knots=temp_knot_df,inclx=T))
temp2 <- cbind(temp,temp1)

names(temp2)
head(temp1)

#adjust variables depending on knots
temp3<-temp2 %>%
  group_by(farm,year)%>%
  summarise_at(vars(x,V2),sum)

#match names with temp
names(regdata_soy1)
colnames(regdata_soy1)[colnames(regdata_soy1) == "Year"] <- "year"
colnames(regdata_soy1)[colnames(regdata_soy1) == "Dis_codename"] <- "farm"
names(regdata_soy1)

#join both  data
temp4 <- join(regdata_soy1,temp3, by=c("farm", "year"))

names(temp4)
temp4$reg_year<- as.numeric(as.factor(temp4$District))*temp4$year

#####bootstrapping
set.seed(123)
marginal_temperatures_M1 <- rcspline.eval(seq(10,50,0.225),knots=temp_knot_df, inclx=T)

# Run 1000 models to derive uncertainty in estimates
spline_fitted_M1 <- matrix(NA,nrow=1000, ncol=nrow(marginal_temperatures_M1))

for (l in 1: 1000){
  # Sample by region times year
  data_temp <- temp4[which(temp4$reg_year %in% sample(unique(temp4$reg_year),replace=T)),]
  
  # Run regression with random subsample (data_temp), adjust variables to knots
  fit_boot  <- feols(log(Yield..Tonnes.Hectare.) ~ x + V2  + year + I(year^2) + erarain + I(erarain^2)  | farm , data_temp)
  
  #Save marginal effect of temperatures,adjust variables to knots 
  spline_fitted_M1[l,]<-as.vector((marginal_temperatures_M1  %*% coef(fit_boot)[1:2])) 
  
  # Show progress in %
  print(l/1000 *100)
  rm(fit_boot,data_temp)
}

# Load the packages
library(fixest)
library(ggplot2)
library(gridExtra)
library(robustbase)
library(egg)
library(matrixStats)
library(tidyverse)

#Change structure
spline_sub_melt_M1 <- melt(spline_fitted_M1)
# Get median response for each column (= a temperature from seq(20,40,0.225))
colmedians_spline_M1 <- colMedians(spline_fitted_M1)
# Get distance from median response for each value
dist_colmedian_M1 <- matrix(NA, nrow=nrow(spline_fitted_M1),ncol=ncol(spline_fitted_M1))

for(l in 1:ncol(spline_fitted_M1)){
  dist_colmedian_M1[,l] <- spline_fitted_M1[,l] - colmedians_spline_M1[l]
}
# Normalize distance to median
dist_colmedian_M1_normalized <- matrix(NA, nrow=nrow(spline_fitted_M1),ncol=ncol(spline_fitted_M1))

for(l in 1:ncol(spline_fitted_M1)){
  dist_colmedian_M1_normalized[,l]<-abs(dist_colmedian_M1[,l]/max(abs(dist_colmedian_M1[,l])))
}

# Change structure
dist_colmedian_M1_normalized_melted <- melt(dist_colmedian_M1_normalized)

# Last preparation before plotting
spline_merged_M1 <- merge(spline_sub_melt_M1,dist_colmedian_M1_normalized_melted,by=c("Var1","Var2"))
spline_merged_M1$value.y_minus1 <- 1-spline_merged_M1$value.y

temp1 <- aggregate(spline_merged_M1$value.y_minus1, list(spline_merged_M1$Var1), median, na.rm=T) 
temp2 <- aggregate(spline_merged_M1$value.x, list(spline_merged_M1$Var2), median, na.rm=T)

temp3 <- aggregate(spline_merged_M1$value.x, list(spline_merged_M1$Var2), quantile, probs=0.025,na.rm=T) 
temp4 <- aggregate(spline_merged_M1$value.x, list(spline_merged_M1$Var2), quantile, probs=0.975,na.rm=T)

spline_merged_M1 <- merge(spline_merged_M1, temp1, by.x=c("Var1"), by.y=c("Group.1"))
spline_merged_M1 <- merge(spline_merged_M1, temp2, by.x=c("Var2"), by.y=c("Group.1"))
colnames(spline_merged_M1)[7]<-"average_effect"

spline_merged_M1<-merge(spline_merged_M1, temp3, by.x=c("Var2"), by.y=c("Group.1"))
colnames(spline_merged_M1)[8]<-"lower_bound"

spline_merged_M1<-merge(spline_merged_M1, temp4, by.x=c("Var2"), by.y=c("Group.1"))
colnames(spline_merged_M1)[9]<-"upper_bound"

temp5 <- cbind(seq(10,50,0.225),seq(1:length(seq(10,50,0.225))))
colnames(temp5) <- c("Temp_bin", "Var2")
spline_merged_M1 <-merge(spline_merged_M1, temp5, by=c("Var2"))

# Plot: hourly temperature effect

model_4 <- ggplot()+ ggtitle("Hourly temperature effects: 5 degree knots (alternate) ") + 
  theme(panel.grid.major = element_blank(),
        plot.title = element_text(family="Times New Roman", size=20, colour = "grey25", hjust=0.5),
        axis.title = element_text(family="Times New Roman", size=16, colour = "grey25"),
        axis.text = element_text(family="Times New Roman", size=16),
        panel.grid.minor = element_blank(),
        panel.background = element_blank(),
        axis.line = element_line(colour = "black"),
        legend.position="none")+
  xlab("Temperature (C)") + ylab("Log yield response (Kg/ha)")+
  scale_x_continuous(breaks = seq(10,55, by=5), limits = c(5,55))+
  scale_y_continuous(breaks = seq(-.02,.02, by=.004), limits = c(-.02,.02))+
  geom_hline(yintercept=c(seq(-.02,.02,by=0.004)), colour="gray")+
  geom_vline(xintercept=temp_knot_df, linetype="dotted", colour="gray")+
  geom_line(data=spline_merged_M1, aes(y=value.x, x=Temp_bin, group=Var1, alpha=x.x^200/200), size=.01, colour="brown1")+
  geom_line(data=spline_merged_M1, aes(y=average_effect, x=Temp_bin), alpha=0.85, size=.75, colour="brown4")+
  geom_line(data=spline_merged_M1, aes(y=lower_bound, x=Temp_bin)   , alpha=0.85, size=.75, colour="brown4")+       
  geom_line(data=spline_merged_M1, aes(y=upper_bound, x=Temp_bin)   , alpha=0.85, size=.75, colour="brown4")

model_4

ggsave("C:/Users/Shakila Vyas/OneDrive - Wageningen University & Research/PhD-WUR-Insurance/CSA/County analysis/Results/soydis_IMD_5degree3_20_25_30_discombo.png", width = 20, height = 20, units = "cm")

################################placing knots at 5 degree intervals, not used in main results
hist(temp$temperature)
temp_knot_df <- c(20,25,30,35)

temp_knot_df

# New TS for temperature support of interest- .225 to get the nice plot with small intervals
temp1 <- as.matrix(Hmisc::rcspline.eval(temp$temperature,knots=temp_knot_df,inclx=T))
temp2 <- cbind(temp,temp1)

names(temp2)
head(temp1)

#adjust variables depending on knots
temp3<-temp2 %>%
  group_by(farm,year)%>%
  summarise_at(vars(x,V2,V3),sum)

#match names with temp
names(regdata_soy1)
colnames(regdata_soy1)[colnames(regdata_soy1) == "Year"] <- "year"
colnames(regdata_soy1)[colnames(regdata_soy1) == "Dis_codename"] <- "farm"
names(regdata_soy1)

#join both  data
temp4 <- join(regdata_soy1,temp3, by=c("farm", "year"))

names(temp4)
temp4$reg_year<- as.numeric(as.factor(temp4$District))*temp4$year

#####bootstrapping
set.seed(123)
marginal_temperatures_M1 <- rcspline.eval(seq(10,50,0.225),knots=temp_knot_df, inclx=T)

# Run 1000 models to derive uncertainty in estimates
spline_fitted_M1 <- matrix(NA,nrow=1000, ncol=nrow(marginal_temperatures_M1))

for (l in 1: 1000){
  # Sample by region times year
  data_temp <- temp4[which(temp4$reg_year %in% sample(unique(temp4$reg_year),replace=T)),]
  
  # Run regression with random subsample (data_temp), adjust variables to knots
  fit_boot  <- feols(log(Yield..Tonnes.Hectare.) ~ x + V2  + V3+ year + I(year^2) + erarain + I(erarain^2)  | farm , data_temp)
  
  #Save marginal effect of temperatures,adjust variables to knots 
  spline_fitted_M1[l,]<-as.vector((marginal_temperatures_M1  %*% coef(fit_boot)[1:3])) 
  
  # Show progress in %
  print(l/1000 *100)
  rm(fit_boot,data_temp)
}

# Load the packages
library(fixest)
library(ggplot2)
library(gridExtra)
library(robustbase)
library(egg)
library(matrixStats)
library(tidyverse)

#Change structure
spline_sub_melt_M1 <- melt(spline_fitted_M1)
# Get median response for each column (= a temperature from seq(20,40,0.225))
colmedians_spline_M1 <- colMedians(spline_fitted_M1)
# Get distance from median response for each value
dist_colmedian_M1 <- matrix(NA, nrow=nrow(spline_fitted_M1),ncol=ncol(spline_fitted_M1))

for(l in 1:ncol(spline_fitted_M1)){
  dist_colmedian_M1[,l] <- spline_fitted_M1[,l] - colmedians_spline_M1[l]
}
# Normalize distance to median
dist_colmedian_M1_normalized <- matrix(NA, nrow=nrow(spline_fitted_M1),ncol=ncol(spline_fitted_M1))

for(l in 1:ncol(spline_fitted_M1)){
  dist_colmedian_M1_normalized[,l]<-abs(dist_colmedian_M1[,l]/max(abs(dist_colmedian_M1[,l])))
}

# Change structure
dist_colmedian_M1_normalized_melted <- melt(dist_colmedian_M1_normalized)

# Last preparation before plotting
spline_merged_M1 <- merge(spline_sub_melt_M1,dist_colmedian_M1_normalized_melted,by=c("Var1","Var2"))
spline_merged_M1$value.y_minus1 <- 1-spline_merged_M1$value.y

temp1 <- aggregate(spline_merged_M1$value.y_minus1, list(spline_merged_M1$Var1), median, na.rm=T) 
temp2 <- aggregate(spline_merged_M1$value.x, list(spline_merged_M1$Var2), median, na.rm=T)

temp3 <- aggregate(spline_merged_M1$value.x, list(spline_merged_M1$Var2), quantile, probs=0.025,na.rm=T) 
temp4 <- aggregate(spline_merged_M1$value.x, list(spline_merged_M1$Var2), quantile, probs=0.975,na.rm=T)

spline_merged_M1 <- merge(spline_merged_M1, temp1, by.x=c("Var1"), by.y=c("Group.1"))
spline_merged_M1 <- merge(spline_merged_M1, temp2, by.x=c("Var2"), by.y=c("Group.1"))
colnames(spline_merged_M1)[7]<-"average_effect"

spline_merged_M1<-merge(spline_merged_M1, temp3, by.x=c("Var2"), by.y=c("Group.1"))
colnames(spline_merged_M1)[8]<-"lower_bound"

spline_merged_M1<-merge(spline_merged_M1, temp4, by.x=c("Var2"), by.y=c("Group.1"))
colnames(spline_merged_M1)[9]<-"upper_bound"

temp5 <- cbind(seq(10,50,0.225),seq(1:length(seq(10,50,0.225))))
colnames(temp5) <- c("Temp_bin", "Var2")
spline_merged_M1 <-merge(spline_merged_M1, temp5, by=c("Var2"))

# Plot: hourly temperature effect

model_4 <- ggplot()+ ggtitle("Hourly temperature effects: 5 degree knots ") + 
  theme(panel.grid.major = element_blank(),
        plot.title = element_text(family="Times New Roman", size=20, colour = "grey25", hjust=0.5),
        axis.title = element_text(family="Times New Roman", size=16, colour = "grey25"),
        axis.text = element_text(family="Times New Roman", size=16),
        panel.grid.minor = element_blank(),
        panel.background = element_blank(),
        axis.line = element_line(colour = "black"),
        legend.position="none")+
  xlab("Temperature (°C)") + ylab("Log yield response (Kg/ha)")+
  scale_x_continuous(breaks = seq(10,55, by=5), limits = c(5,55))+
  scale_y_continuous(breaks = seq(-.04,.04, by=.004), limits = c(-.04,.04))+
  geom_hline(yintercept=c(seq(-.04,.04,by=0.004)), colour="gray")+
  geom_vline(xintercept=temp_knot_df, linetype="dotted", colour="gray")+
  geom_line(data=spline_merged_M1, aes(y=value.x, x=Temp_bin, group=Var1, alpha=x.x^200/200), size=.01, colour="brown1")+
  geom_line(data=spline_merged_M1, aes(y=average_effect, x=Temp_bin), alpha=0.85, size=.75, colour="brown4")+
  geom_line(data=spline_merged_M1, aes(y=lower_bound, x=Temp_bin)   , alpha=0.85, size=.75, colour="brown4")+       
  geom_line(data=spline_merged_M1, aes(y=upper_bound, x=Temp_bin)   , alpha=0.85, size=.75, colour="brown4")

model_4

ggsave("C:/Users/Shakila Vyas/OneDrive - Wageningen University & Research/PhD-WUR-Insurance/CSA/County analysis/Results/soydis_IMD_5degree3_knots4_discombo.png", width = 20, height = 20, units = "cm")

#####best fit model
#best knots based on RSS
# lower and upper bound define lowest and highest knot location
lower_bound <- ceiling(quantile(temp$temperature,0.05, type=1))
upper_bound <- floor(quantile(temp$temperature,0.95, type=1))
temp_range <- seq(lower_bound, upper_bound,1) 
kn3_combi <- combn(temp_range,3, simplify = T)
# Minimum space between 2 knots
required_space <- 5
# Get all knot combinations
differences <- matrix(NA, nrow=3, ncol=ncol(kn3_combi))
differences[1,] <- abs(kn3_combi[1,] - kn3_combi[2,])
differences[2,] <- abs(kn3_combi[1,] - kn3_combi[3,])
differences[3,] <- abs(kn3_combi[2,] - kn3_combi[3,])
knots_3_combi_good <- kn3_combi[,which(differences[1,] >= required_space & differences[2,] >= required_space & differences[3,] >= required_space)]
# vector containing residual sum of squares (RSS)
RSS_3knots <- vector(length=ncol(knots_3_combi_good ))

# Get the RSS for each model
for (c in 1:ncol(knots_3_combi_good)){
  
  # Get new time series and aggregate hourly values  
  temp1 <- as.matrix(rcspline.eval(temp$temperature,knots= knots_3_combi_good[,c], inclx=T))
  temp2 <- cbind(temp,temp1)
  
  temp3<-temp2 %>%
    group_by(farm,year)%>%
    summarise_at(vars(x,V2),sum)
  
  # Match yearly values with yearly yields
  temp4 <- join(regdata_wheat1,temp3, by=c("farm", "year"))
  
  # Run regression
  temp_reg <- feols(log(Yield..Tonnes.Hectare.) ~ x + V2  +  year + I(year^2) + erarain + I(erarain^2)  | farm , temp4)
  RSS_3knots[c] <- sum(resid(temp_reg)^2)
  
}

# Best number of knots
temp_knot_df <- knots_3_combi_good[,which.min(RSS_3knots)]
rm(RSS_3knots, knots_3_combi_good, differences, kn3_combi,temp_range,upper_bound, lower_bound)

# resume reg, knots placement from best fit model above
temp_knot_df<- c(12,17,22)
temp1 <- as.matrix(Hmisc::rcspline.eval(temp$temperature,knots=temp_knot_df,inclx=T))
temp2 <- cbind(temp,temp1)
names(temp2)
head(temp1)

#adjust variables depending on knots
temp3<-temp2 %>%
  group_by(farm,year)%>%
  summarise_at(vars(x,V2),sum)

#match names with temp
names(regdata_soy1)
colnames(regdata_soy1)[colnames(regdata_soy1) == "Year"] <- "year"
colnames(regdata_soy1)[colnames(regdata_soy1) == "Dis_codename"] <- "farm"
names(regdata_soy1)

#join both  data
temp4 <- join(regdata_soy1,temp3, by=c("farm", "year"))

names(temp4)
temp4$reg_year<- as.numeric(as.factor(temp4$District))*temp4$year

#####bootstrapping
set.seed(123)
marginal_temperatures_M1 <- rcspline.eval(seq(0,43,0.225),knots=temp_knot_df, inclx=T)

# Run 1000 models to derive uncertainty in estimates
spline_fitted_M1 <- matrix(NA,nrow=1000, ncol=nrow(marginal_temperatures_M1))

for (l in 1: 1000){
  # Sample by region times year
  data_temp <- temp4[which(temp4$reg_year %in% sample(unique(temp4$reg_year),replace=T)),]
  
  # Run regression with random subsample (data_temp), adjust variables to knots
  fit_boot  <- feols(log(Yield..Tonnes.Hectare.) ~ x + V2  +  year + I(year^2) + erarain + I(erarain^2)  | farm , data_temp)
  
  #Save marginal effect of temperatures,adjust variables to knots 
  spline_fitted_M1[l,]<-as.vector((marginal_temperatures_M1  %*% coef(fit_boot)[1:2])) 
  
  # Show progress in %
  print(l/1000 *100)
  rm(fit_boot,data_temp)
}

#Change structure
# Load the packages
library(fixest)
library(ggplot2)
library(gridExtra)
library(robustbase)
library(egg)
library(matrixStats)
library(tidyverse)

spline_sub_melt_M1 <- melt(spline_fitted_M1)
# Get median response for each column (= a temperature from seq(20,40,0.225))
colmedians_spline_M1 <- colMedians(spline_fitted_M1)
# Get distance from median response for each value
dist_colmedian_M1 <- matrix(NA, nrow=nrow(spline_fitted_M1),ncol=ncol(spline_fitted_M1))

for(l in 1:ncol(spline_fitted_M1)){
  dist_colmedian_M1[,l] <- spline_fitted_M1[,l] - colmedians_spline_M1[l]
}
# Normalize distance to median
dist_colmedian_M1_normalized <- matrix(NA, nrow=nrow(spline_fitted_M1),ncol=ncol(spline_fitted_M1))

for(l in 1:ncol(spline_fitted_M1)){
  dist_colmedian_M1_normalized[,l]<-abs(dist_colmedian_M1[,l]/max(abs(dist_colmedian_M1[,l])))
}

# Change structure
dist_colmedian_M1_normalized_melted <- melt(dist_colmedian_M1_normalized)

# Last preparation before plotting
spline_merged_M1 <- merge(spline_sub_melt_M1,dist_colmedian_M1_normalized_melted,by=c("Var1","Var2"))
spline_merged_M1$value.y_minus1 <- 1-spline_merged_M1$value.y

temp1 <- aggregate(spline_merged_M1$value.y_minus1, list(spline_merged_M1$Var1), median, na.rm=T) 
temp2 <- aggregate(spline_merged_M1$value.x, list(spline_merged_M1$Var2), median, na.rm=T)

temp3 <- aggregate(spline_merged_M1$value.x, list(spline_merged_M1$Var2), quantile, probs=0.025,na.rm=T) 
temp4 <- aggregate(spline_merged_M1$value.x, list(spline_merged_M1$Var2), quantile, probs=0.975,na.rm=T)

spline_merged_M1 <- merge(spline_merged_M1, temp1, by.x=c("Var1"), by.y=c("Group.1"))
spline_merged_M1 <- merge(spline_merged_M1, temp2, by.x=c("Var2"), by.y=c("Group.1"))
colnames(spline_merged_M1)[7]<-"average_effect"

spline_merged_M1<-merge(spline_merged_M1, temp3, by.x=c("Var2"), by.y=c("Group.1"))
colnames(spline_merged_M1)[8]<-"lower_bound"

spline_merged_M1<-merge(spline_merged_M1, temp4, by.x=c("Var2"), by.y=c("Group.1"))
colnames(spline_merged_M1)[9]<-"upper_bound"

temp5 <- cbind(seq(0,43,0.225),seq(1:length(seq(0,43,0.225))))
colnames(temp5) <- c("Temp_bin", "Var2")
spline_merged_M1 <-merge(spline_merged_M1, temp5, by=c("Var2"))

# Plot: hourly temperature effect
model_5 <- ggplot()+ ggtitle("Hourly temperature effects: Best fit") + 
  theme(panel.grid.major = element_blank(),
        plot.title = element_text(family="Times New Roman", size=20, colour = "grey25", hjust=0.5),
        axis.title = element_text(family="Times New Roman", size=16, colour = "grey25"),
        axis.text = element_text(family="Times New Roman", size=16),
        panel.grid.minor = element_blank(),
        panel.background = element_blank(),
        axis.line = element_line(colour = "black"),
        legend.position="none")+
  xlab("Temperature (°C)") + ylab("Log yield response (Kg/ha)")+
  scale_x_continuous(breaks = seq(0,43, by=5), limits = c(0,43))+
  scale_y_continuous(breaks = seq(-.004,.004, by=.001), limits = c(-.004,.004))+
  geom_hline(yintercept=c(seq(-.004,.004,by=0.001)), colour="gray")+
  geom_vline(xintercept=temp_knot_df, linetype="dotted", colour="gray")+
  geom_line(data=spline_merged_M1, aes(y=value.x+0.0003830928, x=Temp_bin, group=Var1, alpha=x.x^200/200), size=.01, colour="darkorange1")+
  geom_line(data=spline_merged_M1, aes(y=average_effect+0.0003830928, x=Temp_bin), alpha=0.85, size=.75, colour="darkorange4")+
  geom_line(data=spline_merged_M1, aes(y=lower_bound+0.0003830928, x=Temp_bin)   , alpha=0.85, size=.75, colour="darkorange4")+       
  geom_line(data=spline_merged_M1, aes(y=upper_bound+0.0003830928, x=Temp_bin)   , alpha=0.85, size=.75, colour="darkorange4")

model_5
ggsave("C:/Users/Shakila Vyas/OneDrive - Wageningen University & Research/PhD-WUR-Insurance/CSA/County analysis/Results/soydis_era_bestfit_discombo_cen.png", width = 20, height = 20, units = "cm")

#subset by regions for ERA 
library(raster)
library(dplyr)
library(data.table)
library(reshape2)
library(Hmisc)
library(lubridate)
library(plyr)
library(esmisc)
library(sandwich)
library(MASS)
library(e1071)
library(fixest)

data<- read.csv("C:/Users/Shakila Vyas/OneDrive - Wageningen University & Research/PhD-WUR-Insurance/CSA/County analysis/Dis yield_All India/Wheat_yields_rev.csv")
head(data)
#remove less than 3 years data
data1<- add_count(data, Dist_code)
regdata_wheat<- subset(data1,n>=3,select = Crop.Production.Statistics:n)
head(regdata_wheat)

##preparing data
hourly<- read.csv("C:/Users/Shakila Vyas/OneDrive - Wageningen University & Research/PhD-WUR-Insurance/CSA/County analysis/Weather data/ERA5/Winter/hourlytp15era.csv")
#need this format-colnames(temp_wheat_exposure2) <- c("Farm","Date","Hour", "Temperature")
as.data.frame(hourly)
library(reshape2)
long <- reshape2::melt(hourly, id.vars = c("farm"))
long$year<- 2015

hourly1<- read.csv("C:/Users/Shakila Vyas/OneDrive - Wageningen University & Research/PhD-WUR-Insurance/CSA/County analysis/Weather data/ERA5/Winter/hourlytp16era.csv")
#need this format-colnames(temp_wheat_exposure2) <- c("Farm","Date","Hour", "Temperature")
as.data.frame(hourly1)
long1 <- reshape2::melt(hourly1, id.vars = c("farm"))
long1$year<- 2016

hourly2<- read.csv("C:/Users/Shakila Vyas/OneDrive - Wageningen University & Research/PhD-WUR-Insurance/CSA/County analysis/Weather data/ERA5/Winter/hourlytp17era.csv")
#need this format-colnames(temp_wheat_exposure2) <- c("Farm","Date","Hour", "Temperature")
as.data.frame(hourly2)
long2 <- reshape2::melt(hourly2, id.vars = c("farm"))
long2$year<- 2017

hourly3<- read.csv("C:/Users/Shakila Vyas/OneDrive - Wageningen University & Research/PhD-WUR-Insurance/CSA/County analysis/Weather data/ERA5/Winter/hourlytp18era.csv")
#need this format-colnames(temp_wheat_exposure2) <- c("Farm","Date","Hour", "Temperature")
as.data.frame(hourly3)
long3 <- reshape2::melt(hourly3, id.vars = c("farm"))
long3$year<- 2018

hourly4<- read.csv("C:/Users/Shakila Vyas/OneDrive - Wageningen University & Research/PhD-WUR-Insurance/CSA/County analysis/Weather data/ERA5/Winter/hourlytp19era.csv")
#need this format-colnames(temp_wheat_exposure2) <- c("Farm","Date","Hour", "Temperature")
as.data.frame(hourly4)
long4 <- reshape2::melt(hourly4, id.vars = c("farm"))
long4$year<- 2019

hourly5<- read.csv("C:/Users/Shakila Vyas/OneDrive - Wageningen University & Research/PhD-WUR-Insurance/CSA/County analysis/Weather data/ERA5/Winter/hourlytp20era.csv")
#need this format-colnames(temp_wheat_exposure2) <- c("Farm","Date","Hour", "Temperature")
as.data.frame(hourly5)
long5 <- reshape2::melt(hourly5, id.vars = c("farm"))
long5$year<- 2020

temp<- rbind(long,long1,long2,long3,long4,long5)

range(temp$value)

####outlier removal
hist(regdata_wheat$Yield..Tonnes.Hectare.)
range(regdata_wheat$Yield..Tonnes.Hectare.)

quantile(regdata_wheat$Yield..Tonnes.Hectare.,probs= c(.99))
regdata_wheat1<- subset(regdata_wheat,Yield..Tonnes.Hectare.<quantile(regdata_wheat$Yield..Tonnes.Hectare.,probs= c(.99)),select = Crop.Production.Statistics:n )

#subset by regions, change region name wit every selection
regdata_wheat1<- subset(regdata_wheat1,area=="south",select = Crop.Production.Statistics:n )

#rename temp data
names(temp)[3] <- "temperature"
as.numeric(temp$temperature)
min(temp$temperature)
max(temp$temperature)
range(temp$temperature)
summary(temp$temperature)
hist(temp$temperature,main="Hourly temperature district-Winter")
range(temp$temperature)

#best fit, use previous yield thresholds
temp_knot_df<- c(12,17,22)

# New TS for temperature support of interest- .225 to get the nice plot with small intervals
temp1 <- as.matrix(Hmisc::rcspline.eval(temp$temperature,knots=temp_knot_df,inclx=T))
temp2 <- cbind(temp,temp1)

names(temp2)
head(temp1)

#adjust variables depending on knots
temp3<-temp2 %>%
  group_by(farm,year)%>%
  summarise_at(vars(x,V2),sum)

#match names with temp
names(regdata_wheat1)
colnames(regdata_wheat1)[colnames(regdata_wheat) == "Year.1"] <- "year"
colnames(regdata_wheat1)[colnames(regdata_wheat) == "Dist_code"] <- "farm"
names(regdata_wheat1)

#join both  data
temp4 <- join(regdata_wheat1,temp3, by=c("farm", "year"))

names(temp4)
temp4$reg_year<- as.numeric(as.factor(temp4$District))*temp4$year

library(fixest)
wheat_yield_spline = feols(log(Yield..Tonnes.Hectare.) ~ x + V2 + year + I(year^2) + erarain + I(erarain^2)   | farm , cluster = c( "year","farm"), temp4)
summary(wheat_yield_spline)

#####bootstrapping
set.seed(123)
marginal_temperatures_M1 <- rcspline.eval(seq(0,43,0.225),knots=temp_knot_df, inclx=T)

# Run 1000 models to derive uncertainty in estimates
spline_fitted_M1 <- matrix(NA,nrow=1000, ncol=nrow(marginal_temperatures_M1))

for (l in 1: 1000){
  # Sample by region times year
  data_temp <- temp4[which(temp4$reg_year %in% sample(unique(temp4$reg_year),replace=T)),]
  
  # Run regression with random subsample (data_temp), adjust variables to knots
  fit_boot  <- feols(Yield..Tonnes.Hectare. ~ x + V2  + year + I(year^2) + erarain + I(erarain^2)  | farm , data_temp)
  
  #Save marginal effect of temperatures,adjust variables to knots 
  spline_fitted_M1[l,]<-as.vector((marginal_temperatures_M1  %*% coef(fit_boot)[1:2])) 
  
  # Show progress in %
  print(l/1000 *100)
  rm(fit_boot,data_temp)
}

# Load the packages
library(fixest)
library(ggplot2)
library(gridExtra)
library(robustbase)
library(egg)
library(matrixStats)
library(tidyverse)

#Change structure
spline_sub_melt_M1 <- melt(spline_fitted_M1)
# Get median response for each column (= a temperature from seq(20,40,0.225))
colmedians_spline_M1 <- colMedians(spline_fitted_M1)
# Get distance from median response for each value
dist_colmedian_M1 <- matrix(NA, nrow=nrow(spline_fitted_M1),ncol=ncol(spline_fitted_M1))

for(l in 1:ncol(spline_fitted_M1)){
  dist_colmedian_M1[,l] <- spline_fitted_M1[,l] - colmedians_spline_M1[l]
}
# Normalize distance to median
dist_colmedian_M1_normalized <- matrix(NA, nrow=nrow(spline_fitted_M1),ncol=ncol(spline_fitted_M1))

for(l in 1:ncol(spline_fitted_M1)){
  dist_colmedian_M1_normalized[,l]<-abs(dist_colmedian_M1[,l]/max(abs(dist_colmedian_M1[,l])))
}

# Change structure
dist_colmedian_M1_normalized_melted <- melt(dist_colmedian_M1_normalized)

# Last preparation before plotting
spline_merged_M1 <- merge(spline_sub_melt_M1,dist_colmedian_M1_normalized_melted,by=c("Var1","Var2"))
spline_merged_M1$value.y_minus1 <- 1-spline_merged_M1$value.y

temp1 <- aggregate(spline_merged_M1$value.y_minus1, list(spline_merged_M1$Var1), median, na.rm=T) 
temp2 <- aggregate(spline_merged_M1$value.x, list(spline_merged_M1$Var2), median, na.rm=T)

temp3 <- aggregate(spline_merged_M1$value.x, list(spline_merged_M1$Var2), quantile, probs=0.025,na.rm=T) 
temp4 <- aggregate(spline_merged_M1$value.x, list(spline_merged_M1$Var2), quantile, probs=0.975,na.rm=T)

spline_merged_M1 <- merge(spline_merged_M1, temp1, by.x=c("Var1"), by.y=c("Group.1"))
spline_merged_M1 <- merge(spline_merged_M1, temp2, by.x=c("Var2"), by.y=c("Group.1"))
colnames(spline_merged_M1)[7]<-"average_effect"

spline_merged_M1<-merge(spline_merged_M1, temp3, by.x=c("Var2"), by.y=c("Group.1"))
colnames(spline_merged_M1)[8]<-"lower_bound"

spline_merged_M1<-merge(spline_merged_M1, temp4, by.x=c("Var2"), by.y=c("Group.1"))
colnames(spline_merged_M1)[9]<-"upper_bound"

temp5 <- cbind(seq(0,43,0.225),seq(1:length(seq(0,43,0.225))))
colnames(temp5) <- c("Temp_bin", "Var2")
spline_merged_M1 <-merge(spline_merged_M1, temp5, by=c("Var2"))

# Plot: hourly temperature effect
model_1 <- ggplot()+ ggtitle("Hourly temperature effects: Best fit") + 
  theme(panel.grid.major = element_blank(),
        plot.title = element_text(family="Times New Roman", size=20, colour = "grey25", hjust=0.5),
        axis.title = element_text(family="Times New Roman", size=16, colour = "grey25"),
        axis.text = element_text(family="Times New Roman", size=16),
        panel.grid.minor = element_blank(),
        panel.background = element_blank(),
        axis.line = element_line(colour = "black"),
        legend.position="none")+
  xlab("Temperature (°C)") + ylab("Log yield response (Kg/ha)")+
  scale_x_continuous(breaks = seq(0,43, by=5), limits = c(0,43))+
  scale_y_continuous(breaks = seq(-.008,.008, by=.002), limits = c(-.008,.008))+
  geom_hline(yintercept=c(seq(-.008,.008,by=0.002)), colour="gray")+
  geom_vline(xintercept=temp_knot_df, linetype="dotted", colour="gray")+
  geom_line(data=spline_merged_M1, aes(y=value.x-0.00003442679, x=Temp_bin, group=Var1, alpha=x.x^200/200), size=.01, colour="darkorange1")+
  geom_line(data=spline_merged_M1, aes(y=average_effect-0.00003442679, x=Temp_bin), alpha=0.85, size=.75, colour="darkorange4")+
  geom_line(data=spline_merged_M1, aes(y=lower_bound-0.00003442679, x=Temp_bin)   , alpha=0.85, size=.75, colour="darkorange4")+       
  geom_line(data=spline_merged_M1, aes(y=upper_bound-0.00003442679, x=Temp_bin)   , alpha=0.85, size=.75, colour="darkorange4")

model_1
ggsave("C:/Users/Shakila Vyas/OneDrive - Wageningen University & Research/PhD-WUR-Insurance/CSA/County analysis/Results/wheatdis_era_bestfit_cen.png", width = 20, height = 20, units = "cm")

########################IMD Wheat county 
library(raster)
library(dplyr)
library(data.table)
library(reshape2)
library(Hmisc)
library(lubridate)
library(plyr)
library(esmisc)
library(sandwich)
library(MASS)
library(e1071)
library(fixest)

data<- read.csv("C:/Users/Shakila Vyas/OneDrive - Wageningen University & Research/PhD-WUR-Insurance/CSA/County analysis/Dis yield_All India/Wheat_yields_rev.csv")
head(data)
#remove less than 3 years data
data1<- add_count(data, Dist_code)
regdata_wheat<- subset(data1,n>=3,select = Crop.Production.Statistics:n)
head(regdata_wheat)

##preparing data
hourly<- read.csv("C:/Users/Shakila Vyas/OneDrive - Wageningen University & Research/PhD-WUR-Insurance/CSA/County analysis/Weather data/IMD/Winter/hourlytpimddis15.csv")
#need this format-colnames(temp_wheat_exposure2) <- c("Farm","Date","Hour", "Temperature")
as.data.frame(hourly)
library(reshape2)
long <- reshape2::melt(hourly, id.vars = c("farm"))
long$year<- 2015

hourly1<- read.csv("C:/Users/Shakila Vyas/OneDrive - Wageningen University & Research/PhD-WUR-Insurance/CSA/County analysis/Weather data/IMD/Winter/hourlytpimddis16.csv")
#need this format-colnames(temp_wheat_exposure2) <- c("Farm","Date","Hour", "Temperature")
as.data.frame(hourly1)
long1 <- reshape2::melt(hourly1, id.vars = c("farm"))
long1$year<- 2016

hourly2<- read.csv("C:/Users/Shakila Vyas/OneDrive - Wageningen University & Research/PhD-WUR-Insurance/CSA/County analysis/Weather data/IMD/Winter/hourlytpimddis17.csv")
#need this format-colnames(temp_wheat_exposure2) <- c("Farm","Date","Hour", "Temperature")
as.data.frame(hourly2)
long2 <- reshape2::melt(hourly2, id.vars = c("farm"))
long2$year<- 2017

hourly3<- read.csv("C:/Users/Shakila Vyas/OneDrive - Wageningen University & Research/PhD-WUR-Insurance/CSA/County analysis/Weather data/IMD/Winter/hourlytpimddis18.csv")
#need this format-colnames(temp_wheat_exposure2) <- c("Farm","Date","Hour", "Temperature")
as.data.frame(hourly3)
long3 <- reshape2::melt(hourly3, id.vars = c("farm"))
long3$year<- 2018

hourly4<- read.csv("C:/Users/Shakila Vyas/OneDrive - Wageningen University & Research/PhD-WUR-Insurance/CSA/County analysis/Weather data/IMD/Winter/hourlytpimddis19.csv")
#need this format-colnames(temp_wheat_exposure2) <- c("Farm","Date","Hour", "Temperature")
as.data.frame(hourly4)
long4 <- reshape2::melt(hourly4, id.vars = c("farm"))
long4$year<- 2019

hourly5<- read.csv("C:/Users/Shakila Vyas/OneDrive - Wageningen University & Research/PhD-WUR-Insurance/CSA/County analysis/Weather data/IMD/Winter/hourlytpimddis20.csv")
#need this format-colnames(temp_wheat_exposure2) <- c("Farm","Date","Hour", "Temperature")
as.data.frame(hourly5)
long5 <- reshape2::melt(hourly5, id.vars = c("farm"))
long5$year<- 2020

temp<- rbind(long,long1,long2,long3,long4,long5)

####outlier removal
hist(regdata_wheat$Yield..Tonnes.Hectare.)
range(regdata_wheat$Yield..Tonnes.Hectare.)

quantile(regdata_wheat$Yield..Tonnes.Hectare.,probs= c(.99))
regdata_wheat1<- subset(regdata_wheat,Yield..Tonnes.Hectare.<quantile(regdata_wheat$Yield..Tonnes.Hectare.,probs= c(.99)),select = Crop.Production.Statistics:n )

#rename temp data
names(temp)[3] <- "temperature"
names(temp)[1] <- "farm"
as.numeric(temp$temperature)
min(temp$temperature)
max(temp$temperature)
range(temp$temperature)
summary(temp$temperature)
hist(temp$temperature,main="Hourly temperature district-Winter")

range(temp$temperature)
temp <- na.omit(temp)

#####################model 1: equal spaced 3 knots
temp_knot_df <- c(min(temp$temperature) + ((max(temp$temperature)-min(temp$temperature)) / 4),
                  min(temp$temperature) + 2* ((max(temp$temperature)-min(temp$temperature)) / 4),
                  min(temp$temperature) + 3* ((max(temp$temperature)-min(temp$temperature)) / 4))

# New TS for temperature support of interest- .225 to get the nice plot with small intervals
temp1 <- as.matrix(Hmisc::rcspline.eval(temp$temperature,knots=temp_knot_df,inclx=T))
temp2 <- cbind(temp,temp1)

names(temp2)
head(temp1)

#adjust variables depending on knots
temp3<-temp2 %>%
  group_by(farm,year)%>%
  summarise_at(vars(x,V2),sum)

#match names with temp
names(regdata_wheat1)
colnames(regdata_wheat1)[colnames(regdata_wheat) == "Year.1"] <- "year"
colnames(regdata_wheat1)[colnames(regdata_wheat) == "Dist_code"] <- "farm"
names(regdata_wheat1)

#join both  data
temp4 <- join(regdata_wheat1,temp3, by=c("farm", "year"))

names(temp4)
temp4$reg_year<- as.numeric(as.factor(temp4$District))*temp4$year

library(fixest)
wheat_yield_spline = feols(log(Yield..Tonnes.Hectare.) ~ x + V2 + year + I(year^2)    | farm , cluster = c( "year","farm"), temp4)
summary(wheat_yield_spline)

results<- etable(soy_yield_spline)
write.csv(results,"C:/Users/Shakila Vyas/OneDrive - Wageningen University & Research/PhD-WUR-Insurance/CSA/ERA5_results_eq_knot3_returns.csv")

#####bootstrapping
set.seed(123)
marginal_temperatures_M1 <- rcspline.eval(seq(5,50,0.225),knots=temp_knot_df, inclx=T)

# Run 1000 models to derive uncertainty in estimates
spline_fitted_M1 <- matrix(NA,nrow=1000, ncol=nrow(marginal_temperatures_M1))

for (l in 1: 1000){
  # Sample by region times year
  data_temp <- temp4[which(temp4$reg_year %in% sample(unique(temp4$reg_year),replace=T)),]
  
  # Run regression with random subsample (data_temp), adjust variables to knots
  fit_boot  <- feols(Yield..Tonnes.Hectare. ~ x + V2  + year + I(year^2)   | farm , data_temp)
  
  #Save marginal effect of temperatures,adjust variables to knots 
  spline_fitted_M1[l,]<-as.vector((marginal_temperatures_M1  %*% coef(fit_boot)[1:2])) 
  
  # Show progress in %
  print(l/1000 *100)
  rm(fit_boot,data_temp)
}

# Load the packages
library(fixest)
library(ggplot2)
library(gridExtra)
library(robustbase)
library(egg)
library(matrixStats)
library(tidyverse)

#Change structure
spline_sub_melt_M1 <- melt(spline_fitted_M1)
# Get median response for each column (= a temperature from seq(20,40,0.225))
colmedians_spline_M1 <- colMedians(spline_fitted_M1)
# Get distance from median response for each value
dist_colmedian_M1 <- matrix(NA, nrow=nrow(spline_fitted_M1),ncol=ncol(spline_fitted_M1))

for(l in 1:ncol(spline_fitted_M1)){
  dist_colmedian_M1[,l] <- spline_fitted_M1[,l] - colmedians_spline_M1[l]
}
# Normalize distance to median
dist_colmedian_M1_normalized <- matrix(NA, nrow=nrow(spline_fitted_M1),ncol=ncol(spline_fitted_M1))

for(l in 1:ncol(spline_fitted_M1)){
  dist_colmedian_M1_normalized[,l]<-abs(dist_colmedian_M1[,l]/max(abs(dist_colmedian_M1[,l])))
}

# Change structure
dist_colmedian_M1_normalized_melted <- melt(dist_colmedian_M1_normalized)

# Last preparation before plotting
spline_merged_M1 <- merge(spline_sub_melt_M1,dist_colmedian_M1_normalized_melted,by=c("Var1","Var2"))
spline_merged_M1$value.y_minus1 <- 1-spline_merged_M1$value.y

temp1 <- aggregate(spline_merged_M1$value.y_minus1, list(spline_merged_M1$Var1), median, na.rm=T) 
temp2 <- aggregate(spline_merged_M1$value.x, list(spline_merged_M1$Var2), median, na.rm=T)

temp3 <- aggregate(spline_merged_M1$value.x, list(spline_merged_M1$Var2), quantile, probs=0.025,na.rm=T) 
temp4 <- aggregate(spline_merged_M1$value.x, list(spline_merged_M1$Var2), quantile, probs=0.975,na.rm=T)

spline_merged_M1 <- merge(spline_merged_M1, temp1, by.x=c("Var1"), by.y=c("Group.1"))
spline_merged_M1 <- merge(spline_merged_M1, temp2, by.x=c("Var2"), by.y=c("Group.1"))
colnames(spline_merged_M1)[7]<-"average_effect"

spline_merged_M1<-merge(spline_merged_M1, temp3, by.x=c("Var2"), by.y=c("Group.1"))
colnames(spline_merged_M1)[8]<-"lower_bound"

spline_merged_M1<-merge(spline_merged_M1, temp4, by.x=c("Var2"), by.y=c("Group.1"))
colnames(spline_merged_M1)[9]<-"upper_bound"

temp5 <- cbind(seq(5,50,0.225),seq(1:length(seq(5,50,0.225))))
colnames(temp5) <- c("Temp_bin", "Var2")
spline_merged_M1 <-merge(spline_merged_M1, temp5, by=c("Var2"))

# Plot: hourly temperature effect

model_1 <- ggplot()+ ggtitle("Hourly temperature effects: Equal Knots 3") + 
  theme(panel.grid.major = element_blank(),
        plot.title = element_text(family="Times New Roman", size=20, colour = "grey25", hjust=0.5),
        axis.title = element_text(family="Times New Roman", size=16, colour = "grey25"),
        axis.text = element_text(family="Times New Roman", size=16),
        panel.grid.minor = element_blank(),
        panel.background = element_blank(),
        axis.line = element_line(colour = "black"),
        legend.position="none")+
  xlab("Temperature (°C)") + ylab("Log yield response (Kg/ha)")+
  scale_x_continuous(breaks = seq(5,55, by=5), limits = c(5,55))+
  scale_y_continuous(breaks = seq(-.004,.004, by=.001), limits = c(-.004,.004))+
  geom_hline(yintercept=c(seq(-.004,.004,by=0.001)), colour="gray")+
  geom_vline(xintercept=temp_knot_df, linetype="dotted", colour="gray")+
  geom_line(data=spline_merged_M1, aes(y=value.x, x=Temp_bin, group=Var1, alpha=x.x^200/200), size=.01, colour="darkorange1")+
  geom_line(data=spline_merged_M1, aes(y=average_effect, x=Temp_bin), alpha=0.85, size=.75, colour="darkorange4")+
  geom_line(data=spline_merged_M1, aes(y=lower_bound, x=Temp_bin)   , alpha=0.85, size=.75, colour="darkorange4")+       
  geom_line(data=spline_merged_M1, aes(y=upper_bound, x=Temp_bin)   , alpha=0.85, size=.75, colour="darkorange4")

model_1
ggsave("C:/Users/Shakila Vyas/OneDrive - Wageningen University & Research/PhD-WUR-Insurance/CSA/County analysis/Results/wheatdis_IMD_eq3_dis_norain.png", width = 20, height = 20, units = "cm")

#####################model 3: quantile 3 knots
# 10%, 50%, 90% quantile
temp_knot_df <- c(quantile(temp$temperature,0.1, type=1),
                  quantile(temp$temperature,0.5, type=1),
                  quantile(temp$temperature,0.9, type=1))
temp_knot_df
# New TS for temperature support of interest- .225 to get the nice plot with small intervals
temp1 <- as.matrix(Hmisc::rcspline.eval(temp$temperature,knots=temp_knot_df,inclx=T))
temp2 <- cbind(temp,temp1)

names(temp2)
head(temp1)

#adjust variables depending on knots
temp3<-temp2 %>%
  group_by(farm,year)%>%
  summarise_at(vars(x,V2),sum)

#join both  data
temp4 <- join(regdata_wheat1,temp3, by=c("farm", "year"))

names(temp4)
temp4$reg_year<- as.numeric(as.factor(temp4$District))*temp4$year

#####bootstrapping
set.seed(123)
marginal_temperatures_M1 <- rcspline.eval(seq(5,50,0.225),knots=temp_knot_df, inclx=T)

# Run 1000 models to derive uncertainty in estimates
spline_fitted_M1 <- matrix(NA,nrow=1000, ncol=nrow(marginal_temperatures_M1))

for (l in 1: 1000){
  # Sample by region times year
  data_temp <- temp4[which(temp4$reg_year %in% sample(unique(temp4$reg_year),replace=T)),]
  
  # Run regression with random subsample (data_temp), adjust variables to knots
  fit_boot  <- feols(log(Yield..Tonnes.Hectare.) ~ x + V2  + year + I(year^2) + winterrainsum + I(winterrainsum^2)  | farm , data_temp)
  
  #Save marginal effect of temperatures,adjust variables to knots 
  spline_fitted_M1[l,]<-as.vector((marginal_temperatures_M1  %*% coef(fit_boot)[1:2])) 
  
  # Show progress in %
  print(l/1000 *100)
  rm(fit_boot,data_temp)
}

# Load the packages
#Change structure
spline_sub_melt_M1 <- melt(spline_fitted_M1)
# Get median response for each column (= a temperature from seq(20,40,0.225))
colmedians_spline_M1 <- colMedians(spline_fitted_M1)
# Get distance from median response for each value
dist_colmedian_M1 <- matrix(NA, nrow=nrow(spline_fitted_M1),ncol=ncol(spline_fitted_M1))

for(l in 1:ncol(spline_fitted_M1)){
  dist_colmedian_M1[,l] <- spline_fitted_M1[,l] - colmedians_spline_M1[l]
}
# Normalize distance to median
dist_colmedian_M1_normalized <- matrix(NA, nrow=nrow(spline_fitted_M1),ncol=ncol(spline_fitted_M1))

for(l in 1:ncol(spline_fitted_M1)){
  dist_colmedian_M1_normalized[,l]<-abs(dist_colmedian_M1[,l]/max(abs(dist_colmedian_M1[,l])))
}

# Change structure
dist_colmedian_M1_normalized_melted <- melt(dist_colmedian_M1_normalized)

# Last preparation before plotting
spline_merged_M1 <- merge(spline_sub_melt_M1,dist_colmedian_M1_normalized_melted,by=c("Var1","Var2"))
spline_merged_M1$value.y_minus1 <- 1-spline_merged_M1$value.y

temp1 <- aggregate(spline_merged_M1$value.y_minus1, list(spline_merged_M1$Var1), median, na.rm=T) 
temp2 <- aggregate(spline_merged_M1$value.x, list(spline_merged_M1$Var2), median, na.rm=T)

temp3 <- aggregate(spline_merged_M1$value.x, list(spline_merged_M1$Var2), quantile, probs=0.025,na.rm=T) 
temp4 <- aggregate(spline_merged_M1$value.x, list(spline_merged_M1$Var2), quantile, probs=0.975,na.rm=T)

spline_merged_M1 <- merge(spline_merged_M1, temp1, by.x=c("Var1"), by.y=c("Group.1"))
spline_merged_M1 <- merge(spline_merged_M1, temp2, by.x=c("Var2"), by.y=c("Group.1"))
colnames(spline_merged_M1)[7]<-"average_effect"

spline_merged_M1<-merge(spline_merged_M1, temp3, by.x=c("Var2"), by.y=c("Group.1"))
colnames(spline_merged_M1)[8]<-"lower_bound"

spline_merged_M1<-merge(spline_merged_M1, temp4, by.x=c("Var2"), by.y=c("Group.1"))
colnames(spline_merged_M1)[9]<-"upper_bound"

temp5 <- cbind(seq(5,50,0.225),seq(1:length(seq(5,50,0.225))))
colnames(temp5) <- c("Temp_bin", "Var2")
spline_merged_M1 <-merge(spline_merged_M1, temp5, by=c("Var2"))

# Plot: hourly temperature effect
model_1 <- ggplot()+ ggtitle("Hourly temperature effects: Quantile Knots 3") + 
  theme(panel.grid.major = element_blank(),
        plot.title = element_text(family="Times New Roman", size=20, colour = "grey25", hjust=0.5),
        axis.title = element_text(family="Times New Roman", size=16, colour = "grey25"),
        axis.text = element_text(family="Times New Roman", size=16),
        panel.grid.minor = element_blank(),
        panel.background = element_blank(),
        axis.line = element_line(colour = "black"),
        legend.position="none")+
  xlab("Temperature (°C)") + ylab("Log yield response (Kg/ha)")+
  scale_x_continuous(breaks = seq(5,55, by=5), limits = c(5,55))+
  scale_y_continuous(breaks = seq(-.004,.004, by=.001), limits = c(-.004,.004))+
  geom_hline(yintercept=c(seq(-.004,.004,by=0.001)), colour="gray")+
  geom_vline(xintercept=temp_knot_df, linetype="dotted", colour="gray")+
  geom_line(data=spline_merged_M1, aes(y=value.x, x=Temp_bin, group=Var1, alpha=x.x^200/200), size=.01, colour="darkorange1")+
  geom_line(data=spline_merged_M1, aes(y=average_effect, x=Temp_bin), alpha=0.85, size=.75, colour="darkorange4")+
  geom_line(data=spline_merged_M1, aes(y=lower_bound, x=Temp_bin)   , alpha=0.85, size=.75, colour="darkorange4")+       
  geom_line(data=spline_merged_M1, aes(y=upper_bound, x=Temp_bin)   , alpha=0.85, size=.75, colour="darkorange4")

model_1
ggsave("C:/Users/Shakila Vyas/OneDrive - Wageningen University & Research/PhD-WUR-Insurance/CSA/County analysis/Results/wheatdis_IMD_qq3_dis.png", width = 20, height = 20, units = "cm")

#####################model 2: equal spaced 4 knots, not used in the main results
temp_knot_df <- c(min(temp$temperature) + ((max(temp$temperature)-min(temp$temperature)) / 5),
                  min(temp$temperature) + 2* ((max(temp$temperature)-min(temp$temperature)) / 5),
                  min(temp$temperature) + 3* ((max(temp$temperature)-min(temp$temperature)) / 5),
                  min(temp$temperature) + 4* ((max(temp$temperature)-min(temp$temperature)) / 5))
temp_knot_df
# New TS for temperature support of interest- .225 to get the nice plot with small intervals
temp1 <- as.matrix(Hmisc::rcspline.eval(temp$temperature,knots=temp_knot_df,inclx=T))
temp2 <- cbind(temp,temp1)

names(temp2)
head(temp1)

#adjust variables depending on knots
temp3<-temp2 %>%
  group_by(farm,year)%>%
  summarise_at(vars(x,V2,V3),sum)

#join both  data
temp4 <- join(regdata_wheat1,temp3, by=c("farm", "year"))
names(temp4)
temp4$reg_year<- as.numeric(as.factor(temp4$District))*temp4$year

#####bootstrapping
set.seed(123)
marginal_temperatures_M1 <- rcspline.eval(seq(10,50,0.225),knots=temp_knot_df, inclx=T)

# Run 1000 models to derive uncertainty in estimates
spline_fitted_M1 <- matrix(NA,nrow=1000, ncol=nrow(marginal_temperatures_M1))

for (l in 1: 1000){
  # Sample by region times year
  data_temp <- temp4[which(temp4$reg_year %in% sample(unique(temp4$reg_year),replace=T)),]
  
  # Run regression with random subsample (data_temp), adjust variables to knots
  fit_boot  <- feols(log(Yield..Tonnes.Hectare.) ~ x + V2 + V3 + year + I(year^2) + winterrainsum + I(winterrainsum^2)  | farm , data_temp)
  
  #Save marginal effect of temperatures,adjust variables to knots 
  spline_fitted_M1[l,]<-as.vector((marginal_temperatures_M1  %*% coef(fit_boot)[1:3])) 
  
  # Show progress in %
  print(l/1000 *100)
  rm(fit_boot,data_temp)
}

# Load the packages
library(fixest)
library(ggplot2)
library(gridExtra)
library(robustbase)
library(egg)
library(matrixStats)
library(tidyverse)

#Change structure
spline_sub_melt_M1 <- melt(spline_fitted_M1)
# Get median response for each column (= a temperature from seq(20,40,0.225))
colmedians_spline_M1 <- colMedians(spline_fitted_M1)
# Get distance from median response for each value
dist_colmedian_M1 <- matrix(NA, nrow=nrow(spline_fitted_M1),ncol=ncol(spline_fitted_M1))

for(l in 1:ncol(spline_fitted_M1)){
  dist_colmedian_M1[,l] <- spline_fitted_M1[,l] - colmedians_spline_M1[l]
}
# Normalize distance to median
dist_colmedian_M1_normalized <- matrix(NA, nrow=nrow(spline_fitted_M1),ncol=ncol(spline_fitted_M1))

for(l in 1:ncol(spline_fitted_M1)){
  dist_colmedian_M1_normalized[,l]<-abs(dist_colmedian_M1[,l]/max(abs(dist_colmedian_M1[,l])))
}

# Change structure
dist_colmedian_M1_normalized_melted <- melt(dist_colmedian_M1_normalized)

# Last preparation before plotting
spline_merged_M1 <- merge(spline_sub_melt_M1,dist_colmedian_M1_normalized_melted,by=c("Var1","Var2"))
spline_merged_M1$value.y_minus1 <- 1-spline_merged_M1$value.y

temp1 <- aggregate(spline_merged_M1$value.y_minus1, list(spline_merged_M1$Var1), median, na.rm=T) 
temp2 <- aggregate(spline_merged_M1$value.x, list(spline_merged_M1$Var2), median, na.rm=T)

temp3 <- aggregate(spline_merged_M1$value.x, list(spline_merged_M1$Var2), quantile, probs=0.025,na.rm=T) 
temp4 <- aggregate(spline_merged_M1$value.x, list(spline_merged_M1$Var2), quantile, probs=0.975,na.rm=T)

spline_merged_M1 <- merge(spline_merged_M1, temp1, by.x=c("Var1"), by.y=c("Group.1"))
spline_merged_M1 <- merge(spline_merged_M1, temp2, by.x=c("Var2"), by.y=c("Group.1"))
colnames(spline_merged_M1)[7]<-"average_effect"

spline_merged_M1<-merge(spline_merged_M1, temp3, by.x=c("Var2"), by.y=c("Group.1"))
colnames(spline_merged_M1)[8]<-"lower_bound"

spline_merged_M1<-merge(spline_merged_M1, temp4, by.x=c("Var2"), by.y=c("Group.1"))
colnames(spline_merged_M1)[9]<-"upper_bound"

temp5 <- cbind(seq(10,50,0.225),seq(1:length(seq(10,50,0.225))))
colnames(temp5) <- c("Temp_bin", "Var2")
spline_merged_M1 <-merge(spline_merged_M1, temp5, by=c("Var2"))

# Plot: hourly temperature effect
model_2 <- ggplot()+ ggtitle("Hourly temperature effects: Equal knots 4") + 
  theme(panel.grid.major = element_blank(),
        plot.title = element_text(family="Times New Roman", size=20, colour = "grey25", hjust=0.5),
        axis.title = element_text(family="Times New Roman", size=16, colour = "grey25"),
        axis.text = element_text(family="Times New Roman", size=16),
        panel.grid.minor = element_blank(),
        panel.background = element_blank(),
        axis.line = element_line(colour = "black"),
        legend.position="none")+
  xlab("Temperature (°C)") + ylab("Log yield response (Kg/ha)")+
  scale_x_continuous(breaks = seq(10,55, by=5), limits = c(5,55))+
  scale_y_continuous(breaks = seq(-.02,.02, by=.004), limits = c(-.02,.02))+
  geom_hline(yintercept=c(seq(-.02,.02,by=0.004)), colour="gray")+
  geom_vline(xintercept=temp_knot_df, linetype="dotted", colour="gray")+
  geom_line(data=spline_merged_M1, aes(y=value.x, x=Temp_bin, group=Var1, alpha=x.x^200/200), size=.01, colour="brown1")+
  geom_line(data=spline_merged_M1, aes(y=average_effect, x=Temp_bin), alpha=0.85, size=.75, colour="brown4")+
  geom_line(data=spline_merged_M1, aes(y=lower_bound, x=Temp_bin)   , alpha=0.85, size=.75, colour="brown4")+       
  geom_line(data=spline_merged_M1, aes(y=upper_bound, x=Temp_bin)   , alpha=0.85, size=.75, colour="brown4")

model_2
ggsave("C:/Users/Shakila Vyas/OneDrive - Wageningen University & Research/PhD-WUR-Insurance/CSA/County analysis/Results/wheatdis_IMD_eq4_dis.png", width = 20, height = 20, units = "cm")

#####################model 4: quantile 4 knots, not used in the main results
temp_knot_df <- c(quantile(temp$temperature,0.2, type=1),
                  quantile(temp$temperature,0.4, type=1),
                  quantile(temp$temperature,0.6, type=1),
                  quantile(temp$temperature,0.8, type=1))

temp_knot_df

# New TS for temperature support of interest- .225 to get the nice plot with small intervals
temp1 <- as.matrix(Hmisc::rcspline.eval(temp$temperature,knots=temp_knot_df,inclx=T))
temp2 <- cbind(temp,temp1)

names(temp2)
head(temp1)

#adjust variables depending on knots
temp3<-temp2 %>%
  group_by(farm,year)%>%
  summarise_at(vars(x,V2,V3),sum)

#join both  data
temp4 <- join(regdata_wheat1,temp3, by=c("farm", "year"))

names(temp4)
temp4$reg_year<- as.numeric(as.factor(temp4$District))*temp4$year

#####bootstrapping
set.seed(123)
marginal_temperatures_M1 <- rcspline.eval(seq(10,50,0.225),knots=temp_knot_df, inclx=T)

# Run 1000 models to derive uncertainty in estimates
spline_fitted_M1 <- matrix(NA,nrow=1000, ncol=nrow(marginal_temperatures_M1))

for (l in 1: 1000){
  # Sample by region times year
  data_temp <- temp4[which(temp4$reg_year %in% sample(unique(temp4$reg_year),replace=T)),]
  
  # Run regression with random subsample (data_temp), adjust variables to knots
  fit_boot  <- feols(log(Yield..Tonnes.Hectare.) ~ x + V2 + V3 + year + I(year^2) + winterrainsum + I(winterrainsum^2)  | farm , data_temp)
  
  #Save marginal effect of temperatures,adjust variables to knots 
  spline_fitted_M1[l,]<-as.vector((marginal_temperatures_M1  %*% coef(fit_boot)[1:3])) 
  
  # Show progress in %
  print(l/1000 *100)
  rm(fit_boot,data_temp)
}

# Load the packages
library(fixest)
library(ggplot2)
library(gridExtra)
library(robustbase)
library(egg)
library(matrixStats)
library(tidyverse)

#Change structure
spline_sub_melt_M1 <- melt(spline_fitted_M1)
# Get median response for each column (= a temperature from seq(20,40,0.225))
colmedians_spline_M1 <- colMedians(spline_fitted_M1)
# Get distance from median response for each value
dist_colmedian_M1 <- matrix(NA, nrow=nrow(spline_fitted_M1),ncol=ncol(spline_fitted_M1))

for(l in 1:ncol(spline_fitted_M1)){
  dist_colmedian_M1[,l] <- spline_fitted_M1[,l] - colmedians_spline_M1[l]
}
# Normalize distance to median
dist_colmedian_M1_normalized <- matrix(NA, nrow=nrow(spline_fitted_M1),ncol=ncol(spline_fitted_M1))

for(l in 1:ncol(spline_fitted_M1)){
  dist_colmedian_M1_normalized[,l]<-abs(dist_colmedian_M1[,l]/max(abs(dist_colmedian_M1[,l])))
}

# Change structure
dist_colmedian_M1_normalized_melted <- melt(dist_colmedian_M1_normalized)

# Last preparation before plotting
spline_merged_M1 <- merge(spline_sub_melt_M1,dist_colmedian_M1_normalized_melted,by=c("Var1","Var2"))
spline_merged_M1$value.y_minus1 <- 1-spline_merged_M1$value.y

temp1 <- aggregate(spline_merged_M1$value.y_minus1, list(spline_merged_M1$Var1), median, na.rm=T) 
temp2 <- aggregate(spline_merged_M1$value.x, list(spline_merged_M1$Var2), median, na.rm=T)

temp3 <- aggregate(spline_merged_M1$value.x, list(spline_merged_M1$Var2), quantile, probs=0.025,na.rm=T) 
temp4 <- aggregate(spline_merged_M1$value.x, list(spline_merged_M1$Var2), quantile, probs=0.975,na.rm=T)

spline_merged_M1 <- merge(spline_merged_M1, temp1, by.x=c("Var1"), by.y=c("Group.1"))
spline_merged_M1 <- merge(spline_merged_M1, temp2, by.x=c("Var2"), by.y=c("Group.1"))
colnames(spline_merged_M1)[7]<-"average_effect"

spline_merged_M1<-merge(spline_merged_M1, temp3, by.x=c("Var2"), by.y=c("Group.1"))
colnames(spline_merged_M1)[8]<-"lower_bound"

spline_merged_M1<-merge(spline_merged_M1, temp4, by.x=c("Var2"), by.y=c("Group.1"))
colnames(spline_merged_M1)[9]<-"upper_bound"

temp5 <- cbind(seq(10,50,0.225),seq(1:length(seq(10,50,0.225))))
colnames(temp5) <- c("Temp_bin", "Var2")
spline_merged_M1 <-merge(spline_merged_M1, temp5, by=c("Var2"))

# Plot: hourly temperature effect
model_2 <- ggplot()+ ggtitle("Hourly temperature effects: Quantile knots 4") + 
  theme(panel.grid.major = element_blank(),
        plot.title = element_text(family="Times New Roman", size=20, colour = "grey25", hjust=0.5),
        axis.title = element_text(family="Times New Roman", size=16, colour = "grey25"),
        axis.text = element_text(family="Times New Roman", size=16),
        panel.grid.minor = element_blank(),
        panel.background = element_blank(),
        axis.line = element_line(colour = "black"),
        legend.position="none")+
  xlab("Temperature (°C)") + ylab("Log yield response (Kg/ha)")+
  scale_x_continuous(breaks = seq(10,55, by=5), limits = c(5,55))+
  scale_y_continuous(breaks = seq(-.02,.02, by=.004), limits = c(-.02,.02))+
  geom_hline(yintercept=c(seq(-.02,.02,by=0.004)), colour="gray")+
  geom_vline(xintercept=temp_knot_df, linetype="dotted", colour="gray")+
  geom_line(data=spline_merged_M1, aes(y=value.x, x=Temp_bin, group=Var1, alpha=x.x^200/200), size=.01, colour="brown1")+
  geom_line(data=spline_merged_M1, aes(y=average_effect, x=Temp_bin), alpha=0.85, size=.75, colour="brown4")+
  geom_line(data=spline_merged_M1, aes(y=lower_bound, x=Temp_bin)   , alpha=0.85, size=.75, colour="brown4")+       
  geom_line(data=spline_merged_M1, aes(y=upper_bound, x=Temp_bin)   , alpha=0.85, size=.75, colour="brown4")

model_2
ggsave("C:/Users/Shakila Vyas/OneDrive - Wageningen University & Research/PhD-WUR-Insurance/CSA/County analysis/Results/wheatdis_IMD_qq4_dis.png", width = 20, height = 20, units = "cm")

################################placing knots at 5 degree intervals, not used in main results
hist(temp$temperature)
temp_knot_df <- c(10,20,30)

temp_knot_df

# New TS for temperature support of interest- .225 to get the nice plot with small intervals
temp1 <- as.matrix(Hmisc::rcspline.eval(temp$temperature,knots=temp_knot_df,inclx=T))
temp2 <- cbind(temp,temp1)

names(temp2)
head(temp1)

#adjust variables depending on knots
temp3<-temp2 %>%
  group_by(farm,year)%>%
  summarise_at(vars(x,V2),sum)

#join both  data
temp4 <- join(regdata_wheat1,temp3, by=c("farm", "year"))

names(temp4)
temp4$reg_year<- as.numeric(as.factor(temp4$District))*temp4$year

#####bootstrapping
set.seed(123)
marginal_temperatures_M1 <- rcspline.eval(seq(10,50,0.225),knots=temp_knot_df, inclx=T)

# Run 1000 models to derive uncertainty in estimates
spline_fitted_M1 <- matrix(NA,nrow=1000, ncol=nrow(marginal_temperatures_M1))

for (l in 1: 1000){
  # Sample by region times year
  data_temp <- temp4[which(temp4$reg_year %in% sample(unique(temp4$reg_year),replace=T)),]
  
  # Run regression with random subsample (data_temp), adjust variables to knots
  fit_boot  <- feols(log(Yield..Tonnes.Hectare.) ~ x + V2  + year + I(year^2) + winterrainsum + I(winterrainsum^2)  | farm , data_temp)
  
  #Save marginal effect of temperatures,adjust variables to knots 
  spline_fitted_M1[l,]<-as.vector((marginal_temperatures_M1  %*% coef(fit_boot)[1:2])) 
  
  # Show progress in %
  print(l/1000 *100)
  rm(fit_boot,data_temp)
}

# Load the packages
library(fixest)
library(ggplot2)
library(gridExtra)
library(robustbase)
library(egg)
library(matrixStats)
library(tidyverse)

#Change structure
spline_sub_melt_M1 <- melt(spline_fitted_M1)
# Get median response for each column (= a temperature from seq(20,40,0.225))
colmedians_spline_M1 <- colMedians(spline_fitted_M1)
# Get distance from median response for each value
dist_colmedian_M1 <- matrix(NA, nrow=nrow(spline_fitted_M1),ncol=ncol(spline_fitted_M1))

for(l in 1:ncol(spline_fitted_M1)){
  dist_colmedian_M1[,l] <- spline_fitted_M1[,l] - colmedians_spline_M1[l]
}
# Normalize distance to median
dist_colmedian_M1_normalized <- matrix(NA, nrow=nrow(spline_fitted_M1),ncol=ncol(spline_fitted_M1))

for(l in 1:ncol(spline_fitted_M1)){
  dist_colmedian_M1_normalized[,l]<-abs(dist_colmedian_M1[,l]/max(abs(dist_colmedian_M1[,l])))
}

# Change structure
dist_colmedian_M1_normalized_melted <- melt(dist_colmedian_M1_normalized)

# Last preparation before plotting
spline_merged_M1 <- merge(spline_sub_melt_M1,dist_colmedian_M1_normalized_melted,by=c("Var1","Var2"))
spline_merged_M1$value.y_minus1 <- 1-spline_merged_M1$value.y

temp1 <- aggregate(spline_merged_M1$value.y_minus1, list(spline_merged_M1$Var1), median, na.rm=T) 
temp2 <- aggregate(spline_merged_M1$value.x, list(spline_merged_M1$Var2), median, na.rm=T)

temp3 <- aggregate(spline_merged_M1$value.x, list(spline_merged_M1$Var2), quantile, probs=0.025,na.rm=T) 
temp4 <- aggregate(spline_merged_M1$value.x, list(spline_merged_M1$Var2), quantile, probs=0.975,na.rm=T)

spline_merged_M1 <- merge(spline_merged_M1, temp1, by.x=c("Var1"), by.y=c("Group.1"))
spline_merged_M1 <- merge(spline_merged_M1, temp2, by.x=c("Var2"), by.y=c("Group.1"))
colnames(spline_merged_M1)[7]<-"average_effect"

spline_merged_M1<-merge(spline_merged_M1, temp3, by.x=c("Var2"), by.y=c("Group.1"))
colnames(spline_merged_M1)[8]<-"lower_bound"

spline_merged_M1<-merge(spline_merged_M1, temp4, by.x=c("Var2"), by.y=c("Group.1"))
colnames(spline_merged_M1)[9]<-"upper_bound"

temp5 <- cbind(seq(10,50,0.225),seq(1:length(seq(10,50,0.225))))
colnames(temp5) <- c("Temp_bin", "Var2")
spline_merged_M1 <-merge(spline_merged_M1, temp5, by=c("Var2"))

# Plot: hourly temperature effect

model_4 <- ggplot()+ ggtitle("Hourly temperature effects: 5 degree knots ") + 
  theme(panel.grid.major = element_blank(),
        plot.title = element_text(family="Times New Roman", size=20, colour = "grey25", hjust=0.5),
        axis.title = element_text(family="Times New Roman", size=16, colour = "grey25"),
        axis.text = element_text(family="Times New Roman", size=16),
        panel.grid.minor = element_blank(),
        panel.background = element_blank(),
        axis.line = element_line(colour = "black"),
        legend.position="none")+
  xlab("Temperature (°C)") + ylab("Log yield response (Kg/ha)")+
  scale_x_continuous(breaks = seq(10,55, by=5), limits = c(5,55))+
  scale_y_continuous(breaks = seq(-.02,.02, by=.004), limits = c(-.02,.02))+
  geom_hline(yintercept=c(seq(-.02,.02,by=0.004)), colour="gray")+
  geom_vline(xintercept=temp_knot_df, linetype="dotted", colour="gray")+
  geom_line(data=spline_merged_M1, aes(y=value.x, x=Temp_bin, group=Var1, alpha=x.x^200/200), size=.01, colour="brown1")+
  geom_line(data=spline_merged_M1, aes(y=average_effect, x=Temp_bin), alpha=0.85, size=.75, colour="brown4")+
  geom_line(data=spline_merged_M1, aes(y=lower_bound, x=Temp_bin)   , alpha=0.85, size=.75, colour="brown4")+       
  geom_line(data=spline_merged_M1, aes(y=upper_bound, x=Temp_bin)   , alpha=0.85, size=.75, colour="brown4")

model_4

ggsave("C:/Users/Shakila Vyas/OneDrive - Wageningen University & Research/PhD-WUR-Insurance/CSA/County analysis/Results/wheatdis_IMD_5degree3_dis.png", width = 20, height = 20, units = "cm")

################################placing knots at 5 degree intervals, not used in main results
hist(temp$temperature)
temp_knot_df <- c(20,25,30)

temp_knot_df

# New TS for temperature support of interest- .225 to get the nice plot with small intervals
temp1 <- as.matrix(Hmisc::rcspline.eval(temp$temperature,knots=temp_knot_df,inclx=T))
temp2 <- cbind(temp,temp1)

names(temp2)
head(temp1)

#adjust variables depending on knots
temp3<-temp2 %>%
  group_by(farm,year)%>%
  summarise_at(vars(x,V2),sum)

#match names with temp
names(regdata_soy1)
colnames(regdata_soy1)[colnames(regdata_soy1) == "Year"] <- "year"
colnames(regdata_soy1)[colnames(regdata_soy1) == "Dis_codename"] <- "farm"
names(regdata_soy1)

#join both  data
temp4 <- join(regdata_soy1,temp3, by=c("farm", "year"))

names(temp4)
temp4$reg_year<- as.numeric(as.factor(temp4$District))*temp4$year

#####bootstrapping
set.seed(123)
marginal_temperatures_M1 <- rcspline.eval(seq(10,50,0.225),knots=temp_knot_df, inclx=T)

# Run 1000 models to derive uncertainty in estimates
spline_fitted_M1 <- matrix(NA,nrow=1000, ncol=nrow(marginal_temperatures_M1))

for (l in 1: 1000){
  # Sample by region times year
  data_temp <- temp4[which(temp4$reg_year %in% sample(unique(temp4$reg_year),replace=T)),]
  
  # Run regression with random subsample (data_temp), adjust variables to knots
  fit_boot  <- feols(log(Yield..Tonnes.Hectare.) ~ x + V2  + year + I(year^2) + summerrainsum + I(summerrainsum^2)  | farm , data_temp)
  
  #Save marginal effect of temperatures,adjust variables to knots 
  spline_fitted_M1[l,]<-as.vector((marginal_temperatures_M1  %*% coef(fit_boot)[1:2])) 
  
  # Show progress in %
  print(l/1000 *100)
  rm(fit_boot,data_temp)
}

# Load the packages
library(fixest)
library(ggplot2)
library(gridExtra)
library(robustbase)
library(egg)
library(matrixStats)
library(tidyverse)

#Change structure
spline_sub_melt_M1 <- melt(spline_fitted_M1)
# Get median response for each column (= a temperature from seq(20,40,0.225))
colmedians_spline_M1 <- colMedians(spline_fitted_M1)
# Get distance from median response for each value
dist_colmedian_M1 <- matrix(NA, nrow=nrow(spline_fitted_M1),ncol=ncol(spline_fitted_M1))

for(l in 1:ncol(spline_fitted_M1)){
  dist_colmedian_M1[,l] <- spline_fitted_M1[,l] - colmedians_spline_M1[l]
}
# Normalize distance to median
dist_colmedian_M1_normalized <- matrix(NA, nrow=nrow(spline_fitted_M1),ncol=ncol(spline_fitted_M1))

for(l in 1:ncol(spline_fitted_M1)){
  dist_colmedian_M1_normalized[,l]<-abs(dist_colmedian_M1[,l]/max(abs(dist_colmedian_M1[,l])))
}

# Change structure
dist_colmedian_M1_normalized_melted <- melt(dist_colmedian_M1_normalized)

# Last preparation before plotting
spline_merged_M1 <- merge(spline_sub_melt_M1,dist_colmedian_M1_normalized_melted,by=c("Var1","Var2"))
spline_merged_M1$value.y_minus1 <- 1-spline_merged_M1$value.y

temp1 <- aggregate(spline_merged_M1$value.y_minus1, list(spline_merged_M1$Var1), median, na.rm=T) 
temp2 <- aggregate(spline_merged_M1$value.x, list(spline_merged_M1$Var2), median, na.rm=T)

temp3 <- aggregate(spline_merged_M1$value.x, list(spline_merged_M1$Var2), quantile, probs=0.025,na.rm=T) 
temp4 <- aggregate(spline_merged_M1$value.x, list(spline_merged_M1$Var2), quantile, probs=0.975,na.rm=T)

spline_merged_M1 <- merge(spline_merged_M1, temp1, by.x=c("Var1"), by.y=c("Group.1"))
spline_merged_M1 <- merge(spline_merged_M1, temp2, by.x=c("Var2"), by.y=c("Group.1"))
colnames(spline_merged_M1)[7]<-"average_effect"

spline_merged_M1<-merge(spline_merged_M1, temp3, by.x=c("Var2"), by.y=c("Group.1"))
colnames(spline_merged_M1)[8]<-"lower_bound"

spline_merged_M1<-merge(spline_merged_M1, temp4, by.x=c("Var2"), by.y=c("Group.1"))
colnames(spline_merged_M1)[9]<-"upper_bound"

temp5 <- cbind(seq(10,50,0.225),seq(1:length(seq(10,50,0.225))))
colnames(temp5) <- c("Temp_bin", "Var2")
spline_merged_M1 <-merge(spline_merged_M1, temp5, by=c("Var2"))

# Plot: hourly temperature effect
model_4 <- ggplot()+ ggtitle("Hourly temperature effects: 5 degree knots (alternate) ") + 
  theme(panel.grid.major = element_blank(),
        plot.title = element_text(family="Times New Roman", size=20, colour = "grey25", hjust=0.5),
        axis.title = element_text(family="Times New Roman", size=16, colour = "grey25"),
        axis.text = element_text(family="Times New Roman", size=16),
        panel.grid.minor = element_blank(),
        panel.background = element_blank(),
        axis.line = element_line(colour = "black"),
        legend.position="none")+
  xlab("Temperature (°C)") + ylab("Log yield response (Kg/ha)")+
  scale_x_continuous(breaks = seq(10,55, by=5), limits = c(5,55))+
  scale_y_continuous(breaks = seq(-.02,.02, by=.004), limits = c(-.02,.02))+
  geom_hline(yintercept=c(seq(-.02,.02,by=0.004)), colour="gray")+
  geom_vline(xintercept=temp_knot_df, linetype="dotted", colour="gray")+
  geom_line(data=spline_merged_M1, aes(y=value.x, x=Temp_bin, group=Var1, alpha=x.x^200/200), size=.01, colour="brown1")+
  geom_line(data=spline_merged_M1, aes(y=average_effect, x=Temp_bin), alpha=0.85, size=.75, colour="brown4")+
  geom_line(data=spline_merged_M1, aes(y=lower_bound, x=Temp_bin)   , alpha=0.85, size=.75, colour="brown4")+       
  geom_line(data=spline_merged_M1, aes(y=upper_bound, x=Temp_bin)   , alpha=0.85, size=.75, colour="brown4")

model_4
ggsave("C:/Users/Shakila Vyas/OneDrive - Wageningen University & Research/PhD-WUR-Insurance/CSA/County analysis/Results/soydis_IMD_5degree3_20_25_30_discombo.png", width = 20, height = 20, units = "cm")

################################placing knots at 5 degree intervals, not used in main results
hist(temp$temperature)
temp_knot_df <- c(20,25,30,35)

temp_knot_df

# New TS for temperature support of interest- .225 to get the nice plot with small intervals
temp1 <- as.matrix(Hmisc::rcspline.eval(temp$temperature,knots=temp_knot_df,inclx=T))
temp2 <- cbind(temp,temp1)

names(temp2)
head(temp1)

#adjust variables depending on knots
temp3<-temp2 %>%
  group_by(farm,year)%>%
  summarise_at(vars(x,V2,V3),sum)

#match names with temp
names(regdata_soy1)
colnames(regdata_soy1)[colnames(regdata_soy1) == "Year"] <- "year"
colnames(regdata_soy1)[colnames(regdata_soy1) == "Dis_codename"] <- "farm"
names(regdata_soy1)

#join both  data
temp4 <- join(regdata_soy1,temp3, by=c("farm", "year"))

names(temp4)
temp4$reg_year<- as.numeric(as.factor(temp4$District))*temp4$year

#####bootstrapping
set.seed(123)
marginal_temperatures_M1 <- rcspline.eval(seq(10,50,0.225),knots=temp_knot_df, inclx=T)

# Run 1000 models to derive uncertainty in estimates
spline_fitted_M1 <- matrix(NA,nrow=1000, ncol=nrow(marginal_temperatures_M1))

for (l in 1: 1000){
  # Sample by region times year
  data_temp <- temp4[which(temp4$reg_year %in% sample(unique(temp4$reg_year),replace=T)),]
  
  # Run regression with random subsample (data_temp), adjust variables to knots
  fit_boot  <- feols(log(Yield..Tonnes.Hectare.) ~ x + V2  + V3+ year + I(year^2) + summerrainsum + I(summerrainsum^2)  | farm , data_temp)
  
  #Save marginal effect of temperatures,adjust variables to knots 
  spline_fitted_M1[l,]<-as.vector((marginal_temperatures_M1  %*% coef(fit_boot)[1:3])) 
  
  # Show progress in %
  print(l/1000 *100)
  rm(fit_boot,data_temp)
}

# Load the packages
library(fixest)
library(ggplot2)
library(gridExtra)
library(robustbase)
library(egg)
library(matrixStats)
library(tidyverse)

#Change structure
spline_sub_melt_M1 <- melt(spline_fitted_M1)
# Get median response for each column (= a temperature from seq(20,40,0.225))
colmedians_spline_M1 <- colMedians(spline_fitted_M1)
# Get distance from median response for each value
dist_colmedian_M1 <- matrix(NA, nrow=nrow(spline_fitted_M1),ncol=ncol(spline_fitted_M1))

for(l in 1:ncol(spline_fitted_M1)){
  dist_colmedian_M1[,l] <- spline_fitted_M1[,l] - colmedians_spline_M1[l]
}
# Normalize distance to median
dist_colmedian_M1_normalized <- matrix(NA, nrow=nrow(spline_fitted_M1),ncol=ncol(spline_fitted_M1))

for(l in 1:ncol(spline_fitted_M1)){
  dist_colmedian_M1_normalized[,l]<-abs(dist_colmedian_M1[,l]/max(abs(dist_colmedian_M1[,l])))
}

# Change structure
dist_colmedian_M1_normalized_melted <- melt(dist_colmedian_M1_normalized)

# Last preparation before plotting
spline_merged_M1 <- merge(spline_sub_melt_M1,dist_colmedian_M1_normalized_melted,by=c("Var1","Var2"))
spline_merged_M1$value.y_minus1 <- 1-spline_merged_M1$value.y

temp1 <- aggregate(spline_merged_M1$value.y_minus1, list(spline_merged_M1$Var1), median, na.rm=T) 
temp2 <- aggregate(spline_merged_M1$value.x, list(spline_merged_M1$Var2), median, na.rm=T)

temp3 <- aggregate(spline_merged_M1$value.x, list(spline_merged_M1$Var2), quantile, probs=0.025,na.rm=T) 
temp4 <- aggregate(spline_merged_M1$value.x, list(spline_merged_M1$Var2), quantile, probs=0.975,na.rm=T)

spline_merged_M1 <- merge(spline_merged_M1, temp1, by.x=c("Var1"), by.y=c("Group.1"))
spline_merged_M1 <- merge(spline_merged_M1, temp2, by.x=c("Var2"), by.y=c("Group.1"))
colnames(spline_merged_M1)[7]<-"average_effect"

spline_merged_M1<-merge(spline_merged_M1, temp3, by.x=c("Var2"), by.y=c("Group.1"))
colnames(spline_merged_M1)[8]<-"lower_bound"

spline_merged_M1<-merge(spline_merged_M1, temp4, by.x=c("Var2"), by.y=c("Group.1"))
colnames(spline_merged_M1)[9]<-"upper_bound"

temp5 <- cbind(seq(10,50,0.225),seq(1:length(seq(10,50,0.225))))
colnames(temp5) <- c("Temp_bin", "Var2")
spline_merged_M1 <-merge(spline_merged_M1, temp5, by=c("Var2"))

# Plot: hourly temperature effect
model_4 <- ggplot()+ ggtitle("Hourly temperature effects: 5 degree knots ") + 
  theme(panel.grid.major = element_blank(),
        plot.title = element_text(family="Times New Roman", size=20, colour = "grey25", hjust=0.5),
        axis.title = element_text(family="Times New Roman", size=16, colour = "grey25"),
        axis.text = element_text(family="Times New Roman", size=16),
        panel.grid.minor = element_blank(),
        panel.background = element_blank(),
        axis.line = element_line(colour = "black"),
        legend.position="none")+
  xlab("Temperature (°C)") + ylab("Log yield response (Kg/ha)")+
  scale_x_continuous(breaks = seq(10,55, by=5), limits = c(5,55))+
  scale_y_continuous(breaks = seq(-.04,.04, by=.004), limits = c(-.04,.04))+
  geom_hline(yintercept=c(seq(-.04,.04,by=0.004)), colour="gray")+
  geom_vline(xintercept=temp_knot_df, linetype="dotted", colour="gray")+
  geom_line(data=spline_merged_M1, aes(y=value.x, x=Temp_bin, group=Var1, alpha=x.x^200/200), size=.01, colour="brown1")+
  geom_line(data=spline_merged_M1, aes(y=average_effect, x=Temp_bin), alpha=0.85, size=.75, colour="brown4")+
  geom_line(data=spline_merged_M1, aes(y=lower_bound, x=Temp_bin)   , alpha=0.85, size=.75, colour="brown4")+       
  geom_line(data=spline_merged_M1, aes(y=upper_bound, x=Temp_bin)   , alpha=0.85, size=.75, colour="brown4")

model_4
ggsave("C:/Users/Shakila Vyas/OneDrive - Wageningen University & Research/PhD-WUR-Insurance/CSA/County analysis/Results/soydis_IMD_5degree3_knots4_discombo.png", width = 20, height = 20, units = "cm")

#####best fit model
#best knots based on RSS
# lower and upper bound define lowest and highest knot location
lower_bound <- ceiling(quantile(temp$temperature,0.05, type=1))
upper_bound <- floor(quantile(temp$temperature,0.95, type=1))
temp_range <- seq(lower_bound, upper_bound,1) 
kn3_combi <- combn(temp_range,3, simplify = T)
# Minimum space between 2 knots
required_space <- 5
# Get all knot combinations
differences <- matrix(NA, nrow=3, ncol=ncol(kn3_combi))
differences[1,] <- abs(kn3_combi[1,] - kn3_combi[2,])
differences[2,] <- abs(kn3_combi[1,] - kn3_combi[3,])
differences[3,] <- abs(kn3_combi[2,] - kn3_combi[3,])
knots_3_combi_good <- kn3_combi[,which(differences[1,] >= required_space & differences[2,] >= required_space & differences[3,] >= required_space)]
# vector containing residual sum of squares (RSS)
RSS_3knots <- vector(length=ncol(knots_3_combi_good ))

# Get the RSS for each model
for (c in 1:ncol(knots_3_combi_good)){
  
  # Get new time series and aggregate hourly values  
  temp1 <- as.matrix(rcspline.eval(temp$temperature,knots= knots_3_combi_good[,c], inclx=T))
  temp2 <- cbind(temp,temp1)
  
  temp3<-temp2 %>%
    group_by(farm,year)%>%
    summarise_at(vars(x,V2),sum)
  
  # Match yearly values with yearly yields
  temp4 <- join(regdata_soy1,temp3, by=c("farm", "year"))
  
  # Run regression
  temp_reg <- feols(log(Yield..Tonnes.Hectare.) ~ x + V2  +  year + I(year^2) + summerrainsum + I(summerrainsum^2)  | farm , temp4)
  RSS_3knots[c] <- sum(resid(temp_reg)^2)
  
}

# Best number of knots
temp_knot_df <- knots_3_combi_good[,which.min(RSS_3knots)]
rm(RSS_3knots, knots_3_combi_good, differences, kn3_combi,temp_range,upper_bound, lower_bound)

# resume reg
temp1 <- as.matrix(Hmisc::rcspline.eval(temp$temperature,knots=temp_knot_df,inclx=T))
temp2 <- cbind(temp,temp1)

names(temp2)
head(temp1)

#adjust variables depending on knots
temp3<-temp2 %>%
  group_by(farm,year)%>%
  summarise_at(vars(x,V2),sum)

#match names with temp
names(regdata_soy1)
colnames(regdata_soy1)[colnames(regdata_soy1) == "Year"] <- "year"
colnames(regdata_soy1)[colnames(regdata_soy1) == "Dis_codename"] <- "farm"
names(regdata_soy1)

#join both  data
temp4 <- join(regdata_soy1,temp3, by=c("farm", "year"))

names(temp4)
temp4$reg_year<- as.numeric(as.factor(temp4$District))*temp4$year

#####bootstrapping
set.seed(123)
marginal_temperatures_M1 <- rcspline.eval(seq(10,50,0.225),knots=temp_knot_df, inclx=T)

# Run 1000 models to derive uncertainty in estimates
spline_fitted_M1 <- matrix(NA,nrow=1000, ncol=nrow(marginal_temperatures_M1))

for (l in 1: 1000){
  # Sample by region times year
  data_temp <- temp4[which(temp4$reg_year %in% sample(unique(temp4$reg_year),replace=T)),]
  
  # Run regression with random subsample (data_temp), adjust variables to knots
  fit_boot  <- feols(log(Yield..Tonnes.Hectare.) ~ x + V2  +  year + I(year^2) + summerrainsum + I(summerrainsum^2)  | farm , data_temp)
  
  #Save marginal effect of temperatures,adjust variables to knots 
  spline_fitted_M1[l,]<-as.vector((marginal_temperatures_M1  %*% coef(fit_boot)[1:2])) 
  
  # Show progress in %
  print(l/1000 *100)
  rm(fit_boot,data_temp)
}

#Change structure
spline_sub_melt_M1 <- melt(spline_fitted_M1)
# Get median response for each column (= a temperature from seq(20,40,0.225))
colmedians_spline_M1 <- colMedians(spline_fitted_M1)
# Get distance from median response for each value
dist_colmedian_M1 <- matrix(NA, nrow=nrow(spline_fitted_M1),ncol=ncol(spline_fitted_M1))

for(l in 1:ncol(spline_fitted_M1)){
  dist_colmedian_M1[,l] <- spline_fitted_M1[,l] - colmedians_spline_M1[l]
}
# Normalize distance to median
dist_colmedian_M1_normalized <- matrix(NA, nrow=nrow(spline_fitted_M1),ncol=ncol(spline_fitted_M1))

for(l in 1:ncol(spline_fitted_M1)){
  dist_colmedian_M1_normalized[,l]<-abs(dist_colmedian_M1[,l]/max(abs(dist_colmedian_M1[,l])))
}

# Change structure
dist_colmedian_M1_normalized_melted <- melt(dist_colmedian_M1_normalized)

# Last preparation before plotting
spline_merged_M1 <- merge(spline_sub_melt_M1,dist_colmedian_M1_normalized_melted,by=c("Var1","Var2"))
spline_merged_M1$value.y_minus1 <- 1-spline_merged_M1$value.y

temp1 <- aggregate(spline_merged_M1$value.y_minus1, list(spline_merged_M1$Var1), median, na.rm=T) 
temp2 <- aggregate(spline_merged_M1$value.x, list(spline_merged_M1$Var2), median, na.rm=T)

temp3 <- aggregate(spline_merged_M1$value.x, list(spline_merged_M1$Var2), quantile, probs=0.025,na.rm=T) 
temp4 <- aggregate(spline_merged_M1$value.x, list(spline_merged_M1$Var2), quantile, probs=0.975,na.rm=T)

spline_merged_M1 <- merge(spline_merged_M1, temp1, by.x=c("Var1"), by.y=c("Group.1"))
spline_merged_M1 <- merge(spline_merged_M1, temp2, by.x=c("Var2"), by.y=c("Group.1"))
colnames(spline_merged_M1)[7]<-"average_effect"

spline_merged_M1<-merge(spline_merged_M1, temp3, by.x=c("Var2"), by.y=c("Group.1"))
colnames(spline_merged_M1)[8]<-"lower_bound"

spline_merged_M1<-merge(spline_merged_M1, temp4, by.x=c("Var2"), by.y=c("Group.1"))
colnames(spline_merged_M1)[9]<-"upper_bound"

temp5 <- cbind(seq(10,50,0.225),seq(1:length(seq(10,50,0.225))))
colnames(temp5) <- c("Temp_bin", "Var2")
spline_merged_M1 <-merge(spline_merged_M1, temp5, by=c("Var2"))

# Plot: hourly temperature effect
model_4 <- ggplot()+ ggtitle("Hourly temperature effects: Best fit") + 
  theme(panel.grid.major = element_blank(),
        plot.title = element_text(family="Times New Roman", size=20, colour = "grey25", hjust=0.5),
        axis.title = element_text(family="Times New Roman", size=16, colour = "grey25"),
        axis.text = element_text(family="Times New Roman", size=16),
        panel.grid.minor = element_blank(),
        panel.background = element_blank(),
        axis.line = element_line(colour = "black"),
        legend.position="none")+
  xlab("Temperature (°C)") + ylab("Log yield response (Kg/ha)")+
  scale_x_continuous(breaks = seq(10,55, by=5), limits = c(5,55))+
  scale_y_continuous(breaks = seq(-.02,.02, by=.004), limits = c(-.02,.02))+
  geom_hline(yintercept=c(seq(-.02,.02,by=0.004)), colour="gray")+
  geom_vline(xintercept=temp_knot_df, linetype="dotted", colour="gray")+
  geom_line(data=spline_merged_M1, aes(y=value.x, x=Temp_bin, group=Var1, alpha=x.x^200/200), size=.01, colour="brown1")+
  geom_line(data=spline_merged_M1, aes(y=average_effect, x=Temp_bin), alpha=0.85, size=.75, colour="brown4")+
  geom_line(data=spline_merged_M1, aes(y=lower_bound, x=Temp_bin)   , alpha=0.85, size=.75, colour="brown4")+       
  geom_line(data=spline_merged_M1, aes(y=upper_bound, x=Temp_bin)   , alpha=0.85, size=.75, colour="brown4")

model_4
ggsave("C:/Users/Shakila Vyas/OneDrive - Wageningen University & Research/PhD-WUR-Insurance/CSA/County analysis/Results/soydis_IMD_bestfit_discombo.png", width = 20, height = 20, units = "cm")

###diff outlier removal in wheat, removing districts with area < 2000 hectares, not used in the main results
library(raster)
library(dplyr)
library(data.table)
library(reshape2)
library(Hmisc)
library(lubridate)
library(plyr)
library(esmisc)
library(sandwich)
library(MASS)
library(e1071)
library(fixest)

data<- read.csv("C:/Users/Shakila Vyas/OneDrive - Wageningen University & Research/PhD-WUR-Insurance/CSA/County analysis/Dis yield_All India/Wheat_yields_rev.csv")
head(data)
#remove less than 3 years data
data1<- add_count(data, Dist_code)
regdata_wheat<- subset(data1,n>=3,select = Crop.Production.Statistics:area)
head(regdata_wheat)
#remove districts with area < 2000 hectares
regdata_wheat<- subset(regdata_wheat,area==1,select = Crop.Production.Statistics:area)

##preparing data
hourly<- read.csv("C:/Users/Shakila Vyas/OneDrive - Wageningen University & Research/PhD-WUR-Insurance/CSA/County analysis/Weather data/IMD/Winter/hourlytpimddis15.csv")
#need this format-colnames(temp_wheat_exposure2) <- c("Farm","Date","Hour", "Temperature")
as.data.frame(hourly)
library(reshape2)
long <- reshape2::melt(hourly, id.vars = c("X1"))
long$year<- 2015

hourly1<- read.csv("C:/Users/Shakila Vyas/OneDrive - Wageningen University & Research/PhD-WUR-Insurance/CSA/County analysis/Weather data/IMD/Winter/hourlytpimddis16.csv")
#need this format-colnames(temp_wheat_exposure2) <- c("Farm","Date","Hour", "Temperature")
as.data.frame(hourly1)
long1 <- reshape2::melt(hourly1, id.vars = c("X1"))
long1$year<- 2016

hourly2<- read.csv("C:/Users/Shakila Vyas/OneDrive - Wageningen University & Research/PhD-WUR-Insurance/CSA/County analysis/Weather data/IMD/Winter/hourlytpimddis17.csv")
#need this format-colnames(temp_wheat_exposure2) <- c("Farm","Date","Hour", "Temperature")
as.data.frame(hourly2)
long2 <- reshape2::melt(hourly2, id.vars = c("X1"))
long2$year<- 2017

hourly3<- read.csv("C:/Users/Shakila Vyas/OneDrive - Wageningen University & Research/PhD-WUR-Insurance/CSA/County analysis/Weather data/IMD/Winter/hourlytpimddis18.csv")
#need this format-colnames(temp_wheat_exposure2) <- c("Farm","Date","Hour", "Temperature")
as.data.frame(hourly3)
long3 <- reshape2::melt(hourly3, id.vars = c("X1"))
long3$year<- 2018

hourly4<- read.csv("C:/Users/Shakila Vyas/OneDrive - Wageningen University & Research/PhD-WUR-Insurance/CSA/County analysis/Weather data/IMD/Winter/hourlytpimddis19.csv")
#need this format-colnames(temp_wheat_exposure2) <- c("Farm","Date","Hour", "Temperature")
as.data.frame(hourly4)
long4 <- reshape2::melt(hourly4, id.vars = c("X1"))
long4$year<- 2019

hourly5<- read.csv("C:/Users/Shakila Vyas/OneDrive - Wageningen University & Research/PhD-WUR-Insurance/CSA/County analysis/Weather data/IMD/Winter/hourlytpimddis20.csv")
#need this format-colnames(temp_wheat_exposure2) <- c("Farm","Date","Hour", "Temperature")
as.data.frame(hourly5)
long5 <- reshape2::melt(hourly5, id.vars = c("X1"))
long5$year<- 2020

temp<- rbind(long,long1,long2,long3,long4,long5)

#rename temp data
names(temp)[3] <- "temperature"
names(temp)[1] <- "farm"
as.numeric(temp$temperature)
min(temp$temperature)
max(temp$temperature)
range(temp$temperature)
summary(temp$temperature)
hist(temp$temperature,main="Hourly temperature district-Winter")

range(temp$temperature)
temp <- na.omit(temp)


#####################model 1: equal spaced 3 knots
temp_knot_df <- c(min(temp$temperature) + ((max(temp$temperature)-min(temp$temperature)) / 4),
                  min(temp$temperature) + 2* ((max(temp$temperature)-min(temp$temperature)) / 4),
                  min(temp$temperature) + 3* ((max(temp$temperature)-min(temp$temperature)) / 4))

# 10%, 50%, 90% quantile
temp_knot_df <- c(quantile(temp$temperature,0.1, type=1),
                  quantile(temp$temperature,0.5, type=1),
                  quantile(temp$temperature,0.9, type=1))

# New TS for temperature support of interest- .225 to get the nice plot with small intervals
temp1 <- as.matrix(Hmisc::rcspline.eval(temp$temperature,knots=temp_knot_df,inclx=T))
temp2 <- cbind(temp,temp1)

names(temp2)
head(temp1)

#adjust variables depending on knots
temp3<-temp2 %>%
  group_by(farm,year)%>%
  summarise_at(vars(x,V2),sum)

#match names with temp
names(regdata_wheat)
colnames(regdata_wheat)[colnames(regdata_wheat) == "Year.1"] <- "year"
colnames(regdata_wheat)[colnames(regdata_wheat) == "Dist_code"] <- "farm"

#join both  data
temp4 <- join(regdata_wheat,temp3, by=c("farm", "year"))

names(temp4)
temp4$reg_year<- as.numeric(as.factor(temp4$District))*temp4$year

library(fixest)
wheat_yield_spline = feols(log(Yield..Tonnes.Hectare.) ~ x + V2 + year + I(year^2) + winterrainsum + I(winterrainsum^2)   | farm , cluster = c( "year","farm"), temp4)
summary(wheat_yield_spline)

#####bootstrapping
set.seed(123)
marginal_temperatures_M1 <- rcspline.eval(seq(5,55,0.225),knots=temp_knot_df, inclx=T)

# Run 1000 models to derive uncertainty in estimates
spline_fitted_M1 <- matrix(NA,nrow=1000, ncol=nrow(marginal_temperatures_M1))

for (l in 1: 1000){
  # Sample by region times year
  data_temp <- temp4[which(temp4$reg_year %in% sample(unique(temp4$reg_year),replace=T)),]
  
  # Run regression with random subsample (data_temp), adjust variables to knots
  fit_boot  <- feols(Yield..Tonnes.Hectare. ~ x + V2  + year + I(year^2) + winterrainsum + I(winterrainsum^2)  | farm , data_temp)
  
  #Save marginal effect of temperatures,adjust variables to knots 
  spline_fitted_M1[l,]<-as.vector((marginal_temperatures_M1  %*% coef(fit_boot)[1:2])) 
  
  # Show progress in %
  print(l/1000 *100)
  rm(fit_boot,data_temp)
}

# Load the packages
library(fixest)
library(ggplot2)
library(gridExtra)
library(robustbase)
library(egg)
library(matrixStats)
library(tidyverse)

#Change structure
spline_sub_melt_M1 <- melt(spline_fitted_M1)
# Get median response for each column (= a temperature from seq(20,40,0.225))
colmedians_spline_M1 <- colMedians(spline_fitted_M1)
# Get distance from median response for each value
dist_colmedian_M1 <- matrix(NA, nrow=nrow(spline_fitted_M1),ncol=ncol(spline_fitted_M1))

for(l in 1:ncol(spline_fitted_M1)){
  dist_colmedian_M1[,l] <- spline_fitted_M1[,l] - colmedians_spline_M1[l]
}
# Normalize distance to median
dist_colmedian_M1_normalized <- matrix(NA, nrow=nrow(spline_fitted_M1),ncol=ncol(spline_fitted_M1))

for(l in 1:ncol(spline_fitted_M1)){
  dist_colmedian_M1_normalized[,l]<-abs(dist_colmedian_M1[,l]/max(abs(dist_colmedian_M1[,l])))
}

# Change structure
dist_colmedian_M1_normalized_melted <- melt(dist_colmedian_M1_normalized)

# Last preparation before plotting
spline_merged_M1 <- merge(spline_sub_melt_M1,dist_colmedian_M1_normalized_melted,by=c("Var1","Var2"))
spline_merged_M1$value.y_minus1 <- 1-spline_merged_M1$value.y

temp1 <- aggregate(spline_merged_M1$value.y_minus1, list(spline_merged_M1$Var1), median, na.rm=T) 
temp2 <- aggregate(spline_merged_M1$value.x, list(spline_merged_M1$Var2), median, na.rm=T)

temp3 <- aggregate(spline_merged_M1$value.x, list(spline_merged_M1$Var2), quantile, probs=0.025,na.rm=T) 
temp4 <- aggregate(spline_merged_M1$value.x, list(spline_merged_M1$Var2), quantile, probs=0.975,na.rm=T)

spline_merged_M1 <- merge(spline_merged_M1, temp1, by.x=c("Var1"), by.y=c("Group.1"))
spline_merged_M1 <- merge(spline_merged_M1, temp2, by.x=c("Var2"), by.y=c("Group.1"))
colnames(spline_merged_M1)[7]<-"average_effect"

spline_merged_M1<-merge(spline_merged_M1, temp3, by.x=c("Var2"), by.y=c("Group.1"))
colnames(spline_merged_M1)[8]<-"lower_bound"

spline_merged_M1<-merge(spline_merged_M1, temp4, by.x=c("Var2"), by.y=c("Group.1"))
colnames(spline_merged_M1)[9]<-"upper_bound"

temp5 <- cbind(seq(5,55,0.225),seq(1:length(seq(5,55,0.225))))
colnames(temp5) <- c("Temp_bin", "Var2")
spline_merged_M1 <-merge(spline_merged_M1, temp5, by=c("Var2"))

# Plot: hourly temperature effect
model_1 <- ggplot()+ ggtitle("Hourly temperature effects: Equal Knots 3") + 
  theme(panel.grid.major = element_blank(),
        plot.title = element_text(family="Times New Roman", size=20, colour = "grey25", hjust=0.5),
        axis.title = element_text(family="Times New Roman", size=16, colour = "grey25"),
        axis.text = element_text(family="Times New Roman", size=16),
        panel.grid.minor = element_blank(),
        panel.background = element_blank(),
        axis.line = element_line(colour = "black"),
        legend.position="none")+
  xlab("Temperature (°C)") + ylab("Log yield response (Kg/ha)")+
  scale_x_continuous(breaks = seq(5,55, by=5), limits = c(0,55))+
  scale_y_continuous(breaks = seq(-.02,.02, by=.004), limits = c(-.02,.02))+
  geom_hline(yintercept=c(seq(-.02,.02,by=0.004)), colour="gray")+
  geom_vline(xintercept=temp_knot_df, linetype="dotted", colour="gray")+
  geom_line(data=spline_merged_M1, aes(y=value.x, x=Temp_bin, group=Var1, alpha=x.x^200/200), size=.01, colour="brown1")+
  geom_line(data=spline_merged_M1, aes(y=average_effect, x=Temp_bin), alpha=0.85, size=.75, colour="brown4")+
  geom_line(data=spline_merged_M1, aes(y=lower_bound, x=Temp_bin)   , alpha=0.85, size=.75, colour="brown4")+       
  geom_line(data=spline_merged_M1, aes(y=upper_bound, x=Temp_bin)   , alpha=0.85, size=.75, colour="brown4")

model_1
ggsave("C:/Users/Shakila Vyas/OneDrive - Wageningen University & Research/PhD-WUR-Insurance/CSA/County analysis/Results/wheatdis_IMD_eq3_dis_area.png", width = 20, height = 20, units = "cm")

#Weather prep for ERA data
options(scipen=999)
memory.limit(100000)
library(reshape2)
library(beepr)
library(dplyr)
library(lspline)
library(splines)
library(Hmisc)
library(raster)
library(RColorBrewer)
library(lspline)
setwd("C:/Users/Shakila Vyas/OneDrive - Wageningen University & Research/PhD-WUR-Insurance/CSA/County analysis/Weather data")
#read data; farms in rows, daily weather observations in columns
rm(temp.max,temp.min,temp.min.mat,temp.max.mat,temp.min.num,temp.max.num)
temp.max <- as.matrix(read.csv("C:/Users/Shakila Vyas/OneDrive - Wageningen University & Research/PhD-WUR-Insurance/CSA/County analysis/Weather data/ERA5/Winter/mxtpimddis20era.csv")) #load daily minimum temperatures at the district level
temp.min <- as.matrix(read.csv("C:/Users/Shakila Vyas/OneDrive - Wageningen University & Research/PhD-WUR-Insurance/CSA/County analysis/Weather data/ERA5/Winter/mntpimddis20era.csv")) #load daily minimum temperatures at the district level

#delete rownumber
temp.min <- temp.min[,-1] 
temp.max <- temp.max[,-1] 

#concert kelvin to Celsius
temp.min <- temp.min-273.15
temp.max <- temp.max-273.15

#delete farms without weather data (farm with mistaken coordinates)
temp.min.mat <- temp.min[complete.cases(temp.min), ]
temp.max.mat <- temp.max[complete.cases(temp.max), ]

###other way-NOT TO BE DONE
#temp.min.mat <- temp.min
#temp.max.mat <- temp.max

#make dataframes to numeric matrices
temp.min.num <- apply(temp.min.mat, 2, as.numeric)
temp.max.num <- apply(temp.max.mat, 2, as.numeric)

#############################################
####CALCULATE DAILY TEMP MINIMA AND MAXIMA####
#############################################

#create empty matrices
temp.min     <- matrix(nrow=nrow(temp.min.num), ncol=ncol(temp.min.num))
temp.max     <- matrix(nrow=nrow(temp.max.num), ncol=ncol(temp.max.num))

###################################################
####CALCULATE HOURLY Temp VALUES FROM SINE CURVE####
###################################################

#we assume a sine curve for the 12 hours between between thi.min_t and thi.max_t and a new sine curve between thi.max_t and thi_min_t+1
#we thus need a matrix with T+1 columns for thi.min

#add one extra unobserved for temp min
temp.min.num             <- cbind(temp.min.num,  1:nrow(temp.min.num)*NA)

#and we thus get two amplitudes and two middle temp's per day
temp.amplitude_1        <-(temp.max.num-temp.min.num[,1:ncol(temp.max.num)])/2 #from temp.min_t until temp.max_t
temp.amplitude_2        <-(temp.max.num-temp.min.num[,2:ncol(temp.min.num)])/2 #from temp.max_t until temp.min_t+1
temp.mid_1              <- temp.max.num-temp.amplitude_1     #from temp.min_t until temp.max_t
temp.mid_2              <- temp.max.num-temp.amplitude_2     #from temp.max_t until temp.min_t
temp.amplitude_2[,ncol(temp.max.num)] <- temp.amplitude_1[,ncol(temp.max.num)]      # assume that the last day has the same amplitude and mid on the first and second half of the day
temp.mid_2[,ncol(temp.max.num)]       <- temp.mid_1[,ncol(temp.max.num)]

#some preparations
hours        <- seq(0,23, by=1) #hours of the day
time.seq     <- unique(substr(colnames(temp.max.num), 2,8)) 

sincurve12_1 <- seq(((-0.5*pi)+(2*pi/48)),(0.5*pi-(2*pi/48)), by=(2*pi/24)) #by 2pi/24 to set the hour steps, and add/substract 2*pi*1/48 to estimate at the middle of the hour
sincurve12_2 <- seq((( 0.5*pi)+(2*pi/48)),(1.5*pi-(2*pi/48)), by=(2*pi/24)) #by 2pi/24 to set the hour steps, and add/substract 2*pi*1/48 to estimate at the middle of the hour

#### ARRAY WITH MATRIX FOR EVERY MONTH (rows=642 firms, cols=different for each day)####
# in the dairy paper we wanted to aggregate per month while we want to aggregate per year
# theref, we can get rid of the first part of the for loop and not create temporary amplitude and mid temperature matrices but we can proceed with the ones that we created above 
# 1 adjust the second triple loop and replace the temp. matrices with the names we created above
# 2 remove all the unnecessary parts of the loop 

temp       <- array(NA, dim=c(nrow(temp.amplitude_1),  ncol(temp.amplitude_1),length(hours))) #create an empty array for hours in bin per day from temp.min_t to temp.max_t

for (j in 1:ncol(temp.amplitude_1)){ #do this for every day in month k, so j
  for (i in 1:nrow(temp.amplitude_1)){ #for all firms
    for (h in 1:length(sincurve12_1)){ #for first 12 hours
      temp[i,j,h]    <-  temp.amplitude_1[i,j]*sin(sincurve12_1[h])+temp.mid_1[i,j] #get Temp value in each hour
    }
    for (h in 1:(length(sincurve12_2))){ #for second 12 hours
      temp[i,j,h+12] <-  temp.amplitude_2[i,j]*sin(sincurve12_2[h])+temp.mid_2[i,j]
    }
  }
} 

write.csv(temp, "C:/Users/Shakila Vyas/OneDrive - Wageningen University & Research/PhD-WUR-Insurance/CSA/County analysis/Weather data/ERA5/Winter/hourlytp20era.csv", row.names=F)

######hourly reg-WHEAT
library(raster)
library(dplyr)
library(data.table)
library(reshape2)
library(Hmisc)
library(lubridate)
library(plyr)
library(esmisc)
library(sandwich)
library(MASS)
library(e1071)
library(fixest)

data<- read.csv("C:/Users/Shakila Vyas/OneDrive - Wageningen University & Research/PhD-WUR-Insurance/CSA/County analysis/Dis yield_All India/Wheat_yields_rev.csv")
head(data)
#remove less than 3 years data
data1<- add_count(data, Dist_code)
regdata_wheat<- subset(data1,n>=3,select = Crop.Production.Statistics:n)
head(regdata_wheat)

##preparing data
rm(temp)

hourly<- read.csv("C:/Users/Shakila Vyas/OneDrive - Wageningen University & Research/PhD-WUR-Insurance/CSA/County analysis/Weather data/ERA5/Winter/hourlytp15era.csv")
#need this format-colnames(temp_wheat_exposure2) <- c("Farm","Date","Hour", "Temperature")
as.data.frame(hourly)
library(reshape2)
long <- reshape2::melt(hourly, id.vars = c("farm"))
long$year<- 2015

hourly1<- read.csv("C:/Users/Shakila Vyas/OneDrive - Wageningen University & Research/PhD-WUR-Insurance/CSA/County analysis/Weather data/ERA5/Winter/hourlytp16era.csv")
#need this format-colnames(temp_wheat_exposure2) <- c("Farm","Date","Hour", "Temperature")
as.data.frame(hourly1)
long1 <- reshape2::melt(hourly1, id.vars = c("farm"))
long1$year<- 2016

hourly2<- read.csv("C:/Users/Shakila Vyas/OneDrive - Wageningen University & Research/PhD-WUR-Insurance/CSA/County analysis/Weather data/ERA5/Winter/hourlytp17era.csv")
#need this format-colnames(temp_wheat_exposure2) <- c("Farm","Date","Hour", "Temperature")
as.data.frame(hourly2)
long2 <- reshape2::melt(hourly2, id.vars = c("farm"))
long2$year<- 2017

hourly3<- read.csv("C:/Users/Shakila Vyas/OneDrive - Wageningen University & Research/PhD-WUR-Insurance/CSA/County analysis/Weather data/ERA5/Winter/hourlytp18era.csv")
#need this format-colnames(temp_wheat_exposure2) <- c("Farm","Date","Hour", "Temperature")
as.data.frame(hourly3)
long3 <- reshape2::melt(hourly3, id.vars = c("farm"))
long3$year<- 2018

hourly4<- read.csv("C:/Users/Shakila Vyas/OneDrive - Wageningen University & Research/PhD-WUR-Insurance/CSA/County analysis/Weather data/ERA5/Winter/hourlytp19era.csv")
#need this format-colnames(temp_wheat_exposure2) <- c("Farm","Date","Hour", "Temperature")
as.data.frame(hourly4)
long4 <- reshape2::melt(hourly4, id.vars = c("farm"))
long4$year<- 2019

hourly5<- read.csv("C:/Users/Shakila Vyas/OneDrive - Wageningen University & Research/PhD-WUR-Insurance/CSA/County analysis/Weather data/ERA5/Winter/hourlytp20era.csv")
#need this format-colnames(temp_wheat_exposure2) <- c("Farm","Date","Hour", "Temperature")
as.data.frame(hourly5)
long5 <- reshape2::melt(hourly5, id.vars = c("farm"))
long5$year<- 2020

temp<- rbind(long,long1,long2,long3,long4,long5)

####outlier removal
hist(regdata_wheat$Yield..Tonnes.Hectare.)
range(regdata_wheat$Yield..Tonnes.Hectare.)

quantile(regdata_wheat$Yield..Tonnes.Hectare.,probs= c(.99))
regdata_wheat1<- subset(regdata_wheat,Yield..Tonnes.Hectare.<quantile(regdata_wheat$Yield..Tonnes.Hectare.,probs= c(.99)),select = Crop.Production.Statistics:n )

#rename temp data
names(temp)[3] <- "temperature"
names(temp)[1] <- "farm"
as.numeric(temp$temperature)
min(temp$temperature)
max(temp$temperature)
range(temp$temperature)
summary(temp$temperature)
hist(temp$temperature,main="Hourly temperature district-Winter")

range(temp$temperature)
temp <- na.omit(temp)

range(temp$temperature)
#####################model 1: equal spaced 3 knots
temp_knot_df <- c(min(temp$temperature) + ((max(temp$temperature)-min(temp$temperature)) / 4),
                  min(temp$temperature) + 2* ((max(temp$temperature)-min(temp$temperature)) / 4),
                  min(temp$temperature) + 3* ((max(temp$temperature)-min(temp$temperature)) / 4))

# New TS for temperature support of interest- .225 to get the nice plot with small intervals
temp1 <- as.matrix(Hmisc::rcspline.eval(temp$temperature,knots=temp_knot_df,inclx=T))
temp2 <- cbind(temp,temp1)

names(temp2)
head(temp1)

#adjust variables depending on knots
temp3<-temp2 %>%
  group_by(farm,year)%>%
  summarise_at(vars(x,V2),sum)

#match names with temp
names(regdata_wheat1)
colnames(regdata_wheat1)[colnames(regdata_wheat) == "Year.1"] <- "year"
colnames(regdata_wheat1)[colnames(regdata_wheat) == "Dist_code"] <- "farm"
names(regdata_wheat1)

#join both  data
temp4 <- join(regdata_wheat1,temp3, by=c("farm", "year"))

names(temp4)
temp4$reg_year<- as.numeric(as.factor(temp4$District))*temp4$year

library(fixest)
wheat_yield_spline = feols(log(Yield..Tonnes.Hectare.) ~ x + V2 + year + I(year^2) + erarain + I(erarain^2)   | farm , cluster = c( "year","farm"), temp4)
summary(wheat_yield_spline)

results<- etable(soy_yield_spline)
write.csv(results,"C:/Users/Shakila Vyas/OneDrive - Wageningen University & Research/PhD-WUR-Insurance/CSA/ERA5_results_eq_knot3_returns.csv")

#####bootstrapping
set.seed(123)
marginal_temperatures_M1 <- rcspline.eval(seq(-20,50,0.225),knots=temp_knot_df, inclx=T)

# Run 1000 models to derive uncertainty in estimates
spline_fitted_M1 <- matrix(NA,nrow=1000, ncol=nrow(marginal_temperatures_M1))

for (l in 1: 1000){
  # Sample by region times year
  data_temp <- temp4[which(temp4$reg_year %in% sample(unique(temp4$reg_year),replace=T)),]
  
  # Run regression with random subsample (data_temp), adjust variables to knots
  fit_boot  <- feols(Yield..Tonnes.Hectare. ~ x + V2  + year + I(year^2) + erarain + I(erarain^2)  | farm , data_temp)
  
  #Save marginal effect of temperatures,adjust variables to knots 
  spline_fitted_M1[l,]<-as.vector((marginal_temperatures_M1  %*% coef(fit_boot)[1:2])) 
  
  # Show progress in %
  print(l/1000 *100)
  rm(fit_boot,data_temp)
}

# Load the packages
library(fixest)
library(ggplot2)
library(gridExtra)
library(robustbase)
library(egg)
library(matrixStats)
library(tidyverse)

#Change structure
spline_sub_melt_M1 <- melt(spline_fitted_M1)
# Get median response for each column (= a temperature from seq(20,40,0.225))
colmedians_spline_M1 <- colMedians(spline_fitted_M1)
# Get distance from median response for each value
dist_colmedian_M1 <- matrix(NA, nrow=nrow(spline_fitted_M1),ncol=ncol(spline_fitted_M1))

for(l in 1:ncol(spline_fitted_M1)){
  dist_colmedian_M1[,l] <- spline_fitted_M1[,l] - colmedians_spline_M1[l]
}
# Normalize distance to median
dist_colmedian_M1_normalized <- matrix(NA, nrow=nrow(spline_fitted_M1),ncol=ncol(spline_fitted_M1))

for(l in 1:ncol(spline_fitted_M1)){
  dist_colmedian_M1_normalized[,l]<-abs(dist_colmedian_M1[,l]/max(abs(dist_colmedian_M1[,l])))
}

# Change structure
dist_colmedian_M1_normalized_melted <- melt(dist_colmedian_M1_normalized)

# Last preparation before plotting
spline_merged_M1 <- merge(spline_sub_melt_M1,dist_colmedian_M1_normalized_melted,by=c("Var1","Var2"))
spline_merged_M1$value.y_minus1 <- 1-spline_merged_M1$value.y

temp1 <- aggregate(spline_merged_M1$value.y_minus1, list(spline_merged_M1$Var1), median, na.rm=T) 
temp2 <- aggregate(spline_merged_M1$value.x, list(spline_merged_M1$Var2), median, na.rm=T)

temp3 <- aggregate(spline_merged_M1$value.x, list(spline_merged_M1$Var2), quantile, probs=0.025,na.rm=T) 
temp4 <- aggregate(spline_merged_M1$value.x, list(spline_merged_M1$Var2), quantile, probs=0.975,na.rm=T)

spline_merged_M1 <- merge(spline_merged_M1, temp1, by.x=c("Var1"), by.y=c("Group.1"))
spline_merged_M1 <- merge(spline_merged_M1, temp2, by.x=c("Var2"), by.y=c("Group.1"))
colnames(spline_merged_M1)[7]<-"average_effect"

spline_merged_M1<-merge(spline_merged_M1, temp3, by.x=c("Var2"), by.y=c("Group.1"))
colnames(spline_merged_M1)[8]<-"lower_bound"

spline_merged_M1<-merge(spline_merged_M1, temp4, by.x=c("Var2"), by.y=c("Group.1"))
colnames(spline_merged_M1)[9]<-"upper_bound"

temp5 <- cbind(seq(-20,50,0.225),seq(1:length(seq(-20,50,0.225))))
colnames(temp5) <- c("Temp_bin", "Var2")
spline_merged_M1 <-merge(spline_merged_M1, temp5, by=c("Var2"))

# Plot: hourly temperature effect

model_1 <- ggplot()+ ggtitle("Hourly temperature effects: Equal Knots 3") + 
  theme(panel.grid.major = element_blank(),
        plot.title = element_text(family="Times New Roman", size=20, colour = "grey25", hjust=0.5),
        axis.title = element_text(family="Times New Roman", size=16, colour = "grey25"),
        axis.text = element_text(family="Times New Roman", size=16),
        panel.grid.minor = element_blank(),
        panel.background = element_blank(),
        axis.line = element_line(colour = "black"),
        legend.position="none")+
  xlab("Temperature (°C)") + ylab("Log yield response (Kg/ha)")+
  scale_x_continuous(breaks = seq(-20,55, by=5), limits = c(-20,55))+
  scale_y_continuous(breaks = seq(-.004,.004, by=.001), limits = c(-.004,.004))+
  geom_hline(yintercept=c(seq(-.004,.004, by=.001)), colour="gray")+
  geom_vline(xintercept=temp_knot_df, linetype="dotted", colour="gray")+
  geom_line(data=spline_merged_M1, aes(y=value.x, x=Temp_bin, group=Var1, alpha=x.x^200/200), size=.01, colour="brown1")+
  geom_line(data=spline_merged_M1, aes(y=average_effect, x=Temp_bin), alpha=0.85, size=.75, colour="brown4")+
  geom_line(data=spline_merged_M1, aes(y=lower_bound, x=Temp_bin)   , alpha=0.85, size=.75, colour="brown4")+       
  geom_line(data=spline_merged_M1, aes(y=upper_bound, x=Temp_bin)   , alpha=0.85, size=.75, colour="brown4")

model_1
ggsave("C:/Users/Shakila Vyas/OneDrive - Wageningen University & Research/PhD-WUR-Insurance/CSA/County analysis/Results/wheatera5eq3.png", width = 20, height = 20, units = "cm")

#####best fit model
#best knots based on RSS
# lower and upper bound define lowest and highest knot location
lower_bound <- ceiling(quantile(temp$temperature,0.05, type=1))
upper_bound <- floor(quantile(temp$temperature,0.95, type=1))
temp_range <- seq(lower_bound, upper_bound,1) 
kn3_combi <- combn(temp_range,3, simplify = T)
# Minimum space between 2 knots
required_space <- 5
# Get all knot combinations
differences <- matrix(NA, nrow=3, ncol=ncol(kn3_combi))
differences[1,] <- abs(kn3_combi[1,] - kn3_combi[2,])
differences[2,] <- abs(kn3_combi[1,] - kn3_combi[3,])
differences[3,] <- abs(kn3_combi[2,] - kn3_combi[3,])
knots_3_combi_good <- kn3_combi[,which(differences[1,] >= required_space & differences[2,] >= required_space & differences[3,] >= required_space)]
# vector containing residual sum of squares (RSS)
RSS_3knots <- vector(length=ncol(knots_3_combi_good ))

# Get the RSS for each model
for (c in 1:ncol(knots_3_combi_good)){
  
  # Get new time series and aggregate hourly values  
  temp1 <- as.matrix(rcspline.eval(temp$temperature,knots= knots_3_combi_good[,c], inclx=T))
  temp2 <- cbind(temp,temp1)
  
  temp3<-temp2 %>%
    group_by(farm,year)%>%
    summarise_at(vars(x,V2),sum)
  
  # Match yearly values with yearly yields
  temp4 <- join(regdata_wheat1,temp3, by=c("farm", "year"))
  
  # Run regression
  temp_reg <- feols(log(Yield..Tonnes.Hectare.) ~ x + V2  +  year + I(year^2) + erarain + I(erarain^2)  | farm , temp4)
  RSS_3knots[c] <- sum(resid(temp_reg)^2)
  
}

# Best number of knots
temp_knot_df <- knots_3_combi_good[,which.min(RSS_3knots)]
rm(RSS_3knots, knots_3_combi_good, differences, kn3_combi,temp_range,upper_bound, lower_bound)

# resume reg
temp1 <- as.matrix(Hmisc::rcspline.eval(temp$temperature,knots=temp_knot_df,inclx=T))
temp2 <- cbind(temp,temp1)

names(temp2)
head(temp1)

#adjust variables depending on knots
temp3<-temp2 %>%
  group_by(farm,year)%>%
  summarise_at(vars(x,V2),sum)

#match names with temp
names(regdata_wheat1)
colnames(regdata_wheat1)[colnames(regdata_soy1) == "Year"] <- "year"
colnames(regdata_wheat1)[colnames(regdata_soy1) == "Dis_codename"] <- "farm"
names(regdata_1)

#join both  data
temp4 <- join(regdata_wheat1,temp3, by=c("farm", "year"))

names(temp4)
temp4$reg_year<- as.numeric(as.factor(temp4$District))*temp4$year

#####bootstrapping
set.seed(123)
marginal_temperatures_M1 <- rcspline.eval(seq(-10,50,0.225),knots=temp_knot_df, inclx=T)

# Run 1000 models to derive uncertainty in estimates
spline_fitted_M1 <- matrix(NA,nrow=1000, ncol=nrow(marginal_temperatures_M1))

for (l in 1: 1000){
  # Sample by region times year
  data_temp <- temp4[which(temp4$reg_year %in% sample(unique(temp4$reg_year),replace=T)),]
  
  # Run regression with random subsample (data_temp), adjust variables to knots
  fit_boot  <- feols(log(Yield..Tonnes.Hectare.) ~ x + V2  +  year + I(year^2) + erarain + I(erarain^2)  | farm , data_temp)
  
  #Save marginal effect of temperatures,adjust variables to knots 
  spline_fitted_M1[l,]<-as.vector((marginal_temperatures_M1  %*% coef(fit_boot)[1:2])) 
  
  # Show progress in %
  print(l/1000 *100)
  rm(fit_boot,data_temp)
}

#Change structure
spline_sub_melt_M1 <- melt(spline_fitted_M1)
# Get median response for each column (= a temperature from seq(20,40,0.225))
colmedians_spline_M1 <- colMedians(spline_fitted_M1)
# Get distance from median response for each value
dist_colmedian_M1 <- matrix(NA, nrow=nrow(spline_fitted_M1),ncol=ncol(spline_fitted_M1))

for(l in 1:ncol(spline_fitted_M1)){
  dist_colmedian_M1[,l] <- spline_fitted_M1[,l] - colmedians_spline_M1[l]
}
# Normalize distance to median
dist_colmedian_M1_normalized <- matrix(NA, nrow=nrow(spline_fitted_M1),ncol=ncol(spline_fitted_M1))

for(l in 1:ncol(spline_fitted_M1)){
  dist_colmedian_M1_normalized[,l]<-abs(dist_colmedian_M1[,l]/max(abs(dist_colmedian_M1[,l])))
}

# Change structure
dist_colmedian_M1_normalized_melted <- melt(dist_colmedian_M1_normalized)

# Last preparation before plotting
spline_merged_M1 <- merge(spline_sub_melt_M1,dist_colmedian_M1_normalized_melted,by=c("Var1","Var2"))
spline_merged_M1$value.y_minus1 <- 1-spline_merged_M1$value.y

temp1 <- aggregate(spline_merged_M1$value.y_minus1, list(spline_merged_M1$Var1), median, na.rm=T) 
temp2 <- aggregate(spline_merged_M1$value.x, list(spline_merged_M1$Var2), median, na.rm=T)

temp3 <- aggregate(spline_merged_M1$value.x, list(spline_merged_M1$Var2), quantile, probs=0.025,na.rm=T) 
temp4 <- aggregate(spline_merged_M1$value.x, list(spline_merged_M1$Var2), quantile, probs=0.975,na.rm=T)

spline_merged_M1 <- merge(spline_merged_M1, temp1, by.x=c("Var1"), by.y=c("Group.1"))
spline_merged_M1 <- merge(spline_merged_M1, temp2, by.x=c("Var2"), by.y=c("Group.1"))
colnames(spline_merged_M1)[7]<-"average_effect"

spline_merged_M1<-merge(spline_merged_M1, temp3, by.x=c("Var2"), by.y=c("Group.1"))
colnames(spline_merged_M1)[8]<-"lower_bound"

spline_merged_M1<-merge(spline_merged_M1, temp4, by.x=c("Var2"), by.y=c("Group.1"))
colnames(spline_merged_M1)[9]<-"upper_bound"

temp5 <- cbind(seq(-10,50,0.225),seq(1:length(seq(-10,50,0.225))))
colnames(temp5) <- c("Temp_bin", "Var2")
spline_merged_M1 <-merge(spline_merged_M1, temp5, by=c("Var2"))

# Plot: hourly temperature effect
model_4 <- ggplot()+ ggtitle("Hourly temperature effects: Best fit") + 
  theme(panel.grid.major = element_blank(),
        plot.title = element_text(family="Times New Roman", size=20, colour = "grey25", hjust=0.5),
        axis.title = element_text(family="Times New Roman", size=16, colour = "grey25"),
        axis.text = element_text(family="Times New Roman", size=16),
        panel.grid.minor = element_blank(),
        panel.background = element_blank(),
        axis.line = element_line(colour = "black"),
        legend.position="none")+
  xlab("Temperature (°C)") + ylab("Log yield response (Kg/ha)")+
  scale_x_continuous(breaks = seq(-10,55, by=5), limits = c(-10,55))+
  scale_y_continuous(breaks = seq(-.02,.02, by=.004), limits = c(-.02,.02))+
  geom_hline(yintercept=c(seq(-.02,.02,by=0.004)), colour="gray")+
  geom_vline(xintercept=temp_knot_df, linetype="dotted", colour="gray")+
  geom_line(data=spline_merged_M1, aes(y=value.x, x=Temp_bin, group=Var1, alpha=x.x^200/200), size=.01, colour="brown1")+
  geom_line(data=spline_merged_M1, aes(y=average_effect, x=Temp_bin), alpha=0.85, size=.75, colour="brown4")+
  geom_line(data=spline_merged_M1, aes(y=lower_bound, x=Temp_bin)   , alpha=0.85, size=.75, colour="brown4")+       
  geom_line(data=spline_merged_M1, aes(y=upper_bound, x=Temp_bin)   , alpha=0.85, size=.75, colour="brown4")

model_4

ggsave("C:/Users/Shakila Vyas/OneDrive - Wageningen University & Research/PhD-WUR-Insurance/CSA/County analysis/Results/soydis_IMD_bestfit_discombo.png", width = 20, height = 20, units = "cm")

#subset by area
data<- read.csv("C:/Users/Shakila Vyas/OneDrive - Wageningen University & Research/PhD-WUR-Insurance/CSA/County analysis/Dis yield_All India/Wheat_yields_rev.csv")
head(data)
#remove less than 3 years data
data1<- add_count(data, Dist_code)
regdata_wheat<- subset(data1,n>=3,select = Crop.Production.Statistics:n)
head(regdata_wheat)

temp<- rbind(long,long1,long2,long3,long4,long5)

####outlier removal
hist(regdata_wheat$Yield..Tonnes.Hectare.)
range(regdata_wheat$Yield..Tonnes.Hectare.)

quantile(regdata_wheat$Yield..Tonnes.Hectare.,probs= c(.99))
regdata_wheat1<- subset(regdata_wheat,Yield..Tonnes.Hectare.<quantile(regdata_wheat$Yield..Tonnes.Hectare.,probs= c(.99)),select = Crop.Production.Statistics:n )

regdata_wheat1<- subset(regdata_wheat,area=="north",select = Crop.Production.Statistics:n )

#rename temp data
names(temp)[3] <- "temperature"
names(temp)[1] <- "farm"
as.numeric(temp$temperature)
min(temp$temperature)
max(temp$temperature)
range(temp$temperature)
summary(temp$temperature)
hist(temp$temperature,main="Hourly temperature district-Winter")

range(temp$temperature)
temp <- na.omit(temp)

#####################model 1: equal spaced 3 knots
temp_knot_df <- c(min(temp$temperature) + ((max(temp$temperature)-min(temp$temperature)) / 4),
                  min(temp$temperature) + 2* ((max(temp$temperature)-min(temp$temperature)) / 4),
                  min(temp$temperature) + 3* ((max(temp$temperature)-min(temp$temperature)) / 4))

# New TS for temperature support of interest- .225 to get the nice plot with small intervals
temp1 <- as.matrix(Hmisc::rcspline.eval(temp$temperature,knots=temp_knot_df,inclx=T))
temp2 <- cbind(temp,temp1)

names(temp2)
head(temp1)

#adjust variables depending on knots
temp3<-temp2 %>%
  group_by(farm,year)%>%
  summarise_at(vars(x,V2),sum)

#match names with temp
names(regdata_wheat1)
colnames(regdata_wheat1)[colnames(regdata_wheat) == "Year.1"] <- "year"
colnames(regdata_wheat1)[colnames(regdata_wheat) == "Dist_code"] <- "farm"
names(regdata_wheat1)

#join both  data
temp4 <- join(regdata_wheat1,temp3, by=c("farm", "year"))

names(temp4)
temp4$reg_year<- as.numeric(as.factor(temp4$District))*temp4$year

library(fixest)
wheat_yield_spline = feols(log(Yield..Tonnes.Hectare.) ~ x + V2 + year + I(year^2) + erarain + I(erarain^2)   | farm , cluster = c( "year","farm"), temp4)
summary(wheat_yield_spline)
results<- etable(soy_yield_spline)
write.csv(results,"C:/Users/Shakila Vyas/OneDrive - Wageningen University & Research/PhD-WUR-Insurance/CSA/ERA5_results_eq_knot3_returns.csv")

#####bootstrapping
set.seed(123)
marginal_temperatures_M1 <- rcspline.eval(seq(0,50,0.225),knots=temp_knot_df, inclx=T)

# Run 1000 models to derive uncertainty in estimates
spline_fitted_M1 <- matrix(NA,nrow=1000, ncol=nrow(marginal_temperatures_M1))

for (l in 1: 1000){
  # Sample by region times year
  data_temp <- temp4[which(temp4$reg_year %in% sample(unique(temp4$reg_year),replace=T)),]
  
  # Run regression with random subsample (data_temp), adjust variables to knots
  fit_boot  <- feols(Yield..Tonnes.Hectare. ~ x + V2  + year + I(year^2) + erarain + I(erarain^2)  | farm , data_temp)
  
  #Save marginal effect of temperatures,adjust variables to knots 
  spline_fitted_M1[l,]<-as.vector((marginal_temperatures_M1  %*% coef(fit_boot)[1:2])) 
  
  # Show progress in %
  print(l/1000 *100)
  rm(fit_boot,data_temp)
}

# Load the packages
library(fixest)
library(ggplot2)
library(gridExtra)
library(robustbase)
library(egg)
library(matrixStats)
library(tidyverse)

#Change structure
spline_sub_melt_M1 <- melt(spline_fitted_M1)
# Get median response for each column (= a temperature from seq(20,40,0.225))
colmedians_spline_M1 <- colMedians(spline_fitted_M1)
# Get distance from median response for each value
dist_colmedian_M1 <- matrix(NA, nrow=nrow(spline_fitted_M1),ncol=ncol(spline_fitted_M1))

for(l in 1:ncol(spline_fitted_M1)){
  dist_colmedian_M1[,l] <- spline_fitted_M1[,l] - colmedians_spline_M1[l]
}
# Normalize distance to median
dist_colmedian_M1_normalized <- matrix(NA, nrow=nrow(spline_fitted_M1),ncol=ncol(spline_fitted_M1))

for(l in 1:ncol(spline_fitted_M1)){
  dist_colmedian_M1_normalized[,l]<-abs(dist_colmedian_M1[,l]/max(abs(dist_colmedian_M1[,l])))
}

# Change structure
dist_colmedian_M1_normalized_melted <- melt(dist_colmedian_M1_normalized)

# Last preparation before plotting
spline_merged_M1 <- merge(spline_sub_melt_M1,dist_colmedian_M1_normalized_melted,by=c("Var1","Var2"))
spline_merged_M1$value.y_minus1 <- 1-spline_merged_M1$value.y

temp1 <- aggregate(spline_merged_M1$value.y_minus1, list(spline_merged_M1$Var1), median, na.rm=T) 
temp2 <- aggregate(spline_merged_M1$value.x, list(spline_merged_M1$Var2), median, na.rm=T)

temp3 <- aggregate(spline_merged_M1$value.x, list(spline_merged_M1$Var2), quantile, probs=0.025,na.rm=T) 
temp4 <- aggregate(spline_merged_M1$value.x, list(spline_merged_M1$Var2), quantile, probs=0.975,na.rm=T)

spline_merged_M1 <- merge(spline_merged_M1, temp1, by.x=c("Var1"), by.y=c("Group.1"))
spline_merged_M1 <- merge(spline_merged_M1, temp2, by.x=c("Var2"), by.y=c("Group.1"))
colnames(spline_merged_M1)[7]<-"average_effect"

spline_merged_M1<-merge(spline_merged_M1, temp3, by.x=c("Var2"), by.y=c("Group.1"))
colnames(spline_merged_M1)[8]<-"lower_bound"

spline_merged_M1<-merge(spline_merged_M1, temp4, by.x=c("Var2"), by.y=c("Group.1"))
colnames(spline_merged_M1)[9]<-"upper_bound"

temp5 <- cbind(seq(0,50,0.225),seq(1:length(seq(0,50,0.225))))
colnames(temp5) <- c("Temp_bin", "Var2")
spline_merged_M1 <-merge(spline_merged_M1, temp5, by=c("Var2"))

# Plot: hourly temperature effect

model_1 <- ggplot()+ ggtitle("Hourly temperature effects: Equal Knots 3") + 
  theme(panel.grid.major = element_blank(),
        plot.title = element_text(family="Times New Roman", size=20, colour = "grey25", hjust=0.5),
        axis.title = element_text(family="Times New Roman", size=16, colour = "grey25"),
        axis.text = element_text(family="Times New Roman", size=16),
        panel.grid.minor = element_blank(),
        panel.background = element_blank(),
        axis.line = element_line(colour = "black"),
        legend.position="none")+
  xlab("Temperature (°C)") + ylab("Log yield response (Kg/ha)")+
  scale_x_continuous(breaks = seq(0,50, by=5), limits = c(0,50))+
  scale_y_continuous(breaks = seq(-.02,.02, by=.005), limits = c(-.02,.02))+
  geom_hline(yintercept=c(seq(-.05,.05, by=.01)), colour="gray")+
  geom_vline(xintercept=temp_knot_df, linetype="dotted", colour="gray")+
  geom_line(data=spline_merged_M1, aes(y=value.x, x=Temp_bin, group=Var1, alpha=x.x^200/200), size=.01, colour="brown1")+
  geom_line(data=spline_merged_M1, aes(y=average_effect, x=Temp_bin), alpha=0.85, size=.75, colour="brown4")+
  geom_line(data=spline_merged_M1, aes(y=lower_bound, x=Temp_bin)   , alpha=0.85, size=.75, colour="brown4")+       
  geom_line(data=spline_merged_M1, aes(y=upper_bound, x=Temp_bin)   , alpha=0.85, size=.75, colour="brown4")

model_1

ggsave("C:/Users/Shakila Vyas/OneDrive - Wageningen University & Research/PhD-WUR-Insurance/CSA/County analysis/Results/wheatera5eq3_north.png", width = 20, height = 20, units = "cm")

######soybean reg ERA5
#hourly temp splines
options(scipen=999)
memory.limit(100000)
library(reshape2)
library(beepr)
library(dplyr)
library(lspline)
library(splines)
library(Hmisc)
library(raster)
library(RColorBrewer)
library(lspline)
setwd("C:/Users/Shakila Vyas/OneDrive - Wageningen University & Research/PhD-WUR-Insurance/CSA/County analysis/Weather data")
#read data; farms in rows, daily weather observations in columns
rm(temp.max,temp.min,temp.min.mat,temp.max.mat,temp.min.num,temp.max.num)
temp.max <- as.matrix(read.csv("C:/Users/Shakila Vyas/OneDrive - Wageningen University & Research/PhD-WUR-Insurance/CSA/County analysis/Weather data/ERA5/Summer/mxtpimddis17era.csv")) #load daily minimum temperatures at the district level
temp.min <- as.matrix(read.csv("C:/Users/Shakila Vyas/OneDrive - Wageningen University & Research/PhD-WUR-Insurance/CSA/County analysis/Weather data/ERA5/Summer/mntpimddis17era.csv")) #load daily minimum temperatures at the district level

#delete rownumber
temp.min <- temp.min[,-1] 
temp.max <- temp.max[,-1] 

#concert kelvin to Celsius
temp.min <- temp.min-273.15
temp.max <- temp.max-273.15

#delete farms without weather data (farm with mistaken coordinates)
temp.min.mat <- temp.min[complete.cases(temp.min), ]
temp.max.mat <- temp.max[complete.cases(temp.max), ]

###other way-NOT TO BE DONE
#temp.min.mat <- temp.min
#temp.max.mat <- temp.max

#make dataframes to numeric matrices
temp.min.num <- apply(temp.min.mat, 2, as.numeric)
temp.max.num <- apply(temp.max.mat, 2, as.numeric)

#############################################
####CALCULATE DAILY TEMP MINIMA AND MAXIMA####
#############################################

#create empty matrices
temp.min     <- matrix(nrow=nrow(temp.min.num), ncol=ncol(temp.min.num))
temp.max     <- matrix(nrow=nrow(temp.max.num), ncol=ncol(temp.max.num))

###################################################
####CALCULATE HOURLY Temp VALUES FROM SINE CURVE####
###################################################

#we assume a sine curve for the 12 hours between between thi.min_t and thi.max_t and a new sine curve between thi.max_t and thi_min_t+1
#we thus need a matrix with T+1 columns for thi.min

#add one extra unobserved for temp min
temp.min.num             <- cbind(temp.min.num,  1:nrow(temp.min.num)*NA)

#and we thus get two amplitudes and two middle temp's per day
temp.amplitude_1        <-(temp.max.num-temp.min.num[,1:ncol(temp.max.num)])/2 #from temp.min_t until temp.max_t
temp.amplitude_2        <-(temp.max.num-temp.min.num[,2:ncol(temp.min.num)])/2 #from temp.max_t until temp.min_t+1
temp.mid_1              <- temp.max.num-temp.amplitude_1     #from temp.min_t until temp.max_t
temp.mid_2              <- temp.max.num-temp.amplitude_2     #from temp.max_t until temp.min_t
temp.amplitude_2[,ncol(temp.max.num)] <- temp.amplitude_1[,ncol(temp.max.num)]      # assume that the last day has the same amplitude and mid on the first and second half of the day
temp.mid_2[,ncol(temp.max.num)]       <- temp.mid_1[,ncol(temp.max.num)]

#some preparations
hours        <- seq(0,23, by=1) #hours of the day
time.seq     <- unique(substr(colnames(temp.max.num), 2,8)) 

sincurve12_1 <- seq(((-0.5*pi)+(2*pi/48)),(0.5*pi-(2*pi/48)), by=(2*pi/24)) #by 2pi/24 to set the hour steps, and add/substract 2*pi*1/48 to estimate at the middle of the hour
sincurve12_2 <- seq((( 0.5*pi)+(2*pi/48)),(1.5*pi-(2*pi/48)), by=(2*pi/24)) #by 2pi/24 to set the hour steps, and add/substract 2*pi*1/48 to estimate at the middle of the hour

#### ARRAY WITH MATRIX FOR EVERY MONTH (rows=642 firms, cols=different for each day)####
# in the dairy paper we wanted to aggregate per month while we want to aggregate per year
# theref, we can get rid of the first part of the for loop and not create temporary amplitude and mid temperature matrices but we can proceed with the ones that we created above 
# 1 adjust the second triple loop and replace the temp. matrices with the names we created above
# 2 remove all the unnecessary parts of the loop 

temp       <- array(NA, dim=c(nrow(temp.amplitude_1),  ncol(temp.amplitude_1),length(hours))) #create an empty array for hours in bin per day from temp.min_t to temp.max_t

for (j in 1:ncol(temp.amplitude_1)){ #do this for every day in month k, so j
  for (i in 1:nrow(temp.amplitude_1)){ #for all firms
    for (h in 1:length(sincurve12_1)){ #for first 12 hours
      temp[i,j,h]    <-  temp.amplitude_1[i,j]*sin(sincurve12_1[h])+temp.mid_1[i,j] #get Temp value in each hour
    }
    for (h in 1:(length(sincurve12_2))){ #for second 12 hours
      temp[i,j,h+12] <-  temp.amplitude_2[i,j]*sin(sincurve12_2[h])+temp.mid_2[i,j]
    }
  }
} 

write.csv(temp, "C:/Users/Shakila Vyas/OneDrive - Wageningen University & Research/PhD-WUR-Insurance/CSA/County analysis/Weather data/ERA5/Summer/hourlytp17era.csv", row.names=F)

######hourly reg
library(raster)
library(dplyr)
library(data.table)
library(reshape2)
library(Hmisc)
library(lubridate)
library(plyr)
library(esmisc)
library(sandwich)
library(MASS)
library(e1071)
library(fixest)

data<- read.csv("C:/Users/Shakila Vyas/OneDrive - Wageningen University & Research/PhD-WUR-Insurance/CSA/County analysis/Dis yield_All India/Soy_yields.csv")
head(data)
#remove less than 3 years data
regdata_soy<- subset(data,Year>=2015,select = District:erarain)

data1<- add_count(regdata_soy, Dis_codename)
regdata_soy1<- subset(data1,n>=3,select = District:erarain)
head(regdata_soy1)

##preparing data
hourly<- read.csv("C:/Users/Shakila Vyas/OneDrive - Wageningen University & Research/PhD-WUR-Insurance/CSA/County analysis/Weather data/ERA5/Summer/hourlytp15era.csv")
#need this format-colnames(temp_wheat_exposure2) <- c("Farm","Date","Hour", "Temperature")
as.data.frame(hourly)
library(reshape2)
long <- reshape2::melt(hourly, id.vars = c("farm"))
long$year<- 2015

hourly1<- read.csv("C:/Users/Shakila Vyas/OneDrive - Wageningen University & Research/PhD-WUR-Insurance/CSA/County analysis/Weather data/ERA5/Summer/hourlytp16era.csv")
#need this format-colnames(temp_wheat_exposure2) <- c("Farm","Date","Hour", "Temperature")
as.data.frame(hourly1)
long1 <- reshape2::melt(hourly1, id.vars = c("farm"))
long1$year<- 2016

hourly2<- read.csv("C:/Users/Shakila Vyas/OneDrive - Wageningen University & Research/PhD-WUR-Insurance/CSA/County analysis/Weather data/ERA5/Summer/hourlytp17era.csv")
#need this format-colnames(temp_wheat_exposure2) <- c("Farm","Date","Hour", "Temperature")
as.data.frame(hourly2)
long2 <- reshape2::melt(hourly2, id.vars = c("farm"))
long2$year<- 2017

hourly3<- read.csv("C:/Users/Shakila Vyas/OneDrive - Wageningen University & Research/PhD-WUR-Insurance/CSA/County analysis/Weather data/ERA5/Summer/hourlytp18era.csv")
#need this format-colnames(temp_wheat_exposure2) <- c("Farm","Date","Hour", "Temperature")
as.data.frame(hourly3)
long3 <- reshape2::melt(hourly3, id.vars = c("farm"))
long3$year<- 2018

hourly4<- read.csv("C:/Users/Shakila Vyas/OneDrive - Wageningen University & Research/PhD-WUR-Insurance/CSA/County analysis/Weather data/ERA5/Summer/hourlytp19era.csv")
#need this format-colnames(temp_wheat_exposure2) <- c("Farm","Date","Hour", "Temperature")
as.data.frame(hourly4)
long4 <- reshape2::melt(hourly4, id.vars = c("farm"))
long4$year<- 2019

hourly5<- read.csv("C:/Users/Shakila Vyas/OneDrive - Wageningen University & Research/PhD-WUR-Insurance/CSA/County analysis/Weather data/ERA5/Summer/hourlytp20era.csv")
#need this format-colnames(temp_wheat_exposure2) <- c("Farm","Date","Hour", "Temperature")
as.data.frame(hourly5)
long5 <- reshape2::melt(hourly5, id.vars = c("farm"))
long5$year<- 2020

temp<- rbind(long,long1,long2,long3,long4,long5)

####outlier removal
hist(regdata_soy1$Yield..Tonnes.Hectare.)
quantile(regdata_soy1$Yield..Tonnes.Hectare.,probs= c(.99))
regdata_soy1<- subset(regdata_soy1,Yield..Tonnes.Hectare.<quantile(regdata_soy$Yield..Tonnes.Hectare.,probs= c(.99)),select = District:n )
hist(regdata_soy1$Yield..Tonnes.Hectare.)

#rename temp data
names(temp)[3] <- "temperature"
as.numeric(temp$temperature)
min(temp$temperature)
max(temp$temperature)
range(temp$temperature)
summary(temp$temperature)
hist(temp$temperature,main="Hourly temperature district-summer")

range(temp$temperature)
temp <- na.omit(temp)

#####################model 1: equal spaced 3 knots
temp_knot_df <- c(min(temp$temperature) + ((max(temp$temperature)-min(temp$temperature)) / 4),
                  min(temp$temperature) + 2* ((max(temp$temperature)-min(temp$temperature)) / 4),
                  min(temp$temperature) + 3* ((max(temp$temperature)-min(temp$temperature)) / 4))

# New TS for temperature support of interest- .225 to get the nice plot with small intervals
temp1 <- as.matrix(Hmisc::rcspline.eval(temp$temperature,knots=temp_knot_df,inclx=T))
temp2 <- cbind(temp,temp1)

names(temp2)
head(temp1)

#adjust variables depending on knots
temp3<-temp2 %>%
  group_by(farm,year)%>%
  summarise_at(vars(x,V2),sum)

#match names with temp
names(regdata_soy1)
colnames(regdata_soy1)[colnames(regdata_soy1) == "Year"] <- "year"
colnames(regdata_soy1)[colnames(regdata_soy1) == "Dis_codename"] <- "farm"
names(regdata_soy1)

#join both  data
temp4 <- join(regdata_soy1,temp3, by=c("farm", "year"))

names(temp4)
temp4$reg_year<- as.numeric(as.factor(temp4$District))*temp4$year

library(fixest)
soy_yield_spline = feols(log(Yield..Tonnes.Hectare.) ~ x + V2 + year + I(year^2) + summerrainsum + I(summerrainsum^2)   | farm , cluster = c( "year","farm"), temp4)
summary(soy_yield_spline)
results<- etable(soy_yield_spline)
write.csv(results,"C:/Users/Shakila Vyas/OneDrive - Wageningen University & Research/PhD-WUR-Insurance/CSA/ERA5_results_eq_knot3_returns.csv")

#####bootstrapping
set.seed(123)
marginal_temperatures_M1 <- rcspline.eval(seq(10,50,0.225),knots=temp_knot_df, inclx=T)

# Run 1000 models to derive uncertainty in estimates
spline_fitted_M1 <- matrix(NA,nrow=1000, ncol=nrow(marginal_temperatures_M1))

for (l in 1: 1000){
  # Sample by region times year
  data_temp <- temp4[which(temp4$reg_year %in% sample(unique(temp4$reg_year),replace=T)),]
  
  # Run regression with random subsample (data_temp), adjust variables to knots
  fit_boot  <- feols(Yield..Tonnes.Hectare. ~ x + V2  + year + I(year^2) + summerrainsum + I(summerrainsum^2)  | farm , data_temp)
  
  #Save marginal effect of temperatures,adjust variables to knots 
  spline_fitted_M1[l,]<-as.vector((marginal_temperatures_M1  %*% coef(fit_boot)[1:2])) 
  
  # Show progress in %
  print(l/1000 *100)
  rm(fit_boot,data_temp)
}

# Load the packages
library(fixest)
library(ggplot2)
library(gridExtra)
library(robustbase)
library(egg)
library(matrixStats)
library(tidyverse)

#Change structure
spline_sub_melt_M1 <- melt(spline_fitted_M1)
# Get median response for each column (= a temperature from seq(20,40,0.225))
colmedians_spline_M1 <- colMedians(spline_fitted_M1)
# Get distance from median response for each value
dist_colmedian_M1 <- matrix(NA, nrow=nrow(spline_fitted_M1),ncol=ncol(spline_fitted_M1))

for(l in 1:ncol(spline_fitted_M1)){
  dist_colmedian_M1[,l] <- spline_fitted_M1[,l] - colmedians_spline_M1[l]
}
# Normalize distance to median
dist_colmedian_M1_normalized <- matrix(NA, nrow=nrow(spline_fitted_M1),ncol=ncol(spline_fitted_M1))

for(l in 1:ncol(spline_fitted_M1)){
  dist_colmedian_M1_normalized[,l]<-abs(dist_colmedian_M1[,l]/max(abs(dist_colmedian_M1[,l])))
}

# Change structure
dist_colmedian_M1_normalized_melted <- melt(dist_colmedian_M1_normalized)

# Last preparation before plotting
spline_merged_M1 <- merge(spline_sub_melt_M1,dist_colmedian_M1_normalized_melted,by=c("Var1","Var2"))
spline_merged_M1$value.y_minus1 <- 1-spline_merged_M1$value.y

temp1 <- aggregate(spline_merged_M1$value.y_minus1, list(spline_merged_M1$Var1), median, na.rm=T) 
temp2 <- aggregate(spline_merged_M1$value.x, list(spline_merged_M1$Var2), median, na.rm=T)

temp3 <- aggregate(spline_merged_M1$value.x, list(spline_merged_M1$Var2), quantile, probs=0.025,na.rm=T) 
temp4 <- aggregate(spline_merged_M1$value.x, list(spline_merged_M1$Var2), quantile, probs=0.975,na.rm=T)

spline_merged_M1 <- merge(spline_merged_M1, temp1, by.x=c("Var1"), by.y=c("Group.1"))
spline_merged_M1 <- merge(spline_merged_M1, temp2, by.x=c("Var2"), by.y=c("Group.1"))
colnames(spline_merged_M1)[7]<-"average_effect"

spline_merged_M1<-merge(spline_merged_M1, temp3, by.x=c("Var2"), by.y=c("Group.1"))
colnames(spline_merged_M1)[8]<-"lower_bound"

spline_merged_M1<-merge(spline_merged_M1, temp4, by.x=c("Var2"), by.y=c("Group.1"))
colnames(spline_merged_M1)[9]<-"upper_bound"

temp5 <- cbind(seq(10,50,0.225),seq(1:length(seq(10,50,0.225))))
colnames(temp5) <- c("Temp_bin", "Var2")
spline_merged_M1 <-merge(spline_merged_M1, temp5, by=c("Var2"))

# Plot: hourly temperature effect
model_1 <- ggplot()+ ggtitle("Hourly temperature effects: Equal Knots 3") + 
  theme(panel.grid.major = element_blank(),
        plot.title = element_text(family="Times New Roman", size=20, colour = "grey25", hjust=0.5),
        axis.title = element_text(family="Times New Roman", size=16, colour = "grey25"),
        axis.text = element_text(family="Times New Roman", size=16),
        panel.grid.minor = element_blank(),
        panel.background = element_blank(),
        axis.line = element_line(colour = "black"),
        legend.position="none")+
  xlab("Temperature (°C)") + ylab("Log yield response (Kg/ha)")+
  scale_x_continuous(breaks = seq(10,55, by=5), limits = c(5,55))+
  scale_y_continuous(breaks = seq(-.02,.02, by=.004), limits = c(-.02,.02))+
  geom_hline(yintercept=c(seq(-.02,.02,by=0.004)), colour="gray")+
  geom_vline(xintercept=temp_knot_df, linetype="dotted", colour="gray")+
  geom_line(data=spline_merged_M1, aes(y=value.x, x=Temp_bin, group=Var1, alpha=x.x^200/200), size=.01, colour="brown1")+
  geom_line(data=spline_merged_M1, aes(y=average_effect, x=Temp_bin), alpha=0.85, size=.75, colour="brown4")+
  geom_line(data=spline_merged_M1, aes(y=lower_bound, x=Temp_bin)   , alpha=0.85, size=.75, colour="brown4")+       
  geom_line(data=spline_merged_M1, aes(y=upper_bound, x=Temp_bin)   , alpha=0.85, size=.75, colour="brown4")

model_1
ggsave("C:/Users/Shakila Vyas/OneDrive - Wageningen University & Research/PhD-WUR-Insurance/CSA/County analysis/Results/soydis_IMD_eq3_discombo_2000.png", width = 20, height = 20, units = "cm")

#####################model 3: quantile 3 knots
# 10%, 50%, 90% quantile
temp_knot_df <- c(quantile(temp$temperature,0.1, type=1),
                  quantile(temp$temperature,0.5, type=1),
                  quantile(temp$temperature,0.9, type=1))
temp_knot_df
# New TS for temperature support of interest- .225 to get the nice plot with small intervals
temp1 <- as.matrix(Hmisc::rcspline.eval(temp$temperature,knots=temp_knot_df,inclx=T))
temp2 <- cbind(temp,temp1)

names(temp2)
head(temp1)

#adjust variables depending on knots
temp3<-temp2 %>%
  group_by(farm,year)%>%
  summarise_at(vars(x,V2),sum)

#match names with temp
names(regdata_soy1)
hist(regdata_soy1$Yield..Tonnes.Hectare.)
colnames(regdata_soy1)[colnames(regdata_soy1) == "Year"] <- "year"
colnames(regdata_soy1)[colnames(regdata_soy1) == "Dis_codename"] <- "farm"
names(regdata_soy1)

#join both  data
temp4 <- join(regdata_soy1,temp3, by=c("farm", "year"))

names(temp4)
temp4$reg_year<- as.numeric(as.factor(temp4$District))*temp4$year

#####bootstrapping
set.seed(123)
marginal_temperatures_M1 <- rcspline.eval(seq(10,50,0.225),knots=temp_knot_df, inclx=T)

# Run 1000 models to derive uncertainty in estimates
spline_fitted_M1 <- matrix(NA,nrow=1000, ncol=nrow(marginal_temperatures_M1))

for (l in 1: 1000){
  # Sample by region times year
  data_temp <- temp4[which(temp4$reg_year %in% sample(unique(temp4$reg_year),replace=T)),]
  
  # Run regression with random subsample (data_temp), adjust variables to knots
  fit_boot  <- feols(log(Yield..Tonnes.Hectare.) ~ x + V2  + year + I(year^2) + summerrainsum + I(summerrainsum^2)  | farm , data_temp)
  
  #Save marginal effect of temperatures,adjust variables to knots 
  spline_fitted_M1[l,]<-as.vector((marginal_temperatures_M1  %*% coef(fit_boot)[1:2])) 
  
  # Show progress in %
  print(l/1000 *100)
  rm(fit_boot,data_temp)
}

# Load the packages
#Change structure
spline_sub_melt_M1 <- melt(spline_fitted_M1)
# Get median response for each column (= a temperature from seq(20,40,0.225))
colmedians_spline_M1 <- colMedians(spline_fitted_M1)
# Get distance from median response for each value
dist_colmedian_M1 <- matrix(NA, nrow=nrow(spline_fitted_M1),ncol=ncol(spline_fitted_M1))

for(l in 1:ncol(spline_fitted_M1)){
  dist_colmedian_M1[,l] <- spline_fitted_M1[,l] - colmedians_spline_M1[l]
}
# Normalize distance to median
dist_colmedian_M1_normalized <- matrix(NA, nrow=nrow(spline_fitted_M1),ncol=ncol(spline_fitted_M1))

for(l in 1:ncol(spline_fitted_M1)){
  dist_colmedian_M1_normalized[,l]<-abs(dist_colmedian_M1[,l]/max(abs(dist_colmedian_M1[,l])))
}

# Change structure
dist_colmedian_M1_normalized_melted <- melt(dist_colmedian_M1_normalized)

# Last preparation before plotting
spline_merged_M1 <- merge(spline_sub_melt_M1,dist_colmedian_M1_normalized_melted,by=c("Var1","Var2"))
spline_merged_M1$value.y_minus1 <- 1-spline_merged_M1$value.y

temp1 <- aggregate(spline_merged_M1$value.y_minus1, list(spline_merged_M1$Var1), median, na.rm=T) 
temp2 <- aggregate(spline_merged_M1$value.x, list(spline_merged_M1$Var2), median, na.rm=T)

temp3 <- aggregate(spline_merged_M1$value.x, list(spline_merged_M1$Var2), quantile, probs=0.025,na.rm=T) 
temp4 <- aggregate(spline_merged_M1$value.x, list(spline_merged_M1$Var2), quantile, probs=0.975,na.rm=T)

spline_merged_M1 <- merge(spline_merged_M1, temp1, by.x=c("Var1"), by.y=c("Group.1"))
spline_merged_M1 <- merge(spline_merged_M1, temp2, by.x=c("Var2"), by.y=c("Group.1"))
colnames(spline_merged_M1)[7]<-"average_effect"

spline_merged_M1<-merge(spline_merged_M1, temp3, by.x=c("Var2"), by.y=c("Group.1"))
colnames(spline_merged_M1)[8]<-"lower_bound"

spline_merged_M1<-merge(spline_merged_M1, temp4, by.x=c("Var2"), by.y=c("Group.1"))
colnames(spline_merged_M1)[9]<-"upper_bound"

temp5 <- cbind(seq(10,50,0.225),seq(1:length(seq(10,50,0.225))))
colnames(temp5) <- c("Temp_bin", "Var2")
spline_merged_M1 <-merge(spline_merged_M1, temp5, by=c("Var2"))

# Plot: hourly temperature effect

model_1 <- ggplot()+ ggtitle("Hourly temperature effects: Quantile Knots 3") + 
  theme(panel.grid.major = element_blank(),
        plot.title = element_text(family="Times New Roman", size=20, colour = "grey25", hjust=0.5),
        axis.title = element_text(family="Times New Roman", size=16, colour = "grey25"),
        axis.text = element_text(family="Times New Roman", size=16),
        panel.grid.minor = element_blank(),
        panel.background = element_blank(),
        axis.line = element_line(colour = "black"),
        legend.position="none")+
  xlab("Temperature (°C)") + ylab("Log yield response (Kg/ha)")+
  scale_x_continuous(breaks = seq(10,55, by=5), limits = c(5,55))+
  scale_y_continuous(breaks = seq(-.02,.02, by=.004), limits = c(-.02,.02))+
  geom_hline(yintercept=c(seq(-.02,.02,by=0.004)), colour="gray")+
  geom_vline(xintercept=temp_knot_df, linetype="dotted", colour="gray")+
  geom_line(data=spline_merged_M1, aes(y=value.x, x=Temp_bin, group=Var1, alpha=x.x^200/200), size=.01, colour="brown1")+
  geom_line(data=spline_merged_M1, aes(y=average_effect, x=Temp_bin), alpha=0.85, size=.75, colour="brown4")+
  geom_line(data=spline_merged_M1, aes(y=lower_bound, x=Temp_bin)   , alpha=0.85, size=.75, colour="brown4")+       
  geom_line(data=spline_merged_M1, aes(y=upper_bound, x=Temp_bin)   , alpha=0.85, size=.75, colour="brown4")

model_1
ggsave("C:/Users/Shakila Vyas/OneDrive - Wageningen University & Research/PhD-WUR-Insurance/CSA/County analysis/Results/soydis_IMD_qq3_discombo_2000.png", width = 20, height = 20, units = "cm")

#####################model 2: equal spaced 4 knots
temp_knot_df <- c(min(temp$temperature) + ((max(temp$temperature)-min(temp$temperature)) / 5),
                  min(temp$temperature) + 2* ((max(temp$temperature)-min(temp$temperature)) / 5),
                  min(temp$temperature) + 3* ((max(temp$temperature)-min(temp$temperature)) / 5),
                  min(temp$temperature) + 4* ((max(temp$temperature)-min(temp$temperature)) / 5))
temp_knot_df
# New TS for temperature support of interest- .225 to get the nice plot with small intervals
temp1 <- as.matrix(Hmisc::rcspline.eval(temp$temperature,knots=temp_knot_df,inclx=T))
temp2 <- cbind(temp,temp1)

names(temp2)
head(temp1)

#adjust variables depending on knots
temp3<-temp2 %>%
  group_by(farm,year)%>%
  summarise_at(vars(x,V2,V3),sum)

#match names with temp
names(regdata_soy1)
colnames(regdata_soy1)[colnames(regdata_soy1) == "Year"] <- "year"
colnames(regdata_soy1)[colnames(regdata_soy1) == "Dis_codename"] <- "farm"
names(regdata_soy1)

#join both  data
temp4 <- join(regdata_soy1,temp3, by=c("farm", "year"))

names(temp4)
temp4$reg_year<- as.numeric(as.factor(temp4$District))*temp4$year

#####bootstrapping
set.seed(123)
marginal_temperatures_M1 <- rcspline.eval(seq(10,50,0.225),knots=temp_knot_df, inclx=T)

# Run 1000 models to derive uncertainty in estimates
spline_fitted_M1 <- matrix(NA,nrow=1000, ncol=nrow(marginal_temperatures_M1))

for (l in 1: 1000){
  # Sample by region times year
  data_temp <- temp4[which(temp4$reg_year %in% sample(unique(temp4$reg_year),replace=T)),]
  
  # Run regression with random subsample (data_temp), adjust variables to knots
  fit_boot  <- feols(log(Yield..Tonnes.Hectare.) ~ x + V2 + V3 + year + I(year^2) + summerrainsum + I(summerrainsum^2)  | farm , data_temp)
  
  #Save marginal effect of temperatures,adjust variables to knots 
  spline_fitted_M1[l,]<-as.vector((marginal_temperatures_M1  %*% coef(fit_boot)[1:3])) 
  
  # Show progress in %
  print(l/1000 *100)
  rm(fit_boot,data_temp)
}

# Load the packages
library(fixest)
library(ggplot2)
library(gridExtra)
library(robustbase)
library(egg)
library(matrixStats)
library(tidyverse)

#Change structure
spline_sub_melt_M1 <- melt(spline_fitted_M1)
# Get median response for each column (= a temperature from seq(20,40,0.225))
colmedians_spline_M1 <- colMedians(spline_fitted_M1)
# Get distance from median response for each value
dist_colmedian_M1 <- matrix(NA, nrow=nrow(spline_fitted_M1),ncol=ncol(spline_fitted_M1))

for(l in 1:ncol(spline_fitted_M1)){
  dist_colmedian_M1[,l] <- spline_fitted_M1[,l] - colmedians_spline_M1[l]
}
# Normalize distance to median
dist_colmedian_M1_normalized <- matrix(NA, nrow=nrow(spline_fitted_M1),ncol=ncol(spline_fitted_M1))

for(l in 1:ncol(spline_fitted_M1)){
  dist_colmedian_M1_normalized[,l]<-abs(dist_colmedian_M1[,l]/max(abs(dist_colmedian_M1[,l])))
}

# Change structure
dist_colmedian_M1_normalized_melted <- melt(dist_colmedian_M1_normalized)

# Last preparation before plotting
spline_merged_M1 <- merge(spline_sub_melt_M1,dist_colmedian_M1_normalized_melted,by=c("Var1","Var2"))
spline_merged_M1$value.y_minus1 <- 1-spline_merged_M1$value.y

temp1 <- aggregate(spline_merged_M1$value.y_minus1, list(spline_merged_M1$Var1), median, na.rm=T) 
temp2 <- aggregate(spline_merged_M1$value.x, list(spline_merged_M1$Var2), median, na.rm=T)

temp3 <- aggregate(spline_merged_M1$value.x, list(spline_merged_M1$Var2), quantile, probs=0.025,na.rm=T) 
temp4 <- aggregate(spline_merged_M1$value.x, list(spline_merged_M1$Var2), quantile, probs=0.975,na.rm=T)

spline_merged_M1 <- merge(spline_merged_M1, temp1, by.x=c("Var1"), by.y=c("Group.1"))
spline_merged_M1 <- merge(spline_merged_M1, temp2, by.x=c("Var2"), by.y=c("Group.1"))
colnames(spline_merged_M1)[7]<-"average_effect"

spline_merged_M1<-merge(spline_merged_M1, temp3, by.x=c("Var2"), by.y=c("Group.1"))
colnames(spline_merged_M1)[8]<-"lower_bound"

spline_merged_M1<-merge(spline_merged_M1, temp4, by.x=c("Var2"), by.y=c("Group.1"))
colnames(spline_merged_M1)[9]<-"upper_bound"

temp5 <- cbind(seq(10,50,0.225),seq(1:length(seq(10,50,0.225))))
colnames(temp5) <- c("Temp_bin", "Var2")
spline_merged_M1 <-merge(spline_merged_M1, temp5, by=c("Var2"))

# Plot: hourly temperature effect

model_2 <- ggplot()+ ggtitle("Hourly temperature effects: Equal knots 4") + 
  theme(panel.grid.major = element_blank(),
        plot.title = element_text(family="Times New Roman", size=20, colour = "grey25", hjust=0.5),
        axis.title = element_text(family="Times New Roman", size=16, colour = "grey25"),
        axis.text = element_text(family="Times New Roman", size=16),
        panel.grid.minor = element_blank(),
        panel.background = element_blank(),
        axis.line = element_line(colour = "black"),
        legend.position="none")+
  xlab("Temperature (°C)") + ylab("Log yield response (Kg/ha)")+
  scale_x_continuous(breaks = seq(10,55, by=5), limits = c(5,55))+
  scale_y_continuous(breaks = seq(-.02,.02, by=.004), limits = c(-.02,.02))+
  geom_hline(yintercept=c(seq(-.02,.02,by=0.004)), colour="gray")+
  geom_vline(xintercept=temp_knot_df, linetype="dotted", colour="gray")+
  geom_line(data=spline_merged_M1, aes(y=value.x, x=Temp_bin, group=Var1, alpha=x.x^200/200), size=.01, colour="brown1")+
  geom_line(data=spline_merged_M1, aes(y=average_effect, x=Temp_bin), alpha=0.85, size=.75, colour="brown4")+
  geom_line(data=spline_merged_M1, aes(y=lower_bound, x=Temp_bin)   , alpha=0.85, size=.75, colour="brown4")+       
  geom_line(data=spline_merged_M1, aes(y=upper_bound, x=Temp_bin)   , alpha=0.85, size=.75, colour="brown4")

model_2
ggsave("C:/Users/Shakila Vyas/OneDrive - Wageningen University & Research/PhD-WUR-Insurance/CSA/County analysis/Results/soydis_IMD_eq4_discombo_2000.png", width = 20, height = 20, units = "cm")

################################
#area charts for county analysis
# Load ggplot2
library(ggplot2)
country<-c("North","South","East", "West")
key<-c("North","South","East", "West")
value<-c(.003,.0434,.0107,.942)
data <-data.frame(country,key,value)
#Create variable
data$key2 <- c("0.3%","4.34%","1.07%", "94.2%")
data$key2 <- paste0(data$key, " ", data$key2)


botswana <- ggplot(data, aes("", value, fill = key2)) + 
  geom_bar(stat = "identity", color = "white", size = 1) +
  coord_polar(theta = "y") +
  labs(colour = NULL) +
  theme_void()

botswana <- botswana + theme(legend.title = element_blank()) + 
  guides(color = guide_legend(override.aes = list(size = 20)))
botswana 

ggsave("C:/Users/Shakila Vyas/OneDrive - Wageningen University & Research/PhD-WUR-Insurance/CSA/County analysis/Results/soyareachart.png", width = 20, height = 20, units = "cm")

#subsamples for wheat
#subset region
#remove less than 3 years data
regdata_wheat1<- subset(regdata_wheat,Year.1>=2015,select = District:n)
regdata_wheat2<- subset(regdata_wheat1,n>=3,select = District:n)
table(regdata_wheat2$area)
regdata_wheat2_e<- subset(regdata_wheat2,area=="east",select = District:n)
regdata_wheat2_w<- subset(regdata_wheat2,area=="west",select = District:n)
regdata_wheat2_n<- subset(regdata_wheat2,area=="north",select = District:n)
regdata_wheat2_s<- subset(regdata_wheat2,area=="south",select = District:n)

#####################best fit taken from county results
temp_knot_df <- c(12,20,32)

# New TS for temperature support of interest- .225 to get the nice plot with small intervals
temp1 <- as.matrix(Hmisc::rcspline.eval(temp$temperature,knots=temp_knot_df,inclx=T))
temp2 <- cbind(temp,temp1)

names(temp2)
head(temp1)

#adjust variables depending on knots
temp3<-temp2 %>%
  group_by(farm,year)%>%
  summarise_at(vars(x,V2),sum)

#match names with temp
names(regdata_wheat2_s)
colnames(regdata_wheat2_s)[colnames(regdata_wheat2_s) == "Year.1"] <- "year"
colnames(regdata_wheat2_s)[colnames(regdata_wheat2_s) == "Dist_code"] <- "farm"
names(regdata_wheat2_s)

#join both  data
temp4 <- join(regdata_wheat2_s,temp3, by=c("farm", "year"))

names(temp4)
temp4$reg_year<- as.numeric(as.factor(temp4$District))*temp4$year

library(fixest)
wheat_yield_spline = feols(log(Yield..Tonnes.Hectare.) ~ x + V2 + year + I(year^2) + erarain + I(erarain^2)   | farm , cluster = c( "year","farm"), temp4)
summary(wheat_yield_spline)

#####bootstrapping
set.seed(123)
marginal_temperatures_M1 <- rcspline.eval(seq(0,50,0.225),knots=temp_knot_df, inclx=T)

# Run 1000 models to derive uncertainty in estimates
spline_fitted_M1 <- matrix(NA,nrow=1000, ncol=nrow(marginal_temperatures_M1))

for (l in 1: 1000){
  # Sample by region times year
  data_temp <- temp4[which(temp4$reg_year %in% sample(unique(temp4$reg_year),replace=T)),]
  
  # Run regression with random subsample (data_temp), adjust variables to knots
  fit_boot  <- feols(Yield..Tonnes.Hectare. ~ x + V2  + year + I(year^2) + erarain + I(erarain^2)  | farm , data_temp)
  
  #Save marginal effect of temperatures,adjust variables to knots 
  spline_fitted_M1[l,]<-as.vector((marginal_temperatures_M1  %*% coef(fit_boot)[1:2])) 
  
  # Show progress in %
  print(l/1000 *100)
  rm(fit_boot,data_temp)
}

# Load the packages
library(fixest)
library(ggplot2)
library(gridExtra)
library(robustbase)
library(egg)
library(matrixStats)
library(tidyverse)

#Change structure
spline_sub_melt_M1 <- melt(spline_fitted_M1)
# Get median response for each column (= a temperature from seq(20,40,0.225))
colmedians_spline_M1 <- colMedians(spline_fitted_M1)
# Get distance from median response for each value
dist_colmedian_M1 <- matrix(NA, nrow=nrow(spline_fitted_M1),ncol=ncol(spline_fitted_M1))

for(l in 1:ncol(spline_fitted_M1)){
  dist_colmedian_M1[,l] <- spline_fitted_M1[,l] - colmedians_spline_M1[l]
}
# Normalize distance to median
dist_colmedian_M1_normalized <- matrix(NA, nrow=nrow(spline_fitted_M1),ncol=ncol(spline_fitted_M1))

for(l in 1:ncol(spline_fitted_M1)){
  dist_colmedian_M1_normalized[,l]<-abs(dist_colmedian_M1[,l]/max(abs(dist_colmedian_M1[,l])))
}

# Change structure
dist_colmedian_M1_normalized_melted <- melt(dist_colmedian_M1_normalized)

# Last preparation before plotting
spline_merged_M1 <- merge(spline_sub_melt_M1,dist_colmedian_M1_normalized_melted,by=c("Var1","Var2"))
spline_merged_M1$value.y_minus1 <- 1-spline_merged_M1$value.y

temp1 <- aggregate(spline_merged_M1$value.y_minus1, list(spline_merged_M1$Var1), median, na.rm=T) 
temp2 <- aggregate(spline_merged_M1$value.x, list(spline_merged_M1$Var2), median, na.rm=T)

temp3 <- aggregate(spline_merged_M1$value.x, list(spline_merged_M1$Var2), quantile, probs=0.025,na.rm=T) 
temp4 <- aggregate(spline_merged_M1$value.x, list(spline_merged_M1$Var2), quantile, probs=0.975,na.rm=T)

spline_merged_M1 <- merge(spline_merged_M1, temp1, by.x=c("Var1"), by.y=c("Group.1"))
spline_merged_M1 <- merge(spline_merged_M1, temp2, by.x=c("Var2"), by.y=c("Group.1"))
colnames(spline_merged_M1)[7]<-"average_effect"

spline_merged_M1<-merge(spline_merged_M1, temp3, by.x=c("Var2"), by.y=c("Group.1"))
colnames(spline_merged_M1)[8]<-"lower_bound"

spline_merged_M1<-merge(spline_merged_M1, temp4, by.x=c("Var2"), by.y=c("Group.1"))
colnames(spline_merged_M1)[9]<-"upper_bound"

temp5 <- cbind(seq(0,50,0.225),seq(1:length(seq(0,50,0.225))))
colnames(temp5) <- c("Temp_bin", "Var2")
spline_merged_M1 <-merge(spline_merged_M1, temp5, by=c("Var2"))

# Plot: hourly temperature effect
model_1 <- ggplot()+ ggtitle("Hourly temperature effects: South") + 
  theme(panel.grid.major = element_blank(),
        plot.title = element_text(family="Times New Roman", size=20, colour = "grey25", hjust=0.5),
        axis.title = element_text(family="Times New Roman", size=16, colour = "grey25"),
        axis.text = element_text(family="Times New Roman", size=16),
        panel.grid.minor = element_blank(),
        panel.background = element_blank(),
        axis.line = element_line(colour = "black"),
        legend.position="none")+
  xlab("Temperature (°C)") + ylab("Log yield response (Kg/ha)")+
  scale_x_continuous(breaks = seq(5,50, by=5), limits = c(5,50))+
  scale_y_continuous(breaks = seq(-.014,.014, by=.002), limits = c(-.014,.014))+
  geom_hline(yintercept=c(seq(-.014,.014, by=.002)), colour="gray")+
  geom_vline(xintercept=temp_knot_df, linetype="dotted", colour="gray")+
  geom_line(data=spline_merged_M1, aes(y=value.x, x=Temp_bin, group=Var1, alpha=x.x^200/200), size=.01, colour="darkorange1")+
  geom_line(data=spline_merged_M1, aes(y=average_effect, x=Temp_bin), alpha=0.85, size=.75, colour="darkorange4")+
  geom_line(data=spline_merged_M1, aes(y=lower_bound, x=Temp_bin)   , alpha=0.85, size=.75, colour="darkorange4")+       
  geom_line(data=spline_merged_M1, aes(y=upper_bound, x=Temp_bin)   , alpha=0.85, size=.75, colour="darkorange4")

model_1

ggsave("C:/Users/Shakila Vyas/OneDrive - Wageningen University & Research/PhD-WUR-Insurance/CSA/County analysis/Results/wheatdis_era_bestfit_south.png", width = 20, height = 20, units = "cm")

#area charts for county
# Load ggplot2
library(ggplot2)
country<-c("North","South","East", "West")
key<-c("North","South","East", "West")
value<-c(.525,.005,.081,.388)
data <-data.frame(country,key,value)
#Create variable
data$key2 <- c("52.5%",".5%","8.1%", "38.8%")
data$key2 <- paste0(data$key, " ", data$key2)
india <- ggplot(data, aes("", value, fill = key2)) + 
  geom_bar(stat = "identity", color = "white", size = 1) +
  coord_polar(theta = "y") +
  labs(colour = NULL) +
  theme_void()

india <- india + theme(legend.title = element_blank()) + 
  guides(color = guide_legend(override.aes = list(size = 20)))
india 

ggsave("C:/Users/Shakila Vyas/OneDrive - Wageningen University & Research/PhD-WUR-Insurance/CSA/County analysis/Results/wheatareachart.png", width = 20, height = 20, units = "cm")