#1. Weather data preparation
library(raster)
library(sp)
library(tidyverse)
library(rgdal)
library(readxl)
library(rgeos)
library(RColorBrewer)
library(sf) 
library(ncdf4)
library(raster)
library(rasterVis)
library(RColorBrewer)
#read file with coordinates of CSA farms (from the survey)
#below is an example to write IMD weather files, change file paths for ERA data
coords<- read.csv("CSA/Coordinates.csv")
#processing IMD data
maxtempimd15 <- raster::stack("CSA/IMD/2015.nc")
mxtp15 <- raster::extract(maxtempimd15 ,coords)
write.csv(mxtp15,"CSA/mxtpimd15.csv")
maxtempimd16 <- raster::stack("CSA/IMD/2016.nc")
mxtp16 <- raster::extract(maxtempimd16 ,coords)
write.csv(mxtp16,"CSA/mxtpimd16.csv")
maxtempimd17 <- raster::stack("CSA/IMD/2017.nc")
mxtp17 <- raster::extract(maxtempimd17 ,coords)
write.csv(mxtp17,"CSA/mxtpimd17.csv")
maxtempimd18 <- raster::stack("CSA/IMD/2018.nc")
mxtp18 <- raster::extract(maxtempimd18 ,coords)
write.csv(mxtp18,"CSA/mxtpimd18.csv")
maxtempimd19 <- raster::stack("CSA/IMD/2019.nc")
mxtp19 <- raster::extract(maxtempimd19 ,coords)
write.csv(mxtp19,"CSA/mxtpimd19.csv")
maxtempimd20 <- raster::stack("CSA/IMD/2020.nc")
mxtp20 <- raster::extract(maxtempimd20 ,coords)
write.csv(mxtp20,"CSA/mxtpimd20.csv")
#min temp
mintempimd15 <- raster::stack("CSA/IMD/Min/2015.nc")
mntp15 <- raster::extract(mintempimd15 ,coords)
write.csv(mntp15,"CSA/mntpimd15.csv")
mintempimd16 <- raster::stack("CSA/IMD/Min/2016.nc")
mntp16 <- raster::extract(mintempimd16 ,coords)
write.csv(mntp16,"CSA/mntpimd16.csv")
mintempimd17 <- raster::stack("CSA/IMD/Min/2017.nc")
mntp17 <- raster::extract(mintempimd17 ,coords)
write.csv(mntp17,"CSA/mntpimd17.csv")
mintempimd18 <- raster::stack("CSA/IMD/Min/2018.nc")
mntp18 <- raster::extract(mintempimd18 ,coords)
write.csv(mntp18,"CSA/mntpimd18.csv")
mintempimd19 <- raster::stack("CSA/IMD/Min/2019.nc")
mntp19 <- raster::extract(mintempimd19 ,coords)
write.csv(mntp19,"CSA/mntpimd19.csv")
mintempimd20 <- raster::stack("CSA/IMD/Min/2020.nc")
mntp20 <- raster::extract(mintempimd20 ,coords)
write.csv(mntp20,"CSA/mntpimd20.csv")
####IMD rainfall
rainimd15 <- raster::stack("CSA/IMD/Rain/_Clim_Pred_LRF_New_RF25_IMD0p252015.nc")
rain15 <- raster::extract(rainimd15 ,coords)
write.csv(rain15,"CSA/rainimd15.csv")
rainimd16 <- raster::stack("CSA/IMD/Rain/_Clim_Pred_LRF_New_RF25_IMD0p252016.nc")
rain16 <- raster::extract(rainimd16 ,coords)
write.csv(rain16,"CSA/rainimd16.csv")
rainimd17 <- raster::stack("CSA/IMD/Rain/_Clim_Pred_LRF_New_RF25_IMD0p252017.nc")
rain17 <- raster::extract(rainimd17 ,coords)
write.csv(rain17,"CSA/rainimd17.csv")
rainimd18 <- raster::stack("CSA/IMD/Rain/_Clim_Pred_LRF_New_RF25_IMD0p252018.nc")
rain18 <- raster::extract(rainimd18 ,coords)
write.csv(rain18,"CSA/rainimd18.csv")
rainimd19 <- raster::stack("CSA/IMD/Rain/_Clim_Pred_LRF_New_RF25_IMD0p252019.nc")
rain19 <- raster::extract(rainimd19 ,coords)
write.csv(rain19,"CSA/rainimd19.csv")
rainimd20 <- raster::stack("CSA/IMD/Rain/_Clim_Pred_LRF_New_RF25_IMD0p252020.nc")
rain20 <- raster::extract(rainimd20 ,coords)
write.csv(rain20,"CSA/rainimd20.csv")

########creating hourly temp. splines###########################################
#rm(list=ls())
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
setwd("C:/Users/Shakila Vyas/OneDrive - Wageningen University & Research/PhD-WUR-Insurance/CSA/Weather data")
#read data; farms in rows, daily weather observations in columns
temp.min <- as.matrix(read.csv("C:/Users/Shakila Vyas/OneDrive - Wageningen University & Research/PhD-WUR-Insurance/CSA/Weather data/mntp15 - Summer.csv")) #load daily minimum temperatures at the farm level
temp.max <- as.matrix(read.csv("C:/Users/Shakila Vyas/OneDrive - Wageningen University & Research/PhD-WUR-Insurance/CSA/Weather data/mxtp15 - Summer.csv")) #load daily minimum temperatures at the farm level

#delete rownumber
temp.min <- temp.min[,-1] 
temp.max <- temp.max[,-1] 

#concert kelvin to Celsius
temp.min <- temp.min-273.15
temp.max <- temp.max-273.15

#delete farms without weather data (farm with mistaken coordinates)
temp.min.mat <- temp.min[complete.cases(temp.min), ]
temp.max.mat <- temp.max[complete.cases(temp.max), ]

#make dataframes to numeric matrices
temp.min.num <- apply(temp.min.mat, 2, as.numeric)
temp.max.num <- apply(temp.max.mat, 2, as.numeric)

days       <- seq(as.Date("2015-06-01"), as.Date("2015-10-31"),by = "day")
start <- "X2015.06.01"
end   <- "X2015.10.31"

months       <- seq(as.Date(substr(start,2,11), format="%Y.%m.%d"), as.Date(substr(end,2,11), format="%Y.%m.%d"),by = "month")

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

write.csv(temp, "C:/Users/Shakila Vyas/OneDrive - Wageningen University & Research/PhD-WUR-Insurance/CSA/Weather data/hourlytp15-Summer.csv", row.names=FALSE)

##formating the results
for(k in 1:length(months)){ 
temp<-as.data.frame(temp)
temp$month <- months[k]
temp$ID <- temp.max.mat[,1]
  
  if (k==1){
    result <- melt(data = temp,   variable.name = "hour",  value.name = "Temp")
    result$hour <- NULL
  }
  if(k>1) { 
    temp <- melt(data = temp,   variable.name = "hour",  value.name = "Temp")
    temp$hour <- temp$hour <- NULL
    result<-rbind(result, temp)
  }
}

rm(result,temp)
rm(temp.ampli_1, temp.ampli_2, temp.mid_1, temp.mid_2, thi.amplitude_1, thi.amplitude_2, thi.mid_1, thi.mid_2)

#write.csv(result, "Samengevoegde data_perexploitatie/THI.hourly_20082014_summermonths.csv", row.names=FALSE)

##################################################################################hourly splines for IMD data
#rm(list=ls())
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
setwd("C:/Users/Shakila Vyas/OneDrive - Wageningen University & Research/PhD-WUR-Insurance/CSA/Weather data")
#read data; farms in rows, daily weather observations in columns
temp.min <- as.matrix(read.csv("C:/Users/Shakila Vyas/OneDrive - Wageningen University & Research/PhD-WUR-Insurance/CSA/Weather data/IMD data/summer/mntpimd20.csv")) #load daily minimum temperatures at the farm level
temp.max <- as.matrix(read.csv("C:/Users/Shakila Vyas/OneDrive - Wageningen University & Research/PhD-WUR-Insurance/CSA/Weather data/IMD data/summer/mxtpimd20.csv")) #load daily minimum temperatures at the farm level

#delete rownumber
temp.min <- temp.min[,-1] 
temp.max <- temp.max[,-1] 

#delete farms without weather data (farm with mistaken coordinates)
temp.min.mat <- temp.min[complete.cases(temp.min), ]
temp.max.mat <- temp.max[complete.cases(temp.max), ]

#make dataframes to numeric matrices
temp.min.num <- apply(temp.min.mat, 2, as.numeric)
temp.max.num <- apply(temp.max.mat, 2, as.numeric)

months       <- seq(as.Date(substr(start,2,11), format="%Y.%m.%d"), as.Date(substr(end,2,11), format="%Y.%m.%d"),by = "month")

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

write.csv(temp, "C:/Users/Shakila Vyas/OneDrive - Wageningen University & Research/PhD-WUR-Insurance/CSA/Weather data/IMD data/hourlytpimd20.csv", row.names=FALSE)

#2. Regression analysis
########Regression analysis- daily data#############################################################################################################
library(fixest)
data<- read.csv("C:/Users/Shakila Vyas/OneDrive - Wageningen University & Research/PhD-WUR-Insurance/CSA/Masterdata_Reg.csv")
head(data)
#remove less than 3 years data
regdata<- subset(data,repeat_id_crop>=3,select = Q1:T45)
head(regdata)
#soybean subset
regdata_soy<- subset(regdata,Q12=="Soybean",select = Q1:T45)
head(regdata_soy)
#check data and save soybean subset from masterdata file
write.csv(regdata_soy,"C:/Users/Shakila Vyas/OneDrive - Wageningen University & Research/PhD-WUR-Insurance/CSA/soy_yieldreg.csv")

#hist yeild and returns
png("C:/Users/Shakila Vyas/OneDrive - Wageningen University & Research/PhD-WUR-Insurance/CSA/Hist_yield.png")
par(mar=c(2,2,2,2))
hist(regdata_soy$Q93,main="Soybean yield (Kg/Ha")
dev.off()

png("C:/Users/Shakila Vyas/OneDrive - Wageningen University & Research/PhD-WUR-Insurance/CSA/Hist_returns.png")
par(mar=c(2,2,2,2))
hist(regdata_soy$Q79,main="Soybean returns (INR/Ha")
dev.off()

#summary stats
summary(regdata_soy$Q93)

##weather histograms
t2015<- read.csv("C:/Users/Shakila Vyas/OneDrive - Wageningen University & Research/PhD-WUR-Insurance/CSA/Weather data/mxtp15.csv")
tc2015<- t2015-273.15
head(tc2015)
tc2015summer <- subset(tc2015,select = X6.1.2015:X10.30.2015)
df = tc2015summer[-1,] 
tc2015summer<-  as.numeric(df)
library(reshape2)
dat<- melt(df)
png("C:/Users/Shakila Vyas/OneDrive - Wageningen University & Research/PhD-WUR-Insurance/CSA/Temp_2015.png")
par(mar=c(2,2,2,2))
hist(dat$value,main="MAX TEMP-2015")
dev.off()

t2016<- read.csv("C:/Users/Shakila Vyas/OneDrive - Wageningen University & Research/PhD-WUR-Insurance/CSA/Weather data/mxtp16.csv")
tc2016<- t2016-273.15
tc2016summer <- subset(tc2016,select = X6.1.2016:X10.30.2016)
df1 = tc2016summer[-1,] 
library(reshape2)
dat1<- melt(df1)
png("C:/Users/Shakila Vyas/OneDrive - Wageningen University & Research/PhD-WUR-Insurance/CSA/Temp_2016.png")
par(mar=c(2,2,2,2))
hist(dat1$value,main="MAX TEMP-2016")
dev.off()

t2017<- read.csv("C:/Users/Shakila Vyas/OneDrive - Wageningen University & Research/PhD-WUR-Insurance/CSA/Weather data/mxtp17.csv")
tc2017<- t2017-273.15
tc2017summer <- subset(tc2017,select = X6.1.2017:X10.30.2017)
df2 = tc2017summer[-1,] 
library(reshape2)
dat2<- melt(df2)
png("C:/Users/Shakila Vyas/OneDrive - Wageningen University & Research/PhD-WUR-Insurance/CSA/Temp_2017.png")
par(mar=c(2,2,2,2))
hist(dat2$value,main="MAX TEMP-2017")
dev.off()

t2018<- read.csv("C:/Users/Shakila Vyas/OneDrive - Wageningen University & Research/PhD-WUR-Insurance/CSA/Weather data/mxtp18.csv")
tc2018<- t2018-273.15
tc2018summer <- subset(tc2018,select = X6.1.2018:X10.30.2018)
df3 = tc2018summer[-1,] 
library(reshape2)
dat3<- melt(df3)
png("C:/Users/Shakila Vyas/OneDrive - Wageningen University & Research/PhD-WUR-Insurance/CSA/Temp_2018.png")
par(mar=c(2,2,2,2))
hist(dat3$value,main="MAX TEMP-2018")
dev.off()

t2019<- read.csv("C:/Users/Shakila Vyas/OneDrive - Wageningen University & Research/PhD-WUR-Insurance/CSA/Weather data/mxtp19.csv")
tc2019<- t2019-273.15
tc2019summer <- subset(tc2019,select = X6.1.2019:X10.30.2019)
df4 = tc2019summer[-1,] 
library(reshape2)
dat4<- melt(df4)
png("C:/Users/Shakila Vyas/OneDrive - Wageningen University & Research/PhD-WUR-Insurance/CSA/Temp_2019.png")
par(mar=c(2,2,2,2))
hist(dat4$value,main="MAX TEMP-2019")
dev.off()

t2020<- read.csv("C:/Users/Shakila Vyas/OneDrive - Wageningen University & Research/PhD-WUR-Insurance/CSA/Weather data/mxtp20.csv")
tc2020<- t2020-273.15
tc2020summer <- subset(tc2020,select = X6.1.2020:X10.30.2020)
df5 = tc2020summer[-1,] 
library(reshape2)
dat5<- melt(df5)
png("C:/Users/Shakila Vyas/OneDrive - Wageningen University & Research/PhD-WUR-Insurance/CSA/Temp_2020.png")
par(mar=c(2,2,2,2))
hist(dat5$value,main="MAX TEMP-2020")
dev.off()

########Regression analysis- hourly data####################################################################
#install below packages if not already downloaded
install.packages("raster")
install.packages("dplyr")
install.packages("data.table")
install.packages("reshape2")
install.packages("Hmisc")
install.packages("lubridate")
install.packages("plyr")
install.packages("esmisc")
install.packages("sandwich")
install.packages("MASS")
install.packages("e1071")

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

#remove previous files if needed
rm(temp)
library(fixest)
data<- read.csv("C:/Users/Shakila Vyas/OneDrive - Wageningen University & Research/PhD-WUR-Insurance/CSA/Masterdata_Reg.csv")
head(data)
table(data$Q90)
#remove less than 3 years data
regdata<- subset(data,repeat_id_crop>=3,select = Q1:T45)
head(regdata)
#soybean subset
regdata_soy<- subset(regdata,Q12=="Soybean",select = Q1:T45)
head(regdata_soy)
print(regdata_soy)
##preparing data
hourly<- read.csv("C:/Users/Shakila Vyas/OneDrive - Wageningen University & Research/PhD-WUR-Insurance/CSA/Weather data/hourlytp15-Summer.csv")
#need this format-colnames(temp_wheat_exposure2) <- c("Farm","Date","Hour", "Temperature")
as.data.frame(hourly)
hourly$farm <- seq.int(nrow(hourly))
library(reshape2)
long <- melt(hourly, id.vars = c("farm"))
long$year<- 2015

hourly1<- read.csv("C:/Users/Shakila Vyas/OneDrive - Wageningen University & Research/PhD-WUR-Insurance/CSA/Weather data/hourlytp16-Summer.csv")
#need this format-colnames(temp_wheat_exposure2) <- c("Farm","Date","Hour", "Temperature")
as.data.frame(hourly1)
hourly1$farm <- seq.int(nrow(hourly1))
library(reshape2)
long1 <- melt(hourly1, id.vars = c("farm"))
long1$year<- 2016

hourly2<- read.csv("C:/Users/Shakila Vyas/OneDrive - Wageningen University & Research/PhD-WUR-Insurance/CSA/Weather data/hourlytp17-Summer.csv")
#need this format-colnames(temp_wheat_exposure2) <- c("Farm","Date","Hour", "Temperature")
as.data.frame(hourly2)
hourly2$farm <- seq.int(nrow(hourly2))
library(reshape2)
long2 <- melt(hourly2, id.vars = c("farm"))
long2$year<- 2017

hourly3<- read.csv("C:/Users/Shakila Vyas/OneDrive - Wageningen University & Research/PhD-WUR-Insurance/CSA/Weather data/hourlytp18-Summer.csv")
#need this format-colnames(temp_wheat_exposure2) <- c("Farm","Date","Hour", "Temperature")
as.data.frame(hourly3)
hourly3$farm <- seq.int(nrow(hourly3))
library(reshape2)
long3 <- melt(hourly3, id.vars = c("farm"))
long3$year<- 2018

hourly4<- read.csv("C:/Users/Shakila Vyas/OneDrive - Wageningen University & Research/PhD-WUR-Insurance/CSA/Weather data/hourlytp19-Summer.csv")
#need this format-colnames(temp_wheat_exposure2) <- c("Farm","Date","Hour", "Temperature")
as.data.frame(hourly4)
hourly4$farm <- seq.int(nrow(hourly4))
library(reshape2)
long4 <- melt(hourly4, id.vars = c("farm"))
long4$year<- 2019

hourly5<- read.csv("C:/Users/Shakila Vyas/OneDrive - Wageningen University & Research/PhD-WUR-Insurance/CSA/Weather data/hourlytp20-Summer.csv")
#need this format-colnames(temp_wheat_exposure2) <- c("Farm","Date","Hour", "Temperature")
as.data.frame(hourly5)
hourly5$farm <- seq.int(nrow(hourly5))
library(reshape2)
long5 <- melt(hourly5, id.vars = c("farm"))
long5$year<- 2020

temp<- rbind(long,long1,long2,long3,long4,long5)

#yearly temp histograms
summary(temp$value)
hist(long$value,main="Hourly temperature 2015",cex.main=2)

##splines and knots
head(regdata_soy)
length(table(regdata_soy$Q2))
length(table(regdata_soy$Q4))
length(table(regdata_soy$Q5))

####outlier removal
summary(regdata_soy$Q93)
quantile(regdata_soy$Q93,probs= c(.99))
regdata_soy1<- subset(regdata_soy,Q93<quantile(regdata_soy$Q93,probs= c(.99)),select = Q1:T45 )
hist(regdata_soy1$Q93)

#observation counts for final data used in the analysis
table(regdata_soy1$Q93)
summary(regdata_soy1$Q93)
library(dplyr)
regdata_soy1 %>% count(regdata_soy1$Q93)
nrow(regdata_soy1$Q93)
table(regdata_soy1$Q71)
summary(regdata_soy1$Summer_rainsum)
hist(regdata_soy1$Q9)

#rename temp data
names(temp)[3] <- "temperature"
min(temp$temperature)
max(temp$temperature)
range(temp$temperature)
summary(temp$temperature)
hist(temp$temperature,main="Hourly temperature-summer")

# Histogram of temperature exposure-for a random farm
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
  scale_x_continuous(breaks = seq(10,50, by=5), limits = c(10,50))

ggsave(temperature_soy, filename=paste("C:/Users/Shakila Vyas/OneDrive - Wageningen University & Research/PhD-WUR-Insurance/CSA/Revised results/ERA5/New results/era5soyhist.png"), 
       dpi=300, height=13, width=18, units="cm")

#####################model 1: equal spaced 3 knots
temp_knot_df <- c(min(temp$temperature) + ((max(temp$temperature)-min(temp$temperature)) / 4),
                      min(temp$temperature) + 2* ((max(temp$temperature)-min(temp$temperature)) / 4),
                      min(temp$temperature) + 3* ((max(temp$temperature)-min(temp$temperature)) / 4))

# New TS for temperature support of interest- .225 to get the nice plot with small intervals
install.packages("scales") 
remove.packages("rlang")
install.packages("rlang")

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
colnames(regdata_soy1)[colnames(regdata_soy1) == "Q90"] <- "year"
colnames(regdata_soy1)[colnames(regdata_soy1) == "spatial_id"] <- "farm"
names(regdata_soy1)

#join both  data
temp4 <- join(regdata_soy1,temp3, by=c("farm", "year"))

names(temp4)
temp4$reg_year<- as.numeric(as.factor(temp4$Q4))*temp4$year

library(fixest)
soy_yield_spline = feols(log(Q93) ~ x + V2 + year + I(year^2) + Summer_rainsum + I(Summer_rainsum^2)   | farm , cluster = c( "year","farm"), temp4)
summary(soy_yield_spline)
results<- etable(soy_yield_spline)
write.csv(results,"C:/Users/Shakila Vyas/OneDrive - Wageningen University & Research/PhD-WUR-Insurance/CSA/ERA5_results_eq_knot3_returns.csv")

#####bootstrapping
set.seed(123)
marginal_temperatures_M1 <- rcspline.eval(seq(10,45,0.225),knots=temp_knot_df, inclx=T)

# Run 1000 models to derive uncertainty in estimates
spline_fitted_M1 <- matrix(NA,nrow=1000, ncol=nrow(marginal_temperatures_M1))

for (l in 1: 1000){
  # Sample by region times year
  data_temp <- temp4[which(temp4$reg_year %in% sample(unique(temp4$reg_year),replace=T)),]
  
  # Run regression with random subsample (data_temp), adjust variables to knots
  fit_boot  <- feols(log(Q93) ~ x + V2  + year + I(year^2) + Summer_rainsum + I(Summer_rainsum^2)  | farm , data_temp)
  
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

temp5 <- cbind(seq(10,45,0.225),seq(1:length(seq(10,45,0.225))))
colnames(temp5) <- c("Temp_bin", "Var2")
spline_merged_M1 <-merge(spline_merged_M1, temp5, by=c("Var2"))

#centering
summary(temp$temperature) #calculate the yield value at mean/median temperature point and use that to centre the plots around mean/median, refer methods

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
  scale_y_continuous(breaks = seq(-.1,.1, by=.02), limits = c(-.1,.1))+
  geom_hline(yintercept=c(seq(-1,1,by=0.02)), colour="gray")+
  geom_vline(xintercept=temp_knot_df, linetype="dotted", colour="gray")+
  geom_line(data=spline_merged_M1, aes(y=value.x-0.004165132, x=Temp_bin, group=Var1, alpha=x.x^200/200), size=.01, colour="brown1")+
  geom_line(data=spline_merged_M1, aes(y=average_effect-0.004165132, x=Temp_bin), alpha=0.85, size=.75, colour="brown4")+
  geom_line(data=spline_merged_M1, aes(y=lower_bound-0.004165132, x=Temp_bin)   , alpha=0.85, size=.75, colour="brown4")+       
  geom_line(data=spline_merged_M1, aes(y=upper_bound-0.004165132, x=Temp_bin)   , alpha=0.85, size=.75, colour="brown4")

model_1
ggsave("C:/Users/Shakila Vyas/OneDrive - Wageningen University & Research/PhD-WUR-Insurance/CSA/Revised results/ERA5/New results/eq3soy_cen.png", width = 20, height = 20, units = "cm")

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
colnames(regdata_soy1)[colnames(regdata_soy1) == "Q90"] <- "year"
colnames(regdata_soy1)[colnames(regdata_soy1) == "spatial_id"] <- "farm"
names(regdata_soy1)

#join both  data
temp4 <- join(regdata_soy1,temp3, by=c("farm", "year"))

names(temp4)
summary(temp4$Q93)
temp4$reg_year<- as.numeric(as.factor(temp4$Q4))*temp4$year

library(fixest)
soy_yield_spline = feols(log(Q93) ~ x + V2 + year + I(year^2) + Summer_rainsum + I(Summer_rainsum^2)   | farm , cluster = c( "year","farm"), temp4)
summary(soy_yield_spline)
results<- etable(soy_yield_spline)
write.csv(results,"C:/Users/Shakila Vyas/OneDrive - Wageningen University & Research/PhD-WUR-Insurance/CSA/ERA5_results_qq_knot3_returns.csv")

#####bootstrapping
set.seed(123)
marginal_temperatures_M1 <- rcspline.eval(seq(10,45,0.225),knots=temp_knot_df, inclx=T)

# Run 1000 models to derive uncertainty in estimates
spline_fitted_M1 <- matrix(NA,nrow=1000, ncol=nrow(marginal_temperatures_M1))

for (l in 1: 1000){
  # Sample by region times year
  data_temp <- temp4[which(temp4$reg_year %in% sample(unique(temp4$reg_year),replace=T)),]
  
  # Run regression with random subsample (data_temp), adjust variables to knots
  fit_boot  <- feols(log(Q93) ~ x + V2 + year + I(year^2) + Summer_rainsum + I(Summer_rainsum^2)  | farm , data_temp)
  
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

temp5 <- cbind(seq(10,45,0.225),seq(1:length(seq(10,45,0.225))))
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
  scale_x_continuous(breaks = seq(10,50, by=5), limits = c(5,50))+
  scale_y_continuous(breaks = seq(-.1,.1, by=.02), limits = c(-.1,.1))+
  geom_hline(yintercept=c(seq(-1,1,by=0.02)), colour="gray")+
  geom_vline(xintercept=temp_knot_df, linetype="dotted", colour="gray")+
  geom_line(data=spline_merged_M1, aes(y=value.x-0.004850659, x=Temp_bin, group=Var1, alpha=x.x^200/200), size=.01, colour="brown1")+
  geom_line(data=spline_merged_M1, aes(y=average_effect-0.004850659, x=Temp_bin), alpha=0.85, size=.75, colour="brown4")+
  geom_line(data=spline_merged_M1, aes(y=lower_bound-0.004850659, x=Temp_bin)   , alpha=0.85, size=.75, colour="brown4")+       
  geom_line(data=spline_merged_M1, aes(y=upper_bound-0.004850659, x=Temp_bin)   , alpha=0.85, size=.75, colour="brown4")

model_1
ggsave("C:/Users/Shakila Vyas/OneDrive - Wageningen University & Research/PhD-WUR-Insurance/CSA/Revised results/ERA5/New results/qq3soy_cen.png", width = 20, height = 20, units = "cm")

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
colnames(regdata_soy1)[colnames(regdata_soy1) == "Q90"] <- "year"
colnames(regdata_soy1)[colnames(regdata_soy1) == "spatial_id"] <- "farm"
names(regdata_soy1)

#join both  data
temp4 <- join(regdata_soy1,temp3, by=c("farm", "year"))
names(temp4)
temp4$reg_year<- as.numeric(as.factor(temp4$Q4))*temp4$year

library(fixest)
soy_yield_spline = feols(log(Q93) ~ x + V2 + V3 + year + I(year^2) + Summer_rainsum + I(Summer_rainsum^2)   | farm , cluster = c( "year","farm"), temp4)
summary(soy_yield_spline)
results<- etable(soy_yield_spline)
write.csv(results,"C:/Users/Shakila Vyas/OneDrive - Wageningen University & Research/PhD-WUR-Insurance/CSA/ERA5_results_eq_knot4.csv")

#others
soy_yield_spline = feols(x ~ x + V2 + V3 + year + I(year^2) + Summer_rainsum + I(Summer_rainsum^2)   | farm , cluster = c( "year","farm"), temp4)
summary(soy_yield_spline)
results<- etable(soy_yield_spline)
write.csv(results,"C:/Users/Shakila Vyas/OneDrive - Wageningen University & Research/PhD-WUR-Insurance/CSA/ERA5_results_eq_knot4_returns.csv")

#####bootstrapping
set.seed(123)
marginal_temperatures_M1 <- rcspline.eval(seq(10,45,0.225),knots=temp_knot_df, inclx=T)

# Run 1000 models to derive uncertainty in estimates
spline_fitted_M1 <- matrix(NA,nrow=1000, ncol=nrow(marginal_temperatures_M1))

for (l in 1: 1000){
  # Sample by region times year
  data_temp <- temp4[which(temp4$reg_year %in% sample(unique(temp4$reg_year),replace=T)),]
  
  # Run regression with random subsample (data_temp), adjust variables to knots
  fit_boot  <- feols(log(Q93) ~ x + V2 +V3 + year + I(year^2) + Summer_rainsum + I(Summer_rainsum^2)  | farm ,data_temp)
  
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

temp5 <- cbind(seq(10,45,0.225),seq(1:length(seq(10,45,0.225))))
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
  scale_x_continuous(breaks = seq(10,50, by=5), limits = c(5,50))+
  scale_y_continuous(breaks = seq(-.1,.1, by=.02), limits = c(-.1,.1))+
  geom_hline(yintercept=c(seq(-1,1,by=0.02)), colour="gray")+
  geom_vline(xintercept=temp_knot_df, linetype="dotted", colour="gray")+
  geom_line(data=spline_merged_M1, aes(y=value.x-0.009126775, x=Temp_bin, group=Var1, alpha=x.x^200/200), size=.01, colour="brown1")+
  geom_line(data=spline_merged_M1, aes(y=average_effect-0.009126775, x=Temp_bin), alpha=0.85, size=.75, colour="brown4")+
  geom_line(data=spline_merged_M1, aes(y=lower_bound-0.009126775, x=Temp_bin)   , alpha=0.85, size=.75, colour="brown4")+       
  geom_line(data=spline_merged_M1, aes(y=upper_bound-0.009126775, x=Temp_bin)   , alpha=0.85, size=.75, colour="brown4")

model_2
ggsave("C:/Users/Shakila Vyas/OneDrive - Wageningen University & Research/PhD-WUR-Insurance/CSA/Revised results/ERA5/New results/eq4soy_cen.png", width = 20, height = 20, units = "cm")

#####################model 4: quantile 4 knots######
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
colnames(regdata_soy1)[colnames(regdata_soy1) == "Q90"] <- "year"
colnames(regdata_soy1)[colnames(regdata_soy1) == "spatial_id"] <- "farm"
names(regdata_soy1)

#join both  data
temp4 <- join(regdata_soy1,temp3, by=c("farm", "year"))

names(temp4)
temp4$reg_year<- as.numeric(as.factor(temp4$Q4))*temp4$year

library(fixest)
soy_yield_spline = feols(X ~ x + V2 + V3 + year + I(year^2) + Summer_rainsum + I(Summer_rainsum^2)   | farm , cluster = c( "year","farm"), temp4)
summary(soy_yield_spline)
results<- etable(soy_yield_spline)
write.csv(results,"C:/Users/Shakila Vyas/OneDrive - Wageningen University & Research/PhD-WUR-Insurance/CSA/ERA5_results_qq_knot4_returns.csv")

#####bootstrapping
set.seed(123)
marginal_temperatures_M1 <- rcspline.eval(seq(10,45,0.225),knots=temp_knot_df, inclx=T)

# Run 1000 models to derive uncertainty in estimates
spline_fitted_M1 <- matrix(NA,nrow=1000, ncol=nrow(marginal_temperatures_M1))

for (l in 1: 1000){
  # Sample by region times year
  data_temp <- temp4[which(temp4$reg_year %in% sample(unique(temp4$reg_year),replace=T)),]
  
  # Run regression with random subsample (data_temp), adjust variables to knots
  fit_boot  <- feols(log(Q93) ~ x + V2 +V3 + year + I(year^2) + Summer_rainsum + I(Summer_rainsum^2)  | farm , data_temp)
  
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

temp5 <- cbind(seq(10,45,0.225),seq(1:length(seq(10,45,0.225))))
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
  scale_x_continuous(breaks = seq(10,50, by=5), limits = c(5,50))+
  scale_y_continuous(breaks = seq(-.1,.1, by=.02), limits = c(-.1,.1))+
  geom_hline(yintercept=c(seq(-1,1,by=0.02)), colour="gray")+
  geom_vline(xintercept=temp_knot_df, linetype="dotted", colour="gray")+
  geom_line(data=spline_merged_M1, aes(y=value.x-0.007090758, x=Temp_bin, group=Var1, alpha=x.x^200/200), size=.01, colour="brown1")+
  geom_line(data=spline_merged_M1, aes(y=average_effect-0.007090758, x=Temp_bin), alpha=0.85, size=.75, colour="brown4")+
  geom_line(data=spline_merged_M1, aes(y=lower_bound-0.007090758, x=Temp_bin)   , alpha=0.85, size=.75, colour="brown4")+       
  geom_line(data=spline_merged_M1, aes(y=upper_bound-0.007090758, x=Temp_bin)   , alpha=0.85, size=.75, colour="brown4")

model_2
ggsave("C:/Users/Shakila Vyas/OneDrive - Wageningen University & Research/PhD-WUR-Insurance/CSA/Revised results/ERA5/New results/qq4soy_cen.png", width = 20, height = 20, units = "cm")

################################placing knots at 5 degree intervals, not used in main results
temp_knot_df <- c(25,30,35,40)

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
colnames(regdata_soy1)[colnames(regdata_soy1) == "Q90"] <- "year"
colnames(regdata_soy1)[colnames(regdata_soy1) == "spatial_id"] <- "farm"
names(regdata_soy1)

#join both  data
temp4 <- join(regdata_soy1,temp3, by=c("farm", "year"))

names(temp4)
temp4$reg_year<- as.numeric(as.factor(temp4$Q4))*temp4$year

library(fixest)
soy_yield_spline = feols(log(Q93) ~ x + V2 + V3 + year + I(year^2) + Summer_rainsum + I(Summer_rainsum^2)   | farm , cluster = c( "year","farm"), temp4)
summary(soy_yield_spline)
results<- etable(soy_yield_spline)
write.csv(results,"C:/Users/Shakila Vyas/OneDrive - Wageningen University & Research/PhD-WUR-Insurance/CSA/ERA5_results_5degree_knot4.csv")

#####bootstrapping
set.seed(123)
marginal_temperatures_M1 <- rcspline.eval(seq(10,50,0.225),knots=temp_knot_df, inclx=T)

# Run 1000 models to derive uncertainty in estimates
spline_fitted_M1 <- matrix(NA,nrow=1000, ncol=nrow(marginal_temperatures_M1))

for (l in 1: 1000){
  # Sample by region times year
  data_temp <- temp4[which(temp4$reg_year %in% sample(unique(temp4$reg_year),replace=T)),]
  
  # Run regression with random subsample (data_temp), adjust variables to knots
  fit_boot  <- feols(log(Q93) ~ x + V2 +V3 + year + I(year^2) + Summer_rainsum + I(Summer_rainsum^2)  | farm , data_temp)
  
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
  scale_y_continuous(breaks = seq(-.1,.1, by=.02), limits = c(-.1,.1))+
  geom_hline(yintercept=c(seq(-1,1,by=0.02)), colour="gray")+
  geom_vline(xintercept=temp_knot_df, linetype="dotted", colour="gray")+
  geom_line(data=spline_merged_M1, aes(y=value.x, x=Temp_bin, group=Var1, alpha=x.x^200/200), size=.01, colour="brown1")+
  geom_line(data=spline_merged_M1, aes(y=average_effect, x=Temp_bin), alpha=0.85, size=.75, colour="brown4")+
  geom_line(data=spline_merged_M1, aes(y=lower_bound, x=Temp_bin)   , alpha=0.85, size=.75, colour="brown4")+       
  geom_line(data=spline_merged_M1, aes(y=upper_bound, x=Temp_bin)   , alpha=0.85, size=.75, colour="brown4")

model_4
ggsave("C:/Users/Shakila Vyas/OneDrive - Wageningen University & Research/PhD-WUR-Insurance/CSA/Revised results/ERA5/New results/5degree4soy.png", width = 20, height = 20, units = "cm")

################################placing five knots at 5 degree intervals, not used in main results
summary(temp$temperature)
temp_knot_df <- c(20,25,30,35,40)

temp_knot_df

# New TS for temperature support of interest- .225 to get the nice plot with small intervals
temp1 <- as.matrix(Hmisc::rcspline.eval(temp$temperature,knots=temp_knot_df,inclx=T))
temp2 <- cbind(temp,temp1)

names(temp2)
head(temp1)

#adjust variables depending on knots
temp3<-temp2 %>%
  group_by(farm,year)%>%
  summarise_at(vars(x,V2,V3,V4),sum)

#match names with temp
names(regdata_soy1)
colnames(regdata_soy1)[colnames(regdata_soy1) == "Q90"] <- "year"
colnames(regdata_soy1)[colnames(regdata_soy1) == "spatial_id"] <- "farm"
names(regdata_soy1)

#join both  data
temp4 <- join(regdata_soy1,temp3, by=c("farm", "year"))

names(temp4)
temp4$reg_year<- as.numeric(as.factor(temp4$Q4))*temp4$year

library(fixest)
soy_yield_spline = feols(log(Q93) ~ x + V2 + V3 + V4 + year + I(year^2) + Summer_rainsum + I(Summer_rainsum^2)   | farm , cluster = c( "year","farm"), temp4)
summary(soy_yield_spline)
results<- etable(soy_yield_spline)
write.csv(results,"C:/Users/Shakila Vyas/OneDrive - Wageningen University & Research/PhD-WUR-Insurance/CSA/ERA5_results_5degree_knot5.csv")

#####bootstrapping
set.seed(123)
marginal_temperatures_M1 <- rcspline.eval(seq(10,50,0.225),knots=temp_knot_df, inclx=T)

# Run 1000 models to derive uncertainty in estimates
spline_fitted_M1 <- matrix(NA,nrow=1000, ncol=nrow(marginal_temperatures_M1))

for (l in 1: 1000){
  # Sample by region times year
  data_temp <- temp4[which(temp4$reg_year %in% sample(unique(temp4$reg_year),replace=T)),]
  
  # Run regression with random subsample (data_temp), adjust variables to knots
  fit_boot  <- feols(log(Q93) ~ x + V2 +V3 +V4 + year + I(year^2) + Summer_rainsum + I(Summer_rainsum^2)  | farm , data_temp)
  
  #Save marginal effect of temperatures,adjust variables to knots 
  spline_fitted_M1[l,]<-as.vector((marginal_temperatures_M1  %*% coef(fit_boot)[1:4])) 
  
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

model_4 <- ggplot()+ ggtitle("Hourly temperature effects: 5 degree knots") + 
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
  scale_y_continuous(breaks = seq(-.1,.1, by=.02), limits = c(-.1,.1))+
  geom_hline(yintercept=c(seq(-1,1,by=0.02)), colour="gray")+
  geom_vline(xintercept=temp_knot_df, linetype="dotted", colour="gray")+
  geom_line(data=spline_merged_M1, aes(y=value.x, x=Temp_bin, group=Var1, alpha=x.x^200/200), size=.01, colour="brown1")+
  geom_line(data=spline_merged_M1, aes(y=average_effect, x=Temp_bin), alpha=0.85, size=.75, colour="brown4")+
  geom_line(data=spline_merged_M1, aes(y=lower_bound, x=Temp_bin)   , alpha=0.85, size=.75, colour="brown4")+       
  geom_line(data=spline_merged_M1, aes(y=upper_bound, x=Temp_bin)   , alpha=0.85, size=.75, colour="brown4")

model_4
ggsave("C:/Users/Shakila Vyas/OneDrive - Wageningen University & Research/PhD-WUR-Insurance/CSA/Revised results/ERA5/New results/5degree5soy.png", width = 20, height = 20, units = "cm")

#####best fit model
#best knots based on RSS
# lower and upper bound define lowest and highest knot location
temp<- rbind(long,long1,long2,long3,long4,long5)

##splines and knots
head(regdata_soy)
length(table(regdata_soy$Q2))
length(table(regdata_soy$Q4))
length(table(regdata_soy$Q5))

####outlier removal
hist(regdata_soy$Q93)
quantile(regdata_soy$Q93,probs= c(.99))
regdata_soy1<- subset(regdata_soy,Q93<quantile(regdata_soy$Q93,probs= c(.99)),select = Q1:T45 )
hist(regdata_soy1$Q93)

#rename temp data
names(temp)[3] <- "temperature"
min(temp$temperature)
max(temp$temperature)
range(temp$temperature)
summary(temp$temperature)
hist(temp$temperature,main="Hourly temperature-summer")

#match names with temp
names(regdata_soy1)
colnames(regdata_soy1)[colnames(regdata_soy1) == "Q90"] <- "year"
colnames(regdata_soy1)[colnames(regdata_soy1) == "spatial_id"] <- "farm"
names(regdata_soy1)


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
  temp_reg <- feols(log(Q93) ~ x + V2 +year + I(year^2) + Summer_rainsum + I(Summer_rainsum^2) | farm ,temp4)
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
colnames(regdata_soy1)[colnames(regdata_soy1) == "Q90"] <- "year"
colnames(regdata_soy1)[colnames(regdata_soy1) == "spatial_id"] <- "farm"
names(regdata_soy1)

#join both  data
temp4 <- join(regdata_soy1,temp3, by=c("farm", "year"))

names(temp4)
temp4$reg_year<- as.numeric(as.factor(temp4$Q4))*temp4$year

#####bootstrapping
set.seed(123)
marginal_temperatures_M1 <- rcspline.eval(seq(10,47,0.225),knots=temp_knot_df, inclx=T)

# Run 1000 models to derive uncertainty in estimates
spline_fitted_M1 <- matrix(NA,nrow=1000, ncol=nrow(marginal_temperatures_M1))

for (l in 1: 1000){
  # Sample by region times year
  data_temp <- temp4[which(temp4$reg_year %in% sample(unique(temp4$reg_year),replace=T)),]
  
  # Run regression with random subsample (data_temp), adjust variables to knots
  fit_boot  <- feols(log(Q93) ~ x + V2 + year + I(year^2) + Summer_rainsum + I(Summer_rainsum^2)  | farm , data_temp)
  
  #Save marginal effect of temperatures,adjust variables to knots 
  spline_fitted_M1[l,]<-as.vector((marginal_temperatures_M1  %*% coef(fit_boot)[1:2])) 
  
  # Show progress in %
  print(l/1000 *100)
  rm(fit_boot,data_temp)
}

library(robustbase)
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

temp5 <- cbind(seq(10,47,0.225),seq(1:length(seq(10,47,0.225))))
colnames(temp5) <- c("Temp_bin", "Var2")
spline_merged_M1 <-merge(spline_merged_M1, temp5, by=c("Var2"))

# Plot: hourly temperature effect
mean(temp$temperature)

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
  scale_x_continuous(breaks = seq(10,47, by=5), limits = c(5,47))+
  scale_y_continuous(breaks = seq(-.1,.1, by=.02), limits = c(-.1,.1))+
  geom_hline(yintercept=c(seq(-1,1,by=0.02)), colour="gray")+
  geom_vline(xintercept=temp_knot_df, linetype="dotted", colour="gray")+
  geom_line(data=spline_merged_M1, aes(y=value.x-0.004380666, x=Temp_bin, group=Var1, alpha=x.x^200/200), size=.01, colour="brown1")+
  geom_line(data=spline_merged_M1, aes(y=average_effect-0.004380666, x=Temp_bin), alpha=0.85, size=.75, colour="brown4")+
  geom_line(data=spline_merged_M1, aes(y=lower_bound-0.004380666, x=Temp_bin)   , alpha=0.85, size=.75, colour="brown4")+       
  geom_line(data=spline_merged_M1, aes(y=upper_bound-0.004380666, x=Temp_bin)   , alpha=0.85, size=.75, colour="brown4")

model_4

ggsave("C:/Users/Shakila Vyas/OneDrive - Wageningen University & Research/PhD-WUR-Insurance/CSA/Revised results/ERA5/New results/bestfitsoy_centered.png", width = 20, height = 20, units = "cm")

####main figure in the paper
model_4 <- ggplot() + 
  theme(panel.grid.major = element_blank(),
        plot.title = element_text(family="Times New Roman", size=16, colour = "grey25", hjust=0.5),
        axis.title = element_text(family="Times New Roman", size=16, colour = "grey25"),
        axis.text = element_text(family="Times New Roman", size=16),
        panel.grid.minor = element_blank(),
        panel.background = element_blank(),
        axis.line = element_line(colour = "black"),
        axis.text.x=element_blank(), #remove x axis labels
        axis.ticks.x=element_blank(),
        legend.position="none")+
  xlab("") + ylab("Log yield response (Kg/ha)")+
  scale_y_continuous(breaks = seq(-.1,.1, by=.02), limits = c(-.1,.1))+
  geom_hline(yintercept=c(seq(-1,1,by=0.02)), colour="gray")+
  geom_vline(xintercept=temp_knot_df, linetype="dotted", colour="gray")+
  geom_line(data=spline_merged_M1, aes(y=value.x-0.004380666, x=Temp_bin, group=Var1, alpha=x.x^200/200), size=.01, colour="brown1")+
  geom_line(data=spline_merged_M1, aes(y=average_effect-0.004380666, x=Temp_bin), alpha=0.85, size=.75, colour="brown4")+
  geom_line(data=spline_merged_M1, aes(y=lower_bound-0.004380666, x=Temp_bin)   , alpha=0.85, size=.75, colour="brown4")+       
  geom_line(data=spline_merged_M1, aes(y=upper_bound-0.004380666, x=Temp_bin)   , alpha=0.85, size=.75, colour="brown4")

model_4 

# Histogram of temperature exposure-for a random farm
hist(temp$temperature)
temperature_soy <- ggplot(temp, aes(x=temperature)) + 
  theme(panel.grid.major = element_blank(),
        axis.ticks.y=element_blank(), 
        plot.title = element_text(family="Times New Roman", size=16, colour = "grey25", hjust=0.5),
        axis.title = element_text(family="Times New Roman", size=16, colour = "grey25"),
        axis.text = element_text(family="Times New Roman", size=16),
        panel.grid.minor = element_blank(),
        panel.background = element_blank(),
        axis.line = element_line(colour = "black"),
        legend.position="none")+
  ylab("Total exposure  
(1'00000 hours)")+ xlab("Temperature (°C)")+
  geom_histogram(binwidth=1, color="black", fill="gray41")+
  scale_x_continuous(breaks = seq(10,47, by=5), limits = c(10,47))+
  scale_y_continuous(breaks = c(0,500000,1000000,1500000), labels = c(0,5,10,15))

temperature_soy

library(ggpubr)
finplot<- ggarrange(model_4, temperature_soy, 
          labels = c("a)", "b)"),
          ncol = 1, nrow = 2, heights = c(2, 1))
ggsave(plot=finplot, file="C:/Users/Shakila Vyas/OneDrive - Wageningen University & Research/PhD-WUR-Insurance/CSA/Revised results/ERA5/New results/bestfitsoy_centered.png", width = 20, height = 20, units = "cm")

#robustness check with model without technology control
#################model 1: equal spaced 3 knots
temp_knot_df <- c(24,29,34)

# New TS for temperature support of interest- .225 to get the nice plot with small intervals
#debug following if needed
#install.packages("scales") 
#remove.packages("rlang")
#install.packages("rlang")

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
colnames(regdata_soy1)[colnames(regdata_soy1) == "Q90"] <- "year"
colnames(regdata_soy1)[colnames(regdata_soy1) == "spatial_id"] <- "farm"
names(regdata_soy1)

#join both  data
temp4 <- join(regdata_soy1,temp3, by=c("farm", "year"))

names(temp4)
temp4$reg_year<- as.numeric(as.factor(temp4$Q4))*temp4$year

#####bootstrapping
set.seed(123)
marginal_temperatures_M1 <- rcspline.eval(seq(10,45,0.225),knots=temp_knot_df, inclx=T)

# Run 1000 models to derive uncertainty in estimates
spline_fitted_M1 <- matrix(NA,nrow=1000, ncol=nrow(marginal_temperatures_M1))

for (l in 1: 1000){
  # Sample by region times year
  data_temp <- temp4[which(temp4$reg_year %in% sample(unique(temp4$reg_year),replace=T)),]
  
  # Run regression with random subsample (data_temp), adjust variables to knots
  fit_boot  <- feols(log(Q93) ~ x + V2 + Summer_rainsum + I(Summer_rainsum^2)  | farm , data_temp)
  
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

temp5 <- cbind(seq(10,45,0.225),seq(1:length(seq(10,45,0.225))))
colnames(temp5) <- c("Temp_bin", "Var2")
spline_merged_M1 <-merge(spline_merged_M1, temp5, by=c("Var2"))

#centering
summary(temp$temperature)

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
  xlab("Temperature (in degree C)") + ylab("Log yield response (Kg/ha)")+
  scale_x_continuous(breaks = seq(10,55, by=5), limits = c(5,55))+
  scale_y_continuous(breaks = seq(-.1,.1, by=.02), limits = c(-.1,.1))+
  geom_hline(yintercept=c(seq(-1,1,by=0.02)), colour="gray")+
  geom_vline(xintercept=temp_knot_df, linetype="dotted", colour="gray")+
  geom_line(data=spline_merged_M1, aes(y=value.x-0.00102454, x=Temp_bin, group=Var1, alpha=x.x^200/200), size=.01, colour="brown1")+
  geom_line(data=spline_merged_M1, aes(y=average_effect-0.00102454, x=Temp_bin), alpha=0.85, size=.75, colour="brown4")+
  geom_line(data=spline_merged_M1, aes(y=lower_bound-0.00102454, x=Temp_bin)   , alpha=0.85, size=.75, colour="brown4")+       
  geom_line(data=spline_merged_M1, aes(y=upper_bound-0.00102454, x=Temp_bin)   , alpha=0.85, size=.75, colour="brown4")

model_1

ggsave("C:/Users/Shakila Vyas/OneDrive - Wageningen University & Research/PhD-WUR-Insurance/CSA/Revised results/ERA5/New results/bestfitsoy_cen_wotech.png", width = 20, height = 20, units = "cm")

#####################SUB-SAMPLE BY CSA bundles
#Knots = best fitfrom previous model
temp_knot_df<- c(24,28,34)
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
colnames(regdata_soy1)[colnames(regdata_soy1) == "Q90"] <- "year"
colnames(regdata_soy1)[colnames(regdata_soy1) == "spatial_id"] <- "farm"
names(regdata_soy1)

#join both  data
temp4 <- join(regdata_soy1,temp3, by=c("farm", "year"))

names(temp4)

#csa bundles
table(regdata_soy1$Q8)
temp4
table(temp4$Q8)
temp_ci1<- subset(temp4,Q8==1,select = Q1:V2)
table(temp_ci1$Q8)
temp_ci1$reg_year<- as.numeric(as.factor(temp_ci1$Q4))*temp_ci1$year
hist(temp_ci1$Q79)
summary(temp_ci1$Q81)

temp4
table(temp4$Q8)
temp_ci2<- subset(temp4,Q8==2,select = Q1:V2)
table(temp_ci1$Q8)
temp_ci1$reg_year<- as.numeric(as.factor(temp_ci1$Q4))*temp_ci1$year
hist(temp_ci2$Q79)
summary(temp_ci2$Q81)

temp4
table(temp4$Q8)
temp_ci3<- subset(temp4,Q8==3,select = Q1:V2)
table(temp_ci1$Q8)
temp_ci1$reg_year<- as.numeric(as.factor(temp_ci1$Q4))*temp_ci1$year
hist(temp_ci3$Q79)
summary(temp_ci3$Q81)

#####bootstrapping
#best fit knots

set.seed(123)
marginal_temperatures_M1 <- rcspline.eval(seq(10,45,0.225),knots=temp_knot_df, inclx=T)

# Run 1000 models to derive uncertainty in estimates
spline_fitted_M1 <- matrix(NA,nrow=1000, ncol=nrow(marginal_temperatures_M1))

for (l in 1: 1000){
  # Sample by region times year
  data_temp <- temp_ci1[which(temp_ci1$reg_year %in% sample(unique(temp_ci1$reg_year),replace=T)),]
  
  # Run regression with random subsample (data_temp), adjust variables to knots
  fit_boot  <- feols(log(Q93) ~ x + V2 + year + I(year^2) + Summer_rainsum + I(Summer_rainsum^2)  | farm ,data_temp)
  
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

temp5 <- cbind(seq(10,45,0.225),seq(1:length(seq(10,45,0.225))))
colnames(temp5) <- c("Temp_bin", "Var2")
spline_merged_M1 <-merge(spline_merged_M1, temp5, by=c("Var2"))

# Plot: hourly temperature effect
model_10 <- ggplot()+ ggtitle("Hourly temperature effects: CSA 1") + 
  theme(panel.grid.major = element_blank(),
        plot.title = element_text(family="Times New Roman", size=18, colour = "grey25", hjust=0.5),
        axis.title = element_text(family="Times New Roman", size=18, colour = "grey25"),
        axis.text = element_text(family="Times New Roman", size=18),
        panel.grid.minor = element_blank(),
        panel.background = element_blank(),
        axis.line = element_line(colour = "black"),
        legend.position="none")+
  xlab("Temperature (°C)") + ylab("Log yield response (Kg/ha)")+
  scale_x_continuous(breaks = seq(10,50, by=5), limits = c(10,50))+
  scale_y_continuous(breaks = seq(-.1,.1, by=.02), limits = c(-.1,.1))+
  geom_hline(yintercept=c(seq(-1,1,by=0.02)), colour="gray")+
  geom_vline(xintercept=temp_knot_df, linetype="dotted", colour="gray")+
  geom_line(data=spline_merged_M1, aes(y=value.x-0.006102638, x=Temp_bin, group=Var1, alpha=x.x^200/200), size=.01, colour="brown1")+
  geom_line(data=spline_merged_M1, aes(y=average_effect-0.006102638, x=Temp_bin), alpha=0.85, size=.75, colour="brown4")+
  geom_line(data=spline_merged_M1, aes(y=lower_bound-0.006102638, x=Temp_bin)   , alpha=0.85, size=.75, colour="brown4")+       
  geom_line(data=spline_merged_M1, aes(y=upper_bound-0.006102638, x=Temp_bin)   , alpha=0.85, size=.75, colour="brown4")

model_10
ggsave("C:/Users/Shakila Vyas/OneDrive - Wageningen University & Research/PhD-WUR-Insurance/CSA/Revised results/ERA5/New results/CSA1_cen.png", width = 20, height = 20, units = "cm")

# subsample by CSA bundle 2
# New TS for temperature support of interest- .225 to get the nice plot with small intervals
#Knots = best fit
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
colnames(regdata_soy1)[colnames(regdata_soy1) == "Q90"] <- "year"
colnames(regdata_soy1)[colnames(regdata_soy1) == "spatial_id"] <- "farm"
names(regdata_soy1)

#join both  data
temp4 <- join(regdata_soy1,temp3, by=c("farm", "year"))

names(temp4)

#csa bundles
temp4
table(temp4$Q8)
temp_ci2<- subset(temp4,Q8==2,select = Q1:V2)
temp_ci2$reg_year<- as.numeric(as.factor(temp_ci2$Q4))*temp_ci2$year
hist(temp_ci2$Q79)
summary(temp_ci2$Q81)

#####bootstrapping
set.seed(123)
marginal_temperatures_M1 <- rcspline.eval(seq(10,45,0.225),knots=temp_knot_df, inclx=T)

# Run 1000 models to derive uncertainty in estimates
spline_fitted_M1 <- matrix(NA,nrow=1000, ncol=nrow(marginal_temperatures_M1))

for (l in 1: 1000){
  # Sample by region times year
  data_temp <- temp_ci2[which(temp_ci2$reg_year %in% sample(unique(temp_ci2$reg_year),replace=T)),]
  
  # Run regression with random subsample (data_temp), adjust variables to knots
  fit_boot  <- feols(log(Q93) ~ x + V2 + year + I(year^2) + Summer_rainsum + I(Summer_rainsum^2)  | farm ,data_temp)
  
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

temp5 <- cbind(seq(10,45,0.225),seq(1:length(seq(10,45,0.225))))
colnames(temp5) <- c("Temp_bin", "Var2")
spline_merged_M1 <-merge(spline_merged_M1, temp5, by=c("Var2"))

# Plot: hourly temperature effect
model_11 <- ggplot()+ ggtitle("Hourly temperature effects: CSA 2") + 
  theme(panel.grid.major = element_blank(),
        plot.title = element_text(family="Times New Roman", size=18, colour = "grey25", hjust=0.5),
        axis.title = element_text(family="Times New Roman", size=18, colour = "grey25"),
        axis.text = element_text(family="Times New Roman", size=18),
        panel.grid.minor = element_blank(),
        panel.background = element_blank(),
        axis.line = element_line(colour = "black"),
        legend.position="none")+
  xlab("Temperature (°C)") + ylab("")+
  scale_x_continuous(breaks = seq(10,45, by=5), limits = c(10,45))+
  scale_y_continuous(breaks = seq(-.1,.1, by=.02), limits = c(-.1,.1))+
  geom_hline(yintercept=c(seq(-1,1,by=0.02)), colour="gray")+
  geom_vline(xintercept=temp_knot_df, linetype="dotted", colour="gray")+
  geom_line(data=spline_merged_M1, aes(y=value.x-0.004249835, x=Temp_bin, group=Var1, alpha=x.x^200/200), size=.01, colour="brown1")+
  geom_line(data=spline_merged_M1, aes(y=average_effect-0.004249835, x=Temp_bin), alpha=0.85, size=.75, colour="brown4")+
  geom_line(data=spline_merged_M1, aes(y=lower_bound-0.004249835, x=Temp_bin)   , alpha=0.85, size=.75, colour="brown4")+       
  geom_line(data=spline_merged_M1, aes(y=upper_bound-0.004249835, x=Temp_bin)   , alpha=0.85, size=.75, colour="brown4")

model_11
ggsave("C:/Users/Shakila Vyas/OneDrive - Wageningen University & Research/PhD-WUR-Insurance/CSA/Revised results/ERA5/New results/CSA2_cen.png", width = 20, height = 20, units = "cm")

#main fig, with wheat result included from the analysis below
model_10_wheat_csa
finplot1<- ggarrange(model_10, model_11,model_10_wheat_csa, 
                    labels = c("a)", "b)","c)"),
                    ncol = 2, nrow = 2)
ggsave(plot=finplot1, file="C:/Users/Shakila Vyas/OneDrive - Wageningen University & Research/PhD-WUR-Insurance/CSA/Revised results/ERA5/New results/csabundles_cen.png", width = 30, height = 25, units = "cm")


# sub-sample by CSA bundle 3, observations very less-no results 

################################################################## WINTER WHEAT
######hourly data prep
#rm(list=ls())
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
setwd("C:/Users/Shakila Vyas/OneDrive - Wageningen University & Research/PhD-WUR-Insurance/CSA/Weather data")
#read data; farms in rows, daily weather observations in columns
temp.min <- as.matrix(read.csv("C:/Users/Shakila Vyas/OneDrive - Wageningen University & Research/PhD-WUR-Insurance/CSA/Weather data/mntp19 - Winter.csv")) #load daily minimum temperatures at the farm level
temp.max <- as.matrix(read.csv("C:/Users/Shakila Vyas/OneDrive - Wageningen University & Research/PhD-WUR-Insurance/CSA/Weather data/mxtp19 - Winter.csv")) #load daily minimum temperatures at the farm level

#delete rownumber
temp.min <- temp.min[,-1] 
temp.max <- temp.max[,-1] 

#concert kelvin to Celsius
temp.min <- temp.min-273.15
temp.max <- temp.max-273.15

#delete farms without weather data (farm with mistaken coordinates)
temp.min.mat <- temp.min[complete.cases(temp.min), ]
temp.max.mat <- temp.max[complete.cases(temp.max), ]

#make dataframes to numeric matrices
temp.min.num <- apply(temp.min.mat, 2, as.numeric)
temp.max.num <- apply(temp.max.mat, 2, as.numeric)

days       <- seq(as.Date("2015-10-01"), as.Date("2016-03-30"),by = "day")
start <- "X2015.10.01"
end   <- "X2016.03.30"

months       <- seq(as.Date(substr(start,2,11), format="%Y.%m.%d"), as.Date(substr(end,2,11), format="%Y.%m.%d"),by = "month")

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

write.csv(temp, "C:/Users/Shakila Vyas/OneDrive - Wageningen University & Research/PhD-WUR-Insurance/CSA/Weather data/hourlytp19-Winter.csv", row.names=FALSE)

#########################################################REG WHEAT#####################################################################
library(fixest)
data<- read.csv("C:/Users/Shakila Vyas/OneDrive - Wageningen University & Research/PhD-WUR-Insurance/CSA/Masterdata_Reg_Winter.csv")
head(data)
#remove less than 3 years data
regdata<- subset(data,repeat_id_crop>=3,select = Q1:Winter_rain)
head(regdata)
#wheat subset
regdata_wheat<- subset(regdata,Q12=="Wheat",select = Q1:Winter_rain)
head(regdata_wheat)
#check data and write wheat subset from master data file
write.csv(regdata_wheat,"C:/Users/Shakila Vyas/OneDrive - Wageningen University & Research/PhD-WUR-Insurance/CSA/wheat_yieldreg.csv")

#load clean wheat yield reg
regdata_wheat<- read.csv("C:/Users/Shakila Vyas/OneDrive - Wageningen University & Research/PhD-WUR-Insurance/CSA/wheat_yieldreg.csv")
head(regdata_wheat)
library(dplyr)

test <- regdata_wheat %>%
  group_by(Q8) %>%
  summarise(mean_yield = mean(Q93))

hist(regdata_wheat$Q93)
summary(regdata_wheat)

quantile(regdata_wheat$Q93,probs= c(.99))

regdata_wheat1<- subset(regdata_wheat,Q93<quantile(regdata_wheat$Q93,probs= c(.99)),select = Q1:Winter_rain )
hist(regdata_wheat1$Q93)

summary(regdata_wheat1$Q93)

#sum stats for wheat
summary(regdata_wheat1$Q93)
table(regdata_wheat1$Q93)

#hist yield and returns
png("C:/Users/Shakila Vyas/OneDrive - Wageningen University & Research/PhD-WUR-Insurance/CSA/Hist_yield_wheat.png")
par(mar=c(2,2,2,2))
hist(regdata_wheat$Q93,main="Wheat yield (Kg/Ha")
dev.off()
summary(regdata_wheat)
table(regdata_soy1$Q13)

png("C:/Users/Shakila Vyas/OneDrive - Wageningen University & Research/PhD-WUR-Insurance/CSA/Hist_returns_wheat.png")
par(mar=c(2,2,2,2))
hist(regdata_wheat$Q79,main="Wheat returns (INR/Ha")
dev.off()

########Regression analysis- hourly data#############################################################################################################
library(raster)
library(plyr)
library(dplyr)
library(data.table)
library(reshape2)
library(Hmisc)
library(lubridate)
library(esmisc)
library(sandwich)
library(MASS)
library(e1071)

##preparing data
hourly<- read.csv("C:/Users/Shakila Vyas/OneDrive - Wageningen University & Research/PhD-WUR-Insurance/CSA/Weather data/hourlytp15-Winter.csv")
#need this format-colnames(temp_wheat_exposure2) <- c("Farm","Date","Hour", "Temperature")
as.data.frame(hourly)
hourly$farm <- seq.int(nrow(hourly))
library(reshape2)
long <- melt(hourly, id.vars = c("farm"))
long$year<- 2015

hourly1<- read.csv("C:/Users/Shakila Vyas/OneDrive - Wageningen University & Research/PhD-WUR-Insurance/CSA/Weather data/hourlytp16-Winter.csv")
#need this format-colnames(temp_wheat_exposure2) <- c("Farm","Date","Hour", "Temperature")
as.data.frame(hourly1)
hourly1$farm <- seq.int(nrow(hourly1))
library(reshape2)
long1 <- melt(hourly1, id.vars = c("farm"))
long1$year<- 2016

hourly2<- read.csv("C:/Users/Shakila Vyas/OneDrive - Wageningen University & Research/PhD-WUR-Insurance/CSA/Weather data/hourlytp17-Winter.csv")
#need this format-colnames(temp_wheat_exposure2) <- c("Farm","Date","Hour", "Temperature")
as.data.frame(hourly2)
hourly2$farm <- seq.int(nrow(hourly2))
library(reshape2)
long2 <- melt(hourly2, id.vars = c("farm"))
long2$year<- 2017

hourly3<- read.csv("C:/Users/Shakila Vyas/OneDrive - Wageningen University & Research/PhD-WUR-Insurance/CSA/Weather data/hourlytp18-Winter.csv")
#need this format-colnames(temp_wheat_exposure2) <- c("Farm","Date","Hour", "Temperature")
as.data.frame(hourly3)
hourly3$farm <- seq.int(nrow(hourly3))
library(reshape2)
long3 <- melt(hourly3, id.vars = c("farm"))
long3$year<- 2018

hourly4<- read.csv("C:/Users/Shakila Vyas/OneDrive - Wageningen University & Research/PhD-WUR-Insurance/CSA/Weather data/hourlytp19-Winter.csv")
#need this format-colnames(temp_wheat_exposure2) <- c("Farm","Date","Hour", "Temperature")
as.data.frame(hourly4)
hourly4$farm <- seq.int(nrow(hourly4))
library(reshape2)
long4 <- melt(hourly4, id.vars = c("farm"))
long4$year<- 2019

#temp histograms
hist(long4$value,main="Hourly temperature 2019",cex.main=2)

####2020 not needed
temp<- rbind(long,long1,long2,long3,long4)
summary(temp$value)

##splines and knots
head(regdata_wheat1)
summary(regdata_wheat1$Winter_rain)
length(table(regdata_wheat1$Q2))
length(table(regdata_wheat1$Q4))
length(table(regdata_wheat1$Q5))

names(temp)[3] <- "temperature"
min(temp$temperature)
max(temp$temperature)
range(temp$temperature)
summary(temp$temperature)
hist(temp$temperature,main="Hourly temperature-winter")

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
  scale_x_continuous(breaks = seq(0,50, by=5), limits = c(0,50))

ggsave(temperature_wheat, filename=paste("C:/Users/Shakila Vyas/OneDrive - Wageningen University & Research/PhD-WUR-Insurance/CSA/Revised results/ERA5/New results/era5wheathist.png"), 
       dpi=300, height=13, width=18, units="cm")

#####################models with different knots placements 
temp_knot_df <- c(min(temp$temperature) + ((max(temp$temperature)-min(temp$temperature)) / 4),
                  min(temp$temperature) + 2* ((max(temp$temperature)-min(temp$temperature)) / 4),
                  min(temp$temperature) + 3* ((max(temp$temperature)-min(temp$temperature)) / 4))

temp_knot_df <- c(min(temp$temperature) + ((max(temp$temperature)-min(temp$temperature)) / 5),
                  min(temp$temperature) + 2* ((max(temp$temperature)-min(temp$temperature)) / 5),
                  min(temp$temperature) + 3* ((max(temp$temperature)-min(temp$temperature)) / 5),
                  min(temp$temperature) + 4* ((max(temp$temperature)-min(temp$temperature)) / 5))

temp_knot_df <- c(quantile(temp$temperature,0.1, type=1),
                  quantile(temp$temperature,0.5, type=1),
                  quantile(temp$temperature,0.9, type=1))

temp_knot_df <- c(quantile(temp$temperature,0.2, type=1),
                  quantile(temp$temperature,0.4, type=1),
                  quantile(temp$temperature,0.6, type=1),
                  quantile(temp$temperature,0.8, type=1))

temp_knot_df <- c(15,20,25,30)

temp_knot_df <- c(15,20,25,30,35)

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


#match names with temp
names(regdata_wheat1)
colnames(regdata_wheat1)[colnames(regdata_wheat1) == "Q90"] <- "year"
colnames(regdata_wheat1)[colnames(regdata_wheat1) == "spatial_id"] <- "farm"
names(regdata_wheat1)
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
  temp_reg <- feols(log(Q93) ~ x + V2 +year + I(year^2) + Winter_rain + I(Winter_rain^2)  | farm ,temp4)
  RSS_3knots[c] <- sum(resid(temp_reg)^2)
  
}

# Best number of knots
temp_knot_df <- knots_3_combi_good[,which.min(RSS_3knots)]
rm(RSS_3knots, knots_3_combi_good, differences, kn3_combi,temp_range,upper_bound, lower_bound)

#######regression resume
# New TS for temperature support of interest- .225 to get the nice plot with small intervals
temp1 <- as.matrix(Hmisc::rcspline.eval(temp$temperature,knots=temp_knot_df,inclx=T))
temp2 <- cbind(temp,temp1)

names(temp2)
head(temp1)

#adjust variables depending on knots, i.e V2 and V3 variables
temp3<-temp2 %>%
  group_by(farm,year)%>%
  summarise_at(vars(x,V2,V3),sum)

#match names with temp
names(regdata_wheat1)
colnames(regdata_wheat1)[colnames(regdata_wheat1) == "Q90"] <- "year"
colnames(regdata_wheat1)[colnames(regdata_wheat1) == "spatial_id"] <- "farm"
names(regdata_wheat1)

#join both  data
temp4 <- join(regdata_wheat1,temp3, by=c("farm", "year"))

names(temp4)
table(temp4$year)
temp4$reg_year<- as.numeric(as.factor(temp4$Q5))*temp4$year

class(temp4$Winter_rain)
temp4$Winter_rain1 <- as.numeric(temp4$Winter_rain)
temp4

#CENTERNG
summary(temp$temperature)

#####bootstrapping
library(fixest)
set.seed(123)
marginal_temperatures_M1 <- rcspline.eval(seq(0,42,0.225),knots=temp_knot_df, inclx=T)

# Run 1000 models to derive uncertainty in estimates
spline_fitted_M1 <- matrix(NA,nrow=1000, ncol=nrow(marginal_temperatures_M1))

for (l in 1: 1000){
  # Sample by region times year
  data_temp <- temp4[which(temp4$reg_year %in% sample(unique(temp4$reg_year),replace=T)),]
  
  # Run regression with random subsample (data_temp), adjust variables to knots
  fit_boot  <- feols(log(Q93) ~ x + V2 + V3+ year + I(year^2) + Winter_rain1 + I(Winter_rain1^2)  | farm ,data_temp)
  
  #Save marginal effect of temperatures,adjust variables to knots 
  spline_fitted_M1[l,]<- as.vector((marginal_temperatures_M1  %*% coef(fit_boot)[1:3])) 
  
  # Show progress in %
  print(l/1000 *100)

}

#precipitation response curve, not used in main results
####main plot for rainfall, do block bootstrapping
library(fixest)
fit_boot  <- feols(log(Q93) ~ x + V2  + year + I(year^2) + Winter_rain1 + I(Winter_rain1^2)  | farm ,cluster = c( "year","farm"),temp4)
plot(coef(fit_boot)[5]*seq(5:1000)+ coef(fit_boot)[6]*seq(5:1000)^2,ylab="log(Wheat yield in kg/ha)",xlab="precipitation (mm)",main=)
abline(h=0, col="blue")

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
#rm(spline_merged_M1)
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

temp5 <- cbind(seq(0,42,0.225),seq(1:length(seq(0,42,0.225))))
colnames(temp5) <- c("Temp_bin", "Var2")
spline_merged_M1 <-merge(spline_merged_M1, temp5, by=c("Var2"))

# Plot: hourly temperature effect
#change plot name, margins, centred values according to knot placement strategy
model_1_wheat <- ggplot()+ ggtitle("Hourly temperature effects: Quantile Knots 4") + 
  theme(panel.grid.major = element_blank(),
        plot.title = element_text(family="Times New Roman", size=20, colour = "grey25", hjust=0.5),
        axis.title = element_text(family="Times New Roman", size=16, colour = "grey25"),
        axis.text = element_text(family="Times New Roman", size=16),
        panel.grid.minor = element_blank(),
        panel.background = element_blank(),
        axis.line = element_line(colour = "black"),
        legend.position="none")+
  xlab("Temperature (°C)") + ylab("Log yield response (Kg/ha)")+
  scale_x_continuous(breaks = seq(5,42, by=5), limits = c(5,42))+
  scale_y_continuous(breaks = seq(-.2,.2, by=.04), limits = c(-.2,.2))+
  geom_hline(yintercept=c(seq(-1,1,by=0.02)), colour="gray")+
  geom_vline(xintercept=temp_knot_df, linetype="dotted", colour="gray")+
  geom_line(data=spline_merged_M1, aes(y=value.x+0.01287811, x=Temp_bin, group=Var1, alpha=x.x^200/200), size=.01, colour="darkorange1")+
  geom_line(data=spline_merged_M1, aes(y=average_effect+0.01287811, x=Temp_bin), alpha=0.85, size=.75, colour="darkorange4")+
  geom_line(data=spline_merged_M1, aes(y=lower_bound+0.01287811, x=Temp_bin)   , alpha=0.85, size=.75, colour="darkorange4")+       
  geom_line(data=spline_merged_M1, aes(y=upper_bound+0.01287811, x=Temp_bin)   , alpha=0.85, size=.75, colour="darkorange4")

model_1_wheat

ggsave("C:/Users/Shakila Vyas/OneDrive - Wageningen University & Research/PhD-WUR-Insurance/CSA/Revised results/ERA5/New results/era5wheatqq4_cen.png", width = 20, height = 20, units = "cm")

#best fit
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
  temp_reg <- feols(log(Q93) ~ x + V2 +year + I(year^2) + Winter_rain + I(Winter_rain^2)  | farm ,temp4)
  RSS_3knots[c] <- sum(resid(temp_reg)^2)
  
}

# Best number of knots
temp_knot_df <- knots_3_combi_good[,which.min(RSS_3knots)]
rm(RSS_3knots, knots_3_combi_good, differences, kn3_combi,temp_range,upper_bound, lower_bound)

#######regression resume
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
colnames(regdata_wheat1)[colnames(regdata_wheat1) == "Q90"] <- "year"
colnames(regdata_wheat1)[colnames(regdata_wheat1) == "spatial_id"] <- "farm"
names(regdata_wheat1)

#join both  data
temp4 <- join(regdata_wheat1,temp3, by=c("farm", "year"))

names(temp4)
table(temp4$year)
temp4$reg_year<- as.numeric(as.factor(temp4$Q5))*temp4$year

class(temp4$Winter_rain)
temp4$Winter_rain1 <- as.numeric(temp4$Winter_rain)

temp4

#####bootstrapping
library(fixest)
set.seed(123)
marginal_temperatures_M1 <- rcspline.eval(seq(10,50,0.225),knots=temp_knot_df, inclx=T)

# Run 1000 models to derive uncertainty in estimates
spline_fitted_M1 <- matrix(NA,nrow=1000, ncol=nrow(marginal_temperatures_M1))

for (l in 1: 1000){
  # Sample by region times year
  data_temp <- temp4[which(temp4$reg_year %in% sample(unique(temp4$reg_year),replace=T)),]
  
  # Run regression with random subsample (data_temp), adjust variables to knots
  fit_boot  <- feols(log(Q93) ~ x + V2 +year + I(year^2) + Winter_rain1 + I(Winter_rain1^2)  | farm ,data_temp)
  
  #Save marginal effect of temperatures,adjust variables to knots 
  spline_fitted_M1[l,]<- as.vector((marginal_temperatures_M1  %*% coef(fit_boot)[1:2])) 
  
  # Show progress in %
  print(l/1000 *100)
  
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
#rm(spline_merged_M1)
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
model_1_wheat <- ggplot()+ ggtitle("Hourly temperature effects: Best fit") + 
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
  scale_y_continuous(breaks = seq(-.2,.2, by=.04), limits = c(-.2,.2))+
  geom_hline(yintercept=c(seq(-1,1,by=0.02)), colour="gray")+
  geom_vline(xintercept=temp_knot_df, linetype="dotted", colour="gray")+
  geom_line(data=spline_merged_M1, aes(y=value.x, x=Temp_bin, group=Var1, alpha=x.x^200/200), size=.01, colour="darkorange1")+
  geom_line(data=spline_merged_M1, aes(y=average_effect, x=Temp_bin), alpha=0.85, size=.75, colour="darkorange4")+
  geom_line(data=spline_merged_M1, aes(y=lower_bound, x=Temp_bin)   , alpha=0.85, size=.75, colour="darkorange4")+       
  geom_line(data=spline_merged_M1, aes(y=upper_bound, x=Temp_bin)   , alpha=0.85, size=.75, colour="darkorange4")

model_1_wheat
ggsave("C:/Users/Shakila Vyas/OneDrive - Wageningen University & Research/PhD-WUR-Insurance/CSA/Revised results/ERA5/New results/era5wheatbestfit.png", width = 20, height = 20, units = "cm")

##plot for main fig used in the paper
# Histogram of temperature exposure-for a random farm
hist(temp$temperature)

temperature_wheat <- ggplot(temp, aes(x=temperature)) + 
  theme(panel.grid.major = element_blank(),
        axis.ticks.y=element_blank(), 
        plot.title = element_text(family="Times New Roman", size=16, colour = "grey25", hjust=0.5),
        axis.title = element_text(family="Times New Roman", size=16, colour = "grey25"),
        axis.text = element_text(family="Times New Roman", size=16),
        panel.grid.minor = element_blank(),
        panel.background = element_blank(),
        axis.line = element_line(colour = "black"),
        legend.position="none")+
  ylab("Total exposure  
(1'00000 hours)")+ xlab("Temperature (°C)")+
  geom_histogram(binwidth=1, color="black", fill="gray41")+
  scale_x_continuous(breaks = seq(5,42, by=5), limits = c(5,42))+
  scale_y_continuous(breaks = c(0,500000, 900000), labels = c(0,5,9))

#main fig in the paper
finplot2<- ggarrange(model_1_wheat, temperature_wheat, 
                     labels = c("a)", "b)"),
                     ncol = 1, nrow = 2, heights = c(2, 1))

finplot2
ggsave(plot=finplot2, file="C:/Users/Shakila Vyas/OneDrive - Wageningen University & Research/PhD-WUR-Insurance/CSA/Revised results/ERA5/New results/bestfitwheat_cen.png", width = 20, height = 20, units = "cm")



######sub-sample by CSA bundles
# New TS for temperature support of interest- .225 to get the nice plot with small intervals
#Knots = best fit from previous model
temp_knot_df<- c(16,21,26)
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
colnames(regdata_wheat1)[colnames(regdata_wheat1) == "Q90"] <- "year"
colnames(regdata_wheat1)[colnames(regdata_wheat1) == "spatial_id"] <- "farm"
names(regdata_wheat1)

#join both  data
temp4 <- join(regdata_wheat1,temp3, by=c("farm", "year"))

names(temp4)

#csa bundles (csa1 and 3 very less)
table(regdata_wheat1$Q8)
temp4
table(temp4$Q8)
temp_ci1<- subset(temp4,Q8==2,select = Q1:V2)
table(temp_ci1$Q8)
temp_ci1$reg_year<- as.numeric(as.factor(temp_ci1$Q4))*temp_ci1$year

#####bootstrapping
set.seed(123)
marginal_temperatures_M1 <- rcspline.eval(seq(0,40,0.225),knots=temp_knot_df, inclx=T)

# Run 1000 models to derive uncertainty in estimates
spline_fitted_M1 <- matrix(NA,nrow=1000, ncol=nrow(marginal_temperatures_M1))

for (l in 1: 1000){
  # Sample by region times year
  data_temp <- temp_ci1[which(temp_ci1$reg_year %in% sample(unique(temp_ci1$reg_year),replace=T)),]
  
  # Run regression with random subsample (data_temp), adjust variables to knots
  fit_boot  <- feols(log(Q93) ~ x + V2 + year + I(year^2) + Winter_rain + I(Winter_rain^2)  | farm ,data_temp)
  
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

temp5 <- cbind(seq(0,40,0.225),seq(1:length(seq(0,40,0.225))))
colnames(temp5) <- c("Temp_bin", "Var2")
spline_merged_M1 <-merge(spline_merged_M1, temp5, by=c("Var2"))

# Plot: hourly temperature effect
model_10_wheat_csa <- ggplot()+ ggtitle("Hourly temperature effects: CSA 2") + 
  theme(panel.grid.major = element_blank(),
        plot.title = element_text(family="Times New Roman", size=18, colour = "grey25", hjust=0.5),
        axis.title = element_text(family="Times New Roman", size=18, colour = "grey25"),
        axis.text = element_text(family="Times New Roman", size=18),
        panel.grid.minor = element_blank(),
        panel.background = element_blank(),
        axis.line = element_line(colour = "black"),
        legend.position="none")+
  xlab("Temperature (°C)") + ylab("Log yield response (Kg/ha)")+
  scale_x_continuous(breaks = seq(5,45, by=5), limits = c(5,45))+
  scale_y_continuous(breaks = seq(-.2,.2, by=.04), limits = c(-.2,.2))+
  geom_hline(yintercept=c(seq(-1,1,by=0.04)), colour="gray")+
  geom_vline(xintercept=temp_knot_df, linetype="dotted", colour="gray")+
  geom_line(data=spline_merged_M1, aes(y=value.x+0.00674162, x=Temp_bin, group=Var1, alpha=x.x^200/200), size=.01, colour="darkorange1")+
  geom_line(data=spline_merged_M1, aes(y=average_effect+0.00674162, x=Temp_bin), alpha=0.85, size=.75, colour="darkorange4")+
  geom_line(data=spline_merged_M1, aes(y=lower_bound+0.00674162, x=Temp_bin)   , alpha=0.85, size=.75, colour="darkorange4")+       
  geom_line(data=spline_merged_M1, aes(y=upper_bound+0.00674162, x=Temp_bin)   , alpha=0.85, size=.75, colour="darkorange4")

model_10_wheat_csa

ggsave("C:/Users/Shakila Vyas/OneDrive - Wageningen University & Research/PhD-WUR-Insurance/CSA/Revised results/ERA5/New results/era5wheatcsabunle2_cen.png", width = 20, height = 20, units = "cm")

################################IMD analysis#################################################################################
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
######hourly data prep
#rm(list=ls())
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
setwd("C:/Users/Shakila Vyas/OneDrive - Wageningen University & Research/PhD-WUR-Insurance/CSA/Weather data")
#read data; farms in rows, daily weather observations in columns
temp.min <- as.matrix(read.csv("C:/Users/Shakila Vyas/OneDrive - Wageningen University & Research/PhD-WUR-Insurance/CSA/Weather data/IMD data/winter/mntpimd19.csv")) #load daily minimum temperatures at the farm level
temp.max <- as.matrix(read.csv("C:/Users/Shakila Vyas/OneDrive - Wageningen University & Research/PhD-WUR-Insurance/CSA/Weather data/IMD data/winter/mxtpimd19.csv")) #load daily minimum temperatures at the farm level

#delete rownumber
temp.min <- temp.min[,-1] 
temp.max <- temp.max[,-1] 

#delete farms without weather data (farm with mistaken coordinates)
temp.min.mat <- temp.min[complete.cases(temp.min), ]
temp.max.mat <- temp.max[complete.cases(temp.max), ]

#make dataframes to numeric matrices
temp.min.num <- apply(temp.min.mat, 2, as.numeric)
temp.max.num <- apply(temp.max.mat, 2, as.numeric)

days       <- seq(as.Date("2015-10-01"), as.Date("2016-03-30"),by = "day")
start <- "X2015.10.01"
end   <- "X2016.03.30"

months       <- seq(as.Date(substr(start,2,11), format="%Y.%m.%d"), as.Date(substr(end,2,11), format="%Y.%m.%d"),by = "month")

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

write.csv(temp, "C:/Users/Shakila Vyas/OneDrive - Wageningen University & Research/PhD-WUR-Insurance/CSA/Weather data/IMD data/winter/hourlytpwinterimd19.csv", row.names=FALSE)

###################################IMD REG WHEAT
#clean wheat yield reg
regdata_wheat<- read.csv("C:/Users/Shakila Vyas/OneDrive - Wageningen University & Research/PhD-WUR-Insurance/CSA/Masterdata_Reg_Winter.csv")
head(regdata_wheat)
#wheat subset
regdata_wheat<- subset(regdata_wheat,Q12=="Wheat",select = Q1:Winter_rain_IMD )
#remove less than 3 years data
regdata_wheat<- subset(regdata_wheat,repeat_id_crop>=3,select = Q1:Winter_rain_IMD)
#99 p outlier
quantile(regdata_wheat$Q93,probs= c(.99))
regdata_wheat1<- subset(regdata_wheat,Q93<quantile(regdata_wheat$Q93,probs= c(.99)),select = Q1:Winter_rain_IMD )
hist(regdata_wheat1$Q93,main="Wheat yield (Kg/Ha)")
summary(regdata_wheat1)

########Regression analysis- hourly data#############################################################################################################
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

##preparing data
hourly<- read.csv("C:/Users/Shakila Vyas/OneDrive - Wageningen University & Research/PhD-WUR-Insurance/CSA/Weather data/IMD data/winter/hourlytpwinterimd15.csv")
#need this format-colnames(temp_wheat_exposure2) <- c("Farm","Date","Hour", "Temperature")
as.data.frame(hourly)
hourly$farm <- seq.int(nrow(hourly))
library(reshape2)
long <- melt(hourly, id.vars = c("farm"))
long$year<- 2015

hourly1<- read.csv("C:/Users/Shakila Vyas/OneDrive - Wageningen University & Research/PhD-WUR-Insurance/CSA/Weather data/IMD data/winter/hourlytpwinterimd16.csv")
#need this format-colnames(temp_wheat_exposure2) <- c("Farm","Date","Hour", "Temperature")
as.data.frame(hourly1)
hourly1$farm <- seq.int(nrow(hourly1))
library(reshape2)
long1 <- melt(hourly1, id.vars = c("farm"))
long1$year<- 2016

hourly2<- read.csv("C:/Users/Shakila Vyas/OneDrive - Wageningen University & Research/PhD-WUR-Insurance/CSA/Weather data/IMD data/winter/hourlytpwinterimd17.csv")
#need this format-colnames(temp_wheat_exposure2) <- c("Farm","Date","Hour", "Temperature")
as.data.frame(hourly2)
hourly2$farm <- seq.int(nrow(hourly2))
library(reshape2)
long2 <- melt(hourly2, id.vars = c("farm"))
long2$year<- 2017

hourly3<- read.csv("C:/Users/Shakila Vyas/OneDrive - Wageningen University & Research/PhD-WUR-Insurance/CSA/Weather data/IMD data/winter/hourlytpwinterimd18.csv")
#need this format-colnames(temp_wheat_exposure2) <- c("Farm","Date","Hour", "Temperature")
as.data.frame(hourly3)
hourly3$farm <- seq.int(nrow(hourly3))
library(reshape2)
long3 <- melt(hourly3, id.vars = c("farm"))
long3$year<- 2018

hourly4<- read.csv("C:/Users/Shakila Vyas/OneDrive - Wageningen University & Research/PhD-WUR-Insurance/CSA/Weather data/IMD data/winter/hourlytpwinterimd19.csv")
#need this format-colnames(temp_wheat_exposure2) <- c("Farm","Date","Hour", "Temperature")
as.data.frame(hourly4)
hourly4$farm <- seq.int(nrow(hourly4))
library(reshape2)
long4 <- melt(hourly4, id.vars = c("farm"))
long4$year<- 2019

####2020 not needed
temp<- rbind(long,long1,long2,long3,long4)

#yearly hist for IMD wheat
hist(long4$value,main="Hourly temperature 2019",cex.main=1.5)
summary(temp$value)

##splines and knots
head(regdata_wheat1)
length(table(regdata_wheat$Q2))
length(table(regdata_wheat$Q4))
length(table(regdata_wheat$Q5))

names(temp)[3] <- "temperature"
min(temp$temperature)
max(temp$temperature)
range(temp$temperature)
summary(temp$temperature)
hist(temp$temperature,main="Hourly temperature-Winter")

# Histogram of temperature exposure-for a random farm
temperature_wheat_IMD <- ggplot(temp, aes(x=temperature)) + 
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

ggsave(temperature_wheat_IMD, filename=paste("C:/Users/Shakila Vyas/OneDrive - Wageningen University & Research/PhD-WUR-Insurance/CSA/Revised results/IMD/IMDwheathist.png"), 
       dpi=300, height=13, width=18, units="cm")


#####################model with different knot placements
temp_knot_df <- c(min(temp$temperature) + ((max(temp$temperature)-min(temp$temperature)) / 4),
                  min(temp$temperature) + 2* ((max(temp$temperature)-min(temp$temperature)) / 4),
                  min(temp$temperature) + 3* ((max(temp$temperature)-min(temp$temperature)) / 4))

temp_knot_df <- c(min(temp$temperature) + ((max(temp$temperature)-min(temp$temperature)) / 5),
                  min(temp$temperature) + 2* ((max(temp$temperature)-min(temp$temperature)) / 5),
                  min(temp$temperature) + 3* ((max(temp$temperature)-min(temp$temperature)) / 5),
                  min(temp$temperature) + 4* ((max(temp$temperature)-min(temp$temperature)) / 5))

temp_knot_df <- c(quantile(temp$temperature,0.1, type=1),
                  quantile(temp$temperature,0.5, type=1),
                  quantile(temp$temperature,0.9, type=1))

temp_knot_df <- c(quantile(temp$temperature,0.2, type=1),
                  quantile(temp$temperature,0.4, type=1),
                  quantile(temp$temperature,0.6, type=1),
                  quantile(temp$temperature,0.8, type=1))

temp_knot_df <- c(10,15,20,25)

temp_knot_df <- c(10,15,20,25,30)

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
colnames(regdata_wheat1)[colnames(regdata_wheat1) == "Q90"] <- "year"
colnames(regdata_wheat1)[colnames(regdata_wheat1) == "spatial_id"] <- "farm"
names(regdata_wheat1)

#join both  data
temp4 <- join(regdata_wheat1,temp3, by=c("farm", "year"))
names(temp4)
table(temp4$year)
temp4$reg_year<- as.numeric(as.factor(temp4$Q5))*temp4$year

class(temp4$Winter_rain_IMD)
#check if numeric otherwise : temp4$Winter_rain1 <- as.numeric(temp4$Winter_rain_IMD)

temp4

#centering
summary(temp$temperature)

#####bootstrapping
set.seed(123)
marginal_temperatures_M1 <- rcspline.eval(seq(0,42,0.225),knots=temp_knot_df, inclx=T)

# Run 1000 models to derive uncertainty in estimates
spline_fitted_M1 <- matrix(NA,nrow=1000, ncol=nrow(marginal_temperatures_M1))

for (l in 1: 1000){
  # Sample by region times year
  data_temp <- temp4[which(temp4$reg_year %in% sample(unique(temp4$reg_year),replace=T)),]
  
  # Run regression with random subsample (data_temp), adjust variables to knots
  fit_boot  <- feols(log(Q93) ~ x + V2 + V3 + Winter_rain_IMD + I(Winter_rain_IMD^2)  | farm ,data_temp)
  
  #Save marginal effect of temperatures,adjust variables to knots 
  spline_fitted_M1[l,]<- as.vector((marginal_temperatures_M1  %*% coef(fit_boot)[1:3])) 
  
  # Show progress in %
  print(l/1000 *100)
  rm(fit_boot,data_temp)
}

#precip response curve
####main plot for rainfall, do block bootstrapping
library(fixest)
fit_boot  <- feols(log(Q93) ~ x + V2  + year + I(year^2) + Winter_rain1 + I(Winter_rain1^2)  | farm ,cluster = c( "year","farm"),temp4)
plot(coef(fit_boot)[5]*seq(5:1000)+ coef(fit_boot)[6]*seq(5:1000)^2,ylab="log(Wheat yield in kg/ha)",xlab="precipitation (mm)",main=)
abline(h=0, col="blue")

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

temp5 <- cbind(seq(0,42,0.225),seq(1:length(seq(0,42,0.225))))
colnames(temp5) <- c("Temp_bin", "Var2")
spline_merged_M1 <-merge(spline_merged_M1, temp5, by=c("Var2"))

# Plot: hourly temperature effect
model_1_wheat <- ggplot()+ ggtitle("Hourly temperature effects: Equal knots 4") + 
  theme(panel.grid.major = element_blank(),
        plot.title = element_text(family="Times New Roman", size=20, colour = "grey25", hjust=0.5),
        axis.title = element_text(family="Times New Roman", size=18, colour = "grey25"),
        axis.text = element_text(family="Times New Roman", size=18),
        panel.grid.minor = element_blank(),
        panel.background = element_blank(),
        axis.line = element_line(colour = "black"),
        legend.position="none")+
  xlab("Temperature (°C)") + ylab("Log yield response (Kg/ha)")+
  scale_x_continuous(breaks = seq(5,42, by=5), limits = c(5,42))+
  scale_y_continuous(breaks = seq(-.2,.2, by=0.04), limits = c(-.2,.2))+
  geom_hline(yintercept=c(seq(-1,1,by=0.02)), colour="gray")+
  geom_vline(xintercept=temp_knot_df, linetype="dotted", colour="gray")+
  geom_line(data=spline_merged_M1, aes(y=value.x-0.005772691, x=Temp_bin, group=Var1, alpha=x.x^200/200), size=.01, colour="darkorange1")+
  geom_line(data=spline_merged_M1, aes(y=average_effect-0.005772691, x=Temp_bin), alpha=0.85, size=.75, colour="darkorange4")+
  geom_line(data=spline_merged_M1, aes(y=lower_bound-0.005772691, x=Temp_bin)   , alpha=0.85, size=.75, colour="darkorange4")+       
  geom_line(data=spline_merged_M1, aes(y=upper_bound-0.005772691, x=Temp_bin)   , alpha=0.85, size=.75, colour="darkorange4")

model_1_wheat

ggsave("C:/Users/Shakila Vyas/OneDrive - Wageningen University & Research/PhD-WUR-Insurance/CSA/Revised results/IMD/eq4wheat_cen.png", width = 20, height = 20, units = "cm")

#best fit model
#best knots based on RSS
#temp
temp<- rbind(long,long1,long2,long3,long4)
##splines and knots
head(regdata_wheat1)
length(table(regdata_wheat$Q2))
length(table(regdata_wheat$Q4))
length(table(regdata_wheat$Q5))
names(temp)[3] <- "temperature"
min(temp$temperature)
max(temp$temperature)
range(temp$temperature)
summary(temp$temperature)
hist(temp$temperature,main="Hourly temperature-Winter")
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
  temp_reg <- feols(log(Q93) ~ x + V2 +year + I(year^2) + Winter_rain_IMD + I(Winter_rain_IMD^2)  | farm ,temp4)
  RSS_3knots[c] <- sum(resid(temp_reg)^2)
  
}

# Best number of knots
temp_knot_df <- knots_3_combi_good[,which.min(RSS_3knots)]
rm(RSS_3knots, knots_3_combi_good, differences, kn3_combi,temp_range,upper_bound, lower_bound)

temp_knot_df <- c(22,27,32)

#######regression resume
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
colnames(regdata_wheat1)[colnames(regdata_wheat1) == "Q90"] <- "year"
colnames(regdata_wheat1)[colnames(regdata_wheat1) == "spatial_id"] <- "farm"
names(regdata_wheat1)

#join both  data
temp4 <- join(regdata_wheat1,temp3, by=c("farm", "year"))

names(temp4)
table(temp4$year)
temp4$reg_year<- as.numeric(as.factor(temp4$Q5))*temp4$year

class(temp4$Winter_rain)
temp4$Winter_rain1 <- as.numeric(temp4$Winter_rain)

temp4

#####bootstrapping
library(fixest)
set.seed(123)
marginal_temperatures_M1 <- rcspline.eval(seq(0,42,0.225),knots=temp_knot_df, inclx=T)

# Run 1000 models to derive uncertainty in estimates
spline_fitted_M1 <- matrix(NA,nrow=1000, ncol=nrow(marginal_temperatures_M1))

for (l in 1: 1000){
  # Sample by region times year
  data_temp <- temp4[which(temp4$reg_year %in% sample(unique(temp4$reg_year),replace=T)),]
  
  # Run regression with random subsample (data_temp), adjust variables to knots
  fit_boot  <- feols(log(Q93) ~ x + V2 +year + I(year^2) + Winter_rain_IMD + I(Winter_rain_IMD^2)  | farm ,data_temp)
  
  #Save marginal effect of temperatures,adjust variables to knots 
  spline_fitted_M1[l,]<- as.vector((marginal_temperatures_M1  %*% coef(fit_boot)[1:2])) 
  
  # Show progress in %
  print(l/1000 *100)
  
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

temp5 <- cbind(seq(0,42,0.225),seq(1:length(seq(0,42,0.225))))
colnames(temp5) <- c("Temp_bin", "Var2")
spline_merged_M1 <-merge(spline_merged_M1, temp5, by=c("Var2"))

# Plot: hourly temperature effect
model_1_wheat <- ggplot()+ ggtitle("Hourly temperature effects: Best fit") + 
  theme(panel.grid.major = element_blank(),
        plot.title = element_text(family="Times New Roman", size=20, colour = "grey25", hjust=0.5),
        axis.title = element_text(family="Times New Roman", size=18, colour = "grey25"),
        axis.text = element_text(family="Times New Roman", size=18),
        panel.grid.minor = element_blank(),
        panel.background = element_blank(),
        axis.line = element_line(colour = "black"),
        legend.position="none")+
  xlab("Temperature (°C)") + ylab("Log yield response (Kg/ha)")+
  scale_x_continuous(breaks = seq(5,42, by=5), limits = c(5,42))+
  scale_y_continuous(breaks = seq(-.4,.4, by=0.04), limits = c(-.4,.4))+
  geom_hline(yintercept=c(seq(-1,1,by=0.04)), colour="gray")+
  geom_vline(xintercept=temp_knot_df, linetype="dotted", colour="gray")+
  geom_line(data=spline_merged_M1, aes(y=value.x+0.02158176, x=Temp_bin, group=Var1, alpha=x.x^200/200), size=.01, colour="darkorange1")+
  geom_line(data=spline_merged_M1, aes(y=average_effect+0.02158176, x=Temp_bin), alpha=0.85, size=.75, colour="darkorange4")+
  geom_line(data=spline_merged_M1, aes(y=lower_bound+0.02158176, x=Temp_bin)   , alpha=0.85, size=.75, colour="darkorange4")+       
  geom_line(data=spline_merged_M1, aes(y=upper_bound+0.02158176, x=Temp_bin)   , alpha=0.85, size=.75, colour="darkorange4")
model_1_wheat

ggsave("C:/Users/Shakila Vyas/OneDrive - Wageningen University & Research/PhD-WUR-Insurance/CSA/Revised results/IMD/bestfitwheat_cen.png", width = 20, height = 20, units = "cm")

########IMD soy analysis
##preparing data
hourly<- read.csv("C:/Users/Shakila Vyas/OneDrive - Wageningen University & Research/PhD-WUR-Insurance/CSA/Weather data/IMD data/hourlytpimd15.csv")
#need this format-colnames(temp_wheat_exposure2) <- c("Farm","Date","Hour", "Temperature")
as.data.frame(hourly)
hourly$farm <- seq.int(nrow(hourly))
library(reshape2)
long <- melt(hourly, id.vars = c("farm"))
long$year<- 2015

hourly1<- read.csv("C:/Users/Shakila Vyas/OneDrive - Wageningen University & Research/PhD-WUR-Insurance/CSA/Weather data/IMD data/hourlytpimd16.csv")
#need this format-colnames(temp_wheat_exposure2) <- c("Farm","Date","Hour", "Temperature")
as.data.frame(hourly1)
hourly1$farm <- seq.int(nrow(hourly1))
library(reshape2)
long1 <- melt(hourly1, id.vars = c("farm"))
long1$year<- 2016

hourly2<- read.csv("C:/Users/Shakila Vyas/OneDrive - Wageningen University & Research/PhD-WUR-Insurance/CSA/Weather data/IMD data/hourlytpimd17.csv")
#need this format-colnames(temp_wheat_exposure2) <- c("Farm","Date","Hour", "Temperature")
as.data.frame(hourly2)
hourly2$farm <- seq.int(nrow(hourly2))
library(reshape2)
long2 <- melt(hourly2, id.vars = c("farm"))
long2$year<- 2017

hourly3<- read.csv("C:/Users/Shakila Vyas/OneDrive - Wageningen University & Research/PhD-WUR-Insurance/CSA/Weather data/IMD data/hourlytpimd18.csv")
#need this format-colnames(temp_wheat_exposure2) <- c("Farm","Date","Hour", "Temperature")
as.data.frame(hourly3)
hourly3$farm <- seq.int(nrow(hourly3))
library(reshape2)
long3 <- melt(hourly3, id.vars = c("farm"))
long3$year<- 2018

hourly4<- read.csv("C:/Users/Shakila Vyas/OneDrive - Wageningen University & Research/PhD-WUR-Insurance/CSA/Weather data/IMD data/hourlytpimd19.csv")
#need this format-colnames(temp_wheat_exposure2) <- c("Farm","Date","Hour", "Temperature")
as.data.frame(hourly4)
hourly4$farm <- seq.int(nrow(hourly4))
library(reshape2)
long4 <- melt(hourly4, id.vars = c("farm"))
long4$year<- 2019

hourly5<- read.csv("C:/Users/Shakila Vyas/OneDrive - Wageningen University & Research/PhD-WUR-Insurance/CSA/Weather data/IMD data/hourlytpimd20.csv")
#need this format-colnames(temp_wheat_exposure2) <- c("Farm","Date","Hour", "Temperature")
as.data.frame(hourly5)
hourly5$farm <- seq.int(nrow(hourly5))
library(reshape2)
long5 <- melt(hourly5, id.vars = c("farm"))
long5$year<- 2020

temp<- rbind(long,long1,long2,long3,long4,long5)

#yearly hist for IMD soy
hist(long5$value,main="Hourly temperature 2020",cex.main=1.5)
summary(temp$value)

library(fixest)
data<- read.csv("C:/Users/Shakila Vyas/OneDrive - Wageningen University & Research/PhD-WUR-Insurance/CSA/Masterdata_Reg.csv")
head(data)
#remove less than 3 years data
regdata<- subset(data,repeat_id_crop>=3,select = Q1:Summer_rain_IMD)
head(regdata)
#soybean subset
regdata_soy<- subset(regdata,Q12=="Soybean",select = Q1:Summer_rain_IMD)
head(regdata_soy)

##splines and knots
length(table(regdata_soy$Q2))
length(table(regdata_soy$Q4))
length(table(regdata_soy$Q5))

####outlier removal
hist(regdata_soy$Q93)
quantile(regdata_soy$Q93,probs= c(.99))
regdata_soy1<- subset(regdata_soy,Q93<quantile(regdata_soy$Q93,probs= c(.99)),select = Q1:Summer_rain_IMD )
hist(regdata_soy1$Q93)

#rename temp data
names(temp)[3] <- "temperature"
min(temp$temperature)
max(temp$temperature)
range(temp$temperature)
summary(temp$temperature)
hist(temp$temperature,main="Hourly temperature-summer")

# Histogram of temperature exposure-for a random farm
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
  scale_x_continuous(breaks = seq(10,50, by=5), limits = c(10,50))

ggsave(temperature_soy, filename=paste("C:/Users/Shakila Vyas/OneDrive - Wageningen University & Research/PhD-WUR-Insurance/CSA/Revised results/IMD/IMDsoyhist.png"), 
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
names(regdata_soy1)
colnames(regdata_soy1)[colnames(regdata_soy1) == "Q90"] <- "year"
colnames(regdata_soy1)[colnames(regdata_soy1) == "spatial_id"] <- "farm"
names(regdata_soy1)

#join both  data
temp4 <- join(regdata_soy1,temp3, by=c("farm", "year"))

names(temp4)
temp4$reg_year<- as.numeric(as.factor(temp4$Q4))*temp4$year

library(fixest)
soy_yield_spline = feols(log(Q93) ~ x + V2 + year + I(year^2) + Summer_rainsum + I(Summer_rainsum^2)   | farm , cluster = c( "year","farm"), temp4)
summary(soy_yield_spline)
results<- etable(soy_yield_spline)
write.csv(results,"C:/Users/Shakila Vyas/OneDrive - Wageningen University & Research/PhD-WUR-Insurance/CSA/ERA5_results_eq_knot3_returns.csv")

#####bootstrapping
set.seed(123)
marginal_temperatures_M1 <- rcspline.eval(seq(10,45,0.225),knots=temp_knot_df, inclx=T)

# Run 1000 models to derive uncertainty in estimates
spline_fitted_M1 <- matrix(NA,nrow=1000, ncol=nrow(marginal_temperatures_M1))

for (l in 1: 1000){
  # Sample by region times year
  data_temp <- temp4[which(temp4$reg_year %in% sample(unique(temp4$reg_year),replace=T)),]
  
  # Run regression with random subsample (data_temp), adjust variables to knots
  fit_boot  <- feols(log(Q93) ~ x + V2  + year + I(year^2) + Summer_rainsum + I(Summer_rainsum^2)  | farm , data_temp)
  
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

temp5 <- cbind(seq(10,45,0.225),seq(1:length(seq(10,45,0.225))))
colnames(temp5) <- c("Temp_bin", "Var2")
spline_merged_M1 <-merge(spline_merged_M1, temp5, by=c("Var2"))

#centering
summary(temp$temperature)

# Plot: hourly temperature effect
model_1 <- ggplot()+ ggtitle("Hourly temperature effects: Equal Knots 3") + 
  theme(panel.grid.major = element_blank(),
        plot.title = element_text(family="Times New Roman", size=20, colour = "grey25", hjust=0.5),
        axis.title = element_text(family="Times New Roman", size=18, colour = "grey25"),
        axis.text = element_text(family="Times New Roman", size=18),
        panel.grid.minor = element_blank(),
        panel.background = element_blank(),
        axis.line = element_line(colour = "black"),
        legend.position="none")+
  xlab("Temperature (°C)") + ylab("Log yield response (Kg/ha)")+
  scale_x_continuous(breaks = seq(10,45, by=5), limits = c(5,55))+
  scale_y_continuous(breaks = seq(-.1,.1, by=.02), limits = c(-.1,.1))+
  geom_hline(yintercept=c(seq(-1,1,by=0.02)), colour="gray")+
  geom_vline(xintercept=temp_knot_df, linetype="dotted", colour="gray")+
  geom_line(data=spline_merged_M1, aes(y=value.x+0.00005608513, x=Temp_bin, group=Var1, alpha=x.x^200/200), size=.01, colour="brown1")+
  geom_line(data=spline_merged_M1, aes(y=average_effect+0.00005608513, x=Temp_bin), alpha=0.85, size=.75, colour="brown4")+
  geom_line(data=spline_merged_M1, aes(y=lower_bound+0.00005608513, x=Temp_bin)   , alpha=0.85, size=.75, colour="brown4")+       
  geom_line(data=spline_merged_M1, aes(y=upper_bound+0.00005608513, x=Temp_bin)   , alpha=0.85, size=.75, colour="brown4")

model_1
ggsave("C:/Users/Shakila Vyas/OneDrive - Wageningen University & Research/PhD-WUR-Insurance/CSA/Revised results/IMD/eq3soy_cen.png", width = 20, height = 20, units = "cm")

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
colnames(regdata_soy1)[colnames(regdata_soy1) == "Q90"] <- "year"
colnames(regdata_soy1)[colnames(regdata_soy1) == "spatial_id"] <- "farm"
names(regdata_soy1)

#join both  data
temp4 <- join(regdata_soy1,temp3, by=c("farm", "year"))

names(temp4)
summary(temp4$Q93)
temp4$reg_year<- as.numeric(as.factor(temp4$Q4))*temp4$year

library(fixest)
soy_yield_spline = feols(log(Q93) ~ x + V2 + year + I(year^2) + Summer_rainsum + I(Summer_rainsum^2)   | farm , cluster = c( "year","farm"), temp4)
summary(soy_yield_spline)
results<- etable(soy_yield_spline)
write.csv(results,"C:/Users/Shakila Vyas/OneDrive - Wageningen University & Research/PhD-WUR-Insurance/CSA/ERA5_results_qq_knot3_returns.csv")

#####bootstrapping
set.seed(123)
marginal_temperatures_M1 <- rcspline.eval(seq(10,45,0.225),knots=temp_knot_df, inclx=T)

# Run 1000 models to derive uncertainty in estimates
spline_fitted_M1 <- matrix(NA,nrow=1000, ncol=nrow(marginal_temperatures_M1))

for (l in 1: 1000){
  # Sample by region times year
  data_temp <- temp4[which(temp4$reg_year %in% sample(unique(temp4$reg_year),replace=T)),]
  
  # Run regression with random subsample (data_temp), adjust variables to knots
  fit_boot  <- feols(log(Q93) ~ x + V2 + year + I(year^2) + Summer_rainsum + I(Summer_rainsum^2)  | farm , data_temp)
  
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

temp5 <- cbind(seq(10,45,0.225),seq(1:length(seq(10,45,0.225))))
colnames(temp5) <- c("Temp_bin", "Var2")
spline_merged_M1 <-merge(spline_merged_M1, temp5, by=c("Var2"))

# Plot: hourly temperature effect
model_1 <- ggplot()+ ggtitle("Hourly temperature effects: Quantile Knots 3") + 
  theme(panel.grid.major = element_blank(),
        plot.title = element_text(family="Times New Roman", size=20, colour = "grey25", hjust=0.5),
        axis.title = element_text(family="Times New Roman", size=18, colour = "grey25"),
        axis.text = element_text(family="Times New Roman", size=18),
        panel.grid.minor = element_blank(),
        panel.background = element_blank(),
        axis.line = element_line(colour = "black"),
        legend.position="none")+
  xlab("Temperature (°C)") + ylab("Log yield response (Kg/ha)")+
  scale_x_continuous(breaks = seq(10,55, by=5), limits = c(5,55))+
  scale_y_continuous(breaks = seq(-.1,.1, by=.02), limits = c(-.1,.1))+
  geom_hline(yintercept=c(seq(-1,1,by=0.02)), colour="gray")+
  geom_vline(xintercept=temp_knot_df, linetype="dotted", colour="gray")+
  geom_line(data=spline_merged_M1, aes(y=value.x-0.002915067, x=Temp_bin, group=Var1, alpha=x.x^200/200), size=.01, colour="brown1")+
  geom_line(data=spline_merged_M1, aes(y=average_effect-0.002915067, x=Temp_bin), alpha=0.85, size=.75, colour="brown4")+
  geom_line(data=spline_merged_M1, aes(y=lower_bound-0.002915067, x=Temp_bin)   , alpha=0.85, size=.75, colour="brown4")+       
  geom_line(data=spline_merged_M1, aes(y=upper_bound-0.002915067, x=Temp_bin)   , alpha=0.85, size=.75, colour="brown4")

model_1
ggsave("C:/Users/Shakila Vyas/OneDrive - Wageningen University & Research/PhD-WUR-Insurance/CSA/Revised results/IMD/qq3soy_cen.png", width = 20, height = 20, units = "cm")

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
colnames(regdata_soy1)[colnames(regdata_soy1) == "Q90"] <- "year"
colnames(regdata_soy1)[colnames(regdata_soy1) == "spatial_id"] <- "farm"
names(regdata_soy1)

#join both  data
temp4 <- join(regdata_soy1,temp3, by=c("farm", "year"))
names(temp4)
temp4$reg_year<- as.numeric(as.factor(temp4$Q4))*temp4$year

library(fixest)
soy_yield_spline = feols(log(Q93) ~ x + V2 + V3 + year + I(year^2) + Summer_rainsum + I(Summer_rainsum^2)   | farm , cluster = c( "year","farm"), temp4)
summary(soy_yield_spline)
results<- etable(soy_yield_spline)
write.csv(results,"C:/Users/Shakila Vyas/OneDrive - Wageningen University & Research/PhD-WUR-Insurance/CSA/ERA5_results_eq_knot4.csv")

#others
soy_yield_spline = feols(x ~ x + V2 + V3 + year + I(year^2) + Summer_rainsum + I(Summer_rainsum^2)   | farm , cluster = c( "year","farm"), temp4)
summary(soy_yield_spline)
results<- etable(soy_yield_spline)
write.csv(results,"C:/Users/Shakila Vyas/OneDrive - Wageningen University & Research/PhD-WUR-Insurance/CSA/ERA5_results_eq_knot4_returns.csv")

#####bootstrapping
set.seed(123)
marginal_temperatures_M1 <- rcspline.eval(seq(10,45,0.225),knots=temp_knot_df, inclx=T)

# Run 1000 models to derive uncertainty in estimates
spline_fitted_M1 <- matrix(NA,nrow=1000, ncol=nrow(marginal_temperatures_M1))

for (l in 1: 1000){
  # Sample by region times year
  data_temp <- temp4[which(temp4$reg_year %in% sample(unique(temp4$reg_year),replace=T)),]
  
  # Run regression with random subsample (data_temp), adjust variables to knots
  fit_boot  <- feols(log(Q93) ~ x + V2 +V3 + year + I(year^2) + Summer_rainsum + I(Summer_rainsum^2)  | farm ,data_temp)
  
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

temp5 <- cbind(seq(10,45,0.225),seq(1:length(seq(10,45,0.225))))
colnames(temp5) <- c("Temp_bin", "Var2")
spline_merged_M1 <-merge(spline_merged_M1, temp5, by=c("Var2"))

# Plot: hourly temperature effect
model_2 <- ggplot()+ ggtitle("Hourly temperature effects: Equal knots 4") + 
  theme(panel.grid.major = element_blank(),
        plot.title = element_text(family="Times New Roman", size=20, colour = "grey25", hjust=0.5),
        axis.title = element_text(family="Times New Roman", size=18, colour = "grey25"),
        axis.text = element_text(family="Times New Roman", size=18),
        panel.grid.minor = element_blank(),
        panel.background = element_blank(),
        axis.line = element_line(colour = "black"),
        legend.position="none")+
  xlab("Temperature (°C)") + ylab("Log yield response (Kg/ha)")+
  scale_x_continuous(breaks = seq(10,55, by=5), limits = c(5,55))+
  scale_y_continuous(breaks = seq(-.1,.1, by=.02), limits = c(-.1,.1))+
  geom_hline(yintercept=c(seq(-1,1,by=0.02)), colour="gray")+
  geom_vline(xintercept=temp_knot_df, linetype="dotted", colour="gray")+
  geom_line(data=spline_merged_M1, aes(y=value.x+0.003342393, x=Temp_bin, group=Var1, alpha=x.x^200/200), size=.01, colour="brown1")+
  geom_line(data=spline_merged_M1, aes(y=average_effect+0.003342393, x=Temp_bin), alpha=0.85, size=.75, colour="brown4")+
  geom_line(data=spline_merged_M1, aes(y=lower_bound+0.003342393, x=Temp_bin)   , alpha=0.85, size=.75, colour="brown4")+       
  geom_line(data=spline_merged_M1, aes(y=upper_bound+0.003342393, x=Temp_bin)   , alpha=0.85, size=.75, colour="brown4")

model_2
ggsave("C:/Users/Shakila Vyas/OneDrive - Wageningen University & Research/PhD-WUR-Insurance/CSA/Revised results/IMD/eq4soy_cen.png", width = 20, height = 20, units = "cm")

#####################model 4: quantile 4 knots######
# 4 quantiles
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
colnames(regdata_soy1)[colnames(regdata_soy1) == "Q90"] <- "year"
colnames(regdata_soy1)[colnames(regdata_soy1) == "spatial_id"] <- "farm"
names(regdata_soy1)

#join both  data
temp4 <- join(regdata_soy1,temp3, by=c("farm", "year"))

names(temp4)
temp4$reg_year<- as.numeric(as.factor(temp4$Q4))*temp4$year

library(fixest)
soy_yield_spline = feols(X ~ x + V2 + V3 + year + I(year^2) + Summer_rainsum + I(Summer_rainsum^2)   | farm , cluster = c( "year","farm"), temp4)
summary(soy_yield_spline)
results<- etable(soy_yield_spline)
write.csv(results,"C:/Users/Shakila Vyas/OneDrive - Wageningen University & Research/PhD-WUR-Insurance/CSA/ERA5_results_qq_knot4_returns.csv")

#####bootstrapping
set.seed(123)
marginal_temperatures_M1 <- rcspline.eval(seq(10,45,0.225),knots=temp_knot_df, inclx=T)

# Run 1000 models to derive uncertainty in estimates
spline_fitted_M1 <- matrix(NA,nrow=1000, ncol=nrow(marginal_temperatures_M1))

for (l in 1: 1000){
  # Sample by region times year
  data_temp <- temp4[which(temp4$reg_year %in% sample(unique(temp4$reg_year),replace=T)),]
  
  # Run regression with random subsample (data_temp), adjust variables to knots
  fit_boot  <- feols(log(Q93) ~ x + V2 +V3 + year + I(year^2) + Summer_rainsum + I(Summer_rainsum^2)  | farm , data_temp)
  
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

temp5 <- cbind(seq(10,45,0.225),seq(1:length(seq(10,45,0.225))))
colnames(temp5) <- c("Temp_bin", "Var2")
spline_merged_M1 <-merge(spline_merged_M1, temp5, by=c("Var2"))

# Plot: hourly temperature effect

model_2 <- ggplot()+ ggtitle("Hourly temperature effects: Quantile knots 4") + 
  theme(panel.grid.major = element_blank(),
        plot.title = element_text(family="Times New Roman", size=20, colour = "grey25", hjust=0.5),
        axis.title = element_text(family="Times New Roman", size=18, colour = "grey25"),
        axis.text = element_text(family="Times New Roman", size=18),
        panel.grid.minor = element_blank(),
        panel.background = element_blank(),
        axis.line = element_line(colour = "black"),
        legend.position="none")+
  xlab("Temperature (°C)") + ylab("Log yield response (Kg/ha)")+
  scale_x_continuous(breaks = seq(10,55, by=5), limits = c(5,55))+
  scale_y_continuous(breaks = seq(-.18,.18, by=.02), limits = c(-.18,.18))+
  geom_hline(yintercept=c(seq(-1,1,by=0.02)), colour="gray")+
  geom_vline(xintercept=temp_knot_df, linetype="dotted", colour="gray")+
  geom_line(data=spline_merged_M1, aes(y=value.x+0.04377981, x=Temp_bin, group=Var1, alpha=x.x^200/200), size=.01, colour="brown1")+
  geom_line(data=spline_merged_M1, aes(y=average_effect+0.04377981, x=Temp_bin), alpha=0.85, size=.75, colour="brown4")+
  geom_line(data=spline_merged_M1, aes(y=lower_bound+0.04377981, x=Temp_bin)   , alpha=0.85, size=.75, colour="brown4")+       
  geom_line(data=spline_merged_M1, aes(y=upper_bound+0.04377981, x=Temp_bin)   , alpha=0.85, size=.75, colour="brown4")

model_2
ggsave("C:/Users/Shakila Vyas/OneDrive - Wageningen University & Research/PhD-WUR-Insurance/CSA/Revised results/IMD/qq4soy_cen.png", width = 20, height = 20, units = "cm")

#####best fit soy
#best knots based on RSS
# lower and upper bound define lowest and highest knot location
temp<- rbind(long,long1,long2,long3,long4,long5)

##splines and knots
head(regdata_soy)
length(table(regdata_soy$Q2))
length(table(regdata_soy$Q4))
length(table(regdata_soy$Q5))

####outlier removal
hist(regdata_soy$Q93)
quantile(regdata_soy$Q93,probs= c(.99))
regdata_soy1<- subset(regdata_soy,Q93<quantile(regdata_soy$Q93,probs= c(.99)),select = Q1:T45 )
hist(regdata_soy1$Q93)

#rename temp data
names(temp)[3] <- "temperature"
min(temp$temperature)
max(temp$temperature)
range(temp$temperature)
summary(temp$temperature)
hist(temp$temperature,main="Hourly temperature-summer")

#match names with temp
names(regdata_soy1)
colnames(regdata_soy1)[colnames(regdata_soy1) == "Q90"] <- "year"
colnames(regdata_soy1)[colnames(regdata_soy1) == "spatial_id"] <- "farm"
names(regdata_soy1)

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
  temp_reg <- feols(log(Q93) ~ x + V2 +year + I(year^2) + Summer_rainsum + I(Summer_rainsum^2) | farm ,temp4)
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
colnames(regdata_soy1)[colnames(regdata_soy1) == "Q90"] <- "year"
colnames(regdata_soy1)[colnames(regdata_soy1) == "spatial_id"] <- "farm"
names(regdata_soy1)

#join both  data
temp4 <- join(regdata_soy1,temp3, by=c("farm", "year"))

names(temp4)
temp4$reg_year<- as.numeric(as.factor(temp4$Q4))*temp4$year

#####bootstrapping
set.seed(123)
marginal_temperatures_M1 <- rcspline.eval(seq(10,45,0.225),knots=temp_knot_df, inclx=T)

# Run 1000 models to derive uncertainty in estimates
spline_fitted_M1 <- matrix(NA,nrow=1000, ncol=nrow(marginal_temperatures_M1))

for (l in 1: 1000){
  # Sample by region times year
  data_temp <- temp4[which(temp4$reg_year %in% sample(unique(temp4$reg_year),replace=T)),]
  
  # Run regression with random subsample (data_temp), adjust variables to knots
  fit_boot  <- feols(log(Q93) ~ x + V2 + year + I(year^2) + Summer_rainsum + I(Summer_rainsum^2)  | farm , data_temp)
  
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

temp5 <- cbind(seq(10,45,0.225),seq(1:length(seq(10,45,0.225))))
colnames(temp5) <- c("Temp_bin", "Var2")
spline_merged_M1 <-merge(spline_merged_M1, temp5, by=c("Var2"))

# Plot: hourly temperature effect
model_4 <- ggplot()+ ggtitle("Hourly temperature effects: Best fit") + 
  theme(panel.grid.major = element_blank(),
        plot.title = element_text(family="Times New Roman", size=20, colour = "grey25", hjust=0.5),
        axis.title = element_text(family="Times New Roman", size=18, colour = "grey25"),
        axis.text = element_text(family="Times New Roman", size=18),
        panel.grid.minor = element_blank(),
        panel.background = element_blank(),
        axis.line = element_line(colour = "black"),
        legend.position="none")+
  xlab("Temperature (°C)") + ylab("Log yield response (Kg/ha)")+
  scale_x_continuous(breaks = seq(10,55, by=5), limits = c(5,55))+
  scale_y_continuous(breaks = seq(-.1,.1, by=.02), limits = c(-.1,.1))+
  geom_hline(yintercept=c(seq(-1,1,by=0.02)), colour="gray")+
  geom_vline(xintercept=temp_knot_df, linetype="dotted", colour="gray")+
  geom_line(data=spline_merged_M1, aes(y=value.x+0.00006597220, x=Temp_bin, group=Var1, alpha=x.x^200/200), size=.01, colour="brown1")+
  geom_line(data=spline_merged_M1, aes(y=average_effect+0.00006597220, x=Temp_bin), alpha=0.85, size=.75, colour="brown4")+
  geom_line(data=spline_merged_M1, aes(y=lower_bound+0.00006597220, x=Temp_bin)   , alpha=0.85, size=.75, colour="brown4")+       
  geom_line(data=spline_merged_M1, aes(y=upper_bound+0.00006597220, x=Temp_bin)   , alpha=0.85, size=.75, colour="brown4")

model_4

ggsave("C:/Users/Shakila Vyas/OneDrive - Wageningen University & Research/PhD-WUR-Insurance/CSA/Revised results/IMD/bestfitsoy_cen.png", width = 20, height = 20, units = "cm")

#additional robustness checks
##############################################################GDD analysis
#ERA5
#Prepare weather data, count the hours above 30 (try 27 too)
##preparing data
hourly<- read.csv("C:/Users/Shakila Vyas/OneDrive - Wageningen University & Research/PhD-WUR-Insurance/CSA/Weather data/hourlytp15-Summer.csv")
#need this format-colnames(temp_wheat_exposure2) <- c("Farm","Date","Hour", "Temperature")
as.data.frame(hourly)
hourly$farm <- seq.int(nrow(hourly))
library(reshape2)
long <- melt(hourly, id.vars = c("farm"))
long$year<- 2015

hourly1<- read.csv("C:/Users/Shakila Vyas/OneDrive - Wageningen University & Research/PhD-WUR-Insurance/CSA/Weather data/hourlytp16-Summer.csv")
#need this format-colnames(temp_wheat_exposure2) <- c("Farm","Date","Hour", "Temperature")
as.data.frame(hourly1)
hourly1$farm <- seq.int(nrow(hourly1))
library(reshape2)
long1 <- melt(hourly1, id.vars = c("farm"))
long1$year<- 2016

hourly2<- read.csv("C:/Users/Shakila Vyas/OneDrive - Wageningen University & Research/PhD-WUR-Insurance/CSA/Weather data/hourlytp17-Summer.csv")
#need this format-colnames(temp_wheat_exposure2) <- c("Farm","Date","Hour", "Temperature")
as.data.frame(hourly2)
hourly2$farm <- seq.int(nrow(hourly2))
library(reshape2)
long2 <- melt(hourly2, id.vars = c("farm"))
long2$year<- 2017

hourly3<- read.csv("C:/Users/Shakila Vyas/OneDrive - Wageningen University & Research/PhD-WUR-Insurance/CSA/Weather data/hourlytp18-Summer.csv")
#need this format-colnames(temp_wheat_exposure2) <- c("Farm","Date","Hour", "Temperature")
as.data.frame(hourly3)
hourly3$farm <- seq.int(nrow(hourly3))
library(reshape2)
long3 <- melt(hourly3, id.vars = c("farm"))
long3$year<- 2018

hourly4<- read.csv("C:/Users/Shakila Vyas/OneDrive - Wageningen University & Research/PhD-WUR-Insurance/CSA/Weather data/hourlytp19-Summer.csv")
#need this format-colnames(temp_wheat_exposure2) <- c("Farm","Date","Hour", "Temperature")
as.data.frame(hourly4)
hourly4$farm <- seq.int(nrow(hourly4))
library(reshape2)
long4 <- melt(hourly4, id.vars = c("farm"))
long4$year<- 2019

hourly5<- read.csv("C:/Users/Shakila Vyas/OneDrive - Wageningen University & Research/PhD-WUR-Insurance/CSA/Weather data/hourlytp20-Summer.csv")
#need this format-colnames(temp_wheat_exposure2) <- c("Farm","Date","Hour", "Temperature")
as.data.frame(hourly5)
hourly5$farm <- seq.int(nrow(hourly5))
library(reshape2)
long5 <- melt(hourly5, id.vars = c("farm"))
long5$year<- 2020

temp<- rbind(long,long1,long2,long3,long4,long5)
data<- read.csv("C:/Users/Shakila Vyas/OneDrive - Wageningen University & Research/PhD-WUR-Insurance/CSA/Masterdata_Reg.csv")
head(data)
#remove less than 3 years data
regdata<- subset(data,repeat_id_crop>=3,select = Q1:T45)
head(regdata)
#soybean subset
regdata_soy<- subset(regdata,Q12=="Soybean",select = Q1:T45)
head(regdata_soy)
print(regdata_soy)

##splines and knots
head(regdata_soy)
length(table(regdata_soy$Q2))
length(table(regdata_soy$Q4))
length(table(regdata_soy$Q5))

####outlier removal
hist(regdata_soy$Q93)
quantile(regdata_soy$Q93,probs= c(.99))
regdata_soy1<- subset(regdata_soy,Q93<quantile(regdata_soy$Q93,probs= c(.99)),select = Q1:T45 )
hist(regdata_soy1$Q93)

#rename temp data
names(temp)[3] <- "temperature"
min(temp$temperature)
max(temp$temperature)
range(temp$temperature)
summary(temp$temperature)
hist(temp$temperature,main="Hourly temperature-summer")

temp$gdd<- temp$temperature-30
temp$gdd[temp$gdd < 0] <- 0
#adjust variables depending on knots
temp2<-temp %>%
  group_by(farm,year)%>%
  summarise_at(vars(gdd),sum)

#match names with temp
names(regdata_soy1)
colnames(regdata_soy1)[colnames(regdata_soy1) == "Q90"] <- "year"
colnames(regdata_soy1)[colnames(regdata_soy1) == "spatial_id"] <- "farm"
names(regdata_soy1)

#join both  data
temp3 <- join(regdata_soy1,temp2, by=c("farm", "year"))
names(temp3)
temp3$reg_year<- as.numeric(as.factor(temp3$Q4))*temp3$year

library(fixest)
soy_yield_gdd = feols(log(Q93) ~ gdd + year + I(year^2) + Summer_rainsum + I(Summer_rainsum^2)  | farm , cluster = c( "year","farm"), temp3)
summary(soy_yield_gdd)

#export results
resultsgdd<- etable(soy_yield_gdd)
write.csv(resultsgdd,"C:/Users/Shakila Vyas/OneDrive - Wageningen University & Research/PhD-WUR-Insurance/CSA/Addiitonal results/gdd_soybean.csv")

####only 30 gdd in SI, reste below is not added


temp<- rbind(long,long1,long2,long3,long4,long5)
data<- read.csv("C:/Users/Shakila Vyas/OneDrive - Wageningen University & Research/PhD-WUR-Insurance/CSA/Masterdata_Reg.csv")
head(data)
#remove less than 3 years data
regdata<- subset(data,repeat_id_crop>=3,select = Q1:T45)
head(regdata)
#soybean subset
regdata_soy<- subset(regdata,Q12=="Soybean",select = Q1:T45)
head(regdata_soy)
print(regdata_soy)

##splines and knots
head(regdata_soy)
length(table(regdata_soy$Q2))
length(table(regdata_soy$Q4))
length(table(regdata_soy$Q5))

####outlier removal
hist(regdata_soy$Q93)
quantile(regdata_soy$Q93,probs= c(.99))
regdata_soy1<- subset(regdata_soy,Q93<quantile(regdata_soy$Q93,probs= c(.99)),select = Q1:T45 )
hist(regdata_soy1$Q93)

#rename temp data
names(temp)[3] <- "temperature"
min(temp$temperature)
max(temp$temperature)
range(temp$temperature)
summary(temp$temperature)
hist(temp$temperature,main="Hourly temperature-summer")

temp$gdd<- temp$temperature-27
temp$gdd[temp$gdd < 0] <- 0
#adjust variables depending on knots
temp2<-temp %>%
  group_by(farm,year)%>%
  summarise_at(vars(gdd),sum)

#match names with temp
names(regdata_soy1)
colnames(regdata_soy1)[colnames(regdata_soy1) == "Q90"] <- "year"
colnames(regdata_soy1)[colnames(regdata_soy1) == "spatial_id"] <- "farm"
names(regdata_soy1)

#join both  data
temp3 <- join(regdata_soy1,temp2, by=c("farm", "year"))
names(temp3)
temp3$reg_year<- as.numeric(as.factor(temp3$Q4))*temp3$year

library(fixest)
soy_yield_gdd1 = feols(log(Q93) ~ gdd + year + I(year^2) + Summer_rainsum + I(Summer_rainsum^2)  | farm , cluster = c( "year","farm"), temp3)
summary(soy_yield_gdd1)

temp<- rbind(long,long1,long2,long3,long4,long5)
data<- read.csv("C:/Users/Shakila Vyas/OneDrive - Wageningen University & Research/PhD-WUR-Insurance/CSA/Masterdata_Reg.csv")
head(data)
#remove less than 3 years data
regdata<- subset(data,repeat_id_crop>=3,select = Q1:T45)
head(regdata)
#soybean subset
regdata_soy<- subset(regdata,Q12=="Soybean",select = Q1:T45)
head(regdata_soy)
print(regdata_soy)

##splines and knots
head(regdata_soy)
length(table(regdata_soy$Q2))
length(table(regdata_soy$Q4))
length(table(regdata_soy$Q5))

####outlier removal
hist(regdata_soy$Q93)
quantile(regdata_soy$Q93,probs= c(.99))
regdata_soy1<- subset(regdata_soy,Q93<quantile(regdata_soy$Q93,probs= c(.99)),select = Q1:T45 )
hist(regdata_soy1$Q93)

#rename temp data
names(temp)[3] <- "temperature"
min(temp$temperature)
max(temp$temperature)
range(temp$temperature)
summary(temp$temperature)
hist(temp$temperature,main="Hourly temperature-summer")

temp$gdd<- temp$temperature-28
temp$gdd[temp$gdd < 0] <- 0
#adjust variables depending on knots
temp2<-temp %>%
  group_by(farm,year)%>%
  summarise_at(vars(gdd),sum)

#match names with temp
names(regdata_soy1)
colnames(regdata_soy1)[colnames(regdata_soy1) == "Q90"] <- "year"
colnames(regdata_soy1)[colnames(regdata_soy1) == "spatial_id"] <- "farm"
names(regdata_soy1)

#join both  data
temp3 <- join(regdata_soy1,temp2, by=c("farm", "year"))
names(temp3)
temp3$reg_year<- as.numeric(as.factor(temp3$Q4))*temp3$year

library(fixest)
soy_yield_gdd2 = feols(log(Q93) ~ gdd + year + I(year^2) + Summer_rainsum + I(Summer_rainsum^2)  | farm , cluster = c( "year","farm"), temp3)
summary(soy_yield_gdd2)

temp<- rbind(long,long1,long2,long3,long4,long5)
data<- read.csv("C:/Users/Shakila Vyas/OneDrive - Wageningen University & Research/PhD-WUR-Insurance/CSA/Masterdata_Reg.csv")
head(data)
#remove less than 3 years data
regdata<- subset(data,repeat_id_crop>=3,select = Q1:T45)
head(regdata)
#soybean subset
regdata_soy<- subset(regdata,Q12=="Soybean",select = Q1:T45)
head(regdata_soy)
print(regdata_soy)

##splines and knots
head(regdata_soy)
length(table(regdata_soy$Q2))
length(table(regdata_soy$Q4))
length(table(regdata_soy$Q5))

####outlier removal
hist(regdata_soy$Q93)
quantile(regdata_soy$Q93,probs= c(.99))
regdata_soy1<- subset(regdata_soy,Q93<quantile(regdata_soy$Q93,probs= c(.99)),select = Q1:T45 )
hist(regdata_soy1$Q93)

#rename temp data
names(temp)[3] <- "temperature"
min(temp$temperature)
max(temp$temperature)
range(temp$temperature)
summary(temp$temperature)
hist(temp$temperature,main="Hourly temperature-summer")

temp$gdd<- temp$temperature-29
temp$gdd[temp$gdd < 0] <- 0
#adjust variables depending on knots
temp2<-temp %>%
  group_by(farm,year)%>%
  summarise_at(vars(gdd),sum)

#match names with temp
names(regdata_soy1)
colnames(regdata_soy1)[colnames(regdata_soy1) == "Q90"] <- "year"
colnames(regdata_soy1)[colnames(regdata_soy1) == "spatial_id"] <- "farm"
names(regdata_soy1)

#join both  data
temp3 <- join(regdata_soy1,temp2, by=c("farm", "year"))
names(temp3)
temp3$reg_year<- as.numeric(as.factor(temp3$Q4))*temp3$year

library(fixest)
soy_yield_gdd3 = feols(log(Q93) ~ gdd + year + I(year^2) + Summer_rainsum + I(Summer_rainsum^2)  | farm , cluster = c( "year","farm"), temp3)
summary(soy_yield_gdd3)

temp<- rbind(long,long1,long2,long3,long4,long5)
data<- read.csv("C:/Users/Shakila Vyas/OneDrive - Wageningen University & Research/PhD-WUR-Insurance/CSA/Masterdata_Reg.csv")
head(data)
#remove less than 3 years data
regdata<- subset(data,repeat_id_crop>=3,select = Q1:T45)
head(regdata)
#soybean subset
regdata_soy<- subset(regdata,Q12=="Soybean",select = Q1:T45)
head(regdata_soy)
print(regdata_soy)

##splines and knots
head(regdata_soy)
length(table(regdata_soy$Q2))
length(table(regdata_soy$Q4))
length(table(regdata_soy$Q5))

####outlier removal
hist(regdata_soy$Q93)
quantile(regdata_soy$Q93,probs= c(.99))
regdata_soy1<- subset(regdata_soy,Q93<quantile(regdata_soy$Q93,probs= c(.99)),select = Q1:T45 )
hist(regdata_soy1$Q93)

#rename temp data
names(temp)[3] <- "temperature"
min(temp$temperature)
max(temp$temperature)
range(temp$temperature)
summary(temp$temperature)
hist(temp$temperature,main="Hourly temperature-summer")

temp$gdd<- temp$temperature-30
temp$gdd[temp$gdd < 0] <- 0
#adjust variables depending on knots
temp2<-temp %>%
  group_by(farm,year)%>%
  summarise_at(vars(gdd),sum)

#match names with temp
names(regdata_soy1)
colnames(regdata_soy1)[colnames(regdata_soy1) == "Q90"] <- "year"
colnames(regdata_soy1)[colnames(regdata_soy1) == "spatial_id"] <- "farm"
names(regdata_soy1)

#join both  data
temp3 <- join(regdata_soy1,temp2, by=c("farm", "year"))
names(temp3)
temp3$reg_year<- as.numeric(as.factor(temp3$Q4))*temp3$year

library(fixest)
soy_yield_gdd4 = feols(log(Q93) ~ gdd + year + I(year^2) + Summer_rainsum + I(Summer_rainsum^2)  | farm , cluster = c( "year","farm"), temp3)
summary(soy_yield_gdd4)

temp<- rbind(long,long1,long2,long3,long4,long5)
data<- read.csv("C:/Users/Shakila Vyas/OneDrive - Wageningen University & Research/PhD-WUR-Insurance/CSA/Masterdata_Reg.csv")
head(data)
#remove less than 3 years data
regdata<- subset(data,repeat_id_crop>=3,select = Q1:T45)
head(regdata)
#soybean subset
regdata_soy<- subset(regdata,Q12=="Soybean",select = Q1:T45)
head(regdata_soy)
print(regdata_soy)

##splines and knots
head(regdata_soy)
length(table(regdata_soy$Q2))
length(table(regdata_soy$Q4))
length(table(regdata_soy$Q5))

####outlier removal
hist(regdata_soy$Q93)
quantile(regdata_soy$Q93,probs= c(.99))
regdata_soy1<- subset(regdata_soy,Q93<quantile(regdata_soy$Q93,probs= c(.99)),select = Q1:T45 )
hist(regdata_soy1$Q93)

#rename temp data
names(temp)[3] <- "temperature"
min(temp$temperature)
max(temp$temperature)
range(temp$temperature)
summary(temp$temperature)
hist(temp$temperature,main="Hourly temperature-summer")

temp$gdd<- temp$temperature-31
temp$gdd[temp$gdd < 0] <- 0
#adjust variables depending on knots
temp2<-temp %>%
  group_by(farm,year)%>%
  summarise_at(vars(gdd),sum)

#match names with temp
names(regdata_soy1)
colnames(regdata_soy1)[colnames(regdata_soy1) == "Q90"] <- "year"
colnames(regdata_soy1)[colnames(regdata_soy1) == "spatial_id"] <- "farm"
names(regdata_soy1)

#join both  data
temp3 <- join(regdata_soy1,temp2, by=c("farm", "year"))
names(temp3)
temp3$reg_year<- as.numeric(as.factor(temp3$Q4))*temp3$year

library(fixest)
soy_yield_gdd5 = feols(log(Q93) ~ gdd + year + I(year^2) + Summer_rainsum + I(Summer_rainsum^2)  | farm , cluster = c( "year","farm"), temp3)
summary(soy_yield_gdd5)

temp<- rbind(long,long1,long2,long3,long4,long5)
data<- read.csv("C:/Users/Shakila Vyas/OneDrive - Wageningen University & Research/PhD-WUR-Insurance/CSA/Masterdata_Reg.csv")
head(data)
#remove less than 3 years data
regdata<- subset(data,repeat_id_crop>=3,select = Q1:T45)
head(regdata)
#soybean subset
regdata_soy<- subset(regdata,Q12=="Soybean",select = Q1:T45)
head(regdata_soy)
print(regdata_soy)

##splines and knots
head(regdata_soy)
length(table(regdata_soy$Q2))
length(table(regdata_soy$Q4))
length(table(regdata_soy$Q5))

####outlier removal
hist(regdata_soy$Q93)
quantile(regdata_soy$Q93,probs= c(.99))
regdata_soy1<- subset(regdata_soy,Q93<quantile(regdata_soy$Q93,probs= c(.99)),select = Q1:T45 )
hist(regdata_soy1$Q93)

#rename temp data
names(temp)[3] <- "temperature"
min(temp$temperature)
max(temp$temperature)
range(temp$temperature)
summary(temp$temperature)
hist(temp$temperature,main="Hourly temperature-summer")

temp$gdd<- temp$temperature-32
temp$gdd[temp$gdd < 0] <- 0
#adjust variables depending on knots
temp2<-temp %>%
  group_by(farm,year)%>%
  summarise_at(vars(gdd),sum)

#match names with temp
names(regdata_soy1)
colnames(regdata_soy1)[colnames(regdata_soy1) == "Q90"] <- "year"
colnames(regdata_soy1)[colnames(regdata_soy1) == "spatial_id"] <- "farm"
names(regdata_soy1)

#join both  data
temp3 <- join(regdata_soy1,temp2, by=c("farm", "year"))
names(temp3)
temp3$reg_year<- as.numeric(as.factor(temp3$Q4))*temp3$year

library(fixest)
soy_yield_gdd6 = feols(log(Q93) ~ gdd + year + I(year^2) + Summer_rainsum + I(Summer_rainsum^2)  | farm , cluster = c( "year","farm"), temp3)
summary(soy_yield_gdd6)

temp<- rbind(long,long1,long2,long3,long4,long5)
data<- read.csv("C:/Users/Shakila Vyas/OneDrive - Wageningen University & Research/PhD-WUR-Insurance/CSA/Masterdata_Reg.csv")
head(data)
#remove less than 3 years data
regdata<- subset(data,repeat_id_crop>=3,select = Q1:T45)
head(regdata)
#soybean subset
regdata_soy<- subset(regdata,Q12=="Soybean",select = Q1:T45)
head(regdata_soy)
print(regdata_soy)

##splines and knots
head(regdata_soy)
length(table(regdata_soy$Q2))
length(table(regdata_soy$Q4))
length(table(regdata_soy$Q5))

####outlier removal
hist(regdata_soy$Q93)
quantile(regdata_soy$Q93,probs= c(.99))
regdata_soy1<- subset(regdata_soy,Q93<quantile(regdata_soy$Q93,probs= c(.99)),select = Q1:T45 )
hist(regdata_soy1$Q93)

#rename temp data
names(temp)[3] <- "temperature"
min(temp$temperature)
max(temp$temperature)
range(temp$temperature)
summary(temp$temperature)
hist(temp$temperature,main="Hourly temperature-summer")

temp$gdd<- temp$temperature-33
temp$gdd[temp$gdd < 0] <- 0
#adjust variables depending on knots
temp2<-temp %>%
  group_by(farm,year)%>%
  summarise_at(vars(gdd),sum)

#match names with temp
names(regdata_soy1)
colnames(regdata_soy1)[colnames(regdata_soy1) == "Q90"] <- "year"
colnames(regdata_soy1)[colnames(regdata_soy1) == "spatial_id"] <- "farm"
names(regdata_soy1)

#join both  data
temp3 <- join(regdata_soy1,temp2, by=c("farm", "year"))
names(temp3)
temp3$reg_year<- as.numeric(as.factor(temp3$Q4))*temp3$year

library(fixest)
soy_yield_gdd7 = feols(log(Q93) ~ gdd + year + I(year^2) + Summer_rainsum + I(Summer_rainsum^2)  | farm , cluster = c( "year","farm"), temp3)
summary(soy_yield_gdd7)

temp<- rbind(long,long1,long2,long3,long4,long5)
data<- read.csv("C:/Users/Shakila Vyas/OneDrive - Wageningen University & Research/PhD-WUR-Insurance/CSA/Masterdata_Reg.csv")
head(data)
#remove less than 3 years data
regdata<- subset(data,repeat_id_crop>=3,select = Q1:T45)
head(regdata)
#soybean subset
regdata_soy<- subset(regdata,Q12=="Soybean",select = Q1:T45)
head(regdata_soy)
print(regdata_soy)

##splines and knots
head(regdata_soy)
length(table(regdata_soy$Q2))
length(table(regdata_soy$Q4))
length(table(regdata_soy$Q5))

####outlier removal
hist(regdata_soy$Q93)
quantile(regdata_soy$Q93,probs= c(.99))
regdata_soy1<- subset(regdata_soy,Q93<quantile(regdata_soy$Q93,probs= c(.99)),select = Q1:T45 )
hist(regdata_soy1$Q93)

#rename temp data
names(temp)[3] <- "temperature"
min(temp$temperature)
max(temp$temperature)
range(temp$temperature)
summary(temp$temperature)
hist(temp$temperature,main="Hourly temperature-summer")

temp$gdd<- temp$temperature-34
temp$gdd[temp$gdd < 0] <- 0
#adjust variables depending on knots
temp2<-temp %>%
  group_by(farm,year)%>%
  summarise_at(vars(gdd),sum)

#match names with temp
names(regdata_soy1)
colnames(regdata_soy1)[colnames(regdata_soy1) == "Q90"] <- "year"
colnames(regdata_soy1)[colnames(regdata_soy1) == "spatial_id"] <- "farm"
names(regdata_soy1)

#join both  data
temp3 <- join(regdata_soy1,temp2, by=c("farm", "year"))
names(temp3)
temp3$reg_year<- as.numeric(as.factor(temp3$Q4))*temp3$year

library(fixest)
soy_yield_gdd8 = feols(log(Q93) ~ gdd + year + I(year^2) + Summer_rainsum + I(Summer_rainsum^2)  | farm , cluster = c( "year","farm"), temp3)
summary(soy_yield_gdd8)

temp<- rbind(long,long1,long2,long3,long4,long5)
data<- read.csv("C:/Users/Shakila Vyas/OneDrive - Wageningen University & Research/PhD-WUR-Insurance/CSA/Masterdata_Reg.csv")
head(data)
#remove less than 3 years data
regdata<- subset(data,repeat_id_crop>=3,select = Q1:T45)
head(regdata)
#soybean subset
regdata_soy<- subset(regdata,Q12=="Soybean",select = Q1:T45)
head(regdata_soy)
print(regdata_soy)

##splines and knots
head(regdata_soy)
length(table(regdata_soy$Q2))
length(table(regdata_soy$Q4))
length(table(regdata_soy$Q5))

####outlier removal
hist(regdata_soy$Q93)
quantile(regdata_soy$Q93,probs= c(.99))
regdata_soy1<- subset(regdata_soy,Q93<quantile(regdata_soy$Q93,probs= c(.99)),select = Q1:T45 )
hist(regdata_soy1$Q93)

#rename temp data
names(temp)[3] <- "temperature"
min(temp$temperature)
max(temp$temperature)
range(temp$temperature)
summary(temp$temperature)
hist(temp$temperature,main="Hourly temperature-summer")

temp$gdd<- temp$temperature-35
temp$gdd[temp$gdd < 0] <- 0
#adjust variables depending on knots
temp2<-temp %>%
  group_by(farm,year)%>%
  summarise_at(vars(gdd),sum)

#match names with temp
names(regdata_soy1)
colnames(regdata_soy1)[colnames(regdata_soy1) == "Q90"] <- "year"
colnames(regdata_soy1)[colnames(regdata_soy1) == "spatial_id"] <- "farm"
names(regdata_soy1)

#join both  data
temp3 <- join(regdata_soy1,temp2, by=c("farm", "year"))
names(temp3)
temp3$reg_year<- as.numeric(as.factor(temp3$Q4))*temp3$year

library(fixest)
soy_yield_gdd9= feols(log(Q93) ~ gdd + year + I(year^2) + Summer_rainsum + I(Summer_rainsum^2)  | farm , cluster = c( "year","farm"), temp3)
summary(soy_yield_gdd9)

#export results
resultsgdd<- etable(soy_yield_gdd,soy_yield_gdd1,soy_yield_gdd2,soy_yield_gdd3,soy_yield_gdd4,soy_yield_gdd5,soy_yield_gdd6,soy_yield_gdd7,soy_yield_gdd8,soy_yield_gdd9)
write.csv(resultsgdd,"C:/Users/Shakila Vyas/OneDrive - Wageningen University & Research/PhD-WUR-Insurance/CSA/Addiitonal results/gdd_soybean.csv")

#gdd, CUTOFFS 28
##preparing data
hourly<- read.csv("C:/Users/Shakila Vyas/OneDrive - Wageningen University & Research/PhD-WUR-Insurance/CSA/Weather data/hourlytp15-Summer.csv")
#need this format-colnames(temp_wheat_exposure2) <- c("Farm","Date","Hour", "Temperature")
as.data.frame(hourly)
hourly$farm <- seq.int(nrow(hourly))
library(reshape2)
long <- melt(hourly, id.vars = c("farm"))
long$year<- 2015

hourly1<- read.csv("C:/Users/Shakila Vyas/OneDrive - Wageningen University & Research/PhD-WUR-Insurance/CSA/Weather data/hourlytp16-Summer.csv")
#need this format-colnames(temp_wheat_exposure2) <- c("Farm","Date","Hour", "Temperature")
as.data.frame(hourly1)
hourly1$farm <- seq.int(nrow(hourly1))
library(reshape2)
long1 <- melt(hourly1, id.vars = c("farm"))
long1$year<- 2016

hourly2<- read.csv("C:/Users/Shakila Vyas/OneDrive - Wageningen University & Research/PhD-WUR-Insurance/CSA/Weather data/hourlytp17-Summer.csv")
#need this format-colnames(temp_wheat_exposure2) <- c("Farm","Date","Hour", "Temperature")
as.data.frame(hourly2)
hourly2$farm <- seq.int(nrow(hourly2))
library(reshape2)
long2 <- melt(hourly2, id.vars = c("farm"))
long2$year<- 2017

hourly3<- read.csv("C:/Users/Shakila Vyas/OneDrive - Wageningen University & Research/PhD-WUR-Insurance/CSA/Weather data/hourlytp18-Summer.csv")
#need this format-colnames(temp_wheat_exposure2) <- c("Farm","Date","Hour", "Temperature")
as.data.frame(hourly3)
hourly3$farm <- seq.int(nrow(hourly3))
library(reshape2)
long3 <- melt(hourly3, id.vars = c("farm"))
long3$year<- 2018

hourly4<- read.csv("C:/Users/Shakila Vyas/OneDrive - Wageningen University & Research/PhD-WUR-Insurance/CSA/Weather data/hourlytp19-Summer.csv")
#need this format-colnames(temp_wheat_exposure2) <- c("Farm","Date","Hour", "Temperature")
as.data.frame(hourly4)
hourly4$farm <- seq.int(nrow(hourly4))
library(reshape2)
long4 <- melt(hourly4, id.vars = c("farm"))
long4$year<- 2019

hourly5<- read.csv("C:/Users/Shakila Vyas/OneDrive - Wageningen University & Research/PhD-WUR-Insurance/CSA/Weather data/hourlytp20-Summer.csv")
#need this format-colnames(temp_wheat_exposure2) <- c("Farm","Date","Hour", "Temperature")
as.data.frame(hourly5)
hourly5$farm <- seq.int(nrow(hourly5))
library(reshape2)
long5 <- melt(hourly5, id.vars = c("farm"))
long5$year<- 2020

temp<- rbind(long,long1,long2,long3,long4,long5)
data<- read.csv("C:/Users/Shakila Vyas/OneDrive - Wageningen University & Research/PhD-WUR-Insurance/CSA/Masterdata_Reg.csv")
head(data)
#remove less than 3 years data
regdata<- subset(data,repeat_id_crop>=3,select = Q1:T45)
head(regdata)
#soybean subset
regdata_soy<- subset(regdata,Q12=="Soybean",select = Q1:T45)
head(regdata_soy)
print(regdata_soy)

##splines and knots
head(regdata_soy)
length(table(regdata_soy$Q2))
length(table(regdata_soy$Q4))
length(table(regdata_soy$Q5))

####outlier removal
hist(regdata_soy$Q93)
quantile(regdata_soy$Q93,probs= c(.99))
regdata_soy1<- subset(regdata_soy,Q93<quantile(regdata_soy$Q93,probs= c(.99)),select = Q1:T45 )
hist(regdata_soy1$Q93)

#rename temp data
names(temp)[3] <- "temperature"
min(temp$temperature)
max(temp$temperature)
range(temp$temperature)
summary(temp$temperature)
hist(temp$temperature,main="Hourly temperature-summer")

temp$gdd<- temp$temperature-28
temp$gdd1 <- ifelse(temp$gdd < 0, -1*temp$gdd, 0 )
temp$gdd[temp$gdd < 0] <- 0

#adjust variables depending on knots
temp2<-temp %>%
  group_by(farm,year)%>%
  summarise_at(vars(gdd,gdd1),sum)

#match names with temp
names(regdata_soy1)
colnames(regdata_soy1)[colnames(regdata_soy1) == "Q90"] <- "year"
colnames(regdata_soy1)[colnames(regdata_soy1) == "spatial_id"] <- "farm"
names(regdata_soy1)

#join both  data
temp3 <- join(regdata_soy1,temp2, by=c("farm", "year"))
names(temp3)
temp3$reg_year<- as.numeric(as.factor(temp3$Q4))*temp3$year

#wheat
#load clean wheat yield reg
#clean wheat yield reg
regdata_wheat<- read.csv("C:/Users/Shakila Vyas/OneDrive - Wageningen University & Research/PhD-WUR-Insurance/CSA/Masterdata_Reg_Winter.csv")
head(regdata_wheat)
#wheat subset
regdata_wheat<- subset(regdata_wheat,Q12=="Wheat",select = Q1:Winter_rain_IMD )
#remove less than 3 years data
regdata_wheat<- subset(regdata_wheat,repeat_id_crop>=3,select = Q1:Winter_rain_IMD)
#99 p outlier
quantile(regdata_wheat$Q93,probs= c(.99))
regdata_wheat1<- subset(regdata_wheat,Q93<quantile(regdata_wheat$Q93,probs= c(.99)),select = Q1:Winter_rain_IMD )
hist(regdata_wheat1$Q93,main="Wheat yield (Kg/Ha)")
summary(regdata_wheat1)

##preparing data
hourly<- read.csv("C:/Users/Shakila Vyas/OneDrive - Wageningen University & Research/PhD-WUR-Insurance/CSA/Weather data/hourlytp15-Winter.csv")
#need this format-colnames(temp_wheat_exposure2) <- c("Farm","Date","Hour", "Temperature")
as.data.frame(hourly)
hourly$farm <- seq.int(nrow(hourly))
library(reshape2)
long <- melt(hourly, id.vars = c("farm"))
long$year<- 2015

hourly1<- read.csv("C:/Users/Shakila Vyas/OneDrive - Wageningen University & Research/PhD-WUR-Insurance/CSA/Weather data/hourlytp16-Winter.csv")
#need this format-colnames(temp_wheat_exposure2) <- c("Farm","Date","Hour", "Temperature")
as.data.frame(hourly1)
hourly1$farm <- seq.int(nrow(hourly1))
library(reshape2)
long1 <- melt(hourly1, id.vars = c("farm"))
long1$year<- 2016

hourly2<- read.csv("C:/Users/Shakila Vyas/OneDrive - Wageningen University & Research/PhD-WUR-Insurance/CSA/Weather data/hourlytp17-Winter.csv")
#need this format-colnames(temp_wheat_exposure2) <- c("Farm","Date","Hour", "Temperature")
as.data.frame(hourly2)
hourly2$farm <- seq.int(nrow(hourly2))
library(reshape2)
long2 <- melt(hourly2, id.vars = c("farm"))
long2$year<- 2017

hourly3<- read.csv("C:/Users/Shakila Vyas/OneDrive - Wageningen University & Research/PhD-WUR-Insurance/CSA/Weather data/hourlytp18-Winter.csv")
#need this format-colnames(temp_wheat_exposure2) <- c("Farm","Date","Hour", "Temperature")
as.data.frame(hourly3)
hourly3$farm <- seq.int(nrow(hourly3))
library(reshape2)
long3 <- melt(hourly3, id.vars = c("farm"))
long3$year<- 2018

hourly4<- read.csv("C:/Users/Shakila Vyas/OneDrive - Wageningen University & Research/PhD-WUR-Insurance/CSA/Weather data/hourlytp19-Winter.csv")
#need this format-colnames(temp_wheat_exposure2) <- c("Farm","Date","Hour", "Temperature")
as.data.frame(hourly4)
hourly4$farm <- seq.int(nrow(hourly4))
library(reshape2)
long4 <- melt(hourly4, id.vars = c("farm"))
long4$year<- 2019

####2020 not needed
temp<- rbind(long,long1,long2,long3,long4)

##splines and knots
head(regdata_wheat1)
length(table(regdata_wheat1$Q2))
length(table(regdata_wheat1$Q4))
length(table(regdata_wheat1$Q5))

names(temp)[3] <- "temperature"
min(temp$temperature)
max(temp$temperature)
range(temp$temperature)
summary(temp$temperature)
hist(temp$temperature,main="Hourly temperature-winter")

temp$gdd<- temp$temperature-27
temp$gdd1 <- ifelse(temp$gdd < 0, -1*temp$gdd, 0 )
temp$gdd[temp$gdd < 0] <- 0


#adjust variables depending on knots
temp2<-temp %>%
  group_by(farm,year)%>%
  summarise_at(vars(gdd),sum)

#match names with temp
names(regdata_wheat1)
colnames(regdata_wheat1)[colnames(regdata_wheat1) == "Q90"] <- "year"
colnames(regdata_wheat1)[colnames(regdata_wheat1) == "spatial_id"] <- "farm"
names(regdata_wheat1)

#join both  data
temp3 <- join(regdata_wheat1,temp2, by=c("farm", "year"))
names(temp3)
temp3$reg_year<- as.numeric(as.factor(temp3$Q4))*temp3$year
temp3$Winter_rain <- as.numeric(temp3$Winter_rain)

#fit simple linear regression model
wheat_yield_gdd10 = feols(log(Q93) ~ gdd  + year + I(year^2) + Winter_rain + I(Winter_rain^2)  | farm , cluster = c( "year","farm"), temp3)
wheat_yield_gdd10
results<- etable(wheat_yield_gdd10)
write.csv(results,"C:/Users/Shakila Vyas/OneDrive - Wageningen University & Research/PhD-WUR-Insurance/CSA/Addiitonal results/gdd_threshold_wheat.csv")

#######################additional robustness checks
#######################piece wise linear 
install.packages("lspline")
library(lspline)
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

data<- read.csv("C:/Users/Shakila Vyas/OneDrive - Wageningen University & Research/PhD-WUR-Insurance/CSA/Masterdata_Reg.csv")
head(data)
#remove less than 3 years data
regdata<- subset(data,repeat_id_crop>=3,select = Q1:T45)
head(regdata)
#soybean subset
regdata_soy<- subset(regdata,Q12=="Soybean",select = Q1:T45)
head(regdata_soy)
print(regdata_soy)
##preparing data
hourly<- read.csv("C:/Users/Shakila Vyas/OneDrive - Wageningen University & Research/PhD-WUR-Insurance/CSA/Weather data/hourlytp15-Summer.csv")
#need this format-colnames(temp_wheat_exposure2) <- c("Farm","Date","Hour", "Temperature")
as.data.frame(hourly)
hourly$farm <- seq.int(nrow(hourly))
library(reshape2)
long <- melt(hourly, id.vars = c("farm"))
long$year<- 2015

hourly1<- read.csv("C:/Users/Shakila Vyas/OneDrive - Wageningen University & Research/PhD-WUR-Insurance/CSA/Weather data/hourlytp16-Summer.csv")
#need this format-colnames(temp_wheat_exposure2) <- c("Farm","Date","Hour", "Temperature")
as.data.frame(hourly1)
hourly1$farm <- seq.int(nrow(hourly1))
library(reshape2)
long1 <- melt(hourly1, id.vars = c("farm"))
long1$year<- 2016

hourly2<- read.csv("C:/Users/Shakila Vyas/OneDrive - Wageningen University & Research/PhD-WUR-Insurance/CSA/Weather data/hourlytp17-Summer.csv")
#need this format-colnames(temp_wheat_exposure2) <- c("Farm","Date","Hour", "Temperature")
as.data.frame(hourly2)
hourly2$farm <- seq.int(nrow(hourly2))
library(reshape2)
long2 <- melt(hourly2, id.vars = c("farm"))
long2$year<- 2017

hourly3<- read.csv("C:/Users/Shakila Vyas/OneDrive - Wageningen University & Research/PhD-WUR-Insurance/CSA/Weather data/hourlytp18-Summer.csv")
#need this format-colnames(temp_wheat_exposure2) <- c("Farm","Date","Hour", "Temperature")
as.data.frame(hourly3)
hourly3$farm <- seq.int(nrow(hourly3))
library(reshape2)
long3 <- melt(hourly3, id.vars = c("farm"))
long3$year<- 2018

hourly4<- read.csv("C:/Users/Shakila Vyas/OneDrive - Wageningen University & Research/PhD-WUR-Insurance/CSA/Weather data/hourlytp19-Summer.csv")
#need this format-colnames(temp_wheat_exposure2) <- c("Farm","Date","Hour", "Temperature")
as.data.frame(hourly4)
hourly4$farm <- seq.int(nrow(hourly4))
library(reshape2)
long4 <- melt(hourly4, id.vars = c("farm"))
long4$year<- 2019

hourly5<- read.csv("C:/Users/Shakila Vyas/OneDrive - Wageningen University & Research/PhD-WUR-Insurance/CSA/Weather data/hourlytp20-Summer.csv")
#need this format-colnames(temp_wheat_exposure2) <- c("Farm","Date","Hour", "Temperature")
as.data.frame(hourly5)
hourly5$farm <- seq.int(nrow(hourly5))
library(reshape2)
long5 <- melt(hourly5, id.vars = c("farm"))
long5$year<- 2020

temp<- rbind(long,long1,long2,long3,long4,long5)

####outlier removal
hist(regdata_soy$Q93)
quantile(regdata_soy$Q93,probs= c(.99))
regdata_soy1<- subset(regdata_soy,Q93<quantile(regdata_soy$Q93,probs= c(.99)),select = Q1:T45 )
hist(regdata_soy1$Q93)

#rename temp data
names(temp)[3] <- "temperature"
min(temp$temperature)
max(temp$temperature)
range(temp$temperature)
summary(temp$temperature)
hist(temp$temperature,main="Hourly temperature-summer")

#####################model 1: manual breakpoints at 28, 35
temp1 <- as.matrix(lspline(temp$temperature, c(32),marginal = T))
temp2 <- cbind(temp,temp1)
names(temp2)
head(temp2)
colnames(temp2)[5] <- "V1"
colnames(temp2)[6] <- "V2"
#readjust below based on knots
colnames(temp2)[7] <- "V3"
colnames(temp2)[8] <- "V4"

#adjust variables depending on knots
temp3<-temp2 %>%
  group_by(farm,year)%>%
  summarise_at(vars(V1,V2),sum)

#match names with temp
names(regdata_soy1)
colnames(regdata_soy1)[colnames(regdata_soy1) == "Q90"] <- "year"
colnames(regdata_soy1)[colnames(regdata_soy1) == "spatial_id"] <- "farm"
names(regdata_soy1)

#join both  data
temp4 <- join(regdata_soy1,temp3, by=c("farm", "year"))
names(temp4)
temp4$reg_year<- as.numeric(as.factor(temp4$Q4))*temp4$year

#####bootstrapping
marginal_temperatures_M1 <- (lspline(seq(10,50,0.225), c(32),marginal = T))

# Run regression with random subsample (data_temp), adjust variables to knots
fit_boot  <- feols(log(Q93) ~ V1 + V2 + year + I(year^2) + Summer_rainsum + I(Summer_rainsum^2)   | farm , cluster= ~ reg_year, data=temp4)
  
(summary(fit_boot)$coeftable[,2])
str(summary(fit_boot))
view(mean)
###td codes
library(sandwich)
DMat <- lspline(seq(10,50,by=0.225),knots=c(32))
# SEs multway clustered and heteroscedasticity robust using sandwich package
clustered_ses<-(summary(fit_boot)$coeftable[,2])
se<-DMat%*%(abs(clustered_ses[1:2]))
mean<- as.vector((marginal_temperatures_M1  %*% coef(fit_boot)[1:2]))
#plot
png(file="C:/Users/Shakila Vyas/OneDrive - Wageningen University & Research/PhD-WUR-Insurance/CSA/Addiitonal results/soy_pc32.png", res=300, height=13, width=18, units="cm")
plot(mean~seq(10,50,by=0.225), ylab="Log yield response (Kg/Ha)", ylim= c(-.05,.05),xlab="Temperature (°C)", type="l", lwd=1.5, col="brown4", family="A", cex.axis=1,cex.lab=1)
abline(v=c(32), lty=2, col="gray")# plot knot location 
abline(h=c(0), lty=1, lwd=0.5, col="gray")# plot knot location
polygon(x=c(seq(10,50,by=0.225),rev(seq(10,50,by=0.225))),
        y=c((c(mean-((se)*1.96))),rev(c(mean+((se)*1.96)))),
        col =  adjustcolor("brown1", alpha.f = 0.20), border = NA)
dev.off()

####piecewise for wheat
#wheat
#load clean wheat yield reg
#clean wheat yield reg
regdata_wheat<- read.csv("C:/Users/Shakila Vyas/OneDrive - Wageningen University & Research/PhD-WUR-Insurance/CSA/Masterdata_Reg_Winter.csv")
head(regdata_wheat)
#wheat subset
regdata_wheat<- subset(regdata_wheat,Q12=="Wheat",select = Q1:Winter_rain_IMD )
#remove less than 3 years data
regdata_wheat<- subset(regdata_wheat,repeat_id_crop>=3,select = Q1:Winter_rain_IMD)
#99 p outlier
quantile(regdata_wheat$Q93,probs= c(.99))
regdata_wheat1<- subset(regdata_wheat,Q93<quantile(regdata_wheat$Q93,probs= c(.99)),select = Q1:Winter_rain_IMD )
hist(regdata_wheat1$Q93,main="Wheat yield (Kg/Ha)")
summary(regdata_wheat1)

##preparing data
hourly<- read.csv("C:/Users/Shakila Vyas/OneDrive - Wageningen University & Research/PhD-WUR-Insurance/CSA/Weather data/hourlytp15-Winter.csv")
#need this format-colnames(temp_wheat_exposure2) <- c("Farm","Date","Hour", "Temperature")
as.data.frame(hourly)
hourly$farm <- seq.int(nrow(hourly))
library(reshape2)
long <- melt(hourly, id.vars = c("farm"))
long$year<- 2015

hourly1<- read.csv("C:/Users/Shakila Vyas/OneDrive - Wageningen University & Research/PhD-WUR-Insurance/CSA/Weather data/hourlytp16-Winter.csv")
#need this format-colnames(temp_wheat_exposure2) <- c("Farm","Date","Hour", "Temperature")
as.data.frame(hourly1)
hourly1$farm <- seq.int(nrow(hourly1))
library(reshape2)
long1 <- melt(hourly1, id.vars = c("farm"))
long1$year<- 2016

hourly2<- read.csv("C:/Users/Shakila Vyas/OneDrive - Wageningen University & Research/PhD-WUR-Insurance/CSA/Weather data/hourlytp17-Winter.csv")
#need this format-colnames(temp_wheat_exposure2) <- c("Farm","Date","Hour", "Temperature")
as.data.frame(hourly2)
hourly2$farm <- seq.int(nrow(hourly2))
library(reshape2)
long2 <- melt(hourly2, id.vars = c("farm"))
long2$year<- 2017

hourly3<- read.csv("C:/Users/Shakila Vyas/OneDrive - Wageningen University & Research/PhD-WUR-Insurance/CSA/Weather data/hourlytp18-Winter.csv")
#need this format-colnames(temp_wheat_exposure2) <- c("Farm","Date","Hour", "Temperature")
as.data.frame(hourly3)
hourly3$farm <- seq.int(nrow(hourly3))
library(reshape2)
long3 <- melt(hourly3, id.vars = c("farm"))
long3$year<- 2018

hourly4<- read.csv("C:/Users/Shakila Vyas/OneDrive - Wageningen University & Research/PhD-WUR-Insurance/CSA/Weather data/hourlytp19-Winter.csv")
#need this format-colnames(temp_wheat_exposure2) <- c("Farm","Date","Hour", "Temperature")
as.data.frame(hourly4)
hourly4$farm <- seq.int(nrow(hourly4))
library(reshape2)
long4 <- melt(hourly4, id.vars = c("farm"))
long4$year<- 2019

####2020 not needed
temp<- rbind(long,long1,long2,long3,long4)

##splines and knots
head(regdata_wheat1)
length(table(regdata_wheat1$Q2))
length(table(regdata_wheat1$Q4))
length(table(regdata_wheat1$Q5))

names(temp)[3] <- "temperature"
min(temp$temperature)
max(temp$temperature)
range(temp$temperature)
summary(temp$temperature)
hist(temp$temperature,main="Hourly temperature-winter")

#####################model 1: manual breakpoints at 28, 35
temp1 <- as.matrix(lspline(temp$temperature, c(28),marginal = T))
temp2 <- cbind(temp,temp1)
names(temp2)
head(temp2)
colnames(temp2)[5] <- "V1"
colnames(temp2)[6] <- "V2"
#readjust below based on knots
colnames(temp2)[7] <- "V3"
colnames(temp2)[8] <- "V4"

#adjust variables depending on knots
temp3<-temp2 %>%
  group_by(farm,year)%>%
  summarise_at(vars(V1,V2),sum)

#match names with temp
names(regdata_wheat1)
colnames(regdata_wheat1)[colnames(regdata_wheat1) == "Q90"] <- "year"
colnames(regdata_wheat1)[colnames(regdata_wheat1) == "spatial_id"] <- "farm"
names(regdata_wheat1)

#join both  data
temp4 <- join(regdata_wheat1,temp3, by=c("farm", "year"))
names(temp4)
temp4$reg_year<- as.numeric(as.factor(temp4$Q4))*temp4$year
temp4$Winter_rain<- as.numeric(temp4$Winter_rain)

#####bootstrapping
marginal_temperatures_M1 <- (lspline(seq(10,50,0.225), c(28),marginal = T))

# Run regression with random subsample (data_temp), adjust variables to knots
fit_boot  <- feols(log(Q93) ~ V1 + V2 + year + I(year^2) + Winter_rain + I(Winter_rain^2)   | farm , cluster= ~ reg_year, data=temp4)

(summary(fit_boot)$coeftable[,2])
str(summary(fit_boot))

###td codes
library(sandwich)
DMat <- lspline(seq(10,50,by=0.225),knots=c(28))
# SEs multway clustered and heteroscedasticity robust using sandwich package
clustered_ses<-(summary(fit_boot)$coeftable[,2])
se<-DMat%*%(abs(clustered_ses[1:2]))
mean<- as.vector((marginal_temperatures_M1  %*% coef(fit_boot)[1:2]))
#plot
png(file="C:/Users/Shakila Vyas/OneDrive - Wageningen University & Research/PhD-WUR-Insurance/CSA/Addiitonal results/wheat_pc28.png", res=300, height=13, width=18, units="cm")
plot(mean~seq(10,50,by=0.225), ylab="Log yield response (Kg/Ha)", ylim= c(-.06,.06),xlab="Temperature (°C)", type="l", lwd=1.5, col="darkorange4", family="A", cex.axis=.8,cex.lab=1)
abline(v=c(28), lty=2, col="gray")# plot knot location 
abline(h=c(0), lty=1, lwd=0.5, col="gray")# plot knot location
polygon(x=c(seq(10,50,by=0.225),rev(seq(10,50,by=0.225))),
        y=c((c(mean-((se)*1.96))),rev(c(mean+((se)*1.96)))),
        col =  adjustcolor("darkorange1", alpha.f = 0.20), border = NA)
dev.off()

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
temp_knot_df <- c(20,28,32)
model_1 <- ggplot()+ ggtitle("Hourly temperature effects: Piecewise Linear") + 
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
  scale_y_continuous(breaks = seq(-1,1, by=.2), limits = c(-1,1))+
  geom_hline(yintercept=c(seq(-1,1,by=0.2)), colour="gray")+
  geom_vline(xintercept=temp_knot_df, linetype="dotted", colour="gray")+
  geom_line(data=spline_merged_M1, aes(y=value.x, x=Temp_bin, group=Var1, alpha=x.x^200/200), size=.01, colour="brown1")+
  geom_line(data=spline_merged_M1, aes(y=average_effect, x=Temp_bin), alpha=0.85, size=.75, colour="brown4")+
  geom_line(data=spline_merged_M1, aes(y=lower_bound, x=Temp_bin)   , alpha=0.85, size=.75, colour="brown4")+       
  geom_line(data=spline_merged_M1, aes(y=upper_bound, x=Temp_bin)   , alpha=0.85, size=.75, colour="brown4")

model_1

ggsave("C:/Users/Shakila Vyas/OneDrive - Wageningen University & Research/PhD-WUR-Insurance/CSA/Addiitonal results/soy_lin202832.png", width = 20, height = 20, units = "cm")

#repeat with IMD Soy
##preparing data
hourly<- read.csv("C:/Users/Shakila Vyas/OneDrive - Wageningen University & Research/PhD-WUR-Insurance/CSA/Weather data/IMD data/hourlytpimd15.csv")
#need this format-colnames(temp_wheat_exposure2) <- c("Farm","Date","Hour", "Temperature")
as.data.frame(hourly)
hourly$farm <- seq.int(nrow(hourly))
library(reshape2)
long <- melt(hourly, id.vars = c("farm"))
long$year<- 2015

hourly1<- read.csv("C:/Users/Shakila Vyas/OneDrive - Wageningen University & Research/PhD-WUR-Insurance/CSA/Weather data/IMD data/hourlytpimd16.csv")
#need this format-colnames(temp_wheat_exposure2) <- c("Farm","Date","Hour", "Temperature")
as.data.frame(hourly1)
hourly1$farm <- seq.int(nrow(hourly1))
library(reshape2)
long1 <- melt(hourly1, id.vars = c("farm"))
long1$year<- 2016

hourly2<- read.csv("C:/Users/Shakila Vyas/OneDrive - Wageningen University & Research/PhD-WUR-Insurance/CSA/Weather data/IMD data/hourlytpimd17.csv")
#need this format-colnames(temp_wheat_exposure2) <- c("Farm","Date","Hour", "Temperature")
as.data.frame(hourly2)
hourly2$farm <- seq.int(nrow(hourly2))
library(reshape2)
long2 <- melt(hourly2, id.vars = c("farm"))
long2$year<- 2017

hourly3<- read.csv("C:/Users/Shakila Vyas/OneDrive - Wageningen University & Research/PhD-WUR-Insurance/CSA/Weather data/IMD data/hourlytpimd18.csv")
#need this format-colnames(temp_wheat_exposure2) <- c("Farm","Date","Hour", "Temperature")
as.data.frame(hourly3)
hourly3$farm <- seq.int(nrow(hourly3))
library(reshape2)
long3 <- melt(hourly3, id.vars = c("farm"))
long3$year<- 2018

hourly4<- read.csv("C:/Users/Shakila Vyas/OneDrive - Wageningen University & Research/PhD-WUR-Insurance/CSA/Weather data/IMD data/hourlytpimd19.csv")
#need this format-colnames(temp_wheat_exposure2) <- c("Farm","Date","Hour", "Temperature")
as.data.frame(hourly4)
hourly4$farm <- seq.int(nrow(hourly4))
library(reshape2)
long4 <- melt(hourly4, id.vars = c("farm"))
long4$year<- 2019

hourly5<- read.csv("C:/Users/Shakila Vyas/OneDrive - Wageningen University & Research/PhD-WUR-Insurance/CSA/Weather data/IMD data/hourlytpimd20.csv")
#need this format-colnames(temp_wheat_exposure2) <- c("Farm","Date","Hour", "Temperature")
as.data.frame(hourly5)
hourly5$farm <- seq.int(nrow(hourly5))
library(reshape2)
long5 <- melt(hourly5, id.vars = c("farm"))
long5$year<- 2020

temp<- rbind(long,long1,long2,long3,long4,long5)

####outlier removal
data<- read.csv("C:/Users/Shakila Vyas/OneDrive - Wageningen University & Research/PhD-WUR-Insurance/CSA/Masterdata_Reg.csv")
head(data)
#remove less than 3 years data
regdata<- subset(data,repeat_id_crop>=3,select = Q1:T45)
head(regdata)
#soybean subset
regdata_soy<- subset(regdata,Q12=="Soybean",select = Q1:T45)
head(regdata_soy)
print(regdata_soy)
hist(regdata_soy$Q93)
quantile(regdata_soy$Q93,probs= c(.99))
regdata_soy1<- subset(regdata_soy,Q93<quantile(regdata_soy$Q93,probs= c(.99)),select = Q1:T45 )
hist(regdata_soy1$Q93)

#rename temp data
names(temp)[3] <- "temperature"
min(temp$temperature)
max(temp$temperature)
range(temp$temperature)
summary(temp$temperature)
hist(temp$temperature,main="Hourly temperature-summer IMD")

#####################model 1: manual breakpoints at 28, 35
temp1 <- as.matrix(lspline(temp$temperature, c(28,35),marginal = T))
temp2 <- cbind(temp,temp1)
names(temp2)
head(temp2)

colnames(temp2)[5] <- "V1"
colnames(temp2)[6] <- "V2"
colnames(temp2)[7] <- "V3"
colnames(temp2)[8] <- "V4"

#V2 measures the degree days, change acc to specification

#adjust variables depending on knots
temp3<-temp2 %>%
  group_by(farm,year)%>%
  summarise_at(vars(V1,V2,V3),sum)

#match names with temp
names(regdata_soy1)
colnames(regdata_soy1)[colnames(regdata_soy1) == "Q90"] <- "year"
colnames(regdata_soy1)[colnames(regdata_soy1) == "spatial_id"] <- "farm"
names(regdata_soy1)

#join both  data
temp4 <- join(regdata_soy1,temp3, by=c("farm", "year"))

names(temp4)
temp4$reg_year<- as.numeric(as.factor(temp4$Q4))*temp4$year

#####bootstrapping
set.seed(123)
marginal_temperatures_M1 <- (lspline(seq(10,50,0.225), c(28,35),marginal = T))

# Run 1000 models to derive uncertainty in estimates
spline_fitted_M1 <- matrix(NA,nrow=1000, ncol=nrow(marginal_temperatures_M1))

for (l in 1: 1000){
  # Sample by region times year
  data_temp <- temp4[which(temp4$reg_year %in% sample(unique(temp4$reg_year),replace=T)),]
  
  # Run regression with random subsample (data_temp), adjust variables to knots
  fit_boot  <- feols(log(Q93) ~ V1 + V2 + V3+  year + I(year^2) + Summer_rainsum + I(Summer_rainsum^2)   | farm , temp4)
  
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
temp_knot_df <- c(28,35)
model_1 <- ggplot()+ ggtitle("Hourly temperature effects: Piecewise Linear IMD") + 
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
  scale_y_continuous(breaks = seq(-1,1, by=.2), limits = c(-1,1))+
  geom_hline(yintercept=c(seq(-1,1,by=0.2)), colour="gray")+
  geom_vline(xintercept=temp_knot_df, linetype="dotted", colour="gray")+
  geom_line(data=spline_merged_M1, aes(y=value.x, x=Temp_bin, group=Var1, alpha=x.x^200/200), size=.01, colour="brown1")+
  geom_line(data=spline_merged_M1, aes(y=average_effect, x=Temp_bin), alpha=0.85, size=.75, colour="brown4")+
  geom_line(data=spline_merged_M1, aes(y=lower_bound, x=Temp_bin)   , alpha=0.85, size=.75, colour="brown4")+       
  geom_line(data=spline_merged_M1, aes(y=upper_bound, x=Temp_bin)   , alpha=0.85, size=.75, colour="brown4")

model_1

ggsave("C:/Users/Shakila Vyas/OneDrive - Wageningen University & Research/PhD-WUR-Insurance/CSA/Addiitonal results/soy_lin2835_IMD.png", width = 20, height = 20, units = "cm")

######WHEAT
##preparing data
hourly<- read.csv("C:/Users/Shakila Vyas/OneDrive - Wageningen University & Research/PhD-WUR-Insurance/CSA/Weather data/hourlytp15-Winter.csv")
#need this format-colnames(temp_wheat_exposure2) <- c("Farm","Date","Hour", "Temperature")
as.data.frame(hourly)
hourly$farm <- seq.int(nrow(hourly))
library(reshape2)
long <- melt(hourly, id.vars = c("farm"))
long$year<- 2015

hourly1<- read.csv("C:/Users/Shakila Vyas/OneDrive - Wageningen University & Research/PhD-WUR-Insurance/CSA/Weather data/hourlytp16-Winter.csv")
#need this format-colnames(temp_wheat_exposure2) <- c("Farm","Date","Hour", "Temperature")
as.data.frame(hourly1)
hourly1$farm <- seq.int(nrow(hourly1))
library(reshape2)
long1 <- melt(hourly1, id.vars = c("farm"))
long1$year<- 2016

hourly2<- read.csv("C:/Users/Shakila Vyas/OneDrive - Wageningen University & Research/PhD-WUR-Insurance/CSA/Weather data/hourlytp17-Winter.csv")
#need this format-colnames(temp_wheat_exposure2) <- c("Farm","Date","Hour", "Temperature")
as.data.frame(hourly2)
hourly2$farm <- seq.int(nrow(hourly2))
library(reshape2)
long2 <- melt(hourly2, id.vars = c("farm"))
long2$year<- 2017

hourly3<- read.csv("C:/Users/Shakila Vyas/OneDrive - Wageningen University & Research/PhD-WUR-Insurance/CSA/Weather data/hourlytp18-Winter.csv")
#need this format-colnames(temp_wheat_exposure2) <- c("Farm","Date","Hour", "Temperature")
as.data.frame(hourly3)
hourly3$farm <- seq.int(nrow(hourly3))
library(reshape2)
long3 <- melt(hourly3, id.vars = c("farm"))
long3$year<- 2018

hourly4<- read.csv("C:/Users/Shakila Vyas/OneDrive - Wageningen University & Research/PhD-WUR-Insurance/CSA/Weather data/hourlytp19-Winter.csv")
#need this format-colnames(temp_wheat_exposure2) <- c("Farm","Date","Hour", "Temperature")
as.data.frame(hourly4)
hourly4$farm <- seq.int(nrow(hourly4))
library(reshape2)
long4 <- melt(hourly4, id.vars = c("farm"))
long4$year<- 2019

####2020 not needed
temp<- rbind(long,long1,long2,long3,long4)

names(temp)[3] <- "temperature"
min(temp$temperature)
max(temp$temperature)
range(temp$temperature)
summary(temp$temperature)
hist(temp$temperature,main="Hourly temperature-winter")

# New TS for temperature support of interest- .225 to get the nice plot with small intervals
temp1 <- as.matrix(lspline(temp$temperature, c(15,28),marginal = T))
temp2 <- cbind(temp,temp1)
names(temp2)
head(temp2)

colnames(temp2)[5] <- "V1"
colnames(temp2)[6] <- "V2"
colnames(temp2)[7] <- "V3"
colnames(temp2)[8] <- "V4"

#adjust variables depending on knots
temp3<-temp2 %>%
  group_by(farm,year)%>%
  summarise_at(vars(V1,V2,V3),sum)

#match names with temp
data<- read.csv("C:/Users/Shakila Vyas/OneDrive - Wageningen University & Research/PhD-WUR-Insurance/CSA/Masterdata_Reg_Winter.csv")
head(data)
#remove less than 3 years data
regdata<- subset(data,repeat_id_crop>=3,select = Q1:Winter_rain)
head(regdata)
#wheat subset
regdata_wheat<- subset(regdata,Q12=="Wheat",select = Q1:Winter_rain)
head(regdata_wheat)

hist(regdata_wheat$Q93)
summary(regdata_wheat)
quantile(regdata_wheat$Q93,probs= c(.99))
regdata_wheat1<- subset(regdata_wheat,Q93<quantile(regdata_wheat$Q93,probs= c(.99)),select = Q1:Winter_rain )
hist(regdata_wheat1$Q93)
summary(regdata_wheat1)
names(regdata_wheat1)
colnames(regdata_wheat1)[colnames(regdata_wheat1) == "Q90"] <- "year"
colnames(regdata_wheat1)[colnames(regdata_wheat1) == "spatial_id"] <- "farm"
names(regdata_wheat1)

#join both  data
temp4 <- join(regdata_wheat1,temp3, by=c("farm", "year"))
names(temp4)
table(temp4$year)
temp4$reg_year<- as.numeric(as.factor(temp4$Q5))*temp4$year

class(temp4$Winter_rain)
temp4$Winter_rain1 <- as.numeric(temp4$Winter_rain)

temp4

#####bootstrapping
library(fixest)
set.seed(123)
marginal_temperatures_M1 <- (lspline(seq(10,50,0.225), c(15,28),marginal = T))

# Run 1000 models to derive uncertainty in estimates
spline_fitted_M1 <- matrix(NA,nrow=1000, ncol=nrow(marginal_temperatures_M1))

for (l in 1: 1000){
  # Sample by region times year
  data_temp <- temp4[which(temp4$reg_year %in% sample(unique(temp4$reg_year),replace=T)),]
  
  # Run regression with random subsample (data_temp), adjust variables to knots
  fit_boot  <- feols(log(Q93) ~ V1 + V2 + V3+  year + I(year^2) + Winter_rain1 + I(Winter_rain1^2)   | farm , temp4)
  
  #Save marginal effect of temperatures,adjust variables to knots 
  spline_fitted_M1[l,]<- as.vector((marginal_temperatures_M1  %*% coef(fit_boot)[1:3])) 
  
  # Show progress in %
  print(l/1000 *100)
  
}

#precip response curve
####main plot for rainfall, do block bootstrapping
library(fixest)
fit_boot  <- feols(log(Q93) ~ x + V2  + year + I(year^2) + Winter_rain1 + I(Winter_rain1^2)  | farm ,cluster = c( "year","farm"),temp4)
plot(coef(fit_boot)[5]*seq(5:1000)+ coef(fit_boot)[6]*seq(5:1000)^2,ylab="log(Wheat yield in kg/ha)",xlab="precipitation (mm)",main=)
abline(h=0, col="blue")

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
#rm(spline_merged_M1)
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
model_1_wheat <- ggplot()+ ggtitle("Hourly temperature effects: Best fit") + 
  theme(panel.grid.major = element_blank(),
        plot.title = element_text(family="Times New Roman", size=20, colour = "grey25", hjust=0.5),
        axis.title = element_text(family="Times New Roman", size=16, colour = "grey25"),
        axis.text = element_text(family="Times New Roman", size=16),
        panel.grid.minor = element_blank(),
        panel.background = element_blank(),
        axis.line = element_line(colour = "black"),
        legend.position="none")+
  xlab("Temperature (°C)") + ylab("Log yield response (Kg/ha)")+
  scale_x_continuous(breaks = seq(10,50, by=5), limits = c(5,50))+
  scale_y_continuous(breaks = seq(-1,1, by=.2), limits = c(-1,1))+
  geom_hline(yintercept=c(seq(-1,1,by=0.2)), colour="gray")+
  geom_vline(xintercept=c(15,28), linetype="dotted", colour="gray")+
  geom_line(data=spline_merged_M1, aes(y=value.x, x=Temp_bin, group=Var1, alpha=x.x^200/200), size=.01, colour="dodgerblue1")+
  geom_line(data=spline_merged_M1, aes(y=average_effect, x=Temp_bin), alpha=0.85, size=.75, colour="blue4")+
  geom_line(data=spline_merged_M1, aes(y=lower_bound, x=Temp_bin)   , alpha=0.85, size=.75, colour="blue4")+       
  geom_line(data=spline_merged_M1, aes(y=upper_bound, x=Temp_bin)   , alpha=0.85, size=.75, colour="blue4")

model_1_wheat

ggsave("C:/Users/Shakila Vyas/OneDrive - Wageningen University & Research/PhD-WUR-Insurance/CSA/Addiitonal results/wheat1528.png", width = 20, height = 20, units = "cm")

#OVERLAY PLOTS LIKE SCHLENKER PNAS FIG 1, KNOTS= BREAKPOINTS
##########################################other codes
#fe regression
library(fixest)
soy_yield_spline = feols(log(Q93) ~ V2 + year + I(year^2) + Summer_rainsum + I(Summer_rainsum^2)   | farm , cluster = c( "year","farm"), temp4)
summary(soy_yield_spline)
results<- etable(soy_yield_spline)
write.csv(results,"C:/Users/Shakila Vyas/OneDrive - Wageningen University & Research/PhD-WUR-Insurance/CSA/ERA5_results_eq_knot3_returns.csv")
coef(soy_yield_spline)
library(lspline)
m1 <- lm(log(Q93) ~ lspline(temp4$temperature, c(28, 35)), data=temp4)
knitr::kable(broom::tidy(m1))
coef(m1)
library(margins)
mar<- margins(m1)
plot(mar)
library(segmented)
out.lm<-lm(log(Q93)~temperature,data=temp4)
o<-segmented(out.lm, seg.Z=~temperature,npsi=2)
plot(o)
library(segmented)
#fit simple linear regression model, as an additional check, not used in main results
soy_yield_gdd10 = feols(log(Q93) ~ gdd + gdd1 + year + I(year^2) + Summer_rainsum + I(Summer_rainsum^2)  | farm , cluster = c( "year","farm"), temp3)
results<- etable(soy_yield_gdd10)
write.csv(results,"C:/Users/Shakila Vyas/OneDrive - Wageningen University & Research/PhD-WUR-Insurance/CSA/Addiitonal results/gdd_threshold_soybean.csv")
