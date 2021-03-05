pacman::p_load(devtools)
install_github("cqyangvt/BSE4304/BSEHydroModels")

# Save your functions and build up your working "modeldata" dataframe
# so you can save a package.
objects()
rm(list=objects())
dir.create("~/Week06")
setwd("~/Week06")
#
# We will add to the mix a handy date manipulation package
# which you will use the month() function to isolate non-snow months
# below
#
if (!require("pacman")) install.packages("pacman")
pacman::p_load(data.table,httr,EcoHydRology,curl,elevatr,raster,soilDB,
               rgdal,lubridate)

# Cleaning up
objects()
rm(list=objects())
# Installing the packages we will play with today
if (!require("pacman")) install.packages("pacman")
pacman::p_load(elevatr,raster,soilDB,rgdal)
pacman::p_load(EcoHydRology,curl,httr,rnoaa)
myflowgage_id="02032640"
myflowgage=get_usgs_gage(myflowgage_id,begin_date = "2015-01-01",end_date = "2021-03-01")
# Note that flow returned is in m3/day, but we want mm/day for the basin
myflowgage$flowdata$Qmm = myflowgage$flowdata$flow/myflowgage$area/10^3
# stns=meteo_distance(
#   station_data=ghcnd_stations(),
#   lat=myflowgage$declat,
#   long=myflowgage$declon,
#   units = "deg",
#   radius = 30,
#   limit = NULL
# )
# We are looking for stations with elements that have PRCP, TMAX and TMIN 
# and current data (i.e. Year 2021). 
WXStn="USW00093736"
WXData=meteo_pull_monitors(
  monitors=WXStn,
  keep_flags = FALSE,
  date_min = "2016-01-01",
  date_max = NULL,
  var = c("TMAX","TMIN","PRCP") 
)
# Create an aligned modeldata data frame to build our model in
modeldata=merge(WXData,myflowgage$flowdata,by.x="date",by.y="mdate")
modeldata$MaxTemp=modeldata$tmax/10 # Converting to C
modeldata$MinTemp=modeldata$tmin/10 # Converting to C
modeldata$P=modeldata$prcp/10 # Converting to mm
# Compare your precipitation to the flow out of your basin
mean(modeldata$Qmm)
mean(modeldata$P)
modeldata$P[is.na(modeldata$P)]=0
modeldata$MinTemp[is.na(modeldata$MinTemp)]=0
modeldata$MaxTemp[is.na(modeldata$MaxTemp)]=
  modeldata$MinTemp[is.na(modeldata$MaxTemp)] +1
modeldata$MaxTemp[modeldata$MaxTemp<=modeldata$MinTemp]=
  modeldata$MinTemp[modeldata$MaxTemp<=modeldata$MinTemp]+1
modeldata$AvgTemp=(modeldata$MaxTemp+modeldata$MinTemp)/2.0

#
# We will explore
#
url="https://prd-tnm.s3.amazonaws.com/StagedProducts/Hydrography/NHD/HU8/HighResolution/Shape/NHD_H_03010101_HU8_Shape.zip"
curl_download(url,"NHD_H_03010101_HU8_Shape.zip")
unzip("NHD_H_03010101_HU8_Shape.zip",exdir="03010101")
streams=readOGR("03010101/Shape/NHDFlowline.dbf")
mystream=subset(streams,GNIS_ID=="01478950")
plot(mystream,col="red")
#
# Use the spatial extents from our stream to download elevation raster.
#
proj4_ll = "+proj=longlat"
proj4string(mystream) = proj4_ll
mydem=get_aws_terrain(locations=mystream, 
                      z = 11, prj =proj4string(mystream) ,src ="aws",clip="bbox")
#
# Pretty pictures of our area help ease the frustration
#
plot(mydem)
lines(mystream,col="blue",lwd=4)
points(myflowgage$declon,myflowgage$declat,pch = 24, cex=2, col="blue", bg="red", lwd=2)
#
# For initializing slopes, we store the summary stats for terrain slope
#
plot(terrain(mydem, opt='slope',unit = "radians"))
lines(mystream,col="blue",lwd=4)
points(myflowgage$declon,myflowgage$declat,pch = 24, cex=2, col="blue", bg="red", lwd=2)
slope_sum=summary(terrain(mydem, opt='slope',unit = "radians"))
#
# And after our initialization is done, we are ready to get our estimated 
# dP from our TMWB model. Remember that dP = P - ET - SnowFall + SnowMelt
# 
# We are building on our prior lab solutions, need to build out our previous 
# TMWB model. We will grab functions from the solutions from Week 4’s Lab  
# Lab05

BasinTMWB=modeldata
BasinTMWB = TMWB_Model(fnc_TMWB = BasinTMWB,fnc_slope=0, 
                       fnc_aspect=0,func_DAWC=.3,
                       func_z=500,fnc_fcres=.3)


attach(BasinTMWB)
plot(date,AW)
plot(dP,Qmm)
detach(BasinTMWB)
#
# But, we know that our systems behave differently during snowy winter
# months, so we will isolate our June ( month>5) - October ( < 11 ) data (_JO)
#
BasinTMWB_JO=BasinTMWB[(month(BasinTMWB$date) > 5 
                        & month(BasinTMWB$date) < 11),]
attach(BasinTMWB_JO)
plot(dP,Qmm)
detach(BasinTMWB_JO)

