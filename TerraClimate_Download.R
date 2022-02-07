########################################################
###TERRACLIMATE Aggregated Years Download Script     ###
###Author: William Hammond        Date: 16JUL2020    ###
########################################################
library(data.table)
library(ncdf4)

# Function to download terraclimate for a single global tree mortality location (gtm):
terra.gtm <- function(ID, long, lat, mortality.begin){
  var="PDSI"
  baseurlagg <- paste0(paste0("http://thredds.northwestknowledge.net:8080/thredds/dodsC/agg_terraclimate_",var),"_1958_CurrentYear_GLOBE.nc")
  nc <- nc_open(baseurlagg)
  print(ID)
  print(c(long,lat))
  longit <- ncvar_get(nc, "lon")
  # print(lon)
  latit <- ncvar_get(nc, "lat")
  # print(lat)
  flat = match(abs(latit - lat) < 1 / 48, 1)
  latindex = which(flat %in% 1)
  flon = match(abs(longit - long) < 1 / 48, 1)
  lonindex = which(flon %in% 1)
  start <- c(lonindex, latindex, 1)
  print(start)
  count <- c(1, 1,-1)
  nc_close(nc)
  pb <- txtProgressBar(min=0, max=9, style=3)
  i=0
  setTxtProgressBar(pb, i)
  # read in the full period of record using aggregated files
  nc <- nc_open('http://thredds.northwestknowledge.net:8080/thredds/dodsC/agg_terraclimate_PDSI_1958_CurrentYear_GLOBE.nc')
  PDSI <- as.numeric(ncvar_get(nc, varid = "PDSI",start = start, count))
  nc_close(nc)
  i=1
  setTxtProgressBar(pb, i)
  nc <- nc_open('http://thredds.northwestknowledge.net:8080/thredds/dodsC/agg_terraclimate_aet_1958_CurrentYear_GLOBE.nc')
  aet <- as.numeric(ncvar_get(nc, varid = "aet",start = start, count))
  nc_close(nc)
  i=2
  setTxtProgressBar(pb, i)
  nc <- nc_open('http://thredds.northwestknowledge.net:8080/thredds/dodsC/agg_terraclimate_def_1958_CurrentYear_GLOBE.nc')
  def <- as.numeric(ncvar_get(nc, varid = "def",start = start, count))
  nc_close(nc)
  i=3
  setTxtProgressBar(pb, i)
  nc <- nc_open('http://thredds.northwestknowledge.net:8080/thredds/dodsC/agg_terraclimate_ppt_1958_CurrentYear_GLOBE.nc')
  ppt <- as.numeric(ncvar_get(nc, varid = "ppt",start = start, count))
  nc_close(nc)
  i=4
  setTxtProgressBar(pb, i)
  nc <- nc_open('http://thredds.northwestknowledge.net:8080/thredds/dodsC/agg_terraclimate_soil_1958_CurrentYear_GLOBE.nc')
  soil <- as.numeric(ncvar_get(nc, varid = "soil",start = start, count))
  nc_close(nc)
  i=5
  setTxtProgressBar(pb, i)
  nc <- nc_open('http://thredds.northwestknowledge.net:8080/thredds/dodsC/agg_terraclimate_tmax_1958_CurrentYear_GLOBE.nc')
  tmax <- as.numeric(ncvar_get(nc, varid = "tmax",start = start, count))
  nc_close(nc)
  i=6
  setTxtProgressBar(pb, i)
  nc <- nc_open('http://thredds.northwestknowledge.net:8080/thredds/dodsC/agg_terraclimate_vpd_1958_CurrentYear_GLOBE.nc')
  vpd <- as.numeric(ncvar_get(nc, varid = "vpd",start = start, count))
  nc_close(nc)
  i=7
  setTxtProgressBar(pb, i)
  nc <- nc_open('http://thredds.northwestknowledge.net:8080/thredds/dodsC/agg_terraclimate_pet_1958_CurrentYear_GLOBE.nc')
  pet <- as.numeric(ncvar_get(nc, varid = "pet",start = start, count))
  nc_close(nc)
  i=8
  setTxtProgressBar(pb, i)
  nc <- nc_open('http://thredds.northwestknowledge.net:8080/thredds/dodsC/agg_terraclimate_tmin_1958_CurrentYear_GLOBE.nc')
  tmin <- as.numeric(ncvar_get(nc, varid = "tmin",start = start, count))
  nc_close(nc)
  i=9
  setTxtProgressBar(pb, i)
  index<-seq(1:744)
  month<-rep(1:12, 744/12)
  year<-seq(1958,2019)
  year<-rep(year, each=12)
  terradf<-cbind(index, month, year, PDSI, aet, def, ppt, soil, tmax, vpd, pet,tmin)
  terradf<-as.data.frame(terradf)
  head(terradf)
  terradf$gtm <- rep(ID)
  terradf$mortality.begin<-rep(mortality.begin)
  fwrite(terradf, paste("./terraclimate_data/",ID,".csv",sep=""), showProgress = TRUE)
}
