rm(list = ls())
gc()

library(devtools)
library(remotes)
library(rgeos)
#install_github("connectscape/Makurhini", dependencies = TRUE, upgrade = "never")
library(Makurhini)
library(raster)
library(rgdal)
library(xfun)
library(sf)
library(geogrid)
library(rgeos)

#List of rasters to merge

setwd("C:/Data/50Reefs/Felipe/iucn/")

myfiles = list.files("rasters_aves/aves-shp/raster_aves_buffer_20k/elev_filt",
                     pattern="*elev.tif") 

list_raster_hf<-list()
for (i in 1:length(myfiles)){
  print(file_ext(myfiles[[i]]))
  if(file_ext(myfiles[[i]]) == "xml"|file_ext(myfiles[[i]]) == "dbf"
     |file_ext(myfiles[[i]]) == "ovr"|file_ext(myfiles[[i]]) == "cpg"
     |file_ext(myfiles[[i]]) == "tfw"|file_ext(myfiles[[i]]) == "lock"){
    list_raster_hf[[i]]<-NULL
  }else{
    list_raster_hf[[i]]<-myfiles[[i]]
    
  }
}

list_raster_hf<-plyr::compact(list_raster_hf)
myfiles<-unlist(list_raster_hf)


#focus_sp##########

focusbirds<-read.csv("rasters_aves/aves_vul_cambio_climatico.csv")
focusbirds<-subset(focusbirds,focusbirds$OVERALL.VULNERABILITY %in% c("High","H"))
focusbirds<-focusbirds$scientificName
hola<-lapply(focusbirds,strsplit," ")
hola<-lapply(hola,
             function(x)paste0(x[[1]][[1]],"_",x[[1]][[2]],"_elev.tif"))
hola<-unlist(hola)
list_raster_hf<-list_raster_hf[list_raster_hf %in% hola]



focusbirds<-read.csv("rasters_aves/aves-shp/focusbirds.csv")
focusbirds<-focusbirds$filename


hola<-lapply(myfiles,strsplit,"_")
hola<-lapply(hola,
             function(x)paste0("z",x[[1]][[1]]," ",x[[1]][[2]],".tif"))
hola<-unlist(hola)


present<-subset(focusbirds,(focusbirds %in% hola))

missing<-subset(focusbirds,!(focusbirds %in% hola))
# load birds

missing<-(strsplit(missing, "\\.tif|\\,|\\z"))


for(i in 1:length(missing)){
  missing[[i]]<-missing[[i]][[2]]
}

missing<-unlist(missing)

#####################

all<-data.frame(unlist(list_raster_hf),hola)
all<-subset(all,all$hola %in% focusbirds)
list_raster_hf<-all$unlist.list_raster_hf.


####Read focus species


setwd("C:/Data/50Reefs/Felipe/iucn/rasters_aves/forest_specialists")


myfiles = list.files(pattern="*.tif") 

list_raster_fc<-list()
for (i in 1:length(myfiles)){
  print(file_ext(myfiles[[i]]))
  if(file_ext(myfiles[[i]]) == "xml"|file_ext(myfiles[[i]]) == "dbf"
     |file_ext(myfiles[[i]]) == "ovr"|file_ext(myfiles[[i]]) == "cpg"
     |file_ext(myfiles[[i]]) == "tfw"|file_ext(myfiles[[i]]) == "lock"){
    list_raster_fc[[i]]<-NULL
  }else{
    list_raster_fc[[i]]<-myfiles[[i]]
  }
}

list_raster_fc<-plyr::compact(list_raster_fc)


#choose focus sp#####

selected_sp<-list()
for(i in 1:length(list_raster_hf)){
  focus_sp<-list_raster_hf[[i]]
  name1<-strsplit(focus_sp, "_")
  name_sp<-paste0("z",name1[[1]][1]," ",name1[[1]][2],".tif")
  if(name_sp %in% list_raster_fc){
    selected_sp[[i]]<-list_raster_hf[[i]]
  }else{
    selected_sp[[i]]<-NULL
  }
}

list_raster_hf<-plyr::compact(selected_sp)

#####Create richness map

setwd("C:/Data/50Reefs/Felipe/iucn")

DEM<- raster("DEM/DEM_Colombia_resample.tif")

init_rast<-raster(paste0("rasters_aves/aves-shp/raster_aves_buffer_20k/elev_filt/",list_raster_hf[[1]]))
rs1<-extend(init_rast,DEM)
rs1[which(rs1[] > 0)]<-1

for(i in 2:length(list_raster_hf)){
  rast<-raster(paste0("rasters_aves/aves-shp/raster_aves_buffer_20k/elev_filt/",list_raster_hf[[i]]))
  #plot(rast, main = list_raster_hf[[i]])
  rast[which(rast[] > 0)]<-1
  rast<-extend(rast,DEM)
  init_rast<-stack(rs1,rast)
  rs1 <- calc(init_rast, sum, na.rm = T)
  print(i)
  gc()
}

setwd("C:/Data/50Reefs/Felipe/iucn/rasters_aves/aves-shp")
writeRaster(rs1,"./mapas_riqueza/riqueza_aves_wgs84/riqueza_vul_ccl.tif",overwrite = T)
