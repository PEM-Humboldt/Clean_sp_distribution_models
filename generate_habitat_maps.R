library(devtools)
library(remotes)
library(rgeos)
library(raster)
library(rgdal)
library(xfun)
library(sf)
library(geogrid)
library(rgeos)


setwd("C:/Data/50Reefs/Felipe/iucn")

#habitat code data####

load("./habitats_codes/habitats_iavh_codes.RData")
name1<-strsplit(iavh_habitats_tax$scientific_name, " ")
iavh_habitats_tax$species<-unlist(lapply(name1, function(x) paste0(x[[1]],"_",x[[2]])))

#land cover/habitat data####

CORINE<-raster("./habitats_codes/Habitats_CORINE.tif")


#ebird data#####
ebird<-read.csv("rasters_aves/Records_eBird_names_Ayerbe.csv")

#raster_avers####


myfiles = list.files("./rasters_aves/aves-shp/raster_aves_buffer_20k/elev_filt", pattern="*.tif") 

list_pol<-list()
for (i in 1:length(myfiles)){
  print(file_ext(myfiles[[i]]))
  if(file_ext(myfiles[[i]]) == "xml"|file_ext(myfiles[[i]]) == "lock"){
    list_pol[[i]]<-NULL
  }else{
    list_pol[[i]]<-myfiles[[i]]
    
  }
}

list_pol<-plyr::compact(list_pol)
list_pol<-unlist(list_pol)

m<-list()

for(i in 1201:length(list_pol)){
  
  dem_sel<-raster(paste0("./rasters_aves/aves-shp/raster_aves_buffer_20k/elev_filt/",list_pol[[i]]))
  #plot(dem_sel)
  name1<-unlist(strsplit(names(dem_sel), "\\Â|\\,| |_"))
  name_sp<-paste0(name1[[1]],"_",name1[[2]])
  
  dem_sel<-extend(dem_sel,CORINE)
  CORINE2<-extend(CORINE,dem_sel)
  dem_sel<-mask(CORINE2,dem_sel)
  
  # df<-as.data.frame(rasterToPoints(dem_sel))
  # df<-subset(df,df[3] >0)
  # df$CORINE<-raster::extract(CORINE,df[c(1:2)])
  sp<-subset(iavh_habitats_tax,iavh_habitats_tax$species == name_sp)
  if(nrow(sp) >0){
    hab_sp<-unlist(strsplit(sp$Habitats_Code, ","))
    dem_sel[which(!(dem_sel[] %in% as.numeric(hab_sp)))]<-NA
    #plot(dem_sel)
    # transformed_habitats<-c("600","900","1300","1401","1402","1403",
    #                         "1404","1406","1500","1700")
    # 
    names(dem_sel)<-paste0(name1[[1]],"_",name1[[2]])
    writeRaster(dem_sel,paste0("./rasters_aves/aves-shp/raster_aves_buffer_20k/CORINE_filt/",
                               paste0(name1[[1]],"_",name1[[2]]),"_CORINE.tif"), 
                overwrite = T, datatype = "INT1U")
    
    #validate points
    spnames<-paste0(name1[[1]]," ",name1[[2]])
    records<-subset(ebird,ebird$scientific_name == spnames)
    records<-records[!duplicated(records[c("longitude","latitude")]),]
    vals<-raster::extract(dem_sel,records[c("longitude","latitude")])
    records$presence<-vals
    plyr::count(records$presence)
    m[[i]]<-records
  }
  print(i)
  gc()
}

mdf<-do.call(rbind,m)
head(mdf)
write.csv(mdf,"val_birds_CORINE_1201_1700.csv")

library(dplyr)
mdf %>% group_by(scientific_name) %>% summarise(count_sales = n())

a<-records
a[is.na(a$presence),"presence"] <- 0
coordinates(a)<-a[c("longitude","latitude")]
points(a[a$presence != 0,],col = "blue")
points(a[a$presence == 0,],col = "orange")
