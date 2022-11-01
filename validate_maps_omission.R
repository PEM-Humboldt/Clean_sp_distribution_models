setwd("C:/Data/50Reefs/Felipe/iucn/rasters_aves")

ebird<-read.csv("Records_eBird_names_Ayerbe.csv")

myfiles = list.files("aves-shp/raster_aves_buffer_20k/elev_filt", pattern="*.tif") 

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

setwd("C:/Data/50Reefs/Felipe/iucn/rasters_aves/aves-shp/raster_aves_buffer_20k/elev_filt")

m<-list()
for(i in 1:length(list_pol)){
  a<-raster(list_pol[[i]])
  spnames<-strsplit(list_pol[[i]],"_elev.tif")
  spnames<-strsplit(spnames[[1]],"_")
  spnames<-unlist(spnames)
  spnames<-paste0(spnames[[1]]," ",spnames[[2]])
  records<-subset(ebird,ebird$scientific_name == spnames)
  records<-records[!duplicated(records[c("longitude","latitude")]),]
  vals<-raster::extract(a,records[c("longitude","latitude")])
  records$presence<-vals
  m[[i]]<-records
  print(i)
}

mdf<-do.call(rbind,m)
plyr::count(mdf$presence)
write.csv(mdf,"mdf.csv")

setwd("C:/Data/50Reefs/Felipe/iucn/rasters_aves")

mdf<-read.csv("mdf.csv")

mdf[is.na(mdf$presence),"presence"] <- 0

library(dplyr)
mdf %>% group_by(scientific_name) %>% summarise(count_sales = n())

a<-subset(mdf,mdf$scientific_name == "Aburria aburri")
coordinates(a)<-a[c("longitude","latitude")]
points(a[a$presence == 1,],col = "blue")
points(a[a$presence == 0,],col = "orange")
head(a)



