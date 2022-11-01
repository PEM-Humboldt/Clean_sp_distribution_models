#crop human footprint to PNN
rm(list = ls())
gc()

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
library(raster)

#leer poligono de regiones####
library(rgdal)

DEM<- raster("DEM/DEM_Colombia_resample.tif")

#read ECO data####

eco_SA<-raster("./Regiones_bioticas/regiones_bioticos/regiones_bioticas_mod_4.tif")
ECO_df<-data.frame(rasterToPoints(eco_SA))
ECO_df<-plyr::count(ECO_df[3])
colnames(ECO_df)<-c("ECO","freq")

setwd("C:/Data/50Reefs/Felipe/iucn/rasters_aves/aves-shp/aves_buffer_20k")

myfiles = list.files(pattern="*.shp") 

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

##Get elevation data#####

setwd("C:/Data/50Reefs/Felipe/iucn")

tax<-read.csv("rasters_aves/Taxonomic_equivalence_OAC.csv")
head(tax)
#test<-(subset(tax,tax$Species_List_ACO != tax$Species_List_Ayerbe_2019))
#View(test)

tax<-tax[c("Species_List_Ayerbe_2019","Species_List_ACO",
           "max_elev","min_elev")]

elevation<-read.csv("./rasters_aves/birds_elevation_ranges.csv")

elevation$Min<-as.numeric(elevation$Min)
elevation$Max<-as.numeric(elevation$Max)

elevation<-merge(tax,elevation[c("NOMBRE.CIENTÍFICO","Min","Max")],
                 by.x = "Species_List_Ayerbe_2019",
                 by.y = "NOMBRE.CIENTÍFICO")

#fix elevation based on ebird

ebird<-read.csv("./rasters_aves/Records_eBird_names_Ayerbe.csv")

# df<-as.data.frame(extract(DEM,ebird[c("longitude","latitude")]))
# 
# ebird$elev<-df[[1]]
# 
# library(dplyr)
# max_sp<-ebird %>% group_by(scientific_name) %>% summarise(max_elev_ebird = max(elev, na.rm = T))
# min_sp<-ebird %>% group_by(scientific_name) %>% summarise(min_elev_ebird = min(elev, na.rm = T))

elev_ebird<-merge(max_sp,min_sp,"scientific_name")

elev_ebird<-read.csv("./rasters_aves/analisis_Orlando/Elevation_eBird_Colombia_2x2.csv")

elevation<-merge(elevation,elev_ebird,
                 by.x = "Species_List_ACO",
                 by.y = "scientific_name",
                 all.x = T)


elevation$minfinal<-as.numeric(apply(elevation,1,function(x)min(x["Min"],x["min_elev"],na.rm = T)))
elevation$maxfinal<-as.numeric(apply(elevation,1,function(x)max(x["Max"],x["max_elev"],na.rm = T)))

#View(elevation)

elevation[which(elevation$minfinal == 0),"Min"]<--100
elevation$Min<-as.numeric(elevation$minfinal) - 100
elevation$Max<-as.numeric(elevation$maxfinal) + 100
elevation[which(elevation$Max > 3000),"Max"]<-elevation[which(elevation$Max > 3000),"Max"] + 300

head(elevation)

############

areas_per<-data.frame(matrix(nrow = 0,ncol = 5))
colnames(areas_per)<-c("sp","pix_noeco",
                       "pix_eco")

for(i in 1201:length(list_pol)){
  #setTxtProgressBar(pb,1)
  spnames<-strsplit(list_pol[[i]],".shp")
  spnames<-lapply(spnames,function(x)paste0("z",x))
  spnames<-unlist(spnames)
  #if(spnames %in% missing){
  #pol<-readOGR("./rasters_aves/aves-shp/aves_buffer_20k/Cypseloides lemosi.shp")
  pol<-readOGR(paste0("./rasters_aves/aves-shp/aves_buffer_20k/",list_pol[[i]]))
  #pol_buff<-st_buffer(st_as_sf(pol),10000)
  area_m2<-st_area(st_as_sf(pol))
  sp<-subset(elevation,elevation$Species_List_Ayerbe_2019 == pol$Nombre)
  sp<-subset(sp,!is.na(sp$Min))
  print(c(i,"sp",nrow(sp)))
  if(nrow(sp) < 1)next()
  
  multipol<-st_cast(st_as_sf(pol),"POLYGON")
  rast<-list()
  arp<-0
  areanoelev<-0
  for(p in 1:nrow(multipol)){
    
    eco_sp<-fasterize::fasterize(st_as_sf(multipol[p,]),eco_SA)
    eco_sel_r<-mask(eco_SA,eco_sp)
    df_sel<-as.data.frame(rasterToPoints(eco_sel_r))
    #now extract elevation
    df_sel$elev<-raster::extract(DEM,df_sel[1:2])
    
    
    #subset pixel within the elevational range
    ap<-subset(df_sel,df_sel$elev <as.numeric(sp$Max))
    ap<-subset(ap,ap$elev > as.numeric(sp$Min))
    
    #the total number of pixels without filtering by ecorregion:
    arp<- nrow(ap) + arp
    
    if(sp$Min < 800){#I changed this to 800 from 1000
      df_sel<-plyr::count(df_sel[[3]])
      df_sel<-subset(df_sel,df_sel$x !=128)
      colnames(df_sel)<-c("ECO","freq_sel")
      df_sel<-merge(df_sel,ECO_df,"ECO")
      df_sel$perc<-(df_sel$freq_sel/df_sel$freq)*100
      #select ecorregions more than 50(55) % (based on Celeus spectabilis)
      df_sel<-subset(df_sel,df_sel$perc > 53)
      if(nrow(df_sel)>1){
        #plot(eco_sel)
        eco_sel<-unique(df_sel$ECO)
        eco_sel<-subset(eco_sel,!is.na(eco_sel))
        eco_temp<-eco_SA
        eco_temp[which(eco_temp[] %in% eco_sel)]<-1000
        #plot(eco_temp)
        eco_temp[which(eco_temp[] != 1000)]<-NA
        dem_temp<-extend(DEM,eco_temp)
        eco_temp<-extend(eco_temp,dem_temp)
      }else{
        eco_temp<-fasterize::fasterize(st_as_sf(multipol[p,]),DEM)
        dem_temp<-extend(DEM,eco_temp)
        eco_temp<-extend(eco_temp,dem_temp)
      }
      #plot(hola)
    }else{
      eco_temp<-fasterize::fasterize(st_as_sf(multipol[p,]),DEM)
      dem_temp<-extend(DEM,eco_temp)
      eco_temp<-extend(eco_temp,dem_temp)
    }
    dem_sel<-mask(dem_temp,eco_temp)
    areanoelev<-areanoelev+length(dem_sel[which(!is.na(dem_sel[]))])
    #plot(dem_sel,add = T)
    dem_sel[which(dem_sel[]>as.numeric(sp$Max))]<-0L
    dem_sel[which(dem_sel[]<as.numeric(sp$Min))]<-0L
    dem_sel[which(dem_sel[] > 0)]<-1L
    dataType(dem_sel)="INT1U"
    name1<-unlist(strsplit(sp$Species_List_ACO, "\\Â|\\,| "))
    name_sp<-paste0(name1[[1]],"_",name1[[2]])
    #plot(dem_sel)
    df<-as.data.frame(rasterToPoints(dem_sel))
    df<-subset(df,df[3] >0)
    #number of pixels after filtering by ecorregion:
    #print(c(i,"df",nrow(df)))
    if(nrow(df) > 10){
      rast_sp<-rasterFromXYZ(df[c(1,2,3)])
      crs(rast_sp)<-crs(pol)
      rast_sp<-extend(rast_sp,DEM)
    }else{
      rast_sp<-NULL
    }
    rast[[p]]<-rast_sp
  }
  rast<-plyr::compact(rast)
  #set all rasters to same extent
  for (m in 1:length(rast)){
    r<-rast[[m]]
    r[is.na(r[])] <- 0 
    rast[[m]]<-r
  }
  
  #merge all rasters
  rast_sp<-Reduce("+", rast)
  rast_sp[rast_sp[]==0] <- NA
  df<-as.data.frame(rasterToPoints(rast_sp))
  rp<-nrow(df)
  #plot(rast_sp) + theme(legend.title = element_blank())
  #plot(pol, add = T)
  writeRaster(rast_sp,paste0("./rasters_aves/aves-shp/raster_aves_buffer_20k/elev_filt/",
                             name_sp,"_elev.tif"), 
              overwrite = T, datatype = "INT1U")
  #}
  areas_per[i,]<-c(name_sp,arp,rp,areanoelev,area_m2)
  print(i)
  gc()
}

areas_per<-subset(areas_per,!is.na(areas_per$sp))

areas_per$ration<-as.numeric(areas_per$pix_noeco)/as.numeric(areas_per$pix_eco)

write.csv(areas_per,"./rasters_aves/aves-shp/raster_aves_buffer_20k/areas_per_1201_1800.csv")