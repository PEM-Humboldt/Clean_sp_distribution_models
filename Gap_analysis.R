#__________________________________#
# Code written by Felipe Suarez    #
# Version :  01-07-2020            #
#__________________________________#

#clear workspace
rm(list=ls())

library(SpaDES)
library(raster)
#library(spatial.tools)
library(xfun)
library(rgdal)
library(ReIns)


# load birds

setwd("C:/Data/50Reefs/Felipe/iucn/rasters_aves/aves-shp/proj")

myfiles = list.files(pattern="*.tif") 

#read species data

list_raster<-list()
for (i in 1:length(myfiles)){
  print(file_ext(myfiles[[i]]))
  if(file_ext(myfiles[[i]]) == "xml"|file_ext(myfiles[[i]]) == "dbf"
     |file_ext(myfiles[[i]]) == "ovr"|file_ext(myfiles[[i]]) == "cpg"
     |file_ext(myfiles[[i]]) == "tfw"|file_ext(myfiles[[i]]) == "lock"){
    list_raster[[i]]<-NULL
  }else{
    list_raster[[i]]<-myfiles[[i]]
    
  }
}

list_raster<-plyr::compact(list_raster)


#READ PARK DATA
setwd("C:/Data/50Reefs/Felipe/iucn/RUNAP/RUNAP_rasters")

myfiles = list.files(pattern="*.tif") 

list_raster_PNN<-list()
for (i in 1:length(myfiles)){
  print(file_ext(myfiles[[i]]))
  if(file_ext(myfiles[[i]]) == "xml"|file_ext(myfiles[[i]]) == "dbf"
     |file_ext(myfiles[[i]]) == "ovr"|file_ext(myfiles[[i]]) == "cpg"
     |file_ext(myfiles[[i]]) == "tfw"|file_ext(myfiles[[i]]) == "lock"){
    list_raster_PNN[[i]]<-NULL
  }else{
    list_raster_PNN[[i]]<-myfiles[[i]]
  }
}

list_raster_PNN<-plyr::compact(list_raster_PNN)

list_raster_PNN[[5]]<-list_raster_PNN[[4]]

#FOR 1990 YOU HAVE TO TRANSFORM THE 128 DATA TO NA



#here is the model to calculate area to protect
TempDF = data.frame(x=c(1000,2.5e5), y=c(100,10))
m = diff(TempDF$y)/diff(TempDF$x)
b = ((TempDF$y[[1]]*TempDF$x[[2]])-(TempDF$y[[2]]*TempDF$x[[1]]))/(TempDF$x[[2]]-TempDF$x[[1]])
lm.func<-function(x,m,b){y = m*x + b
return(y)}
x <- seq(1000, 250000, 1000)
plot(x, lm.func(x,m,b), xlab="x", ylab="PDF", type="l")

sp_stats<-list()#list to save stats per species
order_allsp_stats<-list()# list to save allsp stats
raster_distsp<-list()#list to save rasters 1
rasters_ord<-list()#list to save rasters 2
for (h in c(8)) {#list of human footprint rasters
  setwd("C:/Data/50Reefs/Felipe/iucn/RUNAP/RUNAP_rasters")
  PNN<-raster(list_raster_PNN[[h]])
  #PNN[which(PNN[]>126)]<-NA#ONLY FOR 1990 DATA!
  for (j in 1201:length(list_raster)){#list_pol is the list with all the orders
    setwd("C:/Data/50Reefs/Felipe/iucn/rasters_aves/aves-shp/proj")
    order<-raster(list_raster[[j]])#read each polygon
    #create list of order
    #order@data$binomial<-as.character(order@data$binomial)
    #order_list<-split(order,order@data$binomial)
    #polygon_list<-order_list#list of species per order
    if (is.null(intersect(extent(PNN), order))){#check if raster and shapefile (each sp) intersect
      dfsp<-as.data.frame(matrix(nrow = 1,ncol = 3))
      colnames(dfsp)<-c("PNN","freq")
      dfsp$species<-names(order)
      dfsp$year_PNN<-strsplit(names(PNN),"_")[[1]][[2]]
    }else{
      hola<-rasterToPoints(order)
      hola<-as.data.frame(hola)
      hola<-subset(hola,hola[3]==1)
      PNN_data<-raster::extract(PNN,hola[1:2])
      #raster.polygon<-rasterFromXYZ(hf_data)
      #raster.polygon<-crop(human.footprint,extent(order_list[[i]]),snap = "near")
      #raster.polygon<- rasterize(order_list[[i]],raster.polygon, mask=TRUE) #crops to polygon edge & converts to raster
      cells<-hola
      cells$PNN<-PNN_data
      #cells<-rasterToPoints(raster.polygon)#extract all the raster cells (coordinates + pixel value)
      if(nrow(cells)>0){
        cells<-as.data.frame(cells)#convert to df
        #cells<-subset(cells,!is.na(cells[4]))#remove NAs
        hola<-plyr::count(cells[4])
        hola$species<-names(order)
        hola$year_PNN<-strsplit(names(PNN),"_")[[1]][[2]]
      }
    }
    #raster.polygon[!is.na(raster.polygon[])]<-1
    #raster_distsp[[j]]<-order
    hola[which(is.na(hola$PNN)),"in_PNN"]<-0
    hola[which(!is.na(hola$PNN)),"in_PNN"]<-1
    count_pnn<-aggregate(hola$freq,list(PNN=hola$in_PNN),"sum")
    total_area<-sum(count_pnn$x)
    total_area_km2<-total_area*((300/1000)^2)
    total_protected<-count_pnn[which(count_pnn$PNN == "1"),2]
    
    if(length(total_protected)>0){
      hola$per_protected<-(total_protected/total_area)*100
    }else{
      hola$per_protected<-0
    }
    
    if(total_area_km2<=1000){
      hola$needed_to_protect<-100
    }else{
      if(total_area_km2>=250000){
      hola$needed_to_protect<-10 
      }else{
        hola$needed_to_protect<-lm.func(total_area_km2,m,b)
      }
    }
    hola$total_area_km2<-total_area_km2
    hola$per_achieved<-hola$per_protected/hola$needed_to_protect
    if(hola$per_achieved[[1]]>0.9){
      hola$achieved<-1
    }else{
      hola$achieved<-0
    }
    print(paste(j))
    sp_stats[[j]]<-hola[1,]
    gc()
  }
  order_allsp_stats[[h]]<-do.call(rbind,sp_stats)
}
setwd("C:/Data/50Reefs/Felipe/iucn/statistics_humboldt")
write.csv(do.call(rbind,order_allsp_stats),"statistics_gap_analysis_aves_2018_3.csv")


#############################
library(ggplot2)
birds<-read.csv("statistics_gap_analysis_aves_2018_1.csv")

birds[which(birds$per_achieved > 1),"per_achieved"]<-1

ggplot(birds, aes(x = per_achieved, 
                  fill = achieved
                  )) +                       # Draw overlaying histogram
  #geom_histogram(position = "dodge"#, 
                 #alpha = 1, 
                 #bins = 6
                 #)+
  #geom_histogram(aes(y=c(..count..[..group..==1]/(sum(..count..[..group..==1])+remforest),
  #                       ..count..[..group..==2]/(sum(..count..[..group..==2])+lostforest),
  #                       ..count..[..group..==3]/(sum(..count..[..group..==3])+lostforest),
  #                       ..count..[..group..==4]/(sum(..count..[..group..==4])+lostforest),
  #                       ..count..[..group..==5]/(sum(..count..[..group..==5])+lostforest),
  #                       ..count..[..group..==6]/(sum(..count..[..group..==5])+lostforest))),
  #               position='dodge', bins = 6)+
  geom_histogram(aes(y=((..count..)/sum(..count..))*100), 
                 position='dodge', bins = 16)+
  theme_bw()  +
  theme(legend.title = element_blank(),
        legend.position="bottom",
        #legend.box = "horizontal",
        #legend.direction = "horizontal",
        panel.grid.major = element_blank(),
        panel.grid.minor = element_blank(),
        panel.border = element_blank(),
        axis.line.x = element_line(color="black", size = 0.5),
        axis.line.y = element_line(color="black", size = 0.5),
        axis.text=element_text(size=20),
        axis.title=element_text(size=20))+
  scale_color_manual(values = c("#f03b20"))+
#,"#FFD37F","#FFAA00", "#73FFDF","#00734C", "#DF73FF", "#A80084"))+
  #scale_fill_manual(values = c("#f03b20","#FFD37F","#FFAA00", "#73FFDF","#00734C", "#DF73FF", "#A80084"))+
  scale_x_continuous(name = "Porcentaje de representatividad alcanzado",
                     breaks = seq(0.25, 1.25, 0.25),
                     limits=c(0, 1.15)
  )+
  scale_y_continuous(name = "Porcentaje de especies analizadas"#,
                     #breaks = seq(0, 0.6, 0.2),
                     #limits=c(0, 0.6)
  )#+
#facet_wrap(~redeco)


###########################

setwd("C:/Data/50Reefs/Felipe/iucn/GEF_SINAP")
order_allsp_stats<-read.csv("statistics_sp_PNN_1990.csv")

order_allsp_stats<-do.call(rbind,order_allsp_stats)

require(dplyr)
ind_PNN<-order_allsp_stats %>% count(PNN, achieved)

PNN_sp<-aggregate(ind_PNN$n,list(PNN=ind_PNN$PNN),"sum")

PNN_achieved<-subset(ind_PNN,ind_PNN$achieved == 1)

PNN_stats<-merge(PNN_sp,PNN_achieved,"PNN")
colnames(PNN_stats)<-c("PNN","no_species","ac","sp_ach")


setwd("C:/Data/50Reefs/Felipe/iucn/GEF_SINAP")

pnn_ids<-read.csv("pnn_ids_1990.csv")

PNN_stats<-merge(PNN_stats,pnn_ids,"PNN")

PNN_stats$ac<-(PNN_stats$sp_ach/PNN_stats$no_species)*100

colnames(PNN_stats)[[6]]<-"id_pnn"

write.csv(PNN_stats,"PNN_gap_analysis_1990.csv")

setwd("C:/Data/50Reefs/Felipe/iucn/")
#read shapefile

PNN_shp<-readOGR("RUNAP/RUNAP_1990.shp")

PNN_shp<-merge(PNN_shp,PNN_stats,"id_pnn")

writeOGR(PNN_shp,dsn="./RUNAP", layer="PNN_gap_1990", driver="ESRI Shapefile")


#########################

setwd("C:/Data/50Reefs/Felipe/iucn/GEF_SINAP")

df2018<-read.csv("PNN_gap_analysis_2018.csv")
df2010<-read.csv("PNN_gap_analysis_2010.csv")
df2000<-read.csv("PNN_gap_analysis_2000.csv")
df1990<-read.csv("PNN_gap_analysis_1990.csv")

df2010$ac<-(df2010$sp_ach/df2010$no_species)*100

df<-merge(df2018[c("id_pnn","no_species","ac","sp_ach")],df2010[c("id_pnn","ac","sp_ach")],"id_pnn",all = T)

colnames(df)<-c("id_pnn","s","rep_2018","pr_2018","rep_2010","pr_2010")

df<-merge(df,df2000[c("id_pnn","ac","sp_ach")],"id_pnn",all = T)

colnames(df)<-c("id_pnn","s","rep_2018","pr_2018",
                "rep_2010","pr_2010","rep_2000","pr_2000")

df<-merge(df,df1990[c("id_pnn","ac","sp_ach")],"id_pnn",all = T)

colnames(df)<-c("id_pnn","s","rep_2018","pr_2018",
                "rep_2010","pr_2010","rep_2000","pr_2000",
                "rep_1990","pr_1990")


write.csv(df,"indicador_rep_species.csv")

data_plot<-data.frame(Mean_fun = c(mean(df$rep_2018,na.rm = T),
                                   mean(df$rep_2010,na.rm = T),
                                   mean(df$rep_2000,na.rm = T),
                                   mean(df$rep_1990,na.rm = T)),Year = c(2018,1990,
                                                                       2000,
                                                                       2010))
library(ggplot2)

ggplot(data_plot,aes(x=Year, 
                y=Mean_fun#, 
                #size = pop,
                #color = continent
                )) +
  geom_point(#alpha=0.5#, 
             size = 2
             ) +
  ylab("Representatividad distribucion especies") +
  xlab("")+
  theme_bw()+
  #scale_size(range = c(.1, 24), name="Population (M)")+
  theme(legend.title = element_blank(),
        legend.position="bottom",
        #legend.box = "horizontal",
        #legend.direction = "horizontal",
        axis.line.x = element_line(color="black", size = 0.5),
        axis.line.y = element_line(color="black", size = 0.5),
        axis.text=element_text(size=12),
        axis.title=element_text(size=12))+
  scale_x_continuous(#name = "Mean ozone in\nparts per billion",
    breaks = seq(1990, 2020, 10),
    limits=c(1990, 2020))+
  scale_y_continuous(#name = "Mean ozone in\nparts per billion",
                     breaks = seq(30, 42, 2),
                     limits=c(30, 42))
 
#geom_text(aes(label=ifelse(pop>1e8,as.character(country),'')),
#         hjust=0,vjust=0)
#coord_flip()