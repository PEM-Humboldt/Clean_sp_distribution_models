#clear workspace
rm(list=ls())
gc()

library(SpaDES)
library(raster)
#library(spatial.tools)
library(xfun)
library(rgdal)

setwd("C:/Data/50Reefs/Felipe/iucn")
#source function to generate biplot map
source('./codigos/codes_colombia_project/create_biplot_function.R')

#call your bird sp richness raster
setwd("C:/Data/50Reefs/Felipe/iucn")
raster_birds<-raster("./rasters_aves/aves-shp/mapas_riqueza/riqueza_aves_wgs84/riqueza_aves_endemic_threatened.tif")
#human footprint raster
human.footprint<-raster("./human_footprint/projected_resampled/IHEH_2030_wgs.tif")
#make sure they are at the same extent
newextent<-extent(Reduce(extend,list(raster_birds,human.footprint)))
raster_birds <- extend(raster_birds,newextent)

#create dataframe from bird raster
toy_df<-rasterToPoints(raster_birds)
toy_df<-as.data.frame(toy_df)
colnames(toy_df)<-c("x","y","bird_richness")
#add a column with human footprint values
toy_df<-subset(toy_df,toy_df$bird_richness !=0)
toy_df$hf<- raster::extract(human.footprint,toy_df[1:2])

toy_df<-toy_df[!is.na(toy_df$hf),]
#name columns as you wish. Make sure coordinates are in columns 1 and 2
#variable to plot in x axis must be in the third column of your df
#variable to plot in y axis must be in the fourth column of your df
colnames(toy_df)<-c("x","y","bird_richness","hf")

#run the function to create a column assigning colours to each cell (gradient based)

#the color codes are in RGB format: https://www.rapidtables.com/web/color/RGB_Color.html
#you need to divide them by 255
#everything else should be self explanatory.


uno<-biplot_values(toy_df,
                   minvaluex=0, #the minimum value of the variable you want to plot on the x axis
                   maxvaluex=100,#the minimum value of the variable you want to plot on the y axis
                   minvaluey=0,#the maximum value of the variable you want to plot on the x axis
                   maxvaluey=max(toy_df$bird_richness),#the maximum value of the variable you want to plot on the y axis
                   col.ll= c(254,241,228)/255,#vector of color that will be used in the bottom left corner of your plot
                   col.ul=c(24,174,229)/255,#vector of color that will be used in the bottom right corner of your plot
                   col.lr=c(253,116,45)/255,#vector of color that will be used in the upper left corner of your plot
                   col.ur=c(105,53,49)/255,#vector of color that will be used in the upper right corner of your plot
                   cells.x=4, #number of cells you want in the x axis to show your gradient
                   cells.y=3, #number of cells you want in the y axis to show your gradient
                   c.x = c(15,40,60,100)#if you want to enter manually the breaks in x axis.Otherwise it will create them
                   #automatically using quantiles
                   #you can also add c.y to represent breaks in y axis. Here I am using
                   #quantiles for species richness.
)

#the function produces 

#calculate the proportion of each category from the uno dataset:
head(uno[[1]])

#generate plot label
cats<-uno[[2]]

plotit<-function(n, cells.x,cells.y,cols){
  image(matrix(c(1:n), cells.x, cells.y, byrow=TRUE), col=cols, axes=F)
}

z.plot1<-function(){plotit(nrow(cats),
                           cells.x = length(unique(cats$Var2)),
                           cells.y =length(unique(cats$Var1)),
                           col = cols)}
z.plot1()
#

image_plot<-image(matrix(c(1:nrow(cats)), 
                         nrow=length(unique(cats$Var2)),
                         ncol=length(unique(cats$Var1)),
                         byrow=TRUE), col=cats$color, axes=F)

#count number of pixels for each category
allcats<-plyr::count(uno[[1]]$rastvalue)
colnames(allcats)<-c("cat","freq")
#multiply by raster resolution (in m)
allcats$area<-allcats$freq*(res(human.footprint)[[1]])^2

#get variables for each category
cats<-uno[[2]]
head(cats)
colnames(cats)<-c("rich_sp","HF","color","cat")

#merge both datasets

allcats<-merge(cats,allcats,"cat")


#create raster (values go from 1 to 16 representing all colours)
myraster<-rasterFromXYZ(uno[[1]][c(1,2,6)])

#plot(myraster)#default R colours

#assign coordinate system
crs(myraster)<-crs(human.footprint)

#save raster  (I have not assigned any coordinate reference system)
#writeRaster(myraster,"./rasters_aves/hf_asoc/forest_sp_hf_2018.tif", overwrite = TRUE)

plot(myraster)

#__________________________________________________________#
#let's create a figure with the map and the biplot together####
#__________________________________________________________#

#PREPARE DATA

cols<-unique(uno[[1]]$color)#get vector of colors from dataframe

#Get polygon Colombia
COL<-readOGR("./COL_polygon","COL_adm0")
COL<-crop(COL,extent(-81, -67, -4, 13))
library(sf)
COL<-st_as_sf(COL)

#get hillshade
hillshade<-raster("COL_polygon/hillshade/hillshade_wgs_84.tif")
hillshade

#CREATE MAPS####

#first option using rastervis and ggplot2####

library(rasterVis)
library(ggplot2)

#create base map#
mapbase<-ggplot(data = COL) +
  geom_sf(color = "black", fill = "white", aes(geometry = geometry))+
  coord_sf()+
  scale_x_continuous(breaks = seq(-82, -66,2))+
  scale_y_continuous(breaks = seq(-5, 13,2))+
  theme_bw()+
  theme(panel.grid.major = element_line(colour = "transparent")#,
        #axis.text.x = element_text(size=20)
        )#+
  #ggsn::scalebar(COL,dist_unit = "km", dist = 200, transform = TRUE,
  #               x.min = -82, x.max = -76,
  #               model = "WGS84",location = "bottomleft",st.size = 3
  #)+
  #ggsn::north(COL,scale = 0.2)

#arreglar ejes
expandy = function(mapbase, xmin, max.x,  ymin=0, max.y) {
  expand_limits(y=c(ymin, max.y),x =c(xmin, max.x))
}
mapbase<- mapbase + expandy(mapbase,-82,-66,0,13)
plot(mapbase)

#function to convert raster  to tibble (needed to add raster on top of vector)
gplot_data <- function(x, maxpixels = 500000)  {
  x <- raster::sampleRegular(x, maxpixels, asRaster = TRUE)
  coords <- raster::xyFromCell(x, seq_len(raster::ncell(x)))
  ## Extract values
  dat <- utils::stack(as.data.frame(raster::getValues(x)))
  names(dat) <- c('value', 'variable')
  
  dat <- tibble::as_tibble(data.frame(coords, dat))
  
  if (!is.null(levels(x))) {
    dat <- dplyr::left_join(dat, levels(x)[[1]],
                            by = c("value" = "ID"))
  }
  dat
}

#convert raster to tibble
gplot_raster <- gplot_data(myraster)
gplot_hillshade <- gplot_data(hillshade)


#generate maps#
mapbase +
  geom_tile(data= gplot_raster, aes(x=x,y=y,fill = value)) +
  scale_fill_gradientn(colours = cols,
                       na.value = "transparent"
  ) +
  labs(title = "",
       x = "longitude",
       y = "latitude")+
  theme(legend.position = "none")

#Second option using only plot####

###No supe c'omo anadir hillshade con una leyenda independiente para
#que los colores no se afectaran. As'i que decid'i hacerlo
#a la antigua
#hay otras opciones con ggmpa usando google earth pero me
#parece que es complicar mucho el mapa
#creo que para el primer borrador est'a bien la opci'on de arriba o esta:

COL<-readOGR("./COL_polygon","COL_adm0")
COL<-crop(COL,extent(-81, -67, -4, 13))


library(GISTools)
#library(prettymapr)

#colors hillshade

colshill<-c("orange","black")
pal <- colorRampPalette(colshill)

plot(hillshade,
     main = "Human footprint 1970",
     col = pal(20),
     axes = TRUE,
     box = FALSE,
     ext = c(-80,-65,-4.5,13),
     legend = FALSE)
# add Colombia
plot(COL, 
     add = TRUE)
plot(myraster,
     add = TRUE,
     col = cols,
     legend = FALSE,
     alpha = 1)#change alpha for more transparency (0 full transparent)
scalebar(400, xy = c(-78,-4), type = "bar", divs = 4, below = "")
north.arrow(-68,9,len = 0.3,lab='N',cex.lab=1,tcol="black")


#last option using tmap####

library(tmap)

tm_shape(hillshade)+
  tm_raster(palette = "Greys",
            legend.show = FALSE)+
tm_shape(myraster)+ 
  tm_raster(style = "cont",
            palette = cols,
            legend.show = FALSE,
            alpha = 0.7)+ 
  tm_scale_bar(position=c("left", "bottom"))
#tm_layout(outer.margins = c(.1,.1,.1,.1)) +
#you can keep adding elements using the tmap package.



