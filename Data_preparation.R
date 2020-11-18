library(sf)
library(raster)
library(stars)
library(sp)
library(tidyverse)
library(magrittr)
library(dplyr)
library(rgdal)
library(ggplot2)
library(RStoolbox)
library(gridExtra)
library(viridis)

my_theme <- function(base_size = 10, base_family = "sans"){
  theme_minimal(base_size = base_size, base_family = base_family) +
    theme(
      axis.text = element_text(size = 10),
      axis.text.x = element_text(angle = 0, vjust = 0.5, hjust = 0.5),
      axis.title = element_text(size = 12),
      panel.grid.major = element_line(color = "grey"),
      panel.grid.minor = element_blank(),
      panel.background = element_rect(fill = "#fffcfc"),
      strip.background = element_rect(fill = "#820000", color = "#820000", size =0.5),
      strip.text = element_text(face = "bold", size = 10, color = "white"),
      legend.position = "bottom",
      legend.justification = "center",
      legend.background = element_blank(),
      panel.border = element_rect(color = "grey30", fill = NA, size = 0.5)
    )
}

theme_set(my_theme())
mycolors=c("#f32440","#2185ef","#d421ef")





# Parcela:: 0
wdir<- "D:/R_projects/CERES_Crop_disturbance/Podaci/0/"
setwd(wdir)
getwd()

# Parcela:: 0

files <- list.files("D:/R_projects/CERES_Crop_disturbance/Podaci/0/", full.names = TRUE,
                    pattern = ".tif")
files <- files[-(length(files))]
# files <- files[sapply(files, file.size) > 0]

# Svaki tif poseban stack
rasList <- list()
for(i in 1:length(files)){
  if(file.size(files[i]) > 0){
    rasList[[i]] <- raster::stack(files[i]) 
  } else {
    rasList[[i]] <- NULL
  }
  # rasList[[i]] <- raster::stack(files[i]) 
}

# names(rasList) <- files

# Svi kanali svih tifova u okviru jednog stack-a
rasStack <- raster::stack(files)

# Svaki tif poseban stack (radi ako se stavi files <- files[sapply(files, file.size) > 0] zato sto ima tifova bez podataka)
rasStack <- lapply(files, stack)


# 2nd layer of the 4th 'RasterStack' object, by
rasList[[4]][[2]]

plotRGB(rasList[[1]],
        stretch="lin")

# Plot 

files.names <- list.files("D:/R_projects/CERES_Crop_disturbance/Podaci/0/", full.names = F,
                          pattern = ".tif")
files.names <- files.names[-(length(files.names))]
files.names %<>% stringr::str_remove(., pattern = ".tif")

f.plot <- ggRGB(rasList[[1]], r = 7, g = 3, b = 2) +
  labs(x = "Easting [m]", y = "Northing [m]",
       title = "Sentinel 2 satellite image - Falsh color composite",
       subtitle = paste("Image: ", files.names[1]),
       caption = "UBFCE (2020) \n
       Spatial resolution 10mx10m, Teritory of the Republic of Serbia") +
  theme(line = element_blank(),
        #axis.text = element_blank(),
        #axis.title = element_blank(),
        #legend.position = "None", ###################### legend
        panel.background = element_blank())
f.plot

c.plot <- ggRGB(rasList[[1]], r = 3, g = 2, b = 1) +
  labs(x = "Easting [m]", y = "Northing [m]",
       title = "Sentinel 2 satellite image - Color composite",
       subtitle = paste("Image: ", files.names[1]),
       caption = "UBFCE (2020) \n
       Spatial resolution 10mx10m, Teritory of the Republic of Serbia") +
  theme(line = element_blank(),
        #axis.text = element_blank(),
        #axis.title = element_blank(),
        #legend.position = "None", ###################### legend
        panel.background = element_blank())
c.plot

grid.arrange(c.plot, f.plot, ncol = 2)

# FUNCTION :::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::

plotS2parcel <- function(listS2parc = listS2parc, files.names = files.names, id = 1){
  
  f.plot <- ggRGB(listS2parc[[id]], r = 7, g = 3, b = 2) +
    labs(x = "Easting [m]", y = "Northing [m]",
         title = "Sentinel 2 satellite image - Falsh color composite",
         subtitle = paste("Image: ", files.names[id]),
         caption = "UBFCE (2020) \n
       Spatial resolution 10mx10m, Teritory of the Republic of Serbia") +
    theme(line = element_blank(),
          panel.background = element_blank())
  
  c.plot <- ggRGB(listS2parc[[id]], r = 3, g = 2, b = 1) +
    labs(x = "Easting [m]", y = "Northing [m]",
         title = "Sentinel 2 satellite image - Color composite",
         subtitle = paste("Image: ", files.names[id]),
         caption = "UBFCE (2020) \n
       Spatial resolution 10mx10m, Teritory of the Republic of Serbia") +
    theme(line = element_blank(),
          panel.background = element_blank())
  
  gridCF <- grid.arrange(c.plot, f.plot, ncol = 2)
  return(gridCF)
}

id1 <- plotS2parcel(listS2parc = rasList, files.names = files.names, id = 1)
id6 <- plotS2parcel(listS2parc = rasList, files.names = files.names, id = 6)
id100 <- plotS2parcel(listS2parc = rasList, files.names = files.names, id = 100)
id150 <- plotS2parcel(listS2parc = rasList, files.names = files.names, id = 150)


# NDVI i hist sa statistikom

# (NIR - Red) / (NIR + Red)
# NIR = B08, i = 7
# RED = BO2, i = 3

# Probati sa overlay funkcijom https://www.earthdatascience.org/courses/earth-analytics/multispectral-remote-sensing-data/vegetation-indices-NDVI-in-R/

ndvi_1 <- (rasList[[1]][[7]] - rasList[[1]][[3]]) / (rasList[[1]][[7]] + rasList[[1]][[3]])

rasListNDVI <- list()
for(i in 1:length(rasList)){
  if(is.null(rasList[[i]])){
    rasListNDVI[[i]] <- NULL
  } else {
    rasListNDVI[[i]] <- (rasList[[i]][[7]] - rasList[[i]][[3]]) / (rasList[[i]][[7]] + rasList[[i]][[3]])
  }
}

# save(rasListNDVI, file = "D:/R_projects/CERES_Crop_disturbance/R/CERES_Crop_disturbance/Data/NDVI/0_NDVI.rda")
load("D:/R_projects/CERES_Crop_disturbance/R/CERES_Crop_disturbance/Data/NDVI/0_NDVI.rda")

star_pred <- st_as_stars(rasListNDVI[[1]])

ggplot()+
  geom_stars(data = star_pred)+
  #scale_fill_distiller(palette = "PuBu") +
  scale_fill_viridis(option = "D", na.value = NA, name = "NDVI: ")+
  labs(x = "Easting [m]", y = "Northing [m]",
       title = "Sentinel 2 satellite image - NDVI",
       subtitle = paste("Image: ", files.names[1]),
       caption = "UBFCE (2020) \n
       Spatial resolution 10mx10m, Teritory of the Republic of Serbia") +
  theme(line = element_blank(),
        panel.background = element_blank(), 
        legend.position = "right")


plotS2parcel <- function(listS2parc = listS2parc, listS2parcNDVI = listS2parcNDVI, files.names = files.names, id = 1){
  
  f.plot <- ggRGB(listS2parc[[id]], r = 7, g = 3, b = 2) +
    labs(x = "Easting [m]", y = "Northing [m]",
         title = "Sentinel 2 satellite image - Falsh color composite",
         subtitle = paste("Image: ", files.names[id]),
         caption = "UBFCE (2020) \n
       Spatial resolution 10mx10m, Teritory of the Republic of Serbia") +
    theme(line = element_blank(),
          panel.background = element_blank())
  
  c.plot <- ggRGB(listS2parc[[id]], r = 3, g = 2, b = 1) +
    labs(x = "Easting [m]", y = "Northing [m]",
         title = "Sentinel 2 satellite image - Color composite",
         subtitle = paste("Image: ", files.names[id]),
         caption = "UBFCE (2020) \n
       Spatial resolution 10mx10m, Teritory of the Republic of Serbia") +
    theme(line = element_blank(),
          panel.background = element_blank())
  
  star_pred <- st_as_stars(listS2parcNDVI[[id]])
  
  ndvi.plot <- ggplot()+
    geom_stars(data = star_pred)+
    scale_fill_viridis(option = "D", na.value = NA, name = "NDVI: ")+
    labs(x = "Easting [m]", y = "Northing [m]",
         title = "Sentinel 2 satellite image - NDVI",
         subtitle = paste("Image: ", files.names[id]),
         caption = "UBFCE (2020) \n
       Spatial resolution 10mx10m, Teritory of the Republic of Serbia") +
    theme(line = element_blank(),
          panel.background = element_blank(), 
          legend.position = "bottom")
  
  df <- as.data.frame(listS2parcNDVI[[id]])
  hist.plot <- ggplot(data=df, aes(layer)) + 
    geom_histogram(binwidth = 0.005,
                   col="red", 
                   aes(y = ..density..,
                       fill = ..count..)) +
    scale_fill_gradient("Count", low = "blue", high = "red")+
    stat_function(fun = dnorm, 
                  color = "black",
                  size = 1.5,
                  args = list(mean = mean(df$layer), sd = sd(df$layer)))+
    labs(x = "NDVI values", y = "Count",
         title = "Sentinel 2 satellite image - NDVI histogram",
         subtitle = paste("Image: ", files.names[id]),
         caption = "UBFCE (2020) \n
       Spatial resolution 10mx10m, Teritory of the Republic of Serbia") +
    theme(line = element_blank(),
          panel.background = element_blank(), 
          legend.position = "bottom")+
    annotate("text",x = 0.5, y = 5.5, label = paste("Mean:",round(mean(df$layer),digits = 2)))+
    annotate("text",x = 0.5, y = 5, label = paste("SD:",round(sd(df$layer),digits = 2)))
  
  gridCF <- grid.arrange(c.plot, f.plot, ndvi.plot, hist.plot, ncol = 2)
  return(gridCF)
}

id1 <- plotS2parcel(listS2parc = rasList, listS2parcNDVI = rasListNDVI, files.names = files.names, id = 1)
id6 <- plotS2parcel(listS2parc = rasList, listS2parcNDVI = rasListNDVI, files.names = files.names, id = 6)
id100 <- plotS2parcel(listS2parc = rasList, listS2parcNDVI = rasListNDVI, files.names = files.names, id = 100)
id150 <- plotS2parcel(listS2parc = rasList, listS2parcNDVI = rasListNDVI, files.names = files.names, id = 150)


# df1 <- as.data.frame(rasListNDVI[[1]])
# histogram <- ggplot(data=df1, aes(layer)) + 
#   geom_histogram(binwidth = 0.005,
#                  col="red", 
#                  aes(y = ..density..,
#                      fill = ..count..)) +
#   scale_fill_gradient("Count", low = "blue", high = "red")+
#   stat_function(fun = dnorm, 
#                 color = "black",
#                 size = 1.5,
#                 args = list(mean = mean(df1$layer), sd = sd(df1$layer)))+
#   labs(x = "NDVI values", y = "Count",
#      title = "Sentinel 2 satellite image - NDVI histogram",
#      subtitle = paste("Image: ", files.names[1]),
#      caption = "UBFCE (2020) \n
#        Spatial resolution 10mx10m, Teritory of the Republic of Serbia") +
#   theme(line = element_blank(),
#         panel.background = element_blank(), 
#         legend.position = "bottom")+
#   annotate("text",x = 0.5, y = 5.5, label = paste("Mean:",round(mean(df1$layer),digits = 2)))+
#   annotate("text",x = 0.5, y = 5, label = paste("SD:",round(sd(df1$layer),digits = 2)))
# histogram


# Mapview sa vise malih sinhronizovanih
library(mapview)
library(leafsync)

viewRGB(x = rasList[[1]], r = 3, g = 2, b = 1,  layer.name = files.names[1])
mapview(rasListNDVI[[1]], na.color = NA, layer.name = "NDVI")
# Opcija plainview:: paket, n Provides methods for plotting potentially large (raster) images
# interactively on a plain HTML canvas. In contrast to package 'mapview'
# data are plotted without background map, but data can be projected to
# any spatial coordinate reference system.

mapS2parcel <- function(listS2parc = listS2parc, listS2parcNDVI = listS2parcNDVI, files.names = files.names, id = 1){
  c.map <- viewRGB(x = listS2parc[[id]], r = 3, g = 2, b = 1,  layer.name = files.names[1], query.type = "mousemove", map.types = "Esri.WorldImagery")
  f.map <- viewRGB(x = listS2parc[[id]], r = 7, g = 3, b = 2,  layer.name = files.names[1], query.type = "mousemove", map.types = "Esri.WorldImagery")
  n.map <- mapview(listS2parcNDVI[[id]], na.color = NA, layer.name = "NDVI", query.type = "mousemove", map.types = "Esri.WorldImagery")
  l.map <- leafsync::sync(c.map, 
                          f.map,
                          n.map,
                          ncol = 2)
  return(l.map)
}


mapS2parcel(listS2parc = rasList, listS2parcNDVI = rasListNDVI, files.names = files.names, id = 1)
mapS2parcel(listS2parc = rasList, listS2parcNDVI = rasListNDVI, files.names = files.names, id = 150)


# granica parcele (centroid extenta i poligona da se podudaraju)

# productivity je dat, mz fajl
# hlorofil sracunati


# za svaki dan imati label, 
# putanju, 
# sf poligon sa promenom,



