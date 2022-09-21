library(raster)
library(shiny)
library(shinydashboard)
library(leaflet)
library(rgdal)
library(tidyverse)
library(rgeos)
library(rsconnect)

dirName = dirname(rstudioapi::getActiveDocumentContext()$path)
setwd(dirName)


# For this purpose, specific web-based application will be developed. 
# It will enable visual inspection of the time-series of images at the parcel level, 
# including also the corresponding products, like different vegetation index images that 
# include the red-edge spectral bands, like NDVI, EVI, and Chlorophyll content. 
# Based on visual inspection, changes in the current growth status within one parcel will be identified, 
# marked and labeled. Identification implies the classification of the identified changes into previously 
# defined class of the occurrence, while the marking involves manual positioning of the changes observed 
# on the images. Finally, labeling implies associating the image with the particular occurrence. 
# As an integral part of this application, it will enable to automatically check if any deviation 
# in the crop growth occur  at the parcel through the growing process. For this purpose, the pixel 
# based time-series comparison will be conducted. Potentially, this option will speed-up the creation 
# of training data sets. 


# :::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
# Data
# :::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::



# :::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
# :::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::

source("ui.R")
source("server.R")
# runApp()
# runApp(ui=ui, server=server)
runApp(host="0.0.0.0", port=3838)



