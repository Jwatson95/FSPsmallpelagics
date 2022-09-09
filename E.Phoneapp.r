#Extracting data for PIL from processors FSP
#@silvia Rodriguez Climent
# 01-09-2020
#---------------------------------------------------------------------##

# ===================================================--
# 0. Set directories----
# ===================================================--
rm(list=ls())
# set input, output directories
inp_dir <- file.path(getwd(), "Data/Processors/PIL/")
plot_dir <- file.path(getwd(), "Data/plots/PIL/")
out_dir <- file.path(getwd(), "Data/Output/")
list.files(inp_dir,recursive=TRUE) # recursive TRUE to see inside the different folders

# load libraries
library(ggplot2);library(data.table);library(xlsx);library(openxlsx);library(dplyr)
library(readxl);library(stringr);library(plyr);library(tidyr);library(reshape2);library(maps)
library(mapdata);library(mapproj);library(mapplots);library(lubridate);library(rgdal)
library(raster);library(GISTools);library(ggspatial);library(XLConnect);library(readxl);library(plyr)
