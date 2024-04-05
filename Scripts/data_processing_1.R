# Required packages
library(terra)
library(dplyr)
library(tidyverse) 
library(data.table)
library(pbapply)
library(parallel) 
library(lubridate)
library(sgsR)
library(reshape2)
library(sgsR)
library(tidyr)
library(sf)


rm(list = ls())
####################################################################
#Load 2020 Summer/Fall 2020 Landsat BAP, 2020 survey data,  and BEAST probability 2020 output


BAP <- rast("D:/FRE_490_final_project/Data/BAP2020.tif")

survey <-vect( "D:/FRE_490_final_project/Data/AOS_2020.shp" ) %>% project(BAP)

prob <- rast("D:/FRE_490_final_project/Data/run2020-probability.tif") %>% project(BAP)

#creat blank raster 
blank <- prob
values(blank) <- 1
###############################################################################
#classify disturbance layer in abiotic and biotic disturbance in new column called cat
#A = abiotic, B = biotic

bool <- substr(survey$FHF, 1, 1) == "N" 


for(i in 1:length(survey$FHF)){
 if(bool[i] == TRUE){survey$type[i] <- "abio"} else {survey$type[i] <- "bio"}}


################################################################################
#Create indices 

#Normalize Bands
co  <- (0.0000275 * BAP[[1]]) - 0.2
blu <- (0.0000275 * BAP[[2]]) - 0.2
grn <- (0.0000275 * BAP[[3]]) - 0.2
red <- (0.0000275 * BAP[[4]]) - 0.2
nir <- (0.0000275 * BAP[[5]]) - 0.2
sw1 <- (0.0000275 * BAP[[6]]) - 0.2
sw2 <- (0.0000275 * BAP[[7]]) - 0.2


#Normalized burn Ratio
nbr <- (nir - sw2)/(nir + sw2)
  
# Normalized Difference Vegetation Index
ndvi <- (nir - red)/(nir + red)

#Enhance vegetation index
evi <-  2.5 * ((nir - red) / (nir + (6 * red) - (7.5 * blu) + 1))

#Normalized Difference Moisture Index
ndmi <- (nir - sw1)/(nir + sw1)

#Tassled Cap Brightnes
tcb <- (.2043*blu) + (.4158*grn) + (.5524*red) + (.5741*nir) + (.3124*sw1) + (.2303*sw2)

#Tassled Cap Greeness
tcg <- (-.1603*blu) + (-.2819*grn) + (-.4934*red) + (.7940*nir) + (.0002*sw1) + (-.1446*sw2)

#Tassled Cap Wetness
tcw <- (.0315*blu) + (.2021*grn) + (.3102*red) + (.1594*nir) + (-.6806*sw1) + (-.6109*sw2)

#Tassled Cap angle
tca <- atan(tcg/tcb)
#############################################################################
#create raster for model building
rast <- c(prob,nbr, evi, ndvi, ndmi, tcb, tcg, tcw, tca)

names(rast) <- c("prob", "nbr", "evi" , "ndvi", "ndmi", "tcb", "tcg", "tcw", "tca")

# Get raster probability raster values great than 0.3,
# we don't want to be pick up cell in the polygon were change probably didn't happen

rast03 <- mask(rast, ifel(prob < 0.2,  NA, 1 ))

#Plot to make everything looks correct

plot(rast03)


################################################################################
#Okay now we want to prepare data for model by taking samples from both abiotic and biotic disturbances

  #Rasterize the survey data by category
  survey_cat <- rasterize(survey, blank, field = "type")
  
  abio <- ifel(survey_cat == "abio", 1, NA)
  bio <- ifel(survey_cat == "bio", 1, NA)
  
  
  abio_cells <- mask(rast03, abio)
  bio_cells <- mask(rast03, bio)
  
  set.seed(123)
  abio_sample <- sample_srs(raster = abio_cells, nSamp = 100)
  bio_sample <- sample_srs(raster = bio_cells, nSamp = 100)
  
  abio_df <- terra::extract(rast03, abio_sample)
  bio_df <- terra::extract(rast03, bio_sample)

  
  # caterorize dat 
  abio_df$type <- "abio"
  
  bio_df$type <- "bio"
  
  #final output for model building <- 
  model_df <- rbind(bio_df, abio_df)

  ##############################################################################
  #create plot  showing different distributions  of indices
  long <- melt(setDT(model_df), id.vars = c("type", "ID", "prob"), variable.name = "indice")
 
  
  ggplot(long, aes(x = indice, y = value, fill = indice )) + 
    geom_point(size = 2) +
    facet_wrap(~ type)
  
  
  
  




