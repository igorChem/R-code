
# Livro online "R as GIS" 

# 1. INTRODUCTION 

# Basic packages


library(sp)  # classes for spatial data
library(raster)  # grids, rasters
library(rasterVis)  # raster visualisation
library(maptools)
library(rgeos)


# 2. SPATIAL VECTOR DATA (points, lines, polygons)


# retrieve occurrence data for the laurel tree (Laurus nobilis) from the Global #Biodiversity Information Facility (GBIF)

library(dismo)  # check also the nice 'rgbif' package! 
laurus <- gbif("Laurus", "nobilis")

# get data frame with spatial coordinates (points)
locs <- subset(laurus, select = c("country", "lat", "lon"))
head(locs)  # a simple data frame with coordinates


# Discard data with errors in coordinates:
locs <- subset(locs, locs$lat < 90)

#3So we have got a simple dataframe containing spatial coordinates. Let's make # these data explicitly spatial

coordinates(locs) <- c("lon", "lat")  # set spatial coordinates
plot(locs)

plot(locs, pch = 20, col = "steelblue")

library(rworldmap)
# library rworldmap provides different types of global maps, e.g:
data(coastsCoarse)
data(countriesLow)
plot(coastsCoarse, add = T)

#Mapping vectorial data (points, polygons, polylines)

#Mapping vectorial data using gmap from dismo


gbmap <- gmap(locs.gb, type = "satellite")
locs.gb.merc <- Mercator(locs.gb)  # Google Maps are in Mercator projection. 
# This function projects the points to that projection to enable mapping
plot(gbmap)











