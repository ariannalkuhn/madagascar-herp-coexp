#or try: shp<-readOGR(dsn = ".", layer = "MDG_msk_alt")

#note:
#'mg_species.csv' has all lat longs + individual IDs. Use to get mg_species_elevation
#'mg_species_elevation' has all individuals with only _population_ iDs (no individual IDs) + elevations. Use to get elevation_summary
#'elevation_summary' has single value summary of elevational details for each population

#Load necessary libraries
library(raster)
library(rworldmap)
library(rgdal)
library(maps)
library(plotrix)
library(sp)
library(ade4)
library(rgeos)
library(spatialEco)
library(elevatr)


#load raster with altitude and crop to extent
setwd("/Users/herpworld/Dropbox/final_second_submis_docs/dryad_files/10_BMA/get_elevation")
#alt=raster("alt.bil")
alt=raster("MDG_msk_alt.grd")
ext_user=extent(c(41,55,-26,-10))
alt.mg=crop(alt,ext_user)

#alt.mg is large, will take time to load,or you can just plot the extent with getMap()
plot(alt.mg)
plot(getMap(), xlim = c(46,50), ylim = c(-25,-10), asp=1)

# check that points plot onto alt map sensibly.
DataSpecies=read.csv<-read.csv("mg_species.csv")
points(DataSpecies[ , 2:3], cex = 0.7, pch = 16, col = "dodgerblue")

#extract altitude if CRS for both layers agrees
#prj_dd <- "+proj=longlat +ellps=WGS84 +datum=WGS84 +no_defs"
#sp_points <- SpatialPointsDataFrame(coords = DataSpecies[ , 2:3], data = DataSpecies, proj4string = CRS(prj_dd))
#elevation <- cbind(sp_points, alt = extract(alt.mg, sp_points[,2:3])) 
elevation <- cbind(DataSpecies, alt = extract(alt.mg, DataSpecies[,2:3])) 
data.frame(elevation)
write.csv(elevation, "mg_species_elevation_extras.csv")

#elevation summary
setwd("/Users/herpworld/Desktop/coexp_scripts/dryad/11_BMA/get_elevation")
all<-read.csv("mg_species_elevation.csv")
species<-all[,c(1,4)]

alt.sum <- aggregate(. ~ species.ID, species, function(x) c(mean = mean(x), var = var(x), range = max(x)-min(x), index = var(x)/mean(x) ))
write.csv(alt.sum, "elevation_summary.csv")

#check that species_traits matches species_elevation
setwd("/Users/herpworld/Desktop/coexp_scripts/dryad/11_BMA/arid_vs_humid_SVL_habitat/")
traits<-read.csv("MG_traits_raw_working.csv") 
nrow(traits)
setwd("/Users/herpworld/Desktop/coexp_scripts/dryad/11_BMA/get_elevation/")
elev<-read.csv("elevation_summary.csv")
nrow(elev)

x<-merge(traits,elev,by="species.ID")
nrow(x)


library(sqldf)

a1 <- data.frame(traits$species.ID)
a2 <- data.frame(elev$species.ID)


a1NotIna2 <- sqldf('SELECT * FROM a1 EXCEPT SELECT * FROM a2')
a2NotIna1 <- sqldf('SELECT * FROM a2 EXCEPT SELECT * FROM a1')

