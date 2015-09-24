###############################################################################################################
# This code is set up for Dirk Hartog Island individual plot location maps but with tweaking can produce site
# location maps for any project.
#
# Things to note:
# 1. Because it is drawing a base image/map from Google all layers must be projected to geograhic CRS. This is 
# handled within the script.
# 
# 2. Extracting the right coords for the right site to individually locate each site for each plot map was 
# problematic initially. Make sure end result is compared to plot locations in a GIS and follow the subset 
# methodollogy as presented in the for loop.
# 
# 3. PNG format is good for these maps and other raster based outputs.
# 
# 4. If going in a report or paper, ensure ggmap is appropriately cited. citation("ggmap").
# 
# 5. There are many different base image/maps available and some may be more suitable in different situations. 
# For e.g. a road type might be more applicable for urban locations. Try them out.
#
# 6. This uses a point data set. If you are using polygons a rewrite of the shape file import and handling
# needs to be done.
#
# Many thanks to Ewan Gallic http://egallic.fr/scale-bar-and-north-arrow-on-a-ggplot2-map/ for functions to
# place scale bars and north arrows in a ggplot.
#
# By Bart Huntley 24/09/2015




rm(list=ls())

#General libraries for spatial work
library(ggmap)
library(ggplot2)
library(knitr)
library(raster)
library(rgdal)
library(maptools)
library(sp)
library(plyr)
library(tools)


## DHI example

##User inputs
shp = "dhi_photo_pt_2015_MGA49_join.shp" ##Point file
shpname = file_path_sans_ext(shp)
shp.ID="Photo_pt_i"
dir="Z:\\DEC\\Dirk_Hartog_Island_Ecological_Restoration\\Working\\FieldTrip_2015"
dir2="Z:\\DOCUMENTATION\\BART\\R\\R_DEV\\maps"

setwd(dir)

##Find PROJ.4 strings for common projections in WA
EPSG <- make_EPSG()#make list of all world PROJ.4
GCS <- subset(EPSG, code==4283)[1,3]
# MGA50 <- subset(EPSG, code==28350)[1,3]
# MGA51 <- subset(EPSG, code==28351)[1,3]
# MGA49 <- subset(EPSG, code==28349)[1,3]

#read shape file
dhiSHP <- readOGR(dsn=".", layer=shpname)
#project shape file to gcs as google is in geographic
ll <- spTransform(dhiSHP, CRS(GCS))

#get centre lat/long
gc <- geocode("dirk hartog island, australia")
center <- as.numeric(gc)

#get map from google
G <- ggmap(get_googlemap(center = center, scale = 1, color = 'bw'), 
           extent = "device")#change to "panel" or "normal" to include lat/long tick marks

## There are other base maps available. Be advised that settings for text size and arrows etc may
## not be correct if not choosing a google map and will need adjusting

## Offerings from Google (NOTE color = "bw" can be added to any combo)
# G_1 <- ggmap(get_map(location = center, source = "google", maptype = "satellite", crop = FALSE))
# G_2 <- ggmap(get_map(location = center, source = "google", maptype = "terrain", crop = FALSE))
# G_3 <- ggmap(get_map(location = center, source = "google", maptype = "roadmap", crop = FALSE))
# G_4 <- ggmap(get_map(location = center, source = "google", maptype = "hybrid", crop = FALSE))

## Offerings from Stamen (NOTE color = "bw" can be added to any combo)
# S_1 <- ggmap(get_map(location = center, source = "stamen", maptype = "terrain", crop = FALSE))
# S_2 <- ggmap(get_map(location = center, source = "stamen", maptype = "watercolor", crop = FALSE))
# S_3 <- ggmap(get_map(location = center, source = "stamen", maptype = "toner", crop = FALSE))

## Offerings from Open Street Map (NOTE color = "bw" can be added to any combo)
# O_1 <- ggmap(get_map(location = center, source = "osm", crop = FALSE))




## Run the next 3 functions to create scale bar and north arrow

#--------#
# Return a list whose elements are :
# 	- rectangle : a data.frame containing the coordinates to draw the first rectangle ;
# 	- rectangle2 : a data.frame containing the coordinates to draw the second rectangle ;
# 	- legend : a data.frame containing the coordinates of the legend texts, and the texts as well.
#
# Arguments : #
#-------------#
# lon, lat : longitude and latitude of the bottom left point of the first rectangle to draw ;
# distanceLon : length of each rectangle ;
# distanceLat : width of each rectangle ;
# distanceLegend : distance between rectangles and legend texts ;
# dist.units : units of distance "km" (kilometers) (default), "nm" (nautical miles), "mi" (statute miles).
createScaleBar <- function(lon,lat,distanceLon,distanceLat,distanceLegend, dist.units = "km"){
        # First rectangle
        bottomRight <- gcDestination(lon = lon, lat = lat, bearing = 90, dist = distanceLon, dist.units = dist.units, model = "WGS84")
        
        topLeft <- gcDestination(lon = lon, lat = lat, bearing = 0, dist = distanceLat, dist.units = dist.units, model = "WGS84")
        rectangle <- cbind(lon=c(lon, lon, bottomRight[1,"long"], bottomRight[1,"long"], lon),
                           lat = c(lat, topLeft[1,"lat"], topLeft[1,"lat"],lat, lat))
        rectangle <- data.frame(rectangle, stringsAsFactors = FALSE)
        
        # Second rectangle t right of the first rectangle
        bottomRight2 <- gcDestination(lon = lon, lat = lat, bearing = 90, dist = distanceLon*2, dist.units = dist.units, model = "WGS84")
        rectangle2 <- cbind(lon = c(bottomRight[1,"long"], bottomRight[1,"long"], bottomRight2[1,"long"], bottomRight2[1,"long"], bottomRight[1,"long"]),
                            lat=c(lat, topLeft[1,"lat"], topLeft[1,"lat"], lat, lat))
        rectangle2 <- data.frame(rectangle2, stringsAsFactors = FALSE)
        
        # Now let's deal with the text
        onTop <- gcDestination(lon = lon, lat = lat, bearing = 0, dist = distanceLegend, dist.units = dist.units, model = "WGS84")
        onTop2 <- onTop3 <- onTop
        onTop2[1,"long"] <- bottomRight[1,"long"]
        onTop3[1,"long"] <- bottomRight2[1,"long"]
        
        legend <- rbind(onTop, onTop2, onTop3)
        legend <- data.frame(cbind(legend, text = c(0, distanceLon, distanceLon*2)), stringsAsFactors = FALSE, row.names = NULL)
        return(list(rectangle = rectangle, rectangle2 = rectangle2, legend = legend))
}


#--------#
# Returns a list containing :
#	- res : coordinates to draw an arrow ;
#	- coordinates of the middle of the arrow (where the "N" will be plotted).
#
# Arguments : #
#-------------#
# scaleBar : result of createScaleBar() ;
# length : desired length of the arrow ;
# distance : distance between legend rectangles and the bottom of the arrow ;
# dist.units : units of distance "km" (kilometers) (default), "nm" (nautical miles), "mi" (statute miles).
createOrientationArrow <- function(scaleBar, length, distance = 1, dist.units = "km"){
        lon <- scaleBar$rectangle2[1,1]
        lat <- scaleBar$rectangle2[1,2]
        
        # Bottom point of the arrow
        begPoint <- gcDestination(lon = lon, lat = lat, bearing = 0, dist = distance, dist.units = dist.units, model = "WGS84")
        lon <- begPoint[1,"long"]
        lat <- begPoint[1,"lat"]
        
        # Let us create the endpoint
        onTop <- gcDestination(lon = lon, lat = lat, bearing = 0, dist = length, dist.units = dist.units, model = "WGS84")
        
        leftArrow <- gcDestination(lon = onTop[1,"long"], lat = onTop[1,"lat"], bearing = 225, dist = length/5, dist.units = dist.units, model = "WGS84")
        
        rightArrow <- gcDestination(lon = onTop[1,"long"], lat = onTop[1,"lat"], bearing = 135, dist = length/5, dist.units = dist.units, model = "WGS84")
        
        res <- rbind(
                cbind(x = lon, y = lat, xend = onTop[1,"long"], yend = onTop[1,"lat"]),
                cbind(x = leftArrow[1,"long"], y = leftArrow[1,"lat"], xend = onTop[1,"long"], yend = onTop[1,"lat"]),
                cbind(x = rightArrow[1,"long"], y = rightArrow[1,"lat"], xend = onTop[1,"long"], yend = onTop[1,"lat"]))
        
        res <- as.data.frame(res, stringsAsFactors = FALSE)
        
        # Coordinates from which "N" will be plotted
        coordsN <- cbind(x = lon, y = (lat + onTop[1,"lat"])/2)
        
        return(list(res = res, coordsN = coordsN))
}


#--------#
# This function enables to draw a scale bar on a ggplot object, and optionally an orientation arrow #
# Arguments : #
#-------------#
# lon, lat : longitude and latitude of the bottom left point of the first rectangle to draw ;
# distanceLon : length of each rectangle ;
# distanceLat : width of each rectangle ;
# distanceLegend : distance between rectangles and legend texts ;
# dist.units : units of distance "km" (kilometers) (by default), "nm" (nautical miles), "mi" (statute miles) ;
# rec.fill, rec2.fill : filling colour of the rectangles (default to white, and black, resp.);
# rec.colour, rec2.colour : colour of the rectangles (default to black for both);
# legend.colour : legend colour (default to black);
# legend.size : legend size (default to 3);
# orientation : (boolean) if TRUE (default), adds an orientation arrow to the plot ;
# arrow.length : length of the arrow (default to 500 km) ;
# arrow.distance : distance between the scale bar and the bottom of the arrow (default to 300 km) ;
# arrow.North.size : size of the "N" letter (default to 6).
scaleBar <- function(lon, lat, distanceLon, distanceLat, distanceLegend, dist.unit = "km", rec.fill = "white", rec.colour = "black", rec2.fill = "black", rec2.colour = "black", legend.colour = "black", legend.size = 3, orientation = TRUE, arrow.length = 500, arrow.distance = 300, arrow.North.size = 6){
        laScaleBar <- createScaleBar(lon = lon, lat = lat, distanceLon = distanceLon, distanceLat = distanceLat, distanceLegend = distanceLegend, dist.unit = dist.unit)
        # First rectangle
        rectangle1 <- geom_polygon(data = laScaleBar$rectangle, aes(x = lon, y = lat), fill = rec.fill, colour = rec.colour)
        
        # Second rectangle
        rectangle2 <- geom_polygon(data = laScaleBar$rectangle2, aes(x = lon, y = lat), fill = rec2.fill, colour = rec2.colour)
        
        # Legend
        scaleBarLegend <- annotate("text", label = paste(laScaleBar$legend[,"text"], dist.unit, sep=""), x = laScaleBar$legend[,"long"], y = laScaleBar$legend[,"lat"], size = legend.size, colour = legend.colour)
        
        res <- list(rectangle1, rectangle2, scaleBarLegend)
        
        if(orientation){# Add an arrow pointing North
                coordsArrow <- createOrientationArrow(scaleBar = laScaleBar, length = arrow.length, distance = arrow.distance, dist.unit = dist.unit)
                arrow <- list(geom_segment(data = coordsArrow$res, aes(x = x, y = y, xend = xend, yend = yend), size = 0.05), annotate("text", label = "N", x = coordsArrow$coordsN[1,"x"], y = coordsArrow$coordsN[1,"y"], size = arrow.North.size, colour = "black"))
                res <- c(res, arrow)
        }
        return(res)
}

# set output directory
setwd(dir2)



# run loop to create png location map for each site
for(i in 1:length(ll$Photo_pt_i)){
        label.i <- ll$Photo_pt_i[i]
        dbt.i <- as.data.frame(subset(ll, ll$Photo_pt_i == label.i))#subset this way to avoid mismatch of pts to locs
        G1 <- G + geom_point(aes(x=x, y=y ),data=dbt.i, color="red", size=1.8) + 
                xlim(112.9, 113.24)+theme(plot.margin=unit(c(0,0,0,0),"mm")) +
                annotate("text", x = 113.12, y = -25.6, label = label.i, size = 2)+
                scaleBar(lon = 112.95, lat = -26.1, distanceLon = 5, distanceLat = 1, 
                         distanceLegend = 3, dist.unit = "km", arrow.length = 5, arrow.distance = 5,
                         arrow.North.size = 2, legend.size = 1)
        ggsave(paste0(label.i, "-location.png"), plot = G1, width = 1, height = 2.15)
}





