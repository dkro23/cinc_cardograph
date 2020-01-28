
#####
### Creating Cartographs from CINC scores
#####

# Load Packages

library(cshapes)
library(cartogram)
library(rgdal)
library(sf)
library(rgeos)
library(tmap)
library(maptools)
library(sp)
library(leaflet)
library(raster) 
library(png)
library(magick)

### Start with a single year

# Create map

map2000<-cshp(as.Date("2000-1-1"))
plot(map2000)
head(map2000@data)

data(wrld_simpl)
plot(wrld_simpl)
head(wrld_simpl)

# Transform Projection

crs(map2000)
crs(wrld_simpl)
crs.new<-CRS("+proj=longlat +ellps=WGS84 +datum=WGS84 +no_defs +towgs84=0,0,0")

map2000 <- spTransform(map2000, crs.new)
plot(map2000)

map2000<-st_as_sf(map2000)
map2000 <- st_transform(map2000, 3857)
head(map2000)
plot(map2000)

# Load data

cinc<-read.csv("NMC_5_0.csv")
head(cinc)

cinc<-cinc[cinc$year==2000,]

# Merge together

map2000<-merge(map2000,cinc,by.x="COWCODE",by.y="ccode")
head(map2000@data)

# Make cartogram

cinc_cartogram <- cartogram_cont(map2000, "cinc", itermax=5)

# Plot with tmap

tm_shape(map2000) + tm_borders(alpha=0.2) + tm_layout(frame=F)

tm_shape(cinc_cartogram) + tm_borders(alpha=0.5) + tm_fill(col="cinc",style = "jenks",n=5) + 
  tm_layout(frame=F)

### Create a GIF for each year 1946-2000

# Create list of maps

maps_list<-list()
for (i in seq_along(1946:2000)){
  x<-paste(1945+i,"-1-1",sep="")
  maps_list[[i]]<-cshp(as.Date(x))
  maps_list[[i]]<-st_as_sf(maps_list[[i]])
  maps_list[[i]] <- st_transform(maps_list[[i]], 3857)
}

# Merge CINC scores with each one

for (i in seq_along(1946:2000)){
  temp_cinc<-cinc[cinc$year==1945+i,]
  maps_list[[i]] <-merge(maps_list[[i]] ,temp_cinc,by.x="COWCODE",by.y="ccode")
}
head(maps_list[[23]])

# Convert to cardographs

for (i in seq_along(1946:2000)){
  maps_list[[i]] <- cartogram_cont(maps_list[[i]], "cinc", itermax=2)
}

# Create GIF

dir.create("cinc")
for (i in seq_along(1946:2000)){
  x<-tm_shape(maps_list[[i]]) + tm_borders(alpha=0.5) + tm_fill(col="cinc",style = "jenks",n=5) + 
    tm_layout(frame=F)
  tmap_save(x,paste("cinc/",1945+i,"_cinc.png",sep=""))
  
  pics<-c()
  pics[i]<-paste("cinc/",1945+i,"_cinc.png",sep="")
} # took awhile, maybe scale down if replicating

for (i in seq_along(1946:2000)){
  pics[i]<-paste("cinc/",1945+i,"_cinc.png",sep="")
} # creating vectir of file names did not work in above loop for some reason
  
GIF.convert <- function(x, fps, output){
  image_read(x) %>%
    image_animate(fps = fps) %>%
    image_write(output)
}

GIF.convert(pics,1,"cinc.gif")
