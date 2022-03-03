

library(shiny)
library(leaflet)
# Make data with several positions
library(rgdal)
library(sp)
library(sf)
library(raster)
library(htmlwidgets)
library(webshot)
library(mapview)
library(leaflet)
library(sf)
setwd('C:/Users/MatA/OneDrive - Waikato Regional Council/r/nepal')


lakes<-st_read(dsn='Nepal_hydro_lakes.shp')

c2<-st_point_on_surface(lakes)
st_write(c2, "Nepal_hydro_lakes_cent.shp", driver="ESRI Shapefile", delete_dsn=TRUE)  # create to a shapefile
cent <- st_as_sf(c2, coords = c("xcoord", "ycoord"))

centxy<-data.frame(st_coordinates(cent))

cent<-cbind.data.frame(cent,centxy)
#plot(lakes)

leaflet(data = lakes, options = leafletOptions(zoomSnap = 0.25, zoomDelta=0.25)) %>%
  addScaleBar() %>%
  addLayersControl(overlayGroups = c("lakes") , baseGroups = c("Imagery","Map"), options = layersControlOptions(collapsed = FALSE))%>%
  addProviderTiles("Esri.WorldImagery", group="Imagery") %>%
  addTiles(options = providerTileOptions(noWrap = TRUE), group="Map") %>%
  addPolygons(data=lakes, group="lakes")%>%
  addCircleMarkers(data=cent, group="cent",lng=~X, lat=~Y, radius=5)%>%
  #addCircleMarkers(data=centmass1, group="cent",lng=~X1, lat=~X2, radius=5)%>%
  saveWidget( file="nepal.html", selfcontained = TRUE)




