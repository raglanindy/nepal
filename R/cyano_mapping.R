

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

lakes<-st_read(dsn='data/Nepal_hydro_lakes.shp')

wq<-read.csv("data/estimated_wq_lake_mean_cyano.csv")

c2<-st_point_on_surface(lakes)

cent <- st_as_sf(c2, coords = c("xcoord", "ycoord"))

centxy<-data.frame(st_coordinates(cent))

cent<-cbind.data.frame(cent,centxy)
cent$geometry<-NULL

coords <- cent %>%
  dplyr::select(Hylak_id,X,Y)

wq<-left_join(wq, coords, by="Hylak_id")
#wq$mean_cyano<-log10(wq$mean_cyano)
pal <- colorNumeric(palette = colorRamp(c( '#4575B4', '#FFFFBF',  '#D73027'), interpolate="linear"),
                    domain =  wq$mean_cyano)

leaflet(data = lakes, options = leafletOptions(zoomSnap = 0.25, zoomDelta=0.25)) %>%
  addScaleBar() %>%
  addLayersControl(overlayGroups = c("lakes") , baseGroups = c("Imagery","Map"), options = layersControlOptions(collapsed = FALSE))%>%
  addProviderTiles("Esri.WorldImagery", group="Imagery") %>%
  addTiles(options = providerTileOptions(noWrap = TRUE), group="Map") %>%
  addPolygons(data=lakes, group="lakes")%>%
  #addCircleMarkers(data=cent, group="cent",lng=~X, lat=~Y, radius=5)%>%
  addCircleMarkers(data=wq, lng=~X, lat=~Y,opacity = 1,weight = 1, radius = 2.5,fillOpacity = 1, fillColor = ~pal(mean_cyano), stroke = FALSE, label=~as.character(Hylak_id))%>%
  addLegend("bottomleft", pal = pal, values = wq$mean_cyano,
            #addLegend("bottomleft", pal = pal, values = ~s_summary_sd$`s_join$B2`,
            title = "Cyano",
            labFormat = labelFormat(prefix = " "),
            opacity = 1)
#%>%

  #addCircleMarkers(data=centmass1, group="cent",lng=~X1, lat=~X2, radius=5)%>%
  #saveWidget( file="nepal.html", selfcontained = TRUE)



library(ggplot2)
library(tidyverse)
library(ggspatial)

npsfm_colors <- c("A" = "steelblue", "B" = "green4", "C" = "orange", "D" = "red")
npsfm_colors_e <- c("A" = "steelblue", "B" = "green4", "C" = "yellow", "D" = "orange", "E" = "red")

p <- wq %>%

  #arrange(desc(mean_cyano)) %>%
  #mutate(nof = reorder(nof, -area)) %>%  #https://r-graphics.org/recipe-dataprep-factor-rename
  #Note using discrete catchments rather than above solution. ggspatial has different behaiour to ggplot
  ggplot() +
  annotation_map_tile(zoom = 10) +
  #geom_sf(data = lakes, fill = NA, col = "grey50") +
  #geom_sf(aes(fill = mean_cyano), lwd = 0.01) +
  #scale_fill_manual(values = npsfm_colors) +
  theme_minimal(base_size = 18) +
  #facet_grid(cols = vars(statistic_name), rows = vars(param_shortname)) +
  #facet_grid(cols = vars(statistic_display_name), rows = vars(indicator_name)) +
  theme(axis.text = element_blank(),        # remove geographic coordinates
        axis.title = element_blank(),
        axis.ticks = element_blank()
  ) +     # remove ticks
  coord_sf(ndiscr = 0)                     # remove grid in the background
p
