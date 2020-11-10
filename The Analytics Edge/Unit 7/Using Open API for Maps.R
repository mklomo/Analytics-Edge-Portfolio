#Open API for Maps

#Load the required libraries
library(tidyverse)
library(maps)
library(ggmap)
library(devtools)


#lets load the data
Crime_Data <- read_csv(file = "mvt.csv")

#Working with ggmap and google map API with ggmap
chi_bb <- c(left = -87.936287,
            bottom = 41.679835,
            right = -87.447052,
            top = 42.000835)

chicago_stamen <- get_stamenmap(bbox = chi_bb,
                                zoom = 11)
ggmap(chicago_stamen) + geom_point(data = Crime_Data[1:100, ],
                                   aes(x = Longitude, y = Latitude))

LatLonCounts <- as.data.frame(table(round(Crime_Data$Longitude, 2), 
                                    round(Crime_Data$Latitude, 2)))

LatLonCounts$Long <- as.numeric(as.character(LatLonCounts$Var1))

LatLonCounts$Lat <- as.numeric(as.character(LatLonCounts$Var2))

ggmap(chicago_stamen) + geom_point(data = LatLonCounts, 
                                   aes(x = Long, y = Lat, color = Freq, size = Freq))

ggmap(chicago_stamen) + geom_point(data = LatLonCounts, 
                                   aes(x = Long, y = Lat, color = Freq, size = Freq)) +
  scale_color_gradient(low = "yellow", high = "red")


#Policing map
ggmap(chicago_stamen) + 
  geom_tile(data = LatLonCounts, 
            aes(x = Long, y = Lat, alpha = Freq), 
            fill = "red")


#Dataset
Murders_Data <- read_csv(file = "murders.csv")


#US Map
states_map <- map_data("state")

ggplot(states_map, aes(x = long, y = lat, group = group)) +
  geom_polygon(fill = "white", color ="black")
    



#Adding a new dataframe with lowercase state var
Murders_Data$region <- tolower(Murders_Data$State)

#Merging the 2 dataframes
Murder_Map <- merge(Murders_Data, states_map, by = "region")

#Murder Heatmap
ggplot(Murder_Map, 
    aes(x = long, y = lat, group = group, fill = Murders)) +
  geom_polygon(color = "black") + scale_fill_gradient(low = "black", high = "red", guide = "legend")

#Population heatmap
ggplot(Murder_Map, 
       aes(x = long, y = lat, group = group, fill = Population)) +
  geom_polygon(color = "black") + scale_fill_gradient(low = "black", high = "red", guide = "legend")


#Tabulating Murder rate
Murder_Map$Rate <- Murder_Map$Murders/Murder_Map$Population*100000

ggplot(Murder_Map, 
       aes(x = long, y = lat, group = group, fill = Rate)) +
  geom_polygon(color = "black") + scale_fill_gradient(low = "black", high = "red", guide = "legend", limits = c(0, 10))

#Gun Ownership heatmap
ggplot(Murder_Map, 
       aes(x = long, y = lat, group = group, fill = GunOwnership)) +
  geom_polygon(color = "black") + scale_fill_gradient(low = "black", high = "red", guide = "legend")
