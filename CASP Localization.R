library(raster)
library(sp)
library(rgdal)
library(sf)
library(ggplot2)
library(geosphere)
source("functions/data_manager.R")
source("functions/localization.R")

###EDIT THESE VALUES
infile <- "/Users/edlan/OneDrive/Desktop/data_tools/Calibration raw data"
outpath <- "/Users/edlan/OneDrive/Desktop/data_tools/output"

tags <- read.csv("/Users/edlan/OneDrive/Desktop/data_tools/TagID.csv", as.is=TRUE, na.strings=c("NA", "")) #uppercase node letters

all_data <- load_data(infile)
beep_data <- all_data[[1]][[1]]
#beep_data <- beep_data[beep_data$Time > as.POSIXct("2020-08-10"),]

#nodes <- node_file(all_data[[2]][[1]])
###looking for a file with the column names NodeId, lat, lng IN THAT ORDER
nodes <- read.csv("/Users/edlan/OneDrive/Desktop/data_tools/nodes60421.csv", as.is=TRUE, na.strings=c("NA", ""), strip.white=TRUE) #uppercase node letters
nodes <- nodes[,c("NodeId", "lat", "lng")]
nodes$NodeId <- toupper(nodes$NodeId)

beep_data <- beep_data[beep_data$NodeId %in% c("326331", "328452", "325D5F", "32B849", "3288C1", "326182","328F95", "329199","328D01","367E6A","363778","328CF4","328EFE","33CD31","36886D","365972","32B849","3288D8","368405","368851", "3645AA","365217","36563F","36885E","364BBD"),] 
###UNCOMMENT THESE AND FILL WITH YOUR DESIRED VALUES IF YOU WANT YOUR OUTPUT AS ONLY A SUBSET OF THE DATA
#channel <- a vector of RadioId value(s)
#tag_id <- a vector of TagId value(s)
#n_tags <- how many tags go into the "top tags"
#freq <- The interval of the added datetime variable. Any character string that would be accepted by seq.Date or seq.POSIXt

#EXAMPLE POSSIBLE VALUES
tag_id <- tags$TagId#c("1E342D1E","2A2A522D")
#
#channel <- c(2)
freq <- c("5 min")

max_nodes <- 4 #how many nodes should be used in the localization calculation?
df <- merge_df(beep_data, nodes, tag_id, latlng = TRUE)

resampled <- advanced_resampled_stats(beeps = beep_data, node = nodes, freq = freq[1], tag_id = tag_id)
p3 = ggplot(data=resampled, aes(x=freq, y=TagRSSI_max, group=NodeId, colour=NodeId)) +
  geom_line()

##### LOCATION METHODS########
###Example 1: Weighted Average###
locations <- weighted_average(freq[1], beep_data, nodes, all_data[[2]][[1]], 8, tag_id)
#multi_freq <- lapply(freq, weighted_average, beeps=beep_data, node=nodes) 
#export_locs(freq, beep_data, nodes, tag_id, outpath)
######################

###Example 2: Triangulation###
#calibration data frame needs column names: pt, session_id, start, end, TagId, TagLat, TagLng
#start and end need to be in UTC
calibration <- read.csv("/Users/edlan/OneDrive/Desktop/data_tools/Calibration608.csv")
calibration$start <- as.POSIXct(tags$start, tz="UTC")
calibration$end <- as.POSIXct(tags$end, tz="UTC")
calibration$TagId<-gsub(pattern = "_",x = calibration$TagId,replacement  = "")
calibrated <- calibrate(beep_data, calibration, nodes, calibrate = TRUE)
all_data <- calibrated[[1]]
relation <- relate(calibrated[[2]], calibrated[[3]], calibrated[[4]])
out <- triangulate(all_data, distance = relation)
write.csv(out,"/Users/edlan/OneDrive/Desktop/data_tools/output/Calibration527601.csv")
##############################


n <- 3 #this is an example of filtering out locations based on a minimum number of nodes
locations <- locations[locations$unique_nodes > n,]
locations <- cbind(locations, locations@coords)
locations@data <- locations@data[locations@data$TagId %in% tags$TagId,]
writeOGR(obj = locations,dsn = outpath ,layer = "CASP608MAX8",driver = "ESRI Shapefile")

#locations$ID <- paste(locations$TagId, locations$freq, sep="_")
#locations <- locations[!duplicated(locations$ID),]
locations <- cbind(locations, locations@coords)
CASP_wk1<-locations@data

time <- "1 day"
move <- as.data.table(locations)[, .(Lat = mean(avg_y), Lon = mean(avg_x), std_lat = sd(avg_y), std_lon = sd(avg_x), .N), by = .(cut(freq, time),TagId)] #V = mean(SolarVolts), , 
move$lowlat <- move$Lat - move$std_lat
move$uplat <- move$Lat + move$std_lat
move$lowlon <- move$Lon - move$std_lon
move$uplon <- move$Lon + move$std_lon
move$d <- distVincentyEllipsoid(cbind(move$lowlon, move$lowlat), cbind(move$uplon, move$uplat))
move$d <- (move$d)/1000

nodes_spatial <- nodes
coordinates(nodes_spatial) <- 3:2
crs(nodes_spatial) <- CRS("+proj=longlat +datum=WGS84") 

#boulder_df <- locations[,c("TagId","avg_x","avg_y")]
#coordinates(boulder_df) <- 2:3
#utm <- CRS(paste0("+proj=utm +zone=", locations$zone[1], "+datum=WGS84"))
#crs(boulder_df) <- utm
#boulder_df_geog <- spTransform(locations, proj4string(nodes_spatial))
my_locs <- locations[,1]
locs <- st_as_sf(my_locs)
my_nodes <- st_as_sf(nodes_spatial)

ggplot() + 
  #geom_point(data=my_locs, aes(x=long,y=lat))
  #  ggmap(ph_basemap) +
  #geom_sf(data = locs, aes(colour=TagId), inherit.aes = FALSE) + 
  geom_sf(data = my_nodes) +
  geom_text(data = nodes, aes(x=lng, y=lat, label = NodeId), size = 5)
