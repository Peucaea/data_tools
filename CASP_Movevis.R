library(moveVis)
library(move)
library(leaflet)
library(mapview)
library(raster)
library(sp)
library(rgdal)
library(sf)
library(ggplot2)
library(geosphere)
source("functions/data_manager.R")
source("functions/localization.R")

###EDIT THESE VALUES
infile <- "/Users/edlan/OneDrive/Desktop/data_tools/week1"
outpath <- "/Users/edlan/OneDrive/Desktop/data_tools/output"

#tags <- read.csv("/Users/edlan/OneDrive/Desktop/data_tools/TagID.csv", as.is=TRUE, na.strings=c("NA", "")) #uppercase node letters

all_data <- load_data(infile)
beep_data <- all_data[[1]][[1]]
#beep_data <- beep_data[beep_data$Time > as.POSIXct("2020-08-10"),]

#nodes <- node_file(all_data[[2]][[1]])
###looking for a file with the column names NodeId, lat, lng IN THAT ORDER
nodes <- read.csv("/Users/edlan/OneDrive/Desktop/data_tools/nodes60421.csv", as.is=TRUE, na.strings=c("NA", ""), strip.white=TRUE) #uppercase node letters
nodes <- nodes[,c("NodeId", "lat", "lng")]
nodes$NodeId <- toupper(nodes$NodeId)

beep_data <- beep_data[beep_data$NodeId %in% nodes$NodeId,] #c("326317", "326584", "3282fa", "3285ae", "3288f4")

###UNCOMMENT THESE AND FILL WITH YOUR DESIRED VALUES IF YOU WANT YOUR OUTPUT AS ONLY A SUBSET OF THE DATA
#channel <- a vector of RadioId value(s)
#tag_id <- a vector of TagId value(s)
#n_tags <- how many tags go into the "top tags"
#freq <- The interval of the added datetime variable. Any character string that would be accepted by seq.Date or seq.POSIXt

#EXAMPLE POSSIBLE VALUES
tag_id <- c("1E342D1E","2A2A522D")
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
locations <- weighted_average(freq[1], beep_data, nodes, all_data[[2]][[1]], 0, tag_id)
#multi_freq <- lapply(freq, weighted_average, beeps=beep_data, node=nodes) 
#export_locs(freq, beep_data, nodes, tag_id, outpath)
######################
n <- 3 #this is an example of filtering out locations based on a minimum number of nodes
locations <- locations[locations$unique_nodes > n,]

#locations$ID <- paste(locations$TagId, locations$freq, sep="_")
#locations <- locations[!duplicated(locations$ID),]
locations <- cbind(locations, locations@coords)
CASP_wk1<-locations@data
CASP_wk1_move<-df2move(CASP_wk1,
                    proj = "+init=epsg:4326 +proj=longlat +datum=WGS84 +no_defs +ellps=WGS84 +towgs84=0,0,0",
                    x = "avg_x", y = "avg_y", time = "freq",track_id="TagId")
view_spatial(CASP_wk1_move)
unique(timestamps(CASP_wk1_move))
timeLag(CASP_wk1_move, unit = "mins")
CASP_wk1_move <- align_move(CASP_wk1_move, res = 5, unit = "mins")
frames <- frames_spatial(CASP_wk1_move, path_colours = c("red", "blue"),
                         map_service = "osm", map_type = "terrain", alpha = 0.5))
length(frames) # number of frames
frames[[100]] # display one of the frames

