library(moveVis)
library(move)
library(leaflet)
library(mapview)
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

