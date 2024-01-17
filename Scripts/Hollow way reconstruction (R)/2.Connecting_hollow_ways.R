library(sf) 
library(dplyr) 
library(sfheaders)
library(plotly)
library(igraph)
library(sfnetworks)
library(tidygraph)
library(tidyverse)
library(dbscan)
library(plyr)
library(ggplot2)
library(nngeo)
library(tibble)

# R version 4.1.0 (2021-05-18)
# Platform: x86_64-w64-mingw32/x64 (64-bit)
# Running under: Windows 10 x64 (build 19045)
# 
# Matrix products: default
# 
# locale:
#   [1] LC_COLLATE=German_Germany.1252  LC_CTYPE=German_Germany.1252    LC_MONETARY=German_Germany.1252 LC_NUMERIC=C                    LC_TIME=German_Germany.1252    
# 
# attached base packages:
#   [1] stats     graphics  grDevices utils     datasets  methods   base     
# 
# other attached packages:
#   [1] raster_3.6-3     sfheaders_0.4.0  nngeo_0.4.6      rgeos_0.5-9      forcats_0.5.2    stringr_1.4.1    purrr_0.3.4      readr_2.1.3      tidyr_1.2.0     
# [10] tibble_3.1.7     tidyverse_1.3.2  plyr_1.8.7       plotly_4.10.1    ggplot2_3.4.0    shp2graph_0-5    igraph_1.3.5     dplyr_1.0.9      sfnetworks_0.6.1
# [19] sf_1.0-7         sp_1.5-0        
# 
# loaded via a namespace (and not attached):
#   [1] httr_1.4.4          tidygraph_1.2.2     jsonlite_1.8.0      viridisLite_0.4.1   modelr_0.1.9        assertthat_0.2.1    googlesheets4_1.0.1
# [8] cellranger_1.1.0    yaml_2.3.5          pillar_1.8.1        backports_1.4.1     lattice_0.20-44     glue_1.6.2          digest_0.6.29      
# [15] rvest_1.0.3         colorspace_2.0-3    htmltools_0.5.2     pkgconfig_2.0.3     broom_1.0.1         haven_2.5.1         scales_1.2.1       
# [22] terra_1.6-17        tzdb_0.3.0          proxy_0.4-27        googledrive_2.0.0   generics_0.1.3      ellipsis_0.3.2      withr_2.5.0        
# [29] lazyeval_0.2.2      cli_3.4.1           magrittr_2.0.3      crayon_1.5.2        readxl_1.4.1        maptools_1.1-4      fs_1.5.2           
# [36] fansi_1.0.3         xml2_1.3.3          lwgeom_0.2-9        foreign_0.8-81      class_7.3-19        tools_4.1.0         data.table_1.14.2  
# [43] hms_1.1.2           nabor_0.5.0         gargle_1.2.1        lifecycle_1.0.3     munsell_0.5.0       reprex_2.0.2        compiler_4.1.0     
# [50] e1071_1.7-11        rlang_1.0.6         classInt_0.4-7      units_0.8-0         grid_4.1.0          rstudioapi_0.14     htmlwidgets_1.5.4  
# [57] crosstalk_1.2.0     gtable_0.3.1        codetools_0.2-18    DBI_1.1.3           R6_2.5.1            lubridate_1.8.0     fastmap_1.1.0      
# [64] utf8_1.2.2          KernSmooth_2.23-20  stringi_1.7.8       Rcpp_1.0.9          vctrs_0.5.2         dbplyr_2.2.1        tidyselect_1.1.2   

# Data preparation ----
## import hollow ways
hw_original <- st_read("HW_reform.shp", crs = 32637) 
hw_original$line <- seq(1, nrow(hw_original), by = 1)

## import sites
sites <- st_read("Khabur_Ane_Chris_Tuna_32637.shp", crs = 32637)
sites_sf <- st_sf(sites)

# Connect hw and sites ----
allendpoints <- st_line_sample(hw_original, sample=c(0,1))

allendpoints.coord <- st_coordinates(allendpoints)
endpoints.df <- data.frame(allendpoints.coord)

line.no <- unique(endpoints.df[,3])
angle.df <- data.frame()
for (i in 1:length(line.no)) {
  line.data <- subset(endpoints.df, endpoints.df[,3] == i)
  x1 <- line.data$X[1]
  x2 <- line.data$X[2]
  y1 <- line.data$Y[1]
  y2 <- line.data$Y[2]
  slope.l <- (y2-y1)/(x2-x1)
  angle.l <- atan(slope.l) * 180/pi
  angle_abs <- ifelse(angle.l < 0, angle.l+180, angle.l)
  tmp <- c(i, angle.l, angle_abs, x1, x2, y1, y2)
  angle.df <- rbind(angle.df, tmp)
}
names(angle.df) <- c("line.no", "angle", "angle_abs", "startX", "endX", "startY", "endY")

hw_angle <- merge(hw_original, angle.df, by.x = "line", by.y = "line.no")
crs.data <- st_crs(hw_angle)

## Main for loop ----
## set up new empty data sets
new_lines_sites <- NULL
hw_angle$site <- NA
## for every site in the sites data set... 
for(j in 1:nrow(sites_sf)) {
  print(paste("starting site number", j))
  ## ...check every hollow way
  for(jj in 1:nrow(hw_angle)){
    ## extract start point coordinates of the hollow way and convert to sf point 
    start_p <-  st_point(c(hw_angle$startX[jj], hw_angle$startY[jj]))
    start_p.geom <- st_sfc(start_p)
    start_p.geom. <- st_sf(geometry = start_p.geom)
    st_crs(start_p.geom.) <- crs.data
    
    ## extract end point coordinates of the hollow way and convert to sf point
    end_p <-  st_point(c(hw_angle$endX[jj], hw_angle$endY[jj]))
    end_p.geom <- st_sfc(end_p)
    end_p.geom. <- st_sf(geometry = end_p.geom)
    st_crs(end_p.geom.) <- crs.data
    
    ## calculate the distances between the site and the end points of the hollow
    ## way
    dist_start_site <- as.numeric(st_distance(sites_sf[j,], start_p.geom.))
    dist_end_site <- as.numeric(st_distance(sites_sf[j,], end_p.geom.))
    
    ## if the distance between one of the hollow end points and the site is 
    ## less than 1000m
    if(dist_start_site < 2000 | dist_end_site < 2000) {
      print(c(j, jj))
      ## connect the two
      new_line_site <- st_connect(sites_sf[j,], hw_angle[jj,])
      ## add site number to hollow way to be able to trace back the connections
      if (is.na (hw_angle$site[[jj]])) {
        hw_angle$site[[jj]] <- list (sites_sf$FID_1[[j]])
      }
      else {
        hw_angle$site[[jj]] <- append (hw_angle$site[[jj]], sites_sf$FID_1[[j]])
      }
      ## add the new line to the new_lines_sites data set 
      new_lines_sites <- rbind(new_lines_sites, new_line_site)
    }
  }
}

## convert to sf 
new_lines_sites <- st_sfc(new_lines_sites, crs = crs.data)
new_lines_sites_sf <- st_sf(geometry = new_lines_sites)

## write to .shp
st_write(new_lines_sites_sf, dsn = "new_lines_sites_2000m.shp", append = F)

##Network cleaning----
#after https://luukvdmeer.github.io/sfnetworks/articles/sfn02_preprocess_clean.html

##combine the two data sets of original and new lines
hw_new_lines <- rbind.fill(hw_original, new_lines_sites_sf)
hw_new_lines_sf <- st_sf(geometry = hw_new_lines)
##merge the lines as far as possible
hw_new_lines_sf_merge <- st_cast(st_line_merge(st_union(st_cast(hw_new_lines_sf, "MULTILINESTRING"))), "LINESTRING")
hw_new_lines_sf <- st_sf(geometry = hw_new_lines_sf_merge)

##round geometries to avoid having too many nodes/edges because they don't coincide exactly
st_geometry(hw_new_lines_sf) = st_geometry(hw_new_lines_sf) %>%
  lapply(function(x) round(x, 0)) %>%
  st_sfc(crs = st_crs(hw_new_lines_sf))

##create sfnetwork
net <- as_sfnetwork(hw_new_lines_sf, directed = FALSE)
autoplot(net)

##simplify network:
simple = net %>%
  ###switch to data frame for edges
  activate("edges") %>%
  ###order edges by length, i.e. make sure that all multiple edges except the 
  ###shortest one are removed
  dplyr::arrange(edge_length()) %>%
  ###remove multiple edges, i.e. edges that connect the same pair of nodes
  filter(!edge_is_multiple()) %>%
  ###remove loops
  filter(!edge_is_loop())

autoplot(simple)

##remove pseudo-nodes, i.e. nodes that are redundant because they have only 
##two edges (incoming and outgoing in directed networks)
smoothed = convert(simple, to_spatial_smooth)
autoplot(smoothed)

##MERGING ATTRIBUTES POSSIBLE FOR ALL SIMPLIFICATION METHODS AVAILABLE BUT NOT RELEVANT FOR NOW

##compress intersection structures into single nodes, i.e. very close nodes are 
##grouped together according to an algorithm (here: DBSCAN) and merged with their 
##centroid being the new node 
node_coords = smoothed %>%
  ###switch to nodes
  activate("nodes") %>%
  ###retrieve coordinates
  st_coordinates()

##Cluster the nodes with the DBSCAN spatial clustering algorithm. We set eps = 0.5 
##such that: Nodes within a distance of 0.5 from each other will be in the same cluster.
##We set minPts = 1 such that: A node is assigned a cluster even if it is the only member of that cluster.
clusters = dbscan(node_coords, eps = 4, minPts = 1)$cluster

##Add the cluster information to the nodes of the network.
clustered = smoothed %>%
  activate("nodes") %>%
  ###add new column to nodes data frame that contains the clusters
  dplyr::mutate(cls = clusters)

##to only cluster nodes that are connected, define the components of the network
clustered = clustered %>%
  dplyr::mutate(cmp = group_components())

select(clustered, cls, cmp)

##now nodes that are in the same cluster AND the same component can be contracted
##to one node
##Note that by contracting, new multiple or loop edges might emerge. Therefore, 
##add simplify = TRUE 
contracted = tidygraph::convert(
  clustered,
  to_spatial_contracted,
  cls, cmp,
  simplify = TRUE
)

autoplot(contracted)

##smooth again as new pseudo nodes might have emerged when contracting
smoothed2 = convert(contracted, to_spatial_smooth)
autoplot(smoothed2)

##AGAIN: OPTIONS TO KEEP/MERGE ATTRIBUTES AVAILABLE

##convert network to tibble with spatial = FALSE to drop geometry or spatial =
##TRUE to keep geometry 
net_tibble <- as_tibble(st_geometry(smoothed2, "edges"), spatial = TRUE)
##convert tibble to sf object and write as .shp
net_sites_sf <- st_sf(net_tibble)
#numbering the hollow ways
net_sites_sf$line <- seq(1, nrow(net_sites_sf), by = 1)
#remove unnecessary columns
net_sites_sf <- net_sites_sf[,c("line", "geometry")]

##write to .shp
st_write(net_sites_sf, "net_sites_2000m.shp", append = F)


#1. iteration ----
## Preparation---- 

##set up two new data sets, one for the start and one for the end bit of a line
hw_start <- NULL
hw_end <- NULL
##cast the lines into points 
cast_point <- st_sf(st_cast(hw_original, "POINT"))
##extract the unique line numbers
line.no_cast <- unique(cast_point$line)
##loop through the line points
for (i in 1:length(line.no_cast)) {
  ##subset the data according to the line number
  line.data_cast <- subset(cast_point, cast_point$line == i)
  ##take the first point of the line...
  p1 <- line.data_cast$geometry[1]
  ##...the second point of the line...
  p2 <- line.data_cast$geometry[2]
  ##...the second to last point of the line...
  p3 <- line.data_cast$geometry[length(line.data_cast$geometry)-1]
  ##...and the last point of the line... 
  p4 <- tail(line.data_cast$geometry, n = 1)
  ##...and store them in two different data sets
  tmp_p1_p2 <- c(i, p1, p2)
  tmp_p3_p4 <- c(i, p3, p4)
  hw_start <- rbind(hw_start, tmp_p1_p2)
  hw_end <- rbind(hw_end, tmp_p3_p4)
}

#calculate slope and angle for the lines
##select the unique line numbers
line.no <- unique(hw_start[,1])
##set up new data frame for one side of the hollow way, i.e. first two points
angle.start <- data.frame()
##loop through individual lines:
for (i in 1:length(line.no)) {
  ##choose the current line
  line.data <- subset(hw_start, hw_start[,1] == i)
  ##extract x and y coordinates for the endpoints of the lines
  x1_start <- line.data[,2][[1]][[1]]
  x2_start <- line.data[,3][[1]][[1]]
  y1_start <- line.data[,2][[1]][[2]]
  y2_start <- line.data[,3][[1]][[2]]
  ##calculate slope
  slope.l <- (y2_start-y1_start)/(x2_start-x1_start)
  ##calculate angle
  angle.l <- atan(slope.l) * 180/pi
  ##calculate absolute angle
  angle_abs <- ifelse(angle.l < 0, angle.l+180, angle.l)
  ##fill data frame with values
  tmp <- c(i, angle.l, angle_abs, x1_start, y1_start, x2_start, y2_start)
  angle.start <- rbind(angle.start, tmp)
}

names(angle.start) <- c("line.no", "angle_start", "angle_abs_start", "startX", "startY", "endX", "endY")

# same for the other side of the hollow way, i.e. last two points
line.no <- unique(hw_end[,1])
angle.end <- data.frame()
##loop through individual lines:
for (i in 1:length(line.no)) {
  ##choose the current line
  line.data <- subset(hw_end, hw_end[,1] == i)
  ##extract x and y coordinates for the endpoints of the lines
  x1_end <- line.data[,2][[1]][[1]]
  x2_end <- line.data[,3][[1]][[1]]
  y1_end <- line.data[,2][[1]][[2]]
  y2_end <- line.data[,3][[1]][[2]]
  ##calculate slope
  slope.l <- (y2_end-y1_end)/(x2_end-x1_end)
  ##calculate angle
  angle.l <- atan(slope.l) * 180/pi
  ##calculate absolute angle
  angle_abs <- ifelse(angle.l < 0, angle.l+180, angle.l)
  ##fill data frame with values
  tmp <- c(i, angle.l, angle_abs, x1_end, y1_end, x2_end, y2_end)
  angle.end <- rbind(angle.end, tmp)
}

names(angle.end) <- c("line.no", "angle_end", "angle_abs_end", "startX", "startY", "endX", "endY")

#remove lines that exist in both data sets, i.e. hollow ways that consist of 
#only two points (avoiding duplicates)
##create new empty data set 
angle.df <- data.frame()
##columns from angle.start and angle.end
angle.df <- as.data.frame(cbind(angle.start$line.no, angle.start$angle_start, angle.end$angle_end, 
                                angle.start$angle_abs_start, angle.end$angle_abs_end))
names(angle.df) <- c("line.no", "angle_start", "angle_end", "angle_abs_start", "angle_abs_end")

##add those lines that only exist in one data set to angle.df
angle.df <- angle.df %>%
  group_by() %>%
  mutate(startX = angle.start$startX,
         startY = angle.start$startY,
         endX = case_when(angle.start$startX != angle.end$endX && 
                            angle.start$endX != angle.end$startX ~ angle.end$endX,
                          TRUE ~ angle.start$endX),
         endY = case_when(angle.start$startX != angle.end$endX && 
                            angle.start$endX != angle.end$startX ~ angle.end$endY,
                          TRUE ~ angle.start$endY))

hw_angle.1 <- merge(hw_original, angle.df, by.x = "line", by.y = "line.no")
crs.data <- st_crs(hw_angle.1)

##Main for loop ----
#define angle range (e.g. 40 = +-20 degree)
angle_total <- 40
#define distance in which to look for lines to connect
d = 500
length_line <- d/(sin((180 - (angle_total*pi/180))/2))
#create new data set to contain the search areas
all_pizza_slices <- NULL
new_lines <- NULL
#for every row (line) in hw_angle
for (i in 1:nrow(hw_angle.1)) {
  #print the current line
  print(paste("starting line number ", i))
  #extract startpoint of line and convert to sf point
  start_p <-  st_point(c(hw_angle.1[i,]$startX, hw_angle.1[i,]$startY))
  start_p.geom <- st_sfc(start_p)
  start_p.geom. <- st_sf(geometry = start_p.geom)
  st_crs(start_p.geom.) <- crs.data
  
  #extract endpoint of line and convert to sf object
  end_p <-  st_point(c(hw_angle.1[i,]$endX, hw_angle.1[i,]$endY))
  end_p.geom <- st_sfc(end_p)
  end_p.geom. <- st_sf(geometry = end_p.geom)
  st_crs(end_p.geom.) <- crs.data
  
  #create buffer with radius = d around start - and endpoint
  buffer_start <- st_buffer(start_p.geom., d)
  buffer_end <- st_buffer(end_p.geom., d)
  
  ##draw a triangle from the start point with length of of adjacent and 
  ##opposite so that the buffer fits within the triangle.
  ##calculate the length of adjacent and opposite
  
  ###b = angle beta, c = angle gamma of the triangle (a = alpha = 90°)
  b <- hw_angle.1[i,]$angle_abs_start + angle_total/2
  c <- 90 - b
  ###dx = movement on x-axis, dy = movement on y-axis
  dx <- length_line * sin(c*pi/180)
  dy <- length_line * sin(b*pi/180)
  
  ###repeat for athe other point 
  b2 <- hw_angle.1[i,]$angle_abs_start - angle_total/2
  c2 <- 90 - b2
  dx2 <- length_line * sin(c2*pi/180)
  dy2 <- length_line * sin(b2*pi/180)
  
  ###repeat for end part of the line
  b3 <- hw_angle.1[i,]$angle_abs_end + angle_total/2
  c3 <- 90 - b3
  dx3 <- length_line * sin(c3*pi/180)
  dy3 <- length_line * sin(b3*pi/180)
  
  b4 <- hw_angle.1[i,]$angle_abs_end - angle_total/2
  c4 <- 90 - b4
  dx4 <- length_line * sin(c4*pi/180)
  dy4 <- length_line * sin(b4*pi/180)
  
  ###based on the orientation of the line, calculate the coordinates of the two 
  ###points to form a triangle from the start of the line and the two points to 
  ###form a triangle from the end of the line
  if (hw_angle.1[i,]$startY > hw_angle.1[i,]$endY && hw_angle.1[i,]$startX > hw_angle.1[i,]$endX |
      hw_angle.1[i,]$startY > hw_angle.1[i,]$endY && hw_angle.1[i,]$startX < hw_angle.1[i,]$endX) {
    x3 <- hw_angle.1[i,]$startX + dx
    y3 <- hw_angle.1[i,]$startY + dy
    
    point_3 <- st_point(c(x3, y3))
    point_3.geom <- st_sfc(point_3)
    point_3.geom. <- st_sf(geometry = point_3.geom)
    st_crs(point_3.geom.) <- crs.data
    
    x4 <- hw_angle.1[i,]$startX + dx2
    y4 <- hw_angle.1[i,]$startY + dy2
    
    point_4 <- st_point(c(x4, y4))
    point_4.geom <- st_sfc(point_4)
    point_4.geom. <- st_sf(geometry = point_4.geom)
    st_crs(point_4.geom.) <- crs.data
    
    x5 <- hw_angle.1[i,]$endX - dx3
    y5 <- hw_angle.1[i,]$endY - dy3
    
    point_5 <- st_point(c(x5, y5))
    point_5.geom <- st_sfc(point_5)
    point_5.geom. <- st_sf(geometry = point_5.geom)
    st_crs(point_5.geom.) <- crs.data
    
    x6 <- hw_angle.1[i,]$endX - dx4
    y6 <- hw_angle.1[i,]$endY - dy4
    
    point_6 <- st_point(c(x6, y6))
    point_6.geom <- st_sfc(point_6)
    point_6.geom. <- st_sf(geometry = point_6.geom)
    st_crs(point_6.geom.) <- crs.data
  } else if (hw_angle.1[i,]$startY < hw_angle.1[i,]$endY && hw_angle.1[i,]$startX < hw_angle.1[i,]$endX | 
             hw_angle.1[i,]$startY < hw_angle.1[i,]$endY && hw_angle.1[i,]$startX > hw_angle.1[i,]$endX) {
    x3 <- hw_angle.1[i,]$startX - dx
    y3 <- hw_angle.1[i,]$startY - dy
    
    point_3 <- st_point(c(x3, y3))
    point_3.geom <- st_sfc(point_3)
    point_3.geom. <- st_sf(geometry = point_3.geom)
    st_crs(point_3.geom.) <- crs.data
    
    x4 <- hw_angle.1[i,]$startX - dx2
    y4 <- hw_angle.1[i,]$startY - dy2
    
    point_4 <- st_point(c(x4, y4))
    point_4.geom <- st_sfc(point_4)
    point_4.geom. <- st_sf(geometry = point_4.geom)
    st_crs(point_4.geom.) <- crs.data
    
    x5 <- hw_angle.1[i,]$endX + dx3
    y5 <- hw_angle.1[i,]$endY + dy3
    
    point_5 <- st_point(c(x5, y5))
    point_5.geom <- st_sfc(point_5)
    point_5.geom. <- st_sf(geometry = point_5.geom)
    st_crs(point_5.geom.) <- crs.data
    
    x6 <- hw_angle.1[i,]$endX + dx4
    y6 <- hw_angle.1[i,]$endY + dy4
    
    point_6 <- st_point(c(x6, y6))
    point_6.geom <- st_sfc(point_6)
    point_6.geom. <- st_sf(geometry = point_6.geom)
  } 
  
  ##create linestring that encompasses the edges of the triangle at the start point
  L1 <- st_linestring(rbind(c(hw_angle.1[i,]$startX, hw_angle.1[i,]$startY), c(x3, y3), c(x4, y4), c(hw_angle.1[i,]$startX, hw_angle.1[i,]$startY)))
  L1.geom <- st_sfc(L1)
  L1.geom. <- st_sf(geometry = L1.geom)
  
  ##create polygon from the line string
  pol1 <- st_cast(L1.geom., "POLYGON")
  st_crs(pol1) <- crs.data
  
  ##same for traingle at the end point
  L2 <- st_linestring(rbind(c(hw_angle.1[i,]$endX, hw_angle.1[i,]$endY), c(x5, y5), c(x6, y6), c(hw_angle.1[i,]$endX, hw_angle.1[i,]$endY)))
  L2.geom <- st_sfc(L2)
  L2.geom. <- st_sf(geometry = L2.geom)
  
  pol2 <- st_cast(L2.geom., "POLYGON")
  st_crs(pol2) <- crs.data
  
  ##intersect buffers and polygons to get the search area in the form of a 
  ##pizza slice
  pizza_slice1 <- st_intersection(pol1, buffer_start)
  pizza_slice2 <- st_intersection(pol2, buffer_end)
  pizza_slices_i <- rbind(pizza_slice1, pizza_slice2)
  
  ##rbind the pizza slices for all lines together 
  all_pizza_slices <- rbind(all_pizza_slices, pizza_slice1, pizza_slice2)
  
  ##create data set with all hollow ways except i
  other_hw <- hw_angle.1[(hw_angle.1$line != i) ,]
  ## check which other hollow ways intersect with the pizza slices at the end of 
  ## each i
  inter_hw <- st_intersection(pizza_slices_i, other_hw)
  
  ## for every other hollow way
  for (ii in 1:nrow(other_hw)) {
    ## if it is in the search area (pizza slice) of i...
    if(other_hw$line[ii] %in% inter_hw$line) {
      ## find the shortest path between the two lines
      closest_connection <- st_nearest_points(hw_angle.1[i,], other_hw[ii,])
      ## extract the end points of the shortest path...
      closest_i <- st_point(c(closest_connection[[1]][1], closest_connection[[1]][3]))
      closest_ii <- st_point(c(closest_connection[[1]][2], closest_connection[[1]][4]))
      ## and connect them
      newLine <- st_connect(hw_angle.1[i,], other_hw[ii,])
      ## add new line to data set 
      if (exists("newLine")) {
        new_lines <- rbind(new_lines, newLine)
      }
    }
  }
}

## convert to sf 
new_lines_crs <- st_sfc(new_lines, crs = crs.data)
new_lines1_sf <- st_sf(geometry = new_lines_crs)

st_write(new_lines1_sf, "new_lines_500m_40deg_2000net.shp")

##Network cleaning----
#after https://luukvdmeer.github.io/sfnetworks/articles/sfn02_preprocess_clean.html

##combine the two data sets of original and new lines
hw_new_lines <- rbind.fill(net_sites_sf, new_lines1_sf)
hw_new_lines_sf <- st_sf(geometry = hw_new_lines)
##merge the lines as far as possible
hw_new_lines_sf_merge <- st_cast(st_line_merge(st_union(st_cast(hw_new_lines_sf, "MULTILINESTRING"))), "LINESTRING")
hw_new_lines_sf <- st_sf(geometry = hw_new_lines_sf_merge)

##round geometries to avoid having too many nodes/edges because they don't coincide exactly 
st_geometry(hw_new_lines_sf) = st_geometry(hw_new_lines_sf) %>%
  lapply(function(x) round(x, 0)) %>%
  st_sfc(crs = st_crs(hw_new_lines_sf))

##create sfnetwork
net <- as_sfnetwork(hw_new_lines_sf, directed = FALSE)
autoplot(net)

##simplify network:
simple = net %>%
  ###switch to data frame for edges
  activate("edges") %>%
  ###order edges by length, i.e. make sure that all multiple edges except the 
  ###shortest one are removed
  dplyr::arrange(edge_length()) %>%
  ###remove multiple edges, i.e. edges that connect the same pair of nodes
  filter(!edge_is_multiple()) %>%
  ###remove loops
  filter(!edge_is_loop())

autoplot(simple)

##remove pseudo-nodes, i.e. nodes that are redundant because they have only 
##two edges (incoming and outgoing in directed networks)
smoothed = convert(simple, to_spatial_smooth)
autoplot(smoothed)

##MERGING ATTRIBUTES POSSIBLE FOR ALL SIMPLIFICATION METHODS AVAILABLE BUT NOT RELEVANT FOR NOW

##compress intersection structures into single nodes, i.e. very close nodes are 
##grouped together according to an algorithm (here: DBSCAN) and merged with their 
##centroid being the new node 
node_coords = smoothed %>%
  ###switch to nodes
  activate("nodes") %>%
  ###retrieve coordinates
  st_coordinates()

##Cluster the nodes with the DBSCAN spatial clustering algorithm. We set eps = 0.5 
##such that: Nodes within a distance of 4 from each other will be in the same cluster.
##We set minPts = 1 such that: A node is assigned a cluster even if it is the only member of that cluster.
clusters = dbscan(node_coords, eps = 4, minPts = 1)$cluster

##Add the cluster information to the nodes of the network.
clustered = smoothed %>%
  activate("nodes") %>%
  ###add new column to nodes data frame that contains the clusters
  dplyr::mutate(cls = clusters)

##to only cluster nodes that are connected, define the components of the network
clustered = clustered %>%
  dplyr::mutate(cmp = group_components())

##now nodes that are in the same cluster AND the same component can be contracted
##to one node
##Note that by contracting, new multiple or loop edges might emerge. Therefore, 
##add simplify = TRUE 
contracted = convert(
  clustered,
  to_spatial_contracted,
  cls, cmp,
  simplify = TRUE
)

autoplot(contracted)

##smooth again as new pseudo nodes might have emerged when contracting
smoothed2 = convert(contracted, to_spatial_smooth)
autoplot(smoothed2)

# ##AGAIN: OPTIONS TO KEEP/MERGE ATTRIBUTES AVAILABLE

##convert network to tibble with spatial = FALSE to drop geometry or spatial =
##TRUE to keep geometry 
net_tibble <- as_tibble(st_geometry(smoothed2, "edges"), spatial = TRUE)
##convert tibble to sf object and write as .shp
net_500m_40deg_sf <- st_sf(net_tibble)

##write to .shp
st_write(net_500m_40deg_sf, "net_500m_40deg_2000net.shp", append = FALSE)

##plot
plot_ly() %>%
  add_sf(data = net_500m_40deg_sf) %>%
  add_sf(data = hw_angle.1, hoverinfo = 'text',
         text = ~paste('</br> line: ', line)) #%>%
#add_sf(data = new_lines_sf) %>%


#2. iteration----
##Main for loop ---- 
#define angle range (e.g. 20 = +-10 degree)
angle_total <- 15
#define distance in which to look for lines to connect
d = 3000
length_line <- d/(sin((180 - (angle_total*pi/180))/2))
#create new data set to contain the search areas
all_pizza_slices <- NULL
new_lines <- NULL
#for every row (line) in hw_angle
for (i in 1:nrow(hw_angle.1)) {
  #print the current line
  print(paste("starting line number ", i))
  #extract startpoint of line and convert to sf point
  start_p <-  st_point(c(hw_angle.1[i,]$startX, hw_angle.1[i,]$startY))
  start_p.geom <- st_sfc(start_p)
  start_p.geom. <- st_sf(geometry = start_p.geom)
  st_crs(start_p.geom.) <- crs.data
  
  #extract endpoint of line and convert to sf object
  end_p <-  st_point(c(hw_angle.1[i,]$endX, hw_angle.1[i,]$endY))
  end_p.geom <- st_sfc(end_p)
  end_p.geom. <- st_sf(geometry = end_p.geom)
  st_crs(end_p.geom.) <- crs.data
  
  #create buffer with radius = d around start - and endpoint
  buffer_start <- st_buffer(start_p.geom., d)
  buffer_end <- st_buffer(end_p.geom., d)
  
  ##draw a triangle from the start point with length of of adjacent and 
  ##opposite so that the buffer fits within the triangle.
  ##calculate the length of adjacent and opposite
  
  ###b = angle beta, c = angle gamma of the triangle (a = alpha = 90°)
  b <- hw_angle.1[i,]$angle_abs_start + angle_total/2
  c <- 90 - b
  ###dx = movement on x-axis, dy = movement on y-axis
  dx <- length_line * sin(c*pi/180)
  dy <- length_line * sin(b*pi/180)
  
  ###repeat for athe other point 
  b2 <- hw_angle.1[i,]$angle_abs_start - angle_total/2
  c2 <- 90 - b2
  dx2 <- length_line * sin(c2*pi/180)
  dy2 <- length_line * sin(b2*pi/180)
  
  ###repeat for end part of the line
  b3 <- hw_angle.1[i,]$angle_abs_end + angle_total/2
  c3 <- 90 - b3
  dx3 <- length_line * sin(c3*pi/180)
  dy3 <- length_line * sin(b3*pi/180)
  
  b4 <- hw_angle.1[i,]$angle_abs_end - angle_total/2
  c4 <- 90 - b4
  dx4 <- length_line * sin(c4*pi/180)
  dy4 <- length_line * sin(b4*pi/180)
  
  ###based on the orientation of the line, calculate the coordinates of the two 
  ###points to form a triangle from the start of the line and the two points to 
  ###form a triangle from the end of the line
  if (hw_angle.1[i,]$startY > hw_angle.1[i,]$endY && hw_angle.1[i,]$startX > hw_angle.1[i,]$endX |
      hw_angle.1[i,]$startY > hw_angle.1[i,]$endY && hw_angle.1[i,]$startX < hw_angle.1[i,]$endX) {
    x3 <- hw_angle.1[i,]$startX + dx
    y3 <- hw_angle.1[i,]$startY + dy
    
    point_3 <- st_point(c(x3, y3))
    point_3.geom <- st_sfc(point_3)
    point_3.geom. <- st_sf(geometry = point_3.geom)
    st_crs(point_3.geom.) <- crs.data
    
    x4 <- hw_angle.1[i,]$startX + dx2
    y4 <- hw_angle.1[i,]$startY + dy2
    
    point_4 <- st_point(c(x4, y4))
    point_4.geom <- st_sfc(point_4)
    point_4.geom. <- st_sf(geometry = point_4.geom)
    st_crs(point_4.geom.) <- crs.data
    
    x5 <- hw_angle.1[i,]$endX - dx3
    y5 <- hw_angle.1[i,]$endY - dy3
    
    point_5 <- st_point(c(x5, y5))
    point_5.geom <- st_sfc(point_5)
    point_5.geom. <- st_sf(geometry = point_5.geom)
    st_crs(point_5.geom.) <- crs.data
    
    x6 <- hw_angle.1[i,]$endX - dx4
    y6 <- hw_angle.1[i,]$endY - dy4
    
    point_6 <- st_point(c(x6, y6))
    point_6.geom <- st_sfc(point_6)
    point_6.geom. <- st_sf(geometry = point_6.geom)
    st_crs(point_6.geom.) <- crs.data
  } else if (hw_angle.1[i,]$startY < hw_angle.1[i,]$endY && hw_angle.1[i,]$startX < hw_angle.1[i,]$endX | 
             hw_angle.1[i,]$startY < hw_angle.1[i,]$endY && hw_angle.1[i,]$startX > hw_angle.1[i,]$endX) {
    x3 <- hw_angle.1[i,]$startX - dx
    y3 <- hw_angle.1[i,]$startY - dy
    
    point_3 <- st_point(c(x3, y3))
    point_3.geom <- st_sfc(point_3)
    point_3.geom. <- st_sf(geometry = point_3.geom)
    st_crs(point_3.geom.) <- crs.data
    
    x4 <- hw_angle.1[i,]$startX - dx2
    y4 <- hw_angle.1[i,]$startY - dy2
    
    point_4 <- st_point(c(x4, y4))
    point_4.geom <- st_sfc(point_4)
    point_4.geom. <- st_sf(geometry = point_4.geom)
    st_crs(point_4.geom.) <- crs.data
    
    x5 <- hw_angle.1[i,]$endX + dx3
    y5 <- hw_angle.1[i,]$endY + dy3
    
    point_5 <- st_point(c(x5, y5))
    point_5.geom <- st_sfc(point_5)
    point_5.geom. <- st_sf(geometry = point_5.geom)
    st_crs(point_5.geom.) <- crs.data
    
    x6 <- hw_angle.1[i,]$endX + dx4
    y6 <- hw_angle.1[i,]$endY + dy4
    
    point_6 <- st_point(c(x6, y6))
    point_6.geom <- st_sfc(point_6)
    point_6.geom. <- st_sf(geometry = point_6.geom)
  } 
  
  ##create linestring that encompasses the edges of the triangle at the start point
  L1 <- st_linestring(rbind(c(hw_angle.1[i,]$startX, hw_angle.1[i,]$startY), c(x3, y3), c(x4, y4), c(hw_angle.1[i,]$startX, hw_angle.1[i,]$startY)))
  L1.geom <- st_sfc(L1)
  L1.geom. <- st_sf(geometry = L1.geom)
  
  ##create polygon from the line string
  pol1 <- st_cast(L1.geom., "POLYGON")
  st_crs(pol1) <- crs.data
  
  ##same for traingle at the end point
  L2 <- st_linestring(rbind(c(hw_angle.1[i,]$endX, hw_angle.1[i,]$endY), c(x5, y5), c(x6, y6), c(hw_angle.1[i,]$endX, hw_angle.1[i,]$endY)))
  L2.geom <- st_sfc(L2)
  L2.geom. <- st_sf(geometry = L2.geom)
  
  pol2 <- st_cast(L2.geom., "POLYGON")
  st_crs(pol2) <- crs.data
  
  ##intersect buffers and polygons to get the search area in the form of a 
  ##pizza slice
  pizza_slice1 <- st_intersection(pol1, buffer_start)
  pizza_slice2 <- st_intersection(pol2, buffer_end)
  pizza_slices_i <- rbind(pizza_slice1, pizza_slice2)
  
  ##rbind the pizza slices for all lines together 
  all_pizza_slices <- rbind(all_pizza_slices, pizza_slice1, pizza_slice2)
  
  ##create data set with all hollow ways except i
  other_hw <- hw_angle.1[(hw_angle.1$line != i) ,]
  ## check which other hollow ways intersect with the pizza slices at the end of 
  ## each i
  inter_hw <- st_intersection(pizza_slices_i, other_hw)
  
  ## for every other hollow way
  for (ii in 1:nrow(other_hw)) {
    ## if it is in the search area (pizza slice) of i...
    if(other_hw$line[ii] %in% inter_hw$line) {
      ## find the shortest path between the two lines
      closest_connection <- st_nearest_points(hw_angle.1[i,], other_hw[ii,])
      ## extract the end points of the shortest path...
      closest_i <- st_point(c(closest_connection[[1]][1], closest_connection[[1]][3]))
      closest_ii <- st_point(c(closest_connection[[1]][2], closest_connection[[1]][4]))
      ## and connect them
      newLine <- st_connect(hw_angle.1[i,], other_hw[ii,])
      ## add new line to data set 
      if (exists("newLine")) {
        new_lines <- rbind(new_lines, newLine)
      }
    }
  }
}

## convert to sf 
new_lines_crs <- st_sfc(new_lines, crs = crs.data)
new_lines2_sf <- st_sf(geometry = new_lines_crs)

st_write(new_lines2_sf, "new_lines_3000m_15deg.shp")

##Network cleaning----
#after https://luukvdmeer.github.io/sfnetworks/articles/sfn02_preprocess_clean.html

##combine the two data sets of original and new lines
hw_new_lines <- rbind.fill(net_500m_40deg_sf, new_lines2_sf)
hw_new_lines_sf <- st_sf(geometry = hw_new_lines)
##merge the lines as far as possible
hw_new_lines_sf_merge <- st_cast(st_line_merge(st_union(st_cast(hw_new_lines_sf, "MULTILINESTRING"))), "LINESTRING")
hw_new_lines_sf <- st_sf(geometry = hw_new_lines_sf_merge)

##round geometries to avoid having too many nodes/edges because they don't coincide exactly
st_geometry(hw_new_lines_sf) = st_geometry(hw_new_lines_sf) %>%
  lapply(function(x) round(x, 0)) %>%
  st_sfc(crs = st_crs(hw_new_lines_sf))

##create sfnetwork
net <- as_sfnetwork(hw_new_lines_sf, directed = FALSE)
autoplot(net)

##simplify network:
simple = net %>%
  ###switch to data frame for edges
  activate("edges") %>%
  ###order edges by length, i.e. make sure that all multiple edges except the 
  ###shortest one are removed
  dplyr::arrange(edge_length()) %>%
  ###remove multiple edges, i.e. edges that connect the same pair of nodes
  filter(!edge_is_multiple()) %>%
  ###remove loops
  filter(!edge_is_loop())

autoplot(simple)

##remove pseudo-nodes, i.e. nodes that are redundant because they have only 
##two edges (incoming and outgoing in directed networks)
smoothed = convert(simple, to_spatial_smooth)
autoplot(smoothed)

##MERGING ATTRIBUTES POSSIBLE FOR ALL SIMPLIFICATION METHODS AVAILABLE BUT NOT RELEVANT FOR NOW

##compress intersection structures into single nodes, i.e. very close nodes are 
##grouped together according to an algorithm (here: DBSCAN) and merged with their 
##centroid being the new node 
node_coords = smoothed %>%
  ###switch to nodes
  activate("nodes") %>%
  ###retrieve coordinates
  st_coordinates()

##Cluster the nodes with the DBSCAN spatial clustering algorithm. We set eps = 0.5 
##such that: Nodes within a distance of 0.5 from each other will be in the same cluster.
##We set minPts = 1 such that: A node is assigned a cluster even if it is the only member of that cluster.
clusters = dbscan(node_coords, eps = 4, minPts = 1)$cluster

##Add the cluster information to the nodes of the network.
clustered = smoothed %>%
  activate("nodes") %>%
  ###add new column to nodes data frame that contains the clusters
  dplyr::mutate(cls = clusters)

##to only cluster nodes that are connected, define the components of the network
clustered = clustered %>%
  dplyr::mutate(cmp = group_components())

##now nodes that are in the same cluster AND the same component can be contracted
##to one node
##Note that by contracting, new multiple or loop edges might emerge. Therefore, 
##add simplify = TRUE 
contracted = tidygraph::convert(
  clustered,
  to_spatial_contracted,
  cls, cmp,
  simplify = TRUE
)

autoplot(contracted)

##smooth again as new pseudo nodes might have emerged when contracting
smoothed2 = convert(contracted, to_spatial_smooth)
autoplot(smoothed2)

##AGAIN: OPTIONS TO KEEP/MERGE ATTRIBUTES AVAILABLE

##convert network to tibble with spatial = FALSE to drop geometry or spatial =
##TRUE to keep geometry 
net_tibble <- as_tibble(st_geometry(smoothed2, "edges"), spatial = TRUE)
##convert tibble to sf object and write as .shp
net_3000m_15deg_sf <- st_sf(net_tibble)
#numbering the hollow ways
net_3000m_15deg_sf$line <- seq(1, nrow(net_3000m_15deg_sf), by = 1)
#remove unnecessary columns
net_3000m_15deg_sf <- net_3000m_15deg_sf[,c("line", "geometry")]

##write to .shp
st_write(net_3000m_15deg_sf, "net_3000m_15deg.shp", append = F)



#3. iteration----
##Main for loop ---- 
#define angle range (e.g. 20 = +-10 degree)
angle_total <- 10
#define distance in which to look for lines to connect
d = 5500
length_line <- d/(sin((180 - (angle_total*pi/180))/2))
#create new data set to contain the search areas
all_pizza_slices <- NULL
new_lines <- NULL
#for every row (line) in hw_angle
for (i in 1:nrow(hw_angle.1)) {
  #print the current line
  print(paste("starting line number ", i))
  #extract startpoint of line and convert to sf point
  start_p <-  st_point(c(hw_angle.1[i,]$startX, hw_angle.1[i,]$startY))
  start_p.geom <- st_sfc(start_p)
  start_p.geom. <- st_sf(geometry = start_p.geom)
  st_crs(start_p.geom.) <- crs.data
  
  #extract endpoint of line and convert to sf object
  end_p <-  st_point(c(hw_angle.1[i,]$endX, hw_angle.1[i,]$endY))
  end_p.geom <- st_sfc(end_p)
  end_p.geom. <- st_sf(geometry = end_p.geom)
  st_crs(end_p.geom.) <- crs.data
  
  #create buffer with radius = d around start - and endpoint
  buffer_start <- st_buffer(start_p.geom., d)
  buffer_end <- st_buffer(end_p.geom., d)
  
  ##draw a triangle from the start point with length of of adjacent and 
  ##opposite so that the buffer fits within the triangle.
  ##calculate the length of adjacent and opposite
  
  ###b = angle beta, c = angle gamma of the triangle (a = alpha = 90°)
  b <- hw_angle.1[i,]$angle_abs_start + angle_total/2
  c <- 90 - b
  ###dx = movement on x-axis, dy = movement on y-axis
  dx <- length_line * sin(c*pi/180)
  dy <- length_line * sin(b*pi/180)
  
  ###repeat for athe other point 
  b2 <- hw_angle.1[i,]$angle_abs_start - angle_total/2
  c2 <- 90 - b2
  dx2 <- length_line * sin(c2*pi/180)
  dy2 <- length_line * sin(b2*pi/180)
  
  ###repeat for end part of the line
  b3 <- hw_angle.1[i,]$angle_abs_end + angle_total/2
  c3 <- 90 - b3
  dx3 <- length_line * sin(c3*pi/180)
  dy3 <- length_line * sin(b3*pi/180)
  
  b4 <- hw_angle.1[i,]$angle_abs_end - angle_total/2
  c4 <- 90 - b4
  dx4 <- length_line * sin(c4*pi/180)
  dy4 <- length_line * sin(b4*pi/180)
  
  ###based on the orientation of the line, calculate the coordinates of the two 
  ###points to form a triangle from the start of the line and the two points to 
  ###form a triangle from the end of the line
  if (hw_angle.1[i,]$startY > hw_angle.1[i,]$endY && hw_angle.1[i,]$startX > hw_angle.1[i,]$endX |
      hw_angle.1[i,]$startY > hw_angle.1[i,]$endY && hw_angle.1[i,]$startX < hw_angle.1[i,]$endX) {
    x3 <- hw_angle.1[i,]$startX + dx
    y3 <- hw_angle.1[i,]$startY + dy
    
    point_3 <- st_point(c(x3, y3))
    point_3.geom <- st_sfc(point_3)
    point_3.geom. <- st_sf(geometry = point_3.geom)
    st_crs(point_3.geom.) <- crs.data
    
    x4 <- hw_angle.1[i,]$startX + dx2
    y4 <- hw_angle.1[i,]$startY + dy2
    
    point_4 <- st_point(c(x4, y4))
    point_4.geom <- st_sfc(point_4)
    point_4.geom. <- st_sf(geometry = point_4.geom)
    st_crs(point_4.geom.) <- crs.data
    
    x5 <- hw_angle.1[i,]$endX - dx3
    y5 <- hw_angle.1[i,]$endY - dy3
    
    point_5 <- st_point(c(x5, y5))
    point_5.geom <- st_sfc(point_5)
    point_5.geom. <- st_sf(geometry = point_5.geom)
    st_crs(point_5.geom.) <- crs.data
    
    x6 <- hw_angle.1[i,]$endX - dx4
    y6 <- hw_angle.1[i,]$endY - dy4
    
    point_6 <- st_point(c(x6, y6))
    point_6.geom <- st_sfc(point_6)
    point_6.geom. <- st_sf(geometry = point_6.geom)
    st_crs(point_6.geom.) <- crs.data
  } else if (hw_angle.1[i,]$startY < hw_angle.1[i,]$endY && hw_angle.1[i,]$startX < hw_angle.1[i,]$endX | 
             hw_angle.1[i,]$startY < hw_angle.1[i,]$endY && hw_angle.1[i,]$startX > hw_angle.1[i,]$endX) {
    x3 <- hw_angle.1[i,]$startX - dx
    y3 <- hw_angle.1[i,]$startY - dy
    
    point_3 <- st_point(c(x3, y3))
    point_3.geom <- st_sfc(point_3)
    point_3.geom. <- st_sf(geometry = point_3.geom)
    st_crs(point_3.geom.) <- crs.data
    
    x4 <- hw_angle.1[i,]$startX - dx2
    y4 <- hw_angle.1[i,]$startY - dy2
    
    point_4 <- st_point(c(x4, y4))
    point_4.geom <- st_sfc(point_4)
    point_4.geom. <- st_sf(geometry = point_4.geom)
    st_crs(point_4.geom.) <- crs.data
    
    x5 <- hw_angle.1[i,]$endX + dx3
    y5 <- hw_angle.1[i,]$endY + dy3
    
    point_5 <- st_point(c(x5, y5))
    point_5.geom <- st_sfc(point_5)
    point_5.geom. <- st_sf(geometry = point_5.geom)
    st_crs(point_5.geom.) <- crs.data
    
    x6 <- hw_angle.1[i,]$endX + dx4
    y6 <- hw_angle.1[i,]$endY + dy4
    
    point_6 <- st_point(c(x6, y6))
    point_6.geom <- st_sfc(point_6)
    point_6.geom. <- st_sf(geometry = point_6.geom)
  } 
  
  ##create linestring that encompasses the edges of the triangle at the start point
  L1 <- st_linestring(rbind(c(hw_angle.1[i,]$startX, hw_angle.1[i,]$startY), c(x3, y3), c(x4, y4), c(hw_angle.1[i,]$startX, hw_angle.1[i,]$startY)))
  L1.geom <- st_sfc(L1)
  L1.geom. <- st_sf(geometry = L1.geom)
  
  ##create polygon from the line string
  pol1 <- st_cast(L1.geom., "POLYGON")
  st_crs(pol1) <- crs.data
  
  ##same for traingle at the end point
  L2 <- st_linestring(rbind(c(hw_angle.1[i,]$endX, hw_angle.1[i,]$endY), c(x5, y5), c(x6, y6), c(hw_angle.1[i,]$endX, hw_angle.1[i,]$endY)))
  L2.geom <- st_sfc(L2)
  L2.geom. <- st_sf(geometry = L2.geom)
  
  pol2 <- st_cast(L2.geom., "POLYGON")
  st_crs(pol2) <- crs.data
  
  ##intersect buffers and polygons to get the search area in the form of a 
  ##pizza slice
  pizza_slice1 <- st_intersection(pol1, buffer_start)
  pizza_slice2 <- st_intersection(pol2, buffer_end)
  pizza_slices_i <- rbind(pizza_slice1, pizza_slice2)
  
  ##rbind the pizza slices for all lines together 
  all_pizza_slices <- rbind(all_pizza_slices, pizza_slice1, pizza_slice2)
  
  ##create data set with all hollow ways except i
  other_hw <- hw_angle.1[(hw_angle.1$line != i) ,]
  ## check which other hollow ways intersect with the pizza slices at the end of 
  ## each i
  inter_hw <- st_intersection(pizza_slices_i, other_hw)
  
  ## for every other hollow way
  for (ii in 1:nrow(other_hw)) {
    ## if it is in the search area (pizza slice) of i...
    if(other_hw$line[ii] %in% inter_hw$line) {
      ## find the shortest path between the two lines
      closest_connection <- st_nearest_points(hw_angle.1[i,], other_hw[ii,])
      ## extract the end points of the shortest path...
      closest_i <- st_point(c(closest_connection[[1]][1], closest_connection[[1]][3]))
      closest_ii <- st_point(c(closest_connection[[1]][2], closest_connection[[1]][4]))
      ## and connect them
      newLine <- st_connect(hw_angle.1[i,], other_hw[ii,])
      ## add new line to data set 
      if (exists("newLine")) {
        new_lines <- rbind(new_lines, newLine)
      }
    }
  }
}

## convert to sf 
new_lines_crs <- st_sfc(new_lines, crs = crs.data)
new_lines2_sf <- st_sf(geometry = new_lines_crs)

st_write(new_lines2_sf, "new_lines_5500m_10deg.shp")

##Network cleaning----
#after https://luukvdmeer.github.io/sfnetworks/articles/sfn02_preprocess_clean.html

##combine the two data sets of original and new lines
hw_new_lines <- rbind.fill(net_500m_40deg_sf, new_lines2_sf)
hw_new_lines_sf <- st_sf(geometry = hw_new_lines)
##merge the lines as far as possible
hw_new_lines_sf_merge <- st_cast(st_line_merge(st_union(st_cast(hw_new_lines_sf, "MULTILINESTRING"))), "LINESTRING")
hw_new_lines_sf <- st_sf(geometry = hw_new_lines_sf_merge)

##round geometries to avoid having too many nodes/edges because they don't coincide exactly
st_geometry(hw_new_lines_sf) = st_geometry(hw_new_lines_sf) %>%
  lapply(function(x) round(x, 0)) %>%
  st_sfc(crs = st_crs(hw_new_lines_sf))

##create sfnetwork
net <- as_sfnetwork(hw_new_lines_sf, directed = FALSE)
autoplot(net)

##simplify network:
simple = net %>%
  ###switch to data frame for edges
  activate("edges") %>%
  ###order edges by length, i.e. make sure that all multiple edges except the 
  ###shortest one are removed
  dplyr::arrange(edge_length()) %>%
  ###remove multiple edges, i.e. edges that connect the same pair of nodes
  filter(!edge_is_multiple()) %>%
  ###remove loops
  filter(!edge_is_loop())

autoplot(simple)

##remove pseudo-nodes, i.e. nodes that are redundant because they have only 
##two edges (incoming and outgoing in directed networks)
smoothed = convert(simple, to_spatial_smooth)
autoplot(smoothed)

##MERGING ATTRIBUTES POSSIBLE FOR ALL SIMPLIFICATION METHODS AVAILABLE BUT NOT RELEVANT FOR NOW

##compress intersection structures into single nodes, i.e. very close nodes are 
##grouped together according to an algorithm (here: DBSCAN) and merged with their 
##centroid being the new node 
node_coords = smoothed %>%
  ###switch to nodes
  activate("nodes") %>%
  ###retrieve coordinates
  st_coordinates()

##Cluster the nodes with the DBSCAN spatial clustering algorithm. We set eps = 0.5 
##such that: Nodes within a distance of 0.5 from each other will be in the same cluster.
##We set minPts = 1 such that: A node is assigned a cluster even if it is the only member of that cluster.
clusters = dbscan(node_coords, eps = 4, minPts = 1)$cluster

##Add the cluster information to the nodes of the network.
clustered = smoothed %>%
  activate("nodes") %>%
  ###add new column to nodes data frame that contains the clusters
  dplyr::mutate(cls = clusters)

##to only cluster nodes that are connected, define the components of the network
clustered = clustered %>%
  dplyr::mutate(cmp = group_components())

##now nodes that are in the same cluster AND the same component can be contracted
##to one node
##Note that by contracting, new multiple or loop edges might emerge. Therefore, 
##add simplify = TRUE 
contracted = tidygraph::convert(
  clustered,
  to_spatial_contracted,
  cls, cmp,
  simplify = TRUE
)

autoplot(contracted)

##smooth again as new pseudo nodes might have emerged when contracting
smoothed2 = convert(contracted, to_spatial_smooth)
autoplot(smoothed2)

##AGAIN: OPTIONS TO KEEP/MERGE ATTRIBUTES AVAILABLE

##convert network to tibble with spatial = FALSE to drop geometry or spatial =
##TRUE to keep geometry 
net_tibble <- as_tibble(st_geometry(smoothed2, "edges"), spatial = TRUE)
##convert tibble to sf object and write as .shp
net_5500m_10deg_sf <- st_sf(net_tibble)
#numbering the hollow ways
net_5500m_10deg_sf$line <- seq(1, nrow(net_5500m_10deg_sf), by = 1)
#remove unnecessary columns
net_5500m_10deg_sf <- net_5500m_10deg_sf[,c("line", "geometry")]

##write to .shp
st_write(net_5500m_10deg_sf, "net_5500m_10deg.shp", append = F)
